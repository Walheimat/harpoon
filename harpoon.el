;;; harpoon.el --- Hook into major-modes -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/partial-recall
;; Version: 0.3.5

;; Keywords: frames files convenience

;;; Commentary:

;; `harpoon' is a macro to set up major-modes (mainly those of
;; programming languages) in a standardized way.
;;
;; Have a look at `harpoon-function' the main macro calls for
;; explanations of what can be configured.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Customization

(defgroup harpoon nil
  "Harpoon settings."
  :group 'harpoon)

(defcustom harpoon-tabs-prefer nil
  "Whether tabs are preferred for indentation.

`harpoon' will make sure that directory-local settings are
obeyed."
  :type 'boolean
  :group 'harpoon)

(defcustom harpoon-lsp-provider 'lsp-mode
  "The provider for LSP functionality."
  :type 'symbol
  :group 'harpoon)

(defcustom harpoon-lsp-completion-styles '(partial-completion basic)
  "The `completion-styles' used for LSP."
  :type '(repeat sybmol)
  :group 'harpoon)

(defcustom harpoon-lsp-slow-modes '()
  "Modes that have slow language servers.

For these modes completion settings will not be adjusted to be
smaller."
  :type '(repeat symbol)
  :group 'harpoon)

(defcustom harpoon-completion-provider 'corfu
  "The completion provider used."
  :type 'symbol
  :group 'harpoon)

(defcustom harpoon-checker-function 'flycheck-mode
  "The checker to use.

For example `flymake-mode' or `flycheck-mode'. If this variable
is set, the checker function will be called for all hook
functions unless `:checker' is passed symbol `disabled'."
  :type 'function
  :group 'harpoon)

(defcustom harpoon-bind-key "C-c h"
  "The key combination to use for a `major-mode' binding."
  :type 'key-sequence
  :group 'harpoon)

(defcustom harpoon-bind-name-suffix "-harpoon-bind"
  "The name to use when constructing a symbol to bind to."
  :type 'string
  :group 'harpoon)

(defcustom harpoon-bind-function 'harpoon-bind--construct
  "Function to create a symbol to bind to."
  :type 'function
  :group 'harpoon)

(defcustom harpoon-ligature-provider 'ligature
  "The library used to provide ligature support.

Currently, only `ligature' is supported."
  :type 'symbol
  :group 'harpoon)

(defcustom harpoon-log nil
  "Whether to log during macro expansion.

The logging is done to buffer `harpoon-log--buffer'."
  :type 'boolean
  :group 'harpoon)

(defcustom harpoon-suppress-warnings nil
  "Whether to suppress warnings."
  :type 'boolean
  :group 'harpoon)

(defcustom harpoon-message-prefix "}    , ﬞ   ⎠"
  "Prefix for mode messages."
  :type 'string
  :group 'harpoon)

(defcustom harpoon-whitespace 'keep
  "Whether whitespace deletion should always happen before save."
  :type '(choice
          (const :tag "Don't delete" keep)
          (const :tag "Delete" delete))
  :group 'harpoon)

;;; Faces

(defface harpoon-emphasis
  '((t (:inherit (font-lock-type-face))))
  "Face used for emphasis."
  :group 'harpoon)

;;;; Indentation

(defun harpoon-tabs--disable ()
  "Disable tabs.

Sets variable `indent-tabs-mode' to nil."
  (setq indent-tabs-mode nil))

(defun harpoon-tabs--enable ()
  "Enable tabs.

Sets tab variable `indent-tabs-mode' to t."
  (setq indent-tabs-mode t))

(defun harpoon-tabs--maybe-enable ()
  "Maybe enable tabs."
  (if harpoon-tabs-prefer
      (harpoon-tabs--enable)
    (harpoon-tabs--disable)))

;;;; Completion

(defun harpoon-completion--parse (values)
  "Parse VALUES for completion.

This is either a plist or a cons of auto delay and auto prefix.
Return list of four."
  (if (and (harpoon--plistp values '(:provider :delay :prefix)))
      (list (harpoon--maybe-plist-get values :provider harpoon-completion-provider)
            (harpoon--maybe-plist-get values :delay)
            (harpoon--maybe-plist-get values :prefix))
    (list harpoon-completion-provider
          (car values)
          (cadr values))))

;;;; Ligatures

(declare-function ligature-set-ligatures "ext:ligature.el")

(defconst harpoon-ligatures--common-ligatures
  '(
    "==" "!=" ">=" "<="        ; Comparison.
    "+=" "-=" "/=" "%=" "*="   ; Assignment.
    "||" "&&"                  ; Logical.
    "/*" "*/" "//" "/**" "**/" ; Comments.
    "++" "--"                  ; Increment/decrement.
    ">>=" "<<=" "&=" "|=" "^=" ; Bitwise assignments.
    "<<" ">>"                  ; Bitwise shifts.
    "..."                      ; Spread.
    )
  "A list of ligatures available in all programming modes.")

(defun harpoon-ligatures--set-ligatures (mode ligatures)
  "Set LIGATURES for MODE.

All ligatures in `harpoon-ligatures--common-ligatures' will be
appended to LIGATURES."
  (pcase harpoon-ligature-provider
    ('ligature
     (and-let* (((require 'ligature nil t))
                (combined (append ligatures harpoon-ligatures--common-ligatures)))

       (harpoon--log "Setting up ligatures [%s] for `%s'"
                     (string-join combined " ")
                     mode)
       (ligature-set-ligatures mode combined)))
    (_
     (user-error "Unsupported ligature provider `%s'" harpoon-ligature-provider))))

;;;; Messages

(defun harpoon-message--in-a-bottle (bottle)
  "Display a random message from the given BOTTLE after idling.

That bottle is just an array of strings.

WHALE is the string used to prefix the message with or the blue
whale by default."
  (let* ((message-log-max nil) ; Don't clutter.
         (message (nth (harpoon--biased-random (length bottle)) bottle))
         (whale (propertize harpoon-message-prefix 'face 'harpoon-emphasis)))

    (message (concat
              whale
              " "
              (propertize message 'face 'italic)))))

(defun harpoon-message (messages)
  "Show one of MESSAGES."
  (run-with-idle-timer 1 nil #'harpoon-message--in-a-bottle messages))

;;;; LSP

(defun harpoon-lsp--escape-ignore-directory (dir)
  "Escape directory DIR."
  (if (string-prefix-p "." dir)
      (concat "[/\\\\]\\" dir "\\'")
    (concat "[/\\\\]" dir "\\'")))

(defun harpoon-lsp--ignore-directory (dir &optional ignore-list)
  "Make sure DIR is ignored.

It can be either a list of strings or a single string.

The DIR is appended to IGNORE-LIST if it is non-nil; otherwise
the `harpoon-lsp-dir-ignore-list' is used."
  (let ((dirs (if (listp dir) dir (list dir)))
        (ignore-list (or ignore-list
                         (pcase harpoon-lsp-provider
                           ('lsp-mode
                            'lsp-file-watch-ignored-directories)))))

    (thread-last
      dirs
      (mapcar 'harpoon-lsp--escape-ignore-directory)
      (harpoon--append ignore-list))))

(defun harpoon-lsp--slow-server-p (mode)
  "Check if MODE is considered slow."
  (memq mode harpoon-lsp-slow-modes))

(defalias 'harpoon-slow-lsp-p 'harpoon-lsp--slow-server-p)

;;;; Helpers

(defun harpoon--maybe-plist-get (plist key &optional default)
  "Get value of KEY from PLIST (if it is one)."
  (if (and (harpoon--plistp plist)
           (plist-member plist key))
      (plist-get plist key)
    default))

(defun harpoon--plistp (plist &optional expected-keys)
  "Check if PLIST is a plist.

If optional EXPECTED-KEYS is provided, the plist must also
contain at least one expected key."
  (and-let* ((len (proper-list-p plist))
             ((zerop (% len 2))))

    (if expected-keys
        (seq-some (lambda (it) (memq it expected-keys)) plist)
      t)))

(defun harpoon--value-unless-disabled (value &optional default)
  "Return VALUE unless it is the symbol `disabled'.

If VALUE is not disabled but nil, optionally return DEFAULT."
  (unless (eq value 'disabled)
    (or value default)))

(defun harpoon--modern-emacs-p (&optional min-version)
  "Check if we're using a modern version of Emacs.

If MIN-VERSION is set to a number, verify that current version is
at or above it."
  (if min-version
      (if (and (numberp min-version) (>= min-version 28))
          (>= emacs-major-version min-version)
        (user-error "Provided minimum version not acceptable"))
    (>= emacs-major-version 28)))

(defun harpoon--append (target seq)
  "Set TARGET to it with SEQ appended.

Duplicate items are removed."
  (let ((val (symbol-value target)))

    (set target (delq nil (delete-dups (append val seq))))))

(defun harpoon--biased-random (limit &optional bias-low throws)
  "Return a biased random number using LIMIT.

The bias is the high end unless BIAS-LOW is passed. The number of
throws are 3 or THROWS."
  (let ((results (list))
        (throws (or throws 3)))

    (dotimes (_i throws)
      (push (random limit) results))

    (if bias-low
        (seq-min results)
      (seq-max results))))

(defun harpoon--safe-assoc (key alist &optional default)
  "Safely get KEY from ALIST.

If key cannot be found, return DEFAULT."
  (or (cdr-safe (assoc key alist))
      default))

;;;; Prog-like

(defvar harpoon-prog-like-hook nil
  "Commands that should be run for prog-like modes.")

(defun harpoon-prog-like ()
  "Run `prog-like-hook' functions."
  (run-hooks 'harpoon-prog-like-hook))

;;;; IO

(defun harpoon--warn (message &rest args)
  "Warn about MESSAGE.

The message is formatted using optional ARGS."
  (unless harpoon-suppress-warnings
    (let ((formatted (apply #'format (append (list message) args))))

      (display-warning 'harpoon formatted :warning))))

(defvar harpoon-log--buffer " *harpoon*")

(defun harpoon-log--insert (fmt &rest args)
  "Use ARGS to format FMT if logging is enabled."
  (when harpoon-log
    (let ((buffer (get-buffer harpoon-log--buffer))
          (inhibit-read-only t))

      (unless buffer
        (setq buffer (get-buffer-create harpoon-log--buffer))
        (with-current-buffer buffer
          (view-mode)))

      (with-current-buffer buffer

        (when (string-empty-p (buffer-string))
          (insert "Macro expansions\n"))

        (goto-char (point-max))
        (insert (apply #'format fmt args))
        (insert "\n")))))

(defun harpoon-log--insert-indented (fmt &rest args)
  "Use ARGS to format FMT but indent."
  (apply #'harpoon-log--insert (concat "\t" fmt) args))

(defalias 'harpoon--log 'harpoon-log--insert-indented)

;;;; Macro helpers

(defvar harpoon--keywords
  '(:bind
    :completion
    :whitespace
    :functions
    :ligatures
    :lsp
    :messages
    :prog-like
    :tabs
    :checker
    :flat))

(defun harpoon--safe-body (body)
  "Collect everything from BODY that's a key."
  (cl-loop for (key val)
           on body by 'cddr
           unless (memq key harpoon--keywords)
           collect key
           and collect val))

(defun harpoon--mode-name (name)
  "Get mode name for NAME."
  (if-let* ((ready (harpoon-treesit--ready-p name)))
      (harpoon-treesit--name name)
    name))

(defun harpoon--function-name (mode &optional harpoon new)
  "Get the name of the target hook for MODE.

The suffix is `-hook' unless HARPOON is t, then it is `-harpoon'.
If NEW is t, log this name as created."
  (let* ((suffix (if harpoon "harpoon" "hook"))
         (name (thread-first
                 mode
                 (harpoon--mode-name)
                 (symbol-name)
                 (concat "-" suffix)
                 (intern))))

    (when new
      (harpoon--log "Creating function named `%s' for `%s'" name mode))

    name))

;;;; Treesit

(defvar harpoon-treesit--modes '((js-mode . javascript)
                                 (typescript-mode . typescript)
                                 (c++-mode . cpp)
                                 (c-mode . c)
                                 (python-mode . python)
                                 (js-json-mode . json)
                                 (yaml-mode . yaml)
                                 (sh-mode . bash))
  "Alist mapping languages to major modes.")

(defvar harpoon-treesit--aliases '((js-mode . javascript-mode))
  "Alist mapping modes to their alias.")

(defvar harpoon-treesit--replacements '((js-json-mode . json-mode)
                                        (sh-mode . bash-mode))
  "Alist mapping modes to those replacing them.")

(defun harpoon-treesit--ready-p (name)
  "Check if treesit is available for NAME."
  (and (harpoon--modern-emacs-p 29)
       (require 'treesit nil t)
       (and (fboundp 'treesit-available-p)
            (treesit-available-p))
       (and (fboundp 'treesit-ready-p)
            (treesit-ready-p (harpoon-treesit--language name) t))))

(defun harpoon-treesit--language (name)
  "Get language for NAME."
  (harpoon--safe-assoc name harpoon-treesit--modes))

(defun harpoon-treesit--maybe-alias (name)
  "Get the potentially aliased mode name for NAME."
  (harpoon--safe-assoc name harpoon-treesit--aliases name))

(defun harpoon-treesit--maybe-replace (name)
  "Get the potentially replaced mode name for NAME."
  (harpoon--safe-assoc name harpoon-treesit--replacements name))

(defun harpoon-treesit--name (name)
  "Get treesit name for NAME."
  (let* ((name (harpoon-treesit--maybe-replace name))
         (segment (thread-first
                    name
                    (symbol-name)
                    (split-string "-mode")
                    (car))))

    (intern (concat segment "-ts-mode"))))

;;;; Binding

(defun harpoon-bind--construct (name)
  "Construct a symbol for NAME."
  (intern (concat (symbol-name name) harpoon-bind-name-suffix)))

;;;; Macros

(cl-defmacro harpoon-function
    (name
     &body
     body
     &key
     bind
     completion
     functions
     lsp
     messages
     prog-like
     tabs
     checker
     flat
     whitespace
     &allow-other-keys)
  "Create hook function for NAME.

BIND is either a symbol, t or nil. If it is a symbol, the
`harpoon-bind-key' will be bound to it. If it is t, a symbol
yielded from `harpoon-bind-function' will be bound instead.

COMPLETION is either a list of (IDLE-DELAY PREFIX-LENGTH) or a
plist with optional keys PROVIDER (defaults to
`harpoon-completion-provider'), DELAY and PREFIX. Their values
are handled based on the given provider (currently only `corfu'
is supported). See also `harpoon-lsp-completion-styles' if you
enable LSP.

FUNCTIONS is a list of functions (for example modes) that should
be called if they are bound.

LSP is either nil, t or a plist. If it's not a plist, the
defaults depend on the `harpoon-lsp-provider'.

MESSAGES is a list of strings to randomly choose from and
display when the function is run.

PROG-LIKE is either nil or t. If it's t, the created function
will run `prog-like-hook' that other function can hook into.

TABS is either nil, t, `always' or `never'. Nil (or missing)
means: do nothing. The symbol t will call
`harpoon-tabs--maybe-enable'; the symbol `always' will call
`harpoon-tabs--enable' and the symbol `never' will call
`harpoon-tabs--disable'.

CHECKER is the function to call to enable a syntax checker,
defaulting to `harpoon-checker-function'.

If FLAT is t, setup for syntax checker and completion is skipped.
This makes sense if you create a `harpoon' for a major mode that
others derive from and also have a `harpoon'.

WHITESPACE can override `harpoon-whitespace', setting either
`keep' or `delete'.

The rest of the BODY will be spliced into the hook function after
MESSAGES and TABS."
  (declare (indent defun))

  `(defun ,(harpoon--function-name name t t) ()
     ,(format "Hook into `%s'." name)
     ,@(delete
        nil
        `(;; Messages.
          ,(when messages
             (harpoon--log "Will pick random message from [%s] for `%s'"
                           (string-join messages "; ")
                           name)
             `(harpoon-message ',messages))

          ;; Indentation.
          ,@(cond
             ((equal 'never tabs)
              (harpoon--log "No tabs will be used for `%s'" name)
              '((harpoon-tabs--disable)))

             ((equal 'always tabs)
              (harpoon--log "Tabs will be used for `%s'" name)
              '((harpoon-tabs--enable)))

             ((not tabs) nil)

             (t
              (harpoon--log "Tabs will be used conditionally for `%s'" name)
              '((hack-local-variables)
                (harpoon-tabs--maybe-enable))))

          ;; Splice in the body.
          ,@(harpoon--safe-body body)

          ;; Checker.
          ,(and-let* (((not flat))
                      (checker (harpoon--value-unless-disabled checker harpoon-checker-function)))
             (harpoon--log "Setting up checker `%s' for `%s'" checker name)
             `(,checker))

          ;; LSP

          ;; Inlay hints.
          ,(when-let* ((do-show (harpoon--maybe-plist-get lsp :hints)))

             (pcase harpoon-lsp-provider
               ('lsp-mode
                `(setq-local lsp-inlay-hint-enable t))))

          ;; Activation and setting up completion styles.
          ,@(and-let* (lsp
                       (from-provider (pcase harpoon-lsp-provider
                                        ('lsp-mode
                                         'lsp-deferred)
                                        (_
                                         (harpoon--warn "Completion provider `%s' is not handled" harpoon-lsp-provider))))
                       (fun (harpoon--maybe-plist-get lsp :function from-provider)))

              (harpoon--log "Will set up LSP using function `%s' for `%s'" fun name)
              `((unless (harpoon-lsp--slow-server-p major-mode)
                  (setq-local completion-styles harpoon-lsp-completion-styles))
                (,fun)))

          ;; Format with LSP.
          ,(when-let* ((do-format (harpoon--maybe-plist-get lsp :format)))

             (pcase harpoon-lsp-provider
               ('lsp-mode
                `(add-hook 'before-save-hook 'lsp-format-buffer nil t))))

          ,(when (or (and whitespace
                          (eq 'delete whitespace))
                     (and (eq 'delete harpoon-whitespace)
                          (or (not whitespace)
                              (not (eq 'keep whitespace)))))
             `(add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

          ;; Completion.
          ,(unless flat
             (cl-destructuring-bind
                 (provider delay prefix)
                 (harpoon-completion--parse completion)

                 (pcase provider
                   ('corfu
                    (and (or delay prefix)
                         (progn (harpoon--log
                                 "Setting up `corfu' for `%s' using delay %s and prefix %s"
                                 name (or delay "default") (or prefix "default"))

                                `(setq-local ,@(delq
                                                nil
                                                `(,@(when delay `(corfu-auto-delay ,delay))
                                                  ,@(when prefix `(corfu-auto-prefix ,prefix))))))))
                   (_
                    (harpoon--warn "Unknown completion provider `%s'" provider)))))

          ;; Prog-like.
          ,(when prog-like
             (harpoon--log "Will run `prog-like-hook' for `%s'" name)
             '(run-hooks 'harpoon-prog-like-hook))

          ;; Functions.
          ,(when functions
             (harpoon--log "Setting up functions %s for `%s'" functions name)
             `(progn ,@(mapcar (lambda (it)
                                 `(when (fboundp ',it) (,it)))
                               functions)))

          ;; Binding.
          ,(when-let ((setting bind))
             (let ((key (cond
                         ((booleanp setting)
                          (funcall harpoon-bind-function name))
                         ((symbolp setting)
                          setting))))
               (harpoon--log "Binding %s to `%s' for `%s'" harpoon-bind-key key name)
               `(local-set-key
                 (kbd harpoon-bind-key)
                 ',key)))))))

(defvar harpoon-hook--sisters '((typescript-ts-mode-hook . (tsx-ts-mode-hook)))
  "Alist of sister modes.

These are modes that should work exactly like the original mode.")

(defmacro harpoon-hook (name)
  "Create the hook call for NAME."
  (let* ((hook (harpoon--function-name name))
         (harpoon (harpoon--function-name name t))
         (sisters (append (list hook)
                          (cdr-safe (assoc hook harpoon-hook--sisters)))))

    `(progn
       ,@(mapcar
          (lambda (it)
            `(add-hook ',it ',harpoon))
          sisters))))

(cl-defmacro harpoon-ligatures (name &key ligatures &allow-other-keys)
  "Set up ligatures for NAME.

LIGATURES is a list of strings that should be set using
`ligatures-set-ligatures'."
  (declare (indent defun))

  (when-let ((non-empty ligatures))

    `(harpoon-ligatures--set-ligatures ',(harpoon--mode-name name) ',ligatures)))

(cl-defmacro harpoon-lsp (name &key lsp &allow-other-keys)
  "Set up LSP for NAME.

LSP is either nil, t or a plist. If it is a plist, key
`:ignore-dirs' can be used to add additional paths to ignore."
  (when-let ((dirs (harpoon--maybe-plist-get lsp :ignore-dirs)))
    (harpoon--log "Will ignore directories %s for `%s'" dirs name)
    `(with-eval-after-load harpoon-lsp-provider
       (harpoon-lsp--ignore-directory ',(plist-get lsp :ignore-dirs)
                                     ',(plist-get lsp :dir-ignore-list)))))

(cl-defmacro harpoon-treesit (name)
  "Remap mode NAME to tree-sitter variant if possible."
  (declare (indent defun))

  (when-let* ((ready (harpoon-treesit--ready-p name))
              (mode-name name)
              (ts-mode-name (harpoon-treesit--name name)))

    (harpoon--log "Remapping `%s' to `%s'" mode-name ts-mode-name)
    `(progn
       (add-to-list 'major-mode-remap-alist ',(cons (harpoon-treesit--maybe-alias mode-name) ts-mode-name))

       (with-eval-after-load 'all-the-icons
         (defvar all-the-icons-mode-icon-alist)

         (when-let ((setting (cdr (assoc ',mode-name all-the-icons-mode-icon-alist)))
                    (name ',ts-mode-name))

           (add-to-list 'all-the-icons-mode-icon-alist (cons name setting)))))))

;;;; API

(cl-defmacro harpoon (name &rest args)
  "Hook into mode NAME.

The ARGS are a keyword plist provided to sub-macros.

See documentation of macros `harpoon-function',
`harpoon-ligatures' and `harpoon-lsp' for the available keywords."
  (declare (indent defun))

  (harpoon-log--insert "\nSetting up `%s'" name)

  `(progn
     (harpoon-function ,name ,@args)

     (harpoon-hook ,name)

     (harpoon-ligatures ,name ,@args)

     (harpoon-lsp ,name ,@args)

     (harpoon-treesit ,name)))

(defun harpoon-pop-to-logs ()
  "Switch to the log buffer."
  (interactive)

  (let ((buffer (get-buffer harpoon-log--buffer)))

    (unless buffer
      (user-error "You need to set `harpoon-log' to t first"))

    (pop-to-buffer (get-buffer harpoon-log--buffer))))

(provide 'harpoon)

;;; harpoon.el ends here
