;;; harpoon-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'harpoon nil t)

(setq harpoon-suppress-warnings t
      harpoon-completion-provider nil
      harpoon-checker-function nil
      harpoon-lsp-provider 'lsp-mode)

(ert-deftest harpoon-prog-like--runs-hook ()
  "Verify that `harpoon-prog-like' runs the eponymous hook."
  :tags '(prog-like)

  (ert-with-test-buffer (:name "prog-like")

    (add-hook 'harpoon-prog-like-hook (lambda () (insert "ran")) nil t)

    (bydi ((:spy run-hooks))
      (harpoon-prog-like)

      (bydi-was-called-with run-hooks 'harpoon-prog-like-hook))

    (should (string-match-p "ran" (buffer-string)))))

;;; Treesit

(defmacro bydi--treesit-ready-p (toggle &rest args)
  "Convenience `bydi' wrapper.

Checks if function TOGGLE actually toggles the check. Evaluates
ARGS after these checks."
  (declare (indent defun))

  `(bydi ((:sometimes harpoon--modern-emacs-p)
          (:sometimes require)
          (:sometimes treesit-available-p)
          (:sometimes treesit-ready-p)
          (:mock harpoon-treesit--language :with bydi-rf)
          (:spy fboundp))
     (bydi-when fboundp 'treesit-available-p t)
     (bydi-when fboundp 'treesit-ready-p t)

     (bydi-toggle-volatile ',toggle)

     (should-not (harpoon-treesit--ready-p 'test-mode))

     (bydi-toggle-volatile ',toggle t)

     (should (harpoon-treesit--ready-p 'test-mode))
     (bydi-was-called-n-times ,toggle 2)

     ,@args))

(ert-deftest treesit--ready-p--requires-emacs-29 ()
  "Emacs needs to be version 29 at least."
  :tags '(treesit)

  (bydi--treesit-ready-p harpoon--modern-emacs-p
    (bydi-was-called-last-with harpoon--modern-emacs-p 29)))

(ert-deftest treesit--ready-p--treesit-must-be-available ()
  "Tree-sitting must be available."
  :tags '(treesit)

  (bydi--treesit-ready-p treesit-available-p
    (bydi-was-called-n-times treesit-ready-p 1)))

(ert-deftest treesit--ready-p--language-must-be-ready ()
  "Language must be ready."
  :tags '(treesit)

  (bydi--treesit-ready-p treesit-ready-p))

(defmacro bydi--treesit-accessor (accessor var &optional defaults)
  "Test ACCESSOR using VAR.

If DEFAULTS is t, also check defaulting to key."
  `(bydi ((:spy harpoon--safe-assoc))
     (let ((,var '((test-mode . test))))

       (should (eq 'test (,accessor 'test-mode)))
       (bydi-was-called harpoon--safe-assoc)
       (when ,defaults
         (let ((,var nil))
           (should (eq 'test-mode (,accessor 'test-mode))))))))

(ert-deftest treesit--language ()
  "Gets known languages safely."
  :tags '(treesit)

  (bydi--treesit-accessor harpoon-treesit--language harpoon-treesit--modes))

(ert-deftest treesit--maybe-alias ()
  "Only returns known aliases, otherwise the passed name."
  :tags '(treesit)

  (bydi--treesit-accessor harpoon-treesit--maybe-alias harpoon-treesit--aliases t))

(ert-deftest treesit--maybe-replace ()
  "Only returns known aliases, otherwise the passed name."
  :tags '(treesit)

  (bydi--treesit-accessor harpoon-treesit--maybe-replace harpoon-treesit--replacements t))

(ert-deftest treesit--name ()
  "Returns interned symbol with ts infix."
  :tags '(treesit)

  (bydi ((:mock harpoon-treesit--maybe-replace :with bydi-rf)
         (:spy intern))

    (should (eq 'test-ts-mode (harpoon-treesit--name 'test-mode)))

    (bydi-was-called-with intern "test-ts-mode")
    (bydi-was-called harpoon-treesit--maybe-replace)))

(ert-deftest harpoon-ligatures--set-ligatures ()
  (defvar harpoon-ligatures--common-ligatures)

  (let ((harpoon-ligatures--common-ligatures '("?")))

    (bydi ((:always require)
           ligature-set-ligatures)

      (harpoon-ligatures--set-ligatures 'test-mode '("!"))

      (bydi-was-called-with ligature-set-ligatures (list 'test-mode '("!" "?")))))

  (let ((harpoon-ligature-provider 'unknown))

    (should-error (harpoon-ligatures--set-ligatures 'test-mode '("??")))))

(ert-deftest harpoon-completion--parse--defaults ()
  (let ((harpoon-completion-provider 'corfu))
    (should (equal '(corfu 2.1 4)
                   (harpoon-completion--parse (list 2.1 4))))))

(ert-deftest harpoon-completion--parse--sets-corfu-using-plist ()
  (should (equal '(corfu 0.1 5)
                 (harpoon-completion--parse '(:provider corfu :delay 0.1 :prefix 5)))))

(ert-deftest harpoon-lsp--slow-server-p ()
  (let ((harpoon-lsp-slow-modes '(test-mode)))

    (should (harpoon-lsp--slow-server-p 'test-mode))))

(ert-deftest harpoon-lsp--escape-ignore-directory ()
  (should (string= "[/\\\\]tests\\'" (harpoon-lsp--escape-ignore-directory "tests")))
  (should (string= "[/\\\\]\\.tests\\'" (harpoon-lsp--escape-ignore-directory ".tests"))))

(ert-deftest harpoon-lsp--ignore-directory ()
  (bydi (harpoon--append
         (:mock harpoon-lsp--escape-ignore-directory :with bydi-rf))

    (harpoon-lsp--ignore-directory "test")

    (bydi-was-called-with harpoon--append (list 'lsp-file-watch-ignored-directories '("test")))

    (bydi-clear-mocks)

    (harpoon-lsp--ignore-directory '("test" "best"))

    (bydi-was-called-with harpoon--append (list 'lsp-file-watch-ignored-directories '("test" "best")))))

(ert-deftest harpoon--plistp ()
  (should (harpoon--plistp '(1 2)))
  (should-not (harpoon--plistp '(1 2) '(:key)))
  (should (harpoon--plistp '(:a 1 :b 2)))
  (should-not (harpoon--plistp '(:a 1 :b 2) '(:key)))
  (should (harpoon--plistp '(:a 1 :b 2 :key 3) '(:key))))

(ert-deftest harpoon--modern-emacs-p ()
  (let ((emacs-major-version 30))
    (should (harpoon--modern-emacs-p 30)))

  (let ((emacs-major-version 29))
    (should (harpoon--modern-emacs-p)))

  (let ((emacs-major-version 28))
    (should (harpoon--modern-emacs-p)))

  (let ((emacs-major-version 27))
    (should-not (harpoon--modern-emacs-p))))

(ert-deftest harpoon--modern-emacs-p--errors-for-bad-arguments ()
  (should-error (harpoon--modern-emacs-p 27) :type 'user-error)
  (should-error (harpoon--modern-emacs-p "testing") :type 'user-error))

(ert-deftest harpoon-warn--formats ()
  (let ((harpoon-suppress-warnings nil))
    (bydi (display-warning)
      (harpoon--warn "This is a %s" "test")
      (bydi-was-called-with display-warning (list 'harpoon "This is a test" :warning)))))

(ert-deftest harpoon-log--formats ()
  (let ((harpoon-log nil))

    (harpoon-log--insert "This is a %s" "test")

    (should-not (get-buffer harpoon-log--buffer))

    (setq harpoon-log t)

    (harpoon-log--insert "This is the %s message" "first")
    (harpoon-log--insert "This %s the %s message" "will be" "second")

    (with-current-buffer (get-buffer harpoon-log--buffer)
      (should (string= (buffer-string)
                       "Macro expansions\nThis is the first message\nThis will be the second message\n")))))

(ert-deftest harpoon-pop-to-logs ()
  (bydi (pop-to-buffer)
    (harpoon--log "Make sure it exists")

    (harpoon-pop-to-logs)
    (bydi-was-called pop-to-buffer)

    (kill-buffer harpoon-log--buffer)

    (should-error (harpoon-pop-to-logs))))

(defvar test-target nil)

(ert-deftest harpoon--append--appends ()
  (let ((test-target '(a b c))
        (sequence '(d e f)))

    (harpoon--append 'test-target sequence)

    (should (equal test-target '(a b c d e f)))))

(ert-deftest harpoon--append--removes-duplicates ()
  (let ((test-target '(a b c))
        (sequence '(c d a)))

    (harpoon--append 'test-target sequence)

    (should (equal test-target '(a b c d)))))


(ert-deftest harpoon-tabs--disable--disables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode t)

    (harpoon-tabs--disable)

    (should (eq indent-tabs-mode nil))))

(ert-deftest harpoon-tabs--enable--enables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (should (eq indent-tabs-mode nil))

    (harpoon-tabs--enable)

    (should (eq indent-tabs-mode t))))

(ert-deftest harpoon-tabs--maybe-enable--enables-if-tabs-preferred ()
  (with-temp-buffer
    (setq-local harpoon-tabs-prefer t)

    (harpoon-tabs--maybe-enable)

    (should (eq indent-tabs-mode t))))

(ert-deftest harpoon-tabs--maybe-enable--disables-unless-preferred ()
  (with-temp-buffer
    (setq-local harpoon-tabs-prefer nil)

    (harpoon-tabs--maybe-enable)

    (should (eq indent-tabs-mode nil))))

(ert-deftest harpoon--biased-random ()
  (let ((vals '(1 2 3 4)))

    (bydi ((:mock random :with (lambda (_) (pop vals))))

      (should (eq (harpoon--biased-random 4) 3))

      (setq vals '(1 2 3 4))

      (should (eq (harpoon--biased-random 4 t) 1)))))


(ert-deftest harpoon-message--in-a-bottle--shows-blue-whale ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (bydi ((:mock message :with bydi-rf))

      (should (string-equal (harpoon-message--in-a-bottle bottle) "}    , ﬞ   ⎠ Sting is playing bass, yeah")))))

(ert-deftest harpoon-message--in-a-bottle--shows-passed-string ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (bydi ((:mock message :with bydi-rf))

      (let ((harpoon-message-prefix "}< ,.__)"))

        (should (string-equal (harpoon-message--in-a-bottle bottle) "}< ,.__) Sting is playing bass, yeah"))))))

(ert-deftest harpoon-message ()
  (bydi run-with-idle-timer
    (harpoon-message nil)
    (bydi-was-called run-with-idle-timer)))

(ert-deftest harpoon-treesit--ready-p ()
  (defvar harpoon-treesit--alist)
  (bydi ((:always harpoon-modern-emacs-p)
         (:always require)
         (:always treesit-available-p)
         (:mock treesit-ready-p :with (lambda (it &rest _) (equal 'testable it))))

    (let ((harpoon-treesit--modes '((test-mode . testable) (zest-mode . zestable))))

      (should (harpoon-treesit--ready-p 'test-mode))
      (should-not (harpoon-treesit--ready-p 'zest-mode))
      (should-not (harpoon-treesit--ready-p 'no-mapping-mode)))))

(ert-deftest harpoon ()
  (bydi-match-expansion
   (harpoon test-mode
     :messages ("Just testing")
     :lsp t
     :tabs t)
   `(progn
      (harpoon-function test-mode
        :messages ("Just testing")
        :lsp t
        :tabs t)

      (harpoon-hook test-mode)

      (harpoon-ligatures test-mode
        :messages ("Just testing")
        :lsp t
        :tabs t)

      (harpoon-lsp test-mode
       :messages ("Just testing")
       :lsp t
       :tabs t)

      (harpoon-treesit test-mode))))

(ert-deftest harpoon-function ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp t)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (unless (harpoon-lsp--slow-server-p major-mode)
        (setq-local completion-styles harpoon-lsp-completion-styles))
      (lsp-deferred))))

(ert-deftest harpoon-function--custom ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp (:function eglot-ensure))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (unless (harpoon-lsp--slow-server-p major-mode)
          (setq-local completion-styles harpoon-lsp-completion-styles))
        (eglot-ensure))))

(ert-deftest harpoon-function--some-symbol ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :tabs anything)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (hack-local-variables)
      (harpoon-tabs--maybe-enable))))

(ert-deftest harpoon-function--enable-indent ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :tabs always)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (harpoon-tabs--enable))))

(ert-deftest harpoon-function--no-tabs ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp nil
     :tabs never)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (harpoon-tabs--disable))))

(ert-deftest harpoon-function--prog-like ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :prog-like t
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (message "hi")
      (run-hooks 'harpoon-prog-like-hook))))

(ert-deftest harpoon-function--bind ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :bind t)
   '(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (local-set-key
       (kbd harpoon-bind-key)
       'test-mode-harpoon-bind))))

(ert-deftest harpoon-function--bind-symbol ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :bind test-fun)
   '(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (local-set-key
       (kbd harpoon-bind-key)
       'test-fun))))

(ert-deftest harpoon-function--checker ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :checker flycheck-mode
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (message "hi")
      (flycheck-mode)))

  (let ((harpoon-checker-function 'flymake-mode))

    (bydi-match-expansion
     (harpoon-function test-mode
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (message "hi")
        (flymake-mode)))

    (bydi-match-expansion
     (harpoon-function test-mode
       :checker disabled
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (message "hi")))))

(ert-deftest harpoon-function--flat ()
  (let ((harpoon-checker-function 'flymake-mode)
        (harpoon-completion-provider 'corfu))

    (bydi-match-expansion
     (harpoon-function test-mode
       :flat t
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (message "hi")))))

(ert-deftest harpoon-function--missing-provider ()
  :tags '(completion)

  (let ((harpoon-lsp-provider 'none))

    (bydi ((:spy harpoon--warn))
      (bydi-match-expansion
       (harpoon-function test-mode
         :messages ("Just testing")
         :lsp t)
       `(defun test-mode-harpoon ()
          "Hook into `test-mode'."
          (harpoon-message '("Just testing"))))

      (bydi-was-called harpoon--warn))))

(ert-deftest harpoon-function--lsp-formatting ()
  :tags '(lsp)

  (bydi ((:spy harpoon--warn))
    (bydi-match-expansion
     (harpoon-function test-mode
       :messages ("Just testing")
       :lsp (:format t))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (harpoon-message '("Just testing"))
        (unless (harpoon-lsp--slow-server-p major-mode)
          (setq-local completion-styles harpoon-lsp-completion-styles))
        (lsp-deferred)
        (add-hook 'before-save-hook 'lsp-format-buffer nil t)))))

(ert-deftest harpoon-function--lsp-hints ()
  :tags '(lsp)

  (let ((harpoon-whitespace 'keep))
    (bydi-match-expansion
     (harpoon-function test-mode
       :lsp (:hints t))
     `(defun test-mode-harpoon nil
       "Hook into `test-mode'."
       (setq-local lsp-inlay-hint-enable t)
       (unless (harpoon-lsp--slow-server-p major-mode)
         (setq-local completion-styles harpoon-lsp-completion-styles))
       (lsp-deferred)))))

(ert-deftest harpoon-function--whitespace-deletion ()
  :tags '(lsp)

  (let ((harpoon-whitespace 'delete))

    (bydi ((:spy harpoon--warn))
      (bydi-match-expansion
       (harpoon-function test-mode
         :messages ("Just testing"))
       `(defun test-mode-harpoon ()
          "Hook into `test-mode'."
          (harpoon-message '("Just testing"))
          (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))))

  (let ((harpoon-whitespace 'delete))

    (bydi ((:spy harpoon--warn))
      (bydi-match-expansion
       (harpoon-function test-mode
         :messages ("Just testing")
         :whitespace keep)
       `(defun test-mode-harpoon ()
          "Hook into `test-mode'."
          (harpoon-message '("Just testing"))))))

  (let ((harpoon-whitespace 'keep))

    (bydi ((:spy harpoon--warn))
      (bydi-match-expansion
       (harpoon-function test-mode
         :messages ("Just testing")
         :whitespace delete)
       `(defun test-mode-harpoon ()
          "Hook into `test-mode'."
          (harpoon-message '("Just testing"))
          (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))))

  (let ((harpoon-whitespace 'keep))

    (bydi ((:spy harpoon--warn))
      (bydi-match-expansion
       (harpoon-function test-mode
         :messages ("Just testing")
         :whitespace remove)
       `(defun test-mode-harpoon ()
          "Hook into `test-mode'."
          (harpoon-message '("Just testing")))))))

(ert-deftest harpoon-completion--setup ()
  ;; Having set completion provider.
  (let ((harpoon-completion-provider 'corfu))

    ;; Passing normal list works.
    (bydi-match-expansion
     (harpoon-function test-mode
       :messages ("Just testing")
       :completion (0.2 4)
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (harpoon-message '("Just testing"))
        (message "hi")
        (setq-local corfu-auto-delay 0.2
                    corfu-auto-prefix 4)))

    ;; Passing plist with just prefix works.
    (bydi-match-expansion
     (harpoon-function test-mode
       :messages ("Just testing")
       :completion (:prefix 4)
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (harpoon-message '("Just testing"))
        (message "hi")
        (setq-local corfu-auto-prefix 4)))

    ;; Passing normal list only having prefix works.
    (bydi-match-expansion
     (harpoon-function test-mode
       :messages ("Just testing")
       :completion (nil 4)
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (harpoon-message '("Just testing"))
        (message "hi")
        (setq-local corfu-auto-prefix 4)))

    ;; Passing no completion config works.
    (bydi-match-expansion
     (harpoon-function test-mode
       :messages ("Just testing")
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (harpoon-message '("Just testing"))
        (message "hi"))))

  ;; Without setting provider.

  ;; Passing explicit provider works.
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :completion (:provider corfu :delay 4)
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (message "hi")
      (setq-local corfu-auto-delay 4)))

  ;; Passing both delay and prefix works.
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :completion (:provider corfu :delay 0.2 :prefix 4)
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message '("Just testing"))
      (message "hi")
      (setq-local corfu-auto-delay 0.2
                  corfu-auto-prefix 4))))

(ert-deftest harpoon--functions ()
  (let ((harpoon-completion-provider nil))
    (bydi-match-expansion
     (harpoon-function test-mode
       :functions (test-mode testable-mode)
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (message "hi")
        (progn
          (when (fboundp 'test-mode)
            (test-mode))
          (when (fboundp 'testable-mode)
            (testable-mode)))))))

(ert-deftest harpoon-ligatures ()
  (bydi-match-expansion
   (harpoon-ligatures test-mode
     :ligatures ("?!"))
   `(progn
      (harpoon-ligatures--set-ligatures 'test-mode '("?!"))))

  (let ((harpoon-ligatures--sisters '((test-mode . (mock-mode)))))
    (bydi-match-expansion
     (harpoon-ligatures test-mode
       :ligatures ("?!"))
     `(progn
        (harpoon-ligatures--set-ligatures 'test-mode '("?!"))
        (harpoon-ligatures--set-ligatures 'mock-mode '("?!"))))))

(ert-deftest harpoon-lsp ()
  (bydi-match-expansion
   (harpoon-lsp test-mode :lsp (:ignore-dirs (".ignoramus")))
   `(with-eval-after-load harpoon-lsp-provider
      (harpoon-lsp--ignore-directory '(".ignoramus") 'nil))))

(ert-deftest harpoon-lsp--custom-dir-var ()
  (bydi-match-expansion
   (harpoon-lsp test-mode :lsp (:ignore-dirs (".ignoramus") :dir-ignore-list lsp-file-watch-ignored-directories))
   `(with-eval-after-load harpoon-lsp-provider
      (harpoon-lsp--ignore-directory '(".ignoramus") 'lsp-file-watch-ignored-directories))))

(ert-deftest harpoon-treesit ()
  (bydi ((:always harpoon-treesit--ready-p))

    (bydi-match-expansion
     (harpoon-treesit test-mode)
     `(progn
        (add-to-list 'major-mode-remap-alist
                     '(test-mode . test-ts-mode))
        (with-eval-after-load 'all-the-icons
          (defvar all-the-icons-mode-icon-alist)

          (when-let ((setting
                      (cdr
                       (assoc 'test-mode all-the-icons-mode-icon-alist)))
                     (name 'test-ts-mode))

            (add-to-list 'all-the-icons-mode-icon-alist
                         (cons name setting))))))))

(ert-deftest harpoon--mode-name--with-treeesit ()
  (bydi ((:always harpoon-treesit--ready-p))
    (should (equal 'test-ts-mode (harpoon--mode-name 'test-mode)))))

(ert-deftest harpoon-hook ()
  (let ((harpoon-hook--sisters '((test-mode-hook . (test-extra-mode-hook)))))

    (bydi-match-expansion
     (harpoon-hook test-mode)
     `(progn
        (add-hook
         'test-mode-hook
         'test-mode-harpoon)
        (add-hook
         'test-extra-mode-hook
         'test-mode-harpoon)))))

;;; harpoon-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
