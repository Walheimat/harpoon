;;; harpoon-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'harpoon nil t)

(setq harpoon-suppress-warnings t
      harpoon-completion-provider nil)

(ert-deftest harpoon-prog-like ()
  (bydi ((:mock run-hooks :with bydi-rf))

    (should (equal (harpoon-prog-like) 'harpoon-prog-like-hook))))

(ert-deftest test-harpoon-treesit--ready-p ()
  (defvar harpoon-treesit--alist)
  (bydi ((:always wal-modern-emacs-p)
         (:always require)
         (:always treesit-available-p)
         (:mock treesit-ready-p :with (lambda (it &rest _) (equal 'testable it))))

    (let ((harpoon-treesit--modes '((test-mode . testable) (zest-mode . zestable))))

      (should (harpoon-treesit--ready-p 'test-mode))
      (should-not (harpoon-treesit--ready-p 'zest-mode))
      (should-not (harpoon-treesit--ready-p 'no-mapping-mode)))))

(ert-deftest harpoon-ligatures--set-ligatures ()
  (defvar harpoon-ligatures--common-ligatures)

  (let ((harpoon-ligatures--common-ligatures '("?")))

    (bydi ((:always require)
           ligature-set-ligatures)

      (harpoon-ligatures--set-ligatures 'test-mode '("!"))

      (bydi-was-called-with ligature-set-ligatures (list 'test-mode '("!" "?"))))))

(ert-deftest harpoon-completion--parse--defaults ()
  (let ((harpoon-completion-provider 'corfu))
    (should (equal '(corfu t 2.1 4)
                   (harpoon-completion--parse (list 2.1 4))))))

(ert-deftest harpoon-completion--parse--sets-corfu-using-plist ()
  (should (equal '(corfu nil 0.1 5)
                 (harpoon-completion--parse '(:provider corfu :auto nil :delay 0.1 :prefix 5)))))

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

    (bydi-was-called-with harpoon--append (list harpoon-lsp-dir-ignore-list '("test")))

    (bydi-clear-mocks)

    (harpoon-lsp--ignore-directory '("test" "best"))

    (bydi-was-called-with harpoon--append (list harpoon-lsp-dir-ignore-list '("test" "best")))))

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
                       "This is the first message\nThis will be the second message\n")))))

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

      (should (string-equal (harpoon-message--in-a-bottle bottle "}< ,.__)") "}< ,.__) Sting is playing bass, yeah")))))

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
  (let ((harpoon-lsp-function 'lsp-deferred))
    (bydi-match-expansion
     (harpoon-function test-mode
       :messages ("Just testing")
       :lsp t)
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (harpoon-message--in-a-bottle '("Just testing"))
        (unless (harpoon-lsp--slow-server-p major-mode)
          (setq-local completion-styles harpoon-lsp-completion-styles))
        (lsp-deferred)))))

(ert-deftest harpoon-function--custom ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp (:function eglot-ensure))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message--in-a-bottle '("Just testing"))
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
      (harpoon-message--in-a-bottle '("Just testing"))
      (hack-local-variables)
      (harpoon-tabs--maybe-enable))))

(ert-deftest harpoon-function--enable-indent ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :tabs always)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message--in-a-bottle '("Just testing"))
      (harpoon-tabs--enable))))

(ert-deftest harpoon-function--no-tabs ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp nil
     :tabs never)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message--in-a-bottle '("Just testing"))
      (harpoon-tabs--disable))))

(ert-deftest harpoon-function--prog-like ()
  (bydi-match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :prog-like t
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message--in-a-bottle '("Just testing"))
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

(ert-deftest harpoon-completion--setup ()
  (let ((harpoon-completion-provider 'corfu))
    (bydi-match-expansion
     (harpoon-function test-mode
       :messages ("Just testing")
       :completion (0.2 4)
       (message "hi"))
     `(defun test-mode-harpoon ()
        "Hook into `test-mode'."
        (harpoon-message--in-a-bottle '("Just testing"))
        (message "hi")
        (setq-local corfu-auto-delay 0.2
                    corfu-auto-prefix 4
                    corfu-auto t)
        (local-set-key (kbd harpoon-completion-key) #'completion-at-point)))))

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
   `(harpoon-ligatures--set-ligatures 'test-mode '("?!"))))

(ert-deftest harpoon-lsp ()
  (bydi-match-expansion
   (harpoon-lsp test-mode :lsp (:ignore-dirs (".ignoramus")))
   `(with-eval-after-load 'lsp-mode
      (when harpoon-lsp-dir-ignore-list
        (harpoon-lsp--ignore-directory '(".ignoramus") 'nil)))))

(ert-deftest harpoon-lsp--custom-dir-var ()
  (bydi-match-expansion
   (harpoon-lsp test-mode :lsp (:ignore-dirs (".ignoramus") :dir-ignore-list lsp-file-watch-ignored-directories))
   `(with-eval-after-load 'lsp-mode
      (when harpoon-lsp-dir-ignore-list
        (harpoon-lsp--ignore-directory '(".ignoramus") 'lsp-file-watch-ignored-directories)))))

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
  (bydi-match-expansion
   (harpoon-hook test-mode)
   `(add-hook
     'test-mode-hook
     'test-mode-harpoon)))

;;; harpoon-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
