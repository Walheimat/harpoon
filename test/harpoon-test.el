;;; harpoon-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'harpoon nil t)

(ert-deftest harpoon-prog-like ()
  (with-mock ((run-hooks . #'rf))

    (should (equal (harpoon-prog-like) 'harpoon-prog-like-hook))))

(ert-deftest test-harpoon--treesit-ready-p ()
  (defvar harpoon--treesit-alist)
  (with-mock ((wal-modern-emacs-p . #'always)
              (require . #'always)
              (treesit-available-p . #'always)
              (treesit-ready-p . (lambda (it &rest _) (equal 'testable it))))

    (let ((harpoon--treesit-modes '((test-mode . testable) (zest-mode . zestable))))

      (should (harpoon--treesit-ready-p 'test-mode))
      (should-not (harpoon--treesit-ready-p 'zest-mode))
      (should-not (harpoon--treesit-ready-p 'no-mapping-mode)))))


(ert-deftest harpoon-set-ligatures ()
  (defvar harpoon-common-ligatures)

  (let ((harpoon-common-ligatures '("?")))

    (with-mock ((require . #'always) ligature-set-ligatures)

      (harpoon-set-ligatures 'test-mode '("!"))

      (was-called-with ligature-set-ligatures (list 'test-mode '("!" "?"))))))


(ert-deftest harpoon-corfu-auto--sets-corfu ()
  (defvar corfu-auto-delay nil)
  (defvar corfu-auto-prefix nil)

  (with-temp-buffer
    (harpoon-corfu-auto (list 2.1 4))

    (should (equal 2.1 corfu-auto-delay))
    (should (equal 4 corfu-auto-prefix))))

(ert-deftest harpoon-slow-lsp-p ()
  (let ((harpoon-lsp-slow-modes '(test-mode)))

    (should (harpoon-slow-lsp-p 'test-mode))))

(ert-deftest harpoon-lsp--does-not-set-styles-for-slow-modes ()
  (with-mock lsp-deferred

    (let ((harpoon-lsp-slow-modes '(text-mode))
          (harpoon-lsp-function 'lsp-deferred))

      (with-temp-buffer
        (setq major-mode 'text-mode)

        (harpoon-lsp-enable)

        (was-called lsp-deferred)
        (should (equal '(basic partial-completion emacs22) completion-styles))))))

(ert-deftest harpoon-lsp--otherwise-sets-styles ()
  (with-mock lsp-deferred

    (let ((harpoon-lsp-slow-modes nil)
          (harpoon-lsp-function 'lsp-deferred))

      (with-temp-buffer
        (setq major-mode 'text-mode)

        (harpoon-lsp-enable)

        (was-called lsp-deferred)
        (should (equal '(partial-completion basic) completion-styles))))))


(ert-deftest harpoon-lsp-ignore-directory--escape ()
  (should (string= "[/\\\\]tests\\'" (harpoon-lsp-ignore-directory--escape "tests")))
  (should (string= "[/\\\\]\\.tests\\'" (harpoon-lsp-ignore-directory--escape ".tests"))))

(ert-deftest harpoon-lsp-ignore-directory ()
  (with-mock (harpoon-append (harpoon-lsp-ignore-directory--escape . (lambda (it) it)))

    (harpoon-lsp-ignore-directory "test")

    (was-called-with harpoon-append (list harpoon-lsp-dir-ignore-list '("test")))

    (clear-mocks)

    (harpoon-lsp-ignore-directory '("test" "best"))

    (was-called-with harpoon-append (list harpoon-lsp-dir-ignore-list '("test" "best")))))


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

(defvar test-target nil)

(ert-deftest harpoon-append--appends ()
  (let ((test-target '(a b c))
        (sequence '(d e f)))

    (harpoon-append 'test-target sequence)

    (should (equal test-target '(a b c d e f)))))

(ert-deftest harpoon-append--removes-duplicates ()
  (let ((test-target '(a b c))
        (sequence '(c d a)))

    (harpoon-append 'test-target sequence)

    (should (equal test-target '(a b c d)))))


(ert-deftest harpoon-disable-tabs--disables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode t)

    (harpoon-disable-tabs)

    (should (eq indent-tabs-mode nil))))

(ert-deftest harpoon-enable-tabs--enables ()
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (should (eq indent-tabs-mode nil))

    (harpoon-enable-tabs)

    (should (eq indent-tabs-mode t))))

(ert-deftest harpoon-maybe-enable-tabs--enables-if-tabs-preferred ()
  (with-temp-buffer
    (setq-local harpoon-prefer-tabs t)

    (harpoon-maybe-enable-tabs)

    (should (eq indent-tabs-mode t))))

(ert-deftest harpoon-maybe-enable-tabs--disables-unless-preferred ()
  (with-temp-buffer
    (setq-local harpoon-prefer-tabs nil)

    (harpoon-maybe-enable-tabs)

    (should (eq indent-tabs-mode nil))))

(ert-deftest harpoon-biased-random ()
  (let ((vals '(1 2 3 4)))

    (with-mock ((random . (lambda (_) (pop vals))))

      (should (eq (harpoon-biased-random 4) 3))

      (setq vals '(1 2 3 4))

      (should (eq (harpoon-biased-random 4 t) 1)))))


(ert-deftest harpoon-message-in-a-bottle--shows-blue-whale ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (with-mock ((message . #'rf))

      (should (string-equal (harpoon-message-in-a-bottle bottle) "}    , ﬞ   ⎠ Sting is playing bass, yeah")))))

(ert-deftest harpoon-message-in-a-bottle--shows-passed-string ()
  (let ((bottle '("Sting is playing bass, yeah")))

    (with-mock ((message . #'rf))

      (should (string-equal (harpoon-message-in-a-bottle bottle "}< ,.__)") "}< ,.__) Sting is playing bass, yeah")))))

(ert-deftest harpoon--treesit-ready-p ()
  (defvar harpoon--treesit-alist)
  (with-mock ((harpoon-modern-emacs-p . #'always)
              (require . #'always)
              (treesit-available-p . #'always)
              (treesit-ready-p . (lambda (it &rest _) (equal 'testable it))))

    (let ((harpoon--treesit-modes '((test-mode . testable) (zest-mode . zestable))))

      (should (harpoon--treesit-ready-p 'test-mode))
      (should-not (harpoon--treesit-ready-p 'zest-mode))
      (should-not (harpoon--treesit-ready-p 'no-mapping-mode)))))

(ert-deftest harpoon ()
  (match-expansion
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

      (harpoon-lsp
       :messages ("Just testing")
       :lsp t
       :tabs t)

      (harpoon-treesit test-mode))))

(ert-deftest harpoon-function ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp t)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message-in-a-bottle '("Just testing"))
      (harpoon-lsp-enable))))

(ert-deftest harpoon-function--some-symbol ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp t
     :tabs anything)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message-in-a-bottle '("Just testing"))
      (progn
        (hack-local-variables)
        (harpoon-maybe-enable-tabs))
      (harpoon-lsp-enable))))

(ert-deftest harpoon-function--enable-indent ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp t
     :tabs always)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message-in-a-bottle '("Just testing"))
      (harpoon-enable-tabs)
      (harpoon-lsp-enable))))

(ert-deftest harpoon-function--no-tabs ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :lsp nil
     :tabs never)
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message-in-a-bottle '("Just testing"))
      (harpoon-disable-tabs))))

(ert-deftest harpoon-function--prog-like ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :prog-like t
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message-in-a-bottle '("Just testing"))
      (message "hi")
      (run-hooks 'harpoon-prog-like-hook))))

(ert-deftest harpoon-function--major ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :major t
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message-in-a-bottle '("Just testing"))
      (message "hi")
      (local-set-key (kbd harpoon-major-key) 'test-mode-major))))

(ert-deftest harpoon--corfu ()
  (match-expansion
   (harpoon-function test-mode
     :messages ("Just testing")
     :corfu (0.2 4)
     (message "hi"))
   `(defun test-mode-harpoon ()
      "Hook into `test-mode'."
      (harpoon-message-in-a-bottle '("Just testing"))
      (message "hi")
      (progn
        (harpoon-corfu-auto '(0.2 4))
        (local-set-key (kbd "C-M-i") #'completion-at-point)))))

(ert-deftest harpoon--functions ()
  (match-expansion
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
          (testable-mode))))))

(ert-deftest harpoon-ligatures ()
  (match-expansion
   (harpoon-ligatures test-mode
     :ligatures ("?!"))
   `(harpoon-set-ligatures 'test-mode '("?!"))))

(ert-deftest harpoon-lsp ()
  (match-expansion
   (harpoon-lsp :lsp (:ignore-dirs (".ignoramus")))
   `(with-eval-after-load 'lsp-mode
      (when harpoon-lsp-dir-ignore-list
        (harpoon-lsp-ignore-directory '(".ignoramus"))))))

(ert-deftest harpoon-treesit ()
  (with-mock ((harpoon--treesit-ready-p . #'always))

    (match-expansion
     (harpoon-treesit test-mode)
     `(progn
        (message "Remapping %s to %s" 'test-mode 'test-ts-mode)
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
  (with-mock ((harpoon--treesit-ready-p . #'always))
    (should (equal 'test-ts-mode (harpoon--mode-name 'test-mode)))))

(ert-deftest harpoon-hook ()
  (match-expansion
   (harpoon-hook test-mode)
   `(add-hook
     'test-mode-hook
     'test-mode-harpoon)))

;;; harpoon-test.el ends here
