;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'bydi-report)

(declare-function bydi-path-setup "ext:bydi.el")
(declare-function bydi-ert-runner-setup "ext:bydi.el")
(declare-function bydi-undercover-setup "ext:bydi.el")

;; Setup

(bydi-path-setup)
(bydi-undercover-setup (list "harpoon.el"))
(bydi-ert-runner-setup)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
