;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'bydi-report)
(require 'bydi-ci)

;; Setup

(bydi-ci-setup-paths)
(bydi-report-setup-undercover (list "harpoon.el"))
(bydi-report-setup-ert-runner)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
