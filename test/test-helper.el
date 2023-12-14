;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'dinghy-rope)

;; Setup

(dinghy-rope-setup-paths)
(dinghy-rope-setup-undercover (list "harpoon.el"))
(dinghy-rope-setup-ert-runner)
(dinghy-rope-setup-ert :increase-print-depth t)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
