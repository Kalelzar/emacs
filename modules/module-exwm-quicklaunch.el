;;; module-exwm-quicklaunch.el --- -*- lexical-binding: t;  -*-
;;; Commentary:
;;; Code:
(require 'custom)
(require 'consult)
(require 'marginalia)

(defgroup exwm-quicklaunch
  '()
  "Quickly launch EXWM apps from consult's `consult-buffer'."
  :package-version '(exwm-quicklaunch . "1.0.0")
  :tag "EXWM Quicklaunch"
  :group 'exwm)

(defcustom exwm-quicklaunch-alist
  '(("Firefox" . "firefox")
    ("Web search" . "firefox --search \"$(ex-read-string 'Search: ')\"")
    ("Mirror Android Screen" . "scrcpy -m1920"))
  "A associative list of commands to use for quickly launching X apps in EXWM.

Each element of the alist has the form (KEY . VALUE) where KEY and VALUE are STRING.
KEY should be the name of the quicklaunch action.
VALUE should be the shell command to launch the application."
  :type '(alist :key-type (string :tag "Name") :value-type (string :tag "Command"))
  :safe t
  :group 'exwm-quicklaunch
  :package-version '(exwm-quicklaunch . "1.0.0")
  :tag "EXWM Quicklaunch alist")


(defvar exwm-quicklaunch-source
    `(:name     "EXWM quicklaunch"
	        :narrow   ?q
	        :category quicklaunch
	        :items    ,(lambda () (mapcar #'car exwm-quicklaunch-alist))
                :action   ,(lambda (cand)
                             (let ((command (alist-get cand exwm-quicklaunch-alist nil nil #'string=)))
                               (start-process-shell-command command nil command)))
                )
    "`exmw-mode' quicklaunch shortcuts source for `consult-buffer'.")

(add-to-list 'consult-buffer-sources 'exwm-quicklaunch-source 'append)

(defun marginalia-annotate-quicklaunch (cand)
  (marginalia--fields
   ((alist-get cand exwm-quicklaunch-alist)
    :face 'marginalia-key
    :truncate (floor (* 1.5 marginalia-truncate-width)))))

(add-to-list 'marginalia-annotator-registry
             '(quicklaunch marginalia-annotate-quicklaunch builtin none))

(provide 'module-exwm-quicklaunch)
;;; module-exwm-quicklaunch.el ends here.
