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



(defcustom exwm-quicklaunch-list
  '(("Firefox" "firefox" "Launch a new Firefox window.")
("Web search" "firefox --search \"$(ex-read-string 'Search: ')\"" "Do a search with Firefox in a new window.")
("Mirror Android Screen" "scrcpy -m1920" "Mirror the screen of an Android device."))
  "A list of commands to use for quickly launching X apps in EXWM.

Each element of the alist has the form (NAME COMMAND DESCRIPTION)
NAME should be the name of the quicklaunch action.
COMMAND should be the shell command to launch the application.
DESCRIPTION should be a short description of the action."
  :type '(repeat :tag "Quicklaunch actions"
                 (list :tag "Action"
                       (string :tag "Name")
                       (string :tag "Command")
                       (string :tag "Description")))
  :safe t
  :group 'exwm-quicklaunch
  :package-version '(exwm-quicklaunch . "1.0.0")
  :tag "EXWM Quicklaunch List")



(defvar exwm-quicklaunch-source
    `(:name     "EXWM quicklaunch"
	        :narrow   ?q
	        :category exwm-quicklaunch
	        :items    ,(lambda () (mapcar #'car exwm-quicklaunch-list))
                :action   ,(lambda (cand)
                             (let ((command-and-description (alist-get cand exwm-quicklaunch-list nil nil #'string=)))
                               (start-process-shell-command cand nil (car command-and-description))))
                )
    "`exmw-mode' quicklaunch shortcuts source for `consult-buffer'.")

(add-to-list 'consult-buffer-sources 'exwm-quicklaunch-source 'append)

(defun marginalia-annotate-exwm-quicklaunch (cand)
  (marginalia--fields
   ((cadr (alist-get cand exwm-quicklaunch-list nil nil #'string=))
    :face 'marginalia-documentation
    :truncate 60)
   ((car (alist-get cand exwm-quicklaunch-list nil nil #'string=))
    :face 'marginalia-key
    :truncate 60)))

(add-to-list 'marginalia-annotator-registry
             '(exwm-quicklaunch marginalia-annotate-exwm-quicklaunch builtin none))


(defgroup quicklaunch
  '()
  "Quickly launch elisp commands from consult's `consult-buffer'."
  :package-version '(quicklaunch . "1.0.0")
  :tag "Quicklaunch"
  :group 'emacs)

(defcustom quicklaunch-alist
  '(("VTerm" . vterm))
  "A associative list of commands to use for quickly activating elisp commands.

Each element of the alist has the form (KEY . VALUE) where KEY is STRING and VALUE is a function.
KEY should be the name of the quicklaunch action.
VALUE should be the function to call."
  :type '(alist :key-type (string :tag "Name") :value-type (function :tag "Command"))
  :safe t
  :group 'quicklaunch
  :package-version '(quicklaunch . "1.0.0")
  :tag "Quicklaunch alist")

(defvar quicklaunch-source
    `(:name     "Quicklaunch"
	        :narrow   ?q
	        :category quicklaunch
	        :items    ,(lambda () (mapcar #'car quicklaunch-alist))
                :action   ,(lambda (cand)
                             (let ((command (alist-get cand quicklaunch-alist nil nil #'string=)))
                               (funcall command)))
                )
    "Quicklaunch shortcuts source for `consult-buffer'.")

(add-to-list 'consult-buffer-sources 'quicklaunch-source 'append)

(defun marginalia-annotate-quicklaunch (cand)
  (marginalia--fields
   ((elisp--docstring-first-line (marginalia--function-doc (alist-get cand quicklaunch-alist)))
    :truncate 60
    :face 'marginalia-documentation)
   ((symbol-name (alist-get cand quicklaunch-alist))
    :face 'marginalia-key
    :truncate 60)))


(add-to-list 'marginalia-annotator-registry
             '(quicklaunch marginalia-annotate-quicklaunch builtin none))


(provide 'module-exwm-quicklaunch)
;;; module-exwm-quicklaunch.el ends here.
