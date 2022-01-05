;;; module-bluetooth.el --- Bluetooth controls       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Code:

(require 's)
(require 'dash)
(require 'all-the-icons)
(require 'all-the-icons-completion)
(require 'marginalia)

(defvar bluetooth-devices nil)

(defun bluetoothctl (cmd mac)
  "Use bluetoothctl to run CMD on MAC"
  (start-process-shell-command "emacs: bluetoothctl" nil (s-join " " (list "bluetoothctl" cmd mac))))

(add-to-list 'marginalia-prompt-categories '("\\<bluetooth\\>" . bluetooth))


(defun marginalia-annotate-bluetooth-device (cand)
  (let ((entry (--find (string= cand (plist-get it :alias)) bluetooth-devices)))
    (marginalia--fields
     ((plist-get entry :mac)
      :face 'marginalia-key
      :truncate 17
      )
     ((if (plist-get entry :connected) "Connected" "Disconnected")
      :face (if (plist-get entry :connected) 'marginalia-on 'marginalia-off)
      :truncate 12
      )
     ((if (plist-get entry :paired) "Paired" "Not paired")
      :face (if (plist-get entry :paired) 'marginalia-on 'marginalia-off)
      :truncate 10
      )
     ((if (plist-get entry :trusted) "Trusted" "Not trusted")
      :face (if (plist-get entry :trusted) 'marginalia-on 'marginalia-off)
      :truncate 11
      )
     ((plist-get entry :icon)
      :face 'marginalia-documentation
      :truncate (/ marginalia-truncate-width 4)))))

(defun all-the-icons-completion-get-icon (cand cat)
  "Return the icon for the candidate CAND of completion category CAT."
  (cl-case cat
    (file (all-the-icons-completion-get-file-icon cand))
    (project-file (all-the-icons-completion-get-file-icon cand))
    (buffer (all-the-icons-completion-get-buffer-icon cand))
    (bluetooth (all-the-icons-completion-get-bluetooth-icon cand))
    (t "")))

(defun all-the-icons-completion-get-bluetooth-icon (cand)
  (let ((type (plist-get (--find (string= cand (plist-get it :alias)) bluetooth-devices) :icon)))
    (concat
     (cond
      ((string= type "audio-headset") (all-the-icons-material "headset_mic"))
      ((string= type "audio-headphones") (all-the-icons-material "headset"))
      ((string= type "phone") (all-the-icons-material "smartphone"))
      ((string= type "input-mouse") (all-the-icons-material "mouse"))
      )
     " ")))

(add-to-list 'marginalia-annotator-registry
             '(bluetooth marginalia-annotate-bluetooth-device builtin none))

(defun pick-bluetooth-device (prompt)
  (let* ((bluetooth-devices (car (read-from-string (shell-command-to-string "lispify-bluetooth-devices"))))
         (names (--map (plist-get it :alias) bluetooth-devices))
         (selection (completing-read (concat "(bluetooth) " prompt) names nil t))
         (selection-entry (--find (string= selection (plist-get it :alias)) bluetooth-devices))
         )
    selection-entry))

(defun bluetooth-connect ()
  (interactive)
  (let ((device (pick-bluetooth-device "Connect: ")))
    (start-process-shell-command "emacs: Bluetooth connect" nil (format "connect-bluetooth-by-mac %s" (plist-get device :mac)))))

(provide 'module-bluetooth)
;;; module-bluetooth.el ends here
