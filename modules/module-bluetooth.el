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

;;; URL: https://github.com/Kalelzar/emacs
;;; Package-Version: 1.0.0
;;; Package-Requires: ((emacs "24.3") (s "1.12.0") (all-the-icons "5.0.0") (all-the-icons-completion "0.0.1") (dash "2.19.0"))


;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'all-the-icons)
(require 'all-the-icons-completion)
(require 'marginalia)

(defvar bluetooth-devices nil "I do not know if this is necessary here.")

(defun bluetoothctl (cmd mac)
  "Use bluetoothctl to run CMD on MAC"
  (start-process-shell-command "emacs: bluetoothctl" nil (s-join " " (list "bluetoothctl" cmd mac))))

(add-to-list 'marginalia-prompt-categories '("\\<bluetooth\\>" . bluetooth))


(defun marginalia-annotate-bluetooth-device (cand)
  "`marginalia' annotator for bluetooth devices.
Displays in order the device's:
- MAC Address
- Connection Status (Connected/Disconnected)
- Pairing Status (Paired / Not Paired)
- Trust status (Trusted / Not Trusted)
- The name of icon used
"
  (let ((entry (--find (string= cand (plist-get it :alias)) bluetooth-devices)))
    (marginalia--fields
     ((plist-get entry :mac)
      :face 'marginalia-key
      :truncate 17)
     ((if (plist-get entry :connected) "Connected" "Disconnected")
      :face (if (plist-get entry :connected) 'marginalia-on 'marginalia-off)
      :truncate 12)
     ((if (plist-get entry :paired) "Paired" "Not paired")
      :face (if (plist-get entry :paired) 'marginalia-on 'marginalia-off)
      :truncate 10)
     ((if (plist-get entry :trusted) "Trusted" "Not trusted")
      :face (if (plist-get entry :trusted) 'marginalia-on 'marginalia-off)
      :truncate 11)
     ((plist-get entry :icon)
      :face 'marginalia-documentation
      :truncate (/ marginalia-field-width 4)))))

(defun all-the-icons-completion-get-icon (cand cat)
  "Return the icon for the candidate CAND of completion category CAT."
  (cl-case cat
    (file (all-the-icons-completion-get-file-icon cand))
    (project-file (all-the-icons-completion-get-file-icon cand))
    (buffer (all-the-icons-completion-get-buffer-icon cand))
    (bluetooth (all-the-icons-completion-get-bluetooth-icon cand))
    (t "")))

(defun all-the-icons-completion-get-bluetooth-icon (cand)
  "Return the icon for the candidate CAND depending on it's recognised icon."
  (let ((type (plist-get (--find (string= cand (plist-get it :alias)) bluetooth-devices) :icon)))
    (concat
     (cond
      ((string= type "audio-headset") (all-the-icons-material "headset_mic"))
      ((string= type "audio-headphones") (all-the-icons-material "headset"))
      ((string= type "phone") (all-the-icons-material "smartphone"))
      ((string= type "input-mouse") (all-the-icons-material "mouse")))
     " ")))

(add-to-list 'marginalia-annotator-registry
             '(bluetooth marginalia-annotate-bluetooth-device builtin none))

(defun pick-bluetooth-device (prompt)
  "Pick a bluetooth device, showing PROMPT.
This returns a plist with the following properties:
:mac - the mac adress of the device.
:alias - the name or the user-defined alias for the device.
:icon - the name of the icon to use for the device.
:paired - it's pair status.
:connected - it's connection status.
:trusted -it's trust status.

All information is acquired through `bluetoothctl'
This requires the `lispify-bluetooth-devices' script from my dotfiles.
"
  (let* ((bluetooth-devices (car (read-from-string (shell-command-to-string "lispify-bluetooth-devices"))))
         (names (--map (plist-get it :alias) bluetooth-devices))
         (selection (completing-read (concat "(bluetooth) " prompt) names nil t))
         (selection-entry (--find (string= selection (plist-get it :alias)) bluetooth-devices)))
    selection-entry))

(defun bluetooth-connect ()
  "Connect to a bluetooth device, disconnecting and reconnecting if already connected.
See `pick-bluetooth-device' for information on how devices are displayed."
  (interactive)
  (let ((device (pick-bluetooth-device "Connect: ")))
    (start-process-shell-command "emacs: Bluetooth connect" nil (format "connect-bluetooth-by-mac %s" (plist-get device :mac)))))

(provide 'module-bluetooth)
;;; module-bluetooth.el ends here
