;;; module-iwctl.el --- A module for controlling iwctl from inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: convenience, tools, terminals, unix, processes

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


(defvar iwctl-station "wlan0")

(defvar iwctl--signal-strength-bar-width 60)

(defconst iwctl--scan-networks-command "iwctl station %s scan")

(defconst iwctl--read-networks-command "iwctl station %s get-networks | perl -pe 's/^(.*?\\[0m|[[:space:]]+)(.*?)([*]+).*/\\2\\3/g' | sed -E 's/[[:space:]]+/,/g' | grep '*' | perl -pe 's/^(.*?),/(:ssid \"\\1\",/g' | sed -E 's/,([a-z]+),/ :security \"\\1\",/g;s/,(.*)$/ :signal \"\\1\")/' | tr -d '\n' | sed \"s/^/'(/;s/$/)/\"")

(defconst iwctl--read-current-network-command "iwctl station %s show | grep -E 'State|Connected network' | sed -E 's/^[[:space:]]+//;s/[[:space:]]{2,}/;/g' | cut -d';' -f2 | tr '\n' ',' | perl -pe 's/(.*?),(?:(.*),)?$/(:status \"\\1\" :ssid \"\\2\")/g'")

(defconst iwctl--ping-status-command "ping 1.1.1.1 -c 8 -q  | tail -2 | sed -E \"s/^8.*d, (.*),.*$/\\1/;s/rtt.*= //;s|([0-9.]+)/([0-9.]+)/([0-9.]+)/.*| min '\\1 ms', avg '\\2 ms', max '\\3 ms'|\" | tr '\n' ',' | sed -E 's/,$//'")

(defun iwctl--imbue-station (string) (format string iwctl-station))

(defun iwctl--read-current-network ()
  (car (read-from-string (shell-command-to-string (iwctl--imbue-station iwctl--read-current-network-command)))))

(defun iwctl--ping-status ()
  (shell-command-to-string iwctl--ping-status-command))

(defun iwctl-exwm-status ()
  (interactive)
  (iwctl--update-cache)
  (if (string= (plist-get iwctl--current-network-cache :status) "disconnected")
      (propertize "Disconnected" 'face '(:inherit error :reverse-video t :background "#000"))
  (let ((display-msg
         (format "%s\n%s\n%s"
                 (propertize (upcase-initials (plist-get iwctl--current-network-cache :status))
                             'face '(:inherit success :reverse-video t :background "#FFF"))
                 (plist-get iwctl--current-network-cache :ssid)
                 (plist-get
                  (--find (string= (plist-get it :ssid)
                                   (plist-get iwctl--current-network-cache :ssid))
                          iwctl--network-cache)
                  :signal))))
    (exwm-show-msg (format "%s\n %s "
                           display-msg
                           "Calculating ping... please wait.")
                   :center t
                   :timeout nil)
    (async-start #'(lambda ()
                     (let ((iwctl--ping-status-command "ping 1.1.1.1 -c 8 -q  | tail -2 | sed -E \"s/^8.*d, (.*),.*$/\\1/;s/rtt.*= //;s|([0-9.]+)/([0-9.]+)/([0-9.]+)/.*| min '\\1 ms', avg '\\2 ms', max '\\3 ms'|\" | tr '\n' ',' | sed -E 's/,$//'"))
                       (shell-command-to-string iwctl--ping-status-command)))
                 #'(lambda (promised-value)
                     (exwm-show-msg
                        (format "%s\n %s "
                                display-msg
                                promised-value)
                      :center t))))))

(defun iwctl--scan-networks ()
  (shell-command-to-string (iwctl--imbue-station iwctl--scan-networks-command)))
               
(defun iwctl--read-networks ()
  (iwctl--scan-networks)
  (mapcar #'iwctl--signal-strength-replace-asterix-with-progress-bar (eval (car (read-from-string (shell-command-to-string (iwctl--imbue-station iwctl--read-networks-command)))))))

(defun iwctl--signal-strength-replace-asterix-with-progress-bar (network)
  (let* ((signal-strength (plist-get network :signal))
         (signal-level (length signal-strength))
         (signal-bar (make-progress-bar signal-level
                                        :from 0.0
                                        :to 5.0
                                        :width iwctl--signal-strength-bar-width
                                        :label "[Signal Strength]")))
      (plist-put network :signal signal-bar)))

(defun iwctl--read-known-networks ()
  (eval (car (read-from-string (shell-command-to-string "iwctl known-networks list | grep , | cut -d, -f1 | sed -E 's/[[:space:]]+/,/g' | cut -d, -f2 | sed -E 's/(.*)/\"\\1\" /g' | tr -d '\n' | sed \"s/$/)/;s/^/\'(/\"")))))

(defvar iwctl--network-cache nil)
(defvar iwctl--known-network-cache nil)
(defvar iwctl--current-network-cache nil)

(defun iwctl--update-cache ()
  (setq iwctl--network-cache (iwctl--read-networks))
  (setq iwctl--known-network-cache (iwctl--read-known-networks))
  (setq iwctl--current-network-cache (iwctl--read-current-network)))

(defun iwctl-known-p (ssid-or-plist)
  (cl-etypecase ssid-or-plist
    (string (cl-find ssid-or-plist iwctl--known-network-cache :test #'string=))
    (list (iwctl-known-p (plist-get ssid-or-plist :ssid)))))

(defun iwctl-read-network ()
  (interactive)
  (iwctl--update-cache)
  (when-let ((networks (--map (plist-get it :ssid) iwctl--network-cache)))
    (consult--read networks
                   :prompt "Network: "
                   :require-match t
                   :category 'iwctl-wifi)))

(defun marginalia-annotate-iwctl-wifi (cand)
  (let ((data (seq-find #'(lambda (x) (string= (plist-get x :ssid) cand)) iwctl--network-cache)))
    (marginalia--fields
     ((if (string= (plist-get iwctl--current-network-cache :ssid) cand)
          (upcase-initials (plist-get iwctl--current-network-cache :status))
        "")
      :truncate 10
      :face 'success)
     ((plist-get data :security) :truncate 5)
     ((if (iwctl-known-p cand) "Known" "Unknown")
      :truncate 10
      :face (if (iwctl-known-p cand) 'success 'error))
     ((plist-get data :signal) :truncate iwctl--signal-strength-bar-width))))

(defun iwctl-connect (network)
  (interactive (list (iwctl-read-network)))
  (if (iwctl-known-p network)
      (shell-command-to-string (format "iwctl station %s connect %s" iwctl-station network))
    (let ((data (seq-find #'(lambda (x) (string= (plist-get x :ssid) network)) iwctl--network-cache)))
        (if (string= (plist-get data :security) "open")
            (message (shell-command-to-string (format "iwctl station %s connect %s" iwctl-station network)))
          (message (shell-command-to-string (format "iwctl station %s connect %s --passphrase %s"
                                                    iwctl-station
                                                    network
                                                    (read-passwd (format "Passphrase for '%s': " network)))))))))x

(add-to-list 'marginalia-annotator-registry '(iwctl-wifi marginalia-annotate-iwctl-wifi builtin none))

(provide 'module-iwctl)
;;; module-iwctl.el ends here
