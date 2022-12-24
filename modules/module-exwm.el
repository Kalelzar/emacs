;;; module-exwm.el --- EXWM config                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: tools, mouse, processes, unix

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

(use-package exwm
  :after consult
  :demand t
  :config
  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 1)))

(require 'exwm-workspace-group)

(defface exwm-alert-face
  '((default . (:height 1.6)))
  "A face for alerts shown by `posframe' in `exwm'.")

(defvar exwm-datetime-format "+     %H:%M\n%a %b %+2d %+4Y"
  "A date formatted as it would be in the POSIX date utility.
Run 'man date' for more details.")

(defun exwm-show-time ()
  (interactive)
  (exwm-show-alert (shell-command-to-string (concat "date '"
                                                    exwm-datetime-format
                                                    "'"))))

(defun exwm-show-alert (msg)
  (interactive
   (list (read-string "Message: ")))
  (posframe-show " *Alert*"
                 :string (propertize msg 'face 'exwm-alert-face)
                 :poshandler #'posframe-poshandler-window-center
                 :border-width 1
                 :left-fringe 3
                 :height (length (s-lines msg))
                 :right-fringe 3
                 :background-color (face-background 'default)
                 :timeout 4
                 :refposhandler #'posframe-refposhandler-xwininfo))

(cl-defun exwm-show-msg (msg &key (center nil) (timeout 4))
  (interactive
   (list (read-string "Message: ")))
  (posframe-show " *EXMW Message*"
                 :string msg
                 :poshandler #'posframe-poshandler-window-center
                 :border-width 1
                 :height (length (s-lines msg))
                 :left-fringe 3
                 :right-fringe 3
                 :background-color (face-background 'default)
                 :timeout timeout
                 :refposhandler #'posframe-refposhandler-xwininfo)
  (when center
    (with-current-buffer " *EXMW Message*"
      (setq fill-column (-max (--map (length it) (s-lines (substring-no-properties msg)))))
      (center-region (buffer-end -1) (buffer-end 1)))))

(setq posframe-mouse-banish-function #'posframe-mouse-banish-simple)

;; Make class name the buffer name
(add-hook 'exwm-update-title-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-title)));; Global keybindings.
(unless (get 'exwm-input-global-keys 'saved-value)
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)
          ;; 'H-:': Launch application.
          ([?\H-:] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\H-=] . pulseaudio-control-inc-dwim)
          ([?\H-+] . pulseaudio-control-increase-volume)
          ([?\H--] . pulseaudio-control-dec-dwim)
          ([?\H-_] . pulseaudio-control-decrease-volume)
          ([?\H-m] . pulseaudio-control-mute-dwim)
          ([?\H-M] . pulseaudio-control-toggle-sink-input-mute-by-index)
          ([?\H-d] . pulseaudio-sink-input-hydra/body)
          ([?\H-a] . exwm-show-time)
          ([?\s-=] . xbacklight-inc-dwim)
          ([?\s--] . xbacklight-dec-dwim)
          (,(kbd "H-<left>") . windmove-left)
          (,(kbd "H-<right>") . windmove-right)
          (,(kbd "H-<up>") . windmove-up)
          (,(kbd "H-<down>") . windmove-down)
          ;; 'H-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "H-%d" i)) .
                        (lambda ()
                          (interactive)
                          (ewg/switch-create-group ,i))))
                    (number-sequence 0 9)))))
;; Line-editing shortcuts
(unless (get 'exwm-input-simulation-keys 'saved-value)
  (setq exwm-input-simulation-keys
        '(([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ([?\C-t] . [C-S-n]))))

(defun exwm-all-buffers ()
  (seq-filter
   (lambda (buffer)
     (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
   (buffer-list)))

(defvar exwm-buffer-source
  `(:name "EXWM"
          :hidden nil
          :narrow ?x
          :category buffer
          :state ,#'consult--buffer-state
          :action ,(lambda (cand) (exwm-workspace-switch-to-buffer cand))
          :items ,(lambda () (mapcar #'buffer-name (exwm-all-buffers))))
  "`exwm-mode' buffers source for `consult-buffer'.")

(setq consult--source-buffer
        `(:name     "Buffer"
                    :narrow   ?b
                    :category buffer
                    :face     consult-buffer
                    :history  buffer-name-history
                    :state    ,#'consult--buffer-state
                    :default  t
                    :enabled ,(lambda () (not (frame-parameter nil 'bufler-workspace-path)))
                    :items
                    ,(lambda () (consult--buffer-query :sort 'alpha
                                                       :predicate (lambda (buffer) (not (eq 'exwm-mode (buffer-local-value 'major-mode buffer))))
                                                       :as #'buffer-name))))

(add-to-list 'consult-buffer-sources 'exwm-buffer-source)

(ewg/init (list "DVI-D-0" "HDMI-0"))

(exwm-enable)

(fringe-mode 10)



(provide 'module-exwm)
;;; module-exwm.el ends here
