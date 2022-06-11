;;; module-xbacklight.el --- A module for controlling the monitor backlight via xbacklight  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: terminals, convenience, hardware, tools

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

(defun xbacklight-get (&optional callback)
  (if callback
      (async-start #'(lambda ()
                       (shell-command-to-string "xbacklight | tr -d '\n'"))
                   #'(lambda (result)
                       (funcall callback (string-to-number result))
                       ))
      (string-to-number (shell-command-to-string "xbacklight | tr -d '\n'"))))

(defun xbacklight-act (op value)
  (async-start
     `(lambda () (shell-command-to-string (format "xbacklight '%s%d'" ,op ,value)))
    'ignore))

(defun xbacklight-inc (value)
  (interactive (list (read-number "Increment by: ")))
  (xbacklight-act "+" value))

(defun xbacklight-dec (value)
  (interactive (list (read-number "Decrement by: ")))
  (xbacklight-act "-" value))

(defun xbacklight-set (value)
  (interactive (list (read-number "Set to: ")))
  (xbacklight-act "=" value))

(defun xbacklight-show ()
  (interactive)
  (xbacklight-get #'(lambda (value)
                      (message "%d" value)
                      (exwm-show-msg (make-progress-bar value
                                                        :width 80
                                                        :label (format "[Brightness: %d%%]" value))
                 :center t))))

(defvar xbacklight-step 5)

(defun xbacklight-inc-dwim ()
  (interactive)
  (xbacklight-inc xbacklight-step)
  (xbacklight-show))

(defun xbacklight-dec-dwim ()
  (interactive)
  (xbacklight-dec xbacklight-step)
  (xbacklight-show))

(provide 'module-xbacklight)
;;; module-xbacklight.el ends here
