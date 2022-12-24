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


(defvar runs-scheduled '())

(defmacro run-at-time--partial (time repeat fn &optional args)
  `(apply (apply-partially #'run-at-time ,time ,repeat ,fn) ,args))

(defun run-later (time identifier argfn fn &rest args)
  (when-let* ((timer (plist-get runs-scheduled identifier))
              (is-valid (not (timer--triggered timer)))
              (old-args (timer--args timer)))
    (setq args (funcall argfn args old-args))
    (cancel-timer timer))
  (let ((timer (run-at-time--partial time nil fn args)))
    (setq runs-scheduled (plist-put runs-scheduled identifier timer)))) 

(defun xbacklight-get (&optional callback)
  (let ((timer-fn
         (lambda (cmd)
               (funcall callback (string-to-number (shell-command-to-string cmd)))))
        (arg-fn (lambda (new old) new)))
    (run-later 0.2 :xbacklight-get arg-fn timer-fn "xbacklight | tr -d '\n'")))
            

(defun eval-act-args (old-op new-op old-value new-value)
  (cond
   ((string= new-op "=") (list "=" new-value))
   ((and (string= old-op "=")
        (string= new-op "+"))
    (list "=" (min (+ old-value new-value) 100)))
   ((and (string= old-op "=")
        (string= new-op "-"))
    (list "=" (max (- old-value new-value) 0)))
   ((and (string= old-op "+")
        (string= new-op "+"))
    (list "+" (min (+ old-value new-value) 100)))
   ((and (string= old-op "+")
        (string= new-op "-"))
    (list (if (< old-value new-value) "-" "+")
                    (max (- old-value new-value) -100)))
   ((and (string= old-op "-")
        (string= new-op "+"))
    (list (if (> old-value new-value) "-" "+")
                    (max (- new-value old-value) -100)))
   ((and (string= old-op "-")
        (string= new-op "-"))
    (list "-" (min (+ old-value new-value) 100)))
   (t (error "Invalid operation for xbacklight"))))

(defun xbacklight-act-args (old new)
  (cl-destructuring-bind (old-op old-value) old
    (cl-destructuring-bind (new-op new-value) new
      (eval-act-args old-op new-op old-value new-value))))

(defun xbacklight-act (op value)
    (let ((timer-fn (lambda (op value) (shell-command-to-string (format "xbacklight '%s%d'" op value)))))
    (run-later 0.2 :xbacklight-act #'xbacklight-act-args timer-fn op value)))

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
