;;; module-batch.el --- A module for batching together commands when they are activated repeatedly for performace reasons  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(cl-defstruct command-batch
  (name default :type symbol :read-only t)
  (timeout 0.2 :type number :read-only t)
  (max-batch-size 20 :type number :read-only t)
  (min-batch-size 3 :type number :read-only t)
  (batch-size 0 :type number)
  timer
  (arglist nil :type plist))

(defvar command-batch-table (make-hash-table) "A hash table of command-batches.")

(cl-defmacro var (varname initform &body body)
  "Bind VARNAME to INITFORM for the duration of BODY."
  `(let ((,varname ,initform))
     ,@body))

(cl-defmacro when-var (varname initform &body body)
  "Bind VARNAME to INITFORM for the duration of BODY.
  If INITFORM is nil, do nothing."
  `(when-let ((,varname ,initform))
     ,@body))

(cl-defmacro if-var (varname initform success failure)
  "Bind VARNAME to INITFORM for the duration of SUCCESS.
  If INITFORM is nil, execute FAILURE."
  `(if-let ((,varname ,initform))
       ,success
     ,failure))

(defun run-command-batch (batch)
  (apply (command-batch-name batch) (command-batch-arglist batch))
  (remhash (command-batch-name batch) command-batch-table))

(defun symbol-to-prop (sym)
  (intern (format ":%s" (symbol-name sym))))

(defun inline-lambda-2 (it)
  (eval `(lambda (value other) ,it)))

(cl-defmacro with-batching (command
                            &body body
                            &key
                            (timeout 0.2)
                            (max-batch 20)
                            (min-batch 3)
                            (arglist nil)
                            &allow-other-keys)
  (let ((fun (intern (format "%s--batched" command))))
    (let ((argforms (-some--> arglist
                    (--map (plist-get body (symbol-to-prop it)) it)
                    (--keep it it)
                    (--map (inline-lambda-2 it) it))))
      `(defun ,fun (,@arglist)
       ;;; (1) Check if a batch already exists
       (if-var
        batch (gethash ',command command-batch-table nil)
        (progn
;;; (1.1) If it does and it has reached max size or has expired
          ;;        Execute it immediately and go to (1.3)
          (cond
           ((< (command-batch-batch-size batch) (command-batch-min-batch-size batch))
;            (message "Underflowed batch: %s" (command-batch-name batch))
            (run-command-batch batch)
            (let* ((timer (command-batch-timer batch))
              (timeout (command-batch-timeout batch))
              (remaining-time (- (float-time (timer--time (command-batch-timer batch))) (time-to-seconds)))
              (delta (max (- timeout remaining-time) 0)))
              (timer-inc-time timer delta)
              (cl-incf (command-batch-batch-size batch))))
           ((= (command-batch-max-batch-size batch) (command-batch-batch-size batch))
;            (message "Overflowed batch: %s" (command-batch-name batch))
            (run-command-batch batch)
            (cancel-timer (command-batch-timer batch))
            (run-at-time ,timeout
                         nil
                         #'run-command-batch
                         (make-command-batch :name ',command
                                             :timeout ,timeout
                                             :max-batch-size ,max-batch
                                             :min-batch-size ,min-batch
                                             :batch-size ,min-batch
                                             :arglist (list ,@arglist)
                                             )))
           (t
;            (message "Extending batch: %s" (command-batch-name batch))
            (setf (command-batch-arglist batch)
                  (--map 
                   (cl-destructuring-bind (form value other) it
                     (funcall form value other))
                   (-zip-lists '(,@argforms) (list ,@arglist) (command-batch-arglist batch))))
            (let* ((timer (command-batch-timer batch))
              (timeout (command-batch-timeout batch))
              (remaining-time (- (float-time (timer--time (command-batch-timer batch))) (time-to-seconds)))
              (delta (max (- timeout remaining-time) 0)))
              (timer-inc-time timer delta)
              (cl-incf (command-batch-batch-size batch)))
            
            ;;; (1.2) If it does and is still valid
            ;;; (1.2.1) Collect previous arglist
            ;;; (1.2.2) Cancel timer / extend timer by timeout
            ;;          Extending is preferable so we don't generate as much garbage.
            ;;; (1.2.3) Run argforms on both arglists
            ;;; (1.2.4) Update batch with new arglist

            ))
          )
        ;;; (1.3)  If it does not
        ;;; (1.3.1) Create a new batch with arglist
        (progn
          (var new-batch (make-command-batch :name ',command
                                         :timeout ,timeout
                                         :max-batch-size ,max-batch
                                         :min-batch-size ,min-batch
                                         :arglist (list ,@arglist))
               (puthash ',command new-batch command-batch-table)
               (setf (command-batch-timer new-batch)
                     (run-at-time ,timeout
                     nil
                     #'run-command-batch
                     new-batch))))
          ;;; (1.3.2) Create new timer for batch
          )
       ))))


(with-batching pulseaudio-control-inc-dwim
               :arglist (step)
               :min-batch 0
               :max-batch 10
               :timeout 0.1
               :step (+ value other))

(with-batching pulseaudio-control-dec-dwim
               :arglist (step)
               :min-batch 0              
               :max-batch 10
               :timeout 0.1
               :step (+ value other))

(defun pulseaudio-control-inc-dwim-batched ()
  (interactive)
  (pulseaudio-control-inc-dwim--batched 2))

(defun pulseaudio-control-dec-dwim-batched ()
  (interactive)
  (pulseaudio-control-dec-dwim--batched 2))

(bind-key "H-=" #'pulseaudio-control-inc-dwim-batched)
(bind-key "H--" #'pulseaudio-control-dec-dwim-batched)
(bind-key "<XF86AudioRaiseVolume>" #'pulseaudio-control-inc-dwim-batched)
(bind-key "<XF86AudioLowerVolume>" #'pulseaudio-control-dec-dwim-batched)

(provide 'module-batch)
