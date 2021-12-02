;;; module-pulseaudio.el --- Pulseaudio control in emacs 

;; Copyright (C) 2021  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: convenience, tools, multimedia

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

(require 'consult)
(require 'marginalia)
(require 's)
(require 'hydra)


(use-package pulseaudio-control
  :bind
  (("H-=" . pulseaudio-control-increase-volume)
   ("H--" . pulseaudio-control-decrease-volume)
   ("H-m" . pulseaudio-control-toggle-current-sink-mute)
   ("H-M" . pulseaudio-control-toggle-sink-input-mute-by-index)
   ("H-d" . pulseaudio-sink-input-hydra/body))
  :config
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "2%"
        pulseaudio-control--volume-maximum '(("percent" . 100) ("decibels" . 10) ("raw" . 98000)))
  (pulseaudio-control-default-keybindings))

(defun pulseaudio-read-sink-input ()
  (interactive)
  (let* ((valid-sink-inputs (pulseaudio-control--get-sink-inputs))
           (completion-choices
            (mapcar
             (lambda (el)
               (cons (concat
                      (alist-get "media.name" (cdr el)
                                 (alist-get "application.name"
                                            (cdr el)
                                            nil
                                            nil
                                            #'string=)
                                 nil #'string=)
                      " - "
                      (car el))
                     (car el)))
             (pulseaudio-control--get-sink-inputs)))
           (sink-input (consult--read completion-choices
                                      :prompt "Sink input name: "
                                      :category 'sink-input
                                      :lookup #'(lambda (input candidates cand-string) (alist-get cand-string candidates nil nil #'string=)))))
      sink-input))

(defun pulseaudio-control-toggle-sink-input-mute-by-index (index)
  "Toggle muting of Pulse sink-input by index."
  (interactive
   (list
    (pulseaudio-read-sink-input)))
  
  (pulseaudio-control--set-sink-input-mute index "toggle"))

(add-to-list 'marginalia-prompt-categories '("\\Sink input\\>" . sink-input))

(defun marginalia-annotate-sink-input (cand) 
 (let* ((cand-id (cl-second (s-match ".* - \\([0-9]+\\)$" cand)))
         (sink-input (alist-get cand-id (pulseaudio-control--get-sink-inputs) nil nil #'string=)))
    (marginalia--fields
     ((alist-get "Mute" sink-input nil nil #'string=) :truncate 3)
     ((pulseaudio-volume-as-percentage (alist-get "Volume" sink-input nil nil #'string=)) :truncate 9)
     ((alist-get "application.name" sink-input "unknown" nil #'string=)
      :truncate 20))))

(add-to-list 'marginalia-annotator-registry '(sink-input marginalia-annotate-sink-input builtin none))


(defun pulseaudio-volume-as-percentage (volume-string)
  (s-join " " (s-match "\\([0-9]+%\\)" volume-string)))

(defun pulseaudio-control--get-sink-input-prop (id prop)
  (let (beg)
    (with-temp-buffer
      (pulseaudio-control--call-pactl "list sink-inputs")
      (goto-char (point-min))
      (search-forward (concat "Sink Input #" id))
      (search-forward-regexp (format "%s\\(:\\| =\\)" prop))
      (backward-word)
      (setq beg (point))
      (move-end-of-line nil)
      (buffer-substring beg (point)))))

(defun pulseaudio-control--get-sink-input-volume (id)
  "Get volume of the sink input with id ID."
  (pulseaudio-volume-as-percentage (pulseaudio-control--get-sink-input-prop id "Volume")))

(defun pulseaudio-control--get-sink-input-mute (id)
  "Get mute status of the sink input with id ID."
  (s-replace "Mute: yes" "Muted" (s-replace "Mute: no" "Not Muted" (pulseaudio-control--get-sink-input-prop id "Mute"))))

(defvar pulseaudio-control--selected-sink-input-id nil)

(defun pulseaudio-control--act-on-sink-input-volume (id action)
  (let ((valid-volumes-re (concat
                           "[+-]?\\("
                           "[[:digit:]]+%"
                           "\\|[[:digit:]]+dB"
                           "\\|[[:digit:]]+\\.[[:digit:]]+"
                           "\\)")))
    (if (string-match valid-volumes-re action)
        (pulseaudio-control--call-pactl (concat "set-sink-input-volume "
                                                id
                                                " "
                                                action))
      (error "Invalid volume action"))
    (when (> (reduced-volume-of-sink-input id)
             100)
      (pulseaudio-control--call-pactl (concat "set-sink-input-volume "
                                              id
                                              " 100%")))))

(defun pulseaudio-control--set-sink-input-volume (id volume)
  "Set VOLUME of the sink-input ID.

The value of VOLUME can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Argument VOLUME is the volume provided by the user." 
  (pulseaudio-control--act-on-sink-input-volume id volume))

(defun pulseaudio-control--inc-sink-input-volume (id)
  (pulseaudio-control--act-on-sink-input-volume id (concat "+" pulseaudio-control-volume-step)))

(defun pulseaudio-control--dec-sink-input-volume (id)
  (pulseaudio-control--act-on-sink-input-volume id (concat "-" pulseaudio-control-volume-step)))

(defun pulseaudio-control--get-current-sink-input-media ()
  (format "%s" (alist-get "media.name"
                          (alist-get pulseaudio-control--selected-sink-input-id
                                     (pulseaudio-control--get-sink-inputs)
                                     nil
                                     nil
                                     #'string=)
                          nil
                          nil
                          #'string=)))

(defun get-best-fill (value fills)
  (let ((value-as-perc (* 100 value)))
    (cdr (--max-by
          (car it)
          (--filter
           (>= (car it) value-as-perc)
           fills)))))

(cl-defun make-progress-bar (value &key
                                (from 0.0)
                                (to 100.0)
                                (ascii nil)
                                (width "")
                                (show-label t)
                                (label-format "[%d%%]"))
  (let* ((chars
         (cond
          ((numberp width) width)
          ((stringp width) (max (- (frame-width) (length width)) 0))))
         (percentage-fill-unrounded (* 100 (/ (- value from) (- to from))))
         (percentage-fill (round percentage-fill-unrounded))
         (progress-per-char (/ 100.0 chars))
         (chars-of-max-progress (floor percentage-fill-unrounded progress-per-char))
         (leftover-progress (- percentage-fill-unrounded (* chars-of-max-progress progress-per-char))))
    (if ascii
        (let ((fills '((100 . "=") (0 . "-"))))
          (concat
           (s-repeat chars-of-max-progress (get-best-fill 1 fills))
           (get-best-fill (/ leftover-progress progress-per-char) fills)
           (s-repeat (- chars chars-of-max-progress 1) (get-best-fill 0 fills))
           )
          )
      (let ((fills '((100 . "█") (75 . "▓") (50 . "▒") (25 . "░") (0 . "-"))))
        (concat
           (s-repeat chars-of-max-progress (get-best-fill 1 fills))
           (unless  (/ leftover-progress progress-per-char) (get-best-fill (/ leftover-progress progress-per-char) fills))
           (s-repeat (- chars chars-of-max-progress 1) (get-best-fill 0 fills))
           ))
      )))

(defun reduced-volume-of-sink-input (id)
  (let* ((volume-pair (--reduce
                (cons (+ (car it) (car acc)) (+ (cdr it) (cdr acc)))
                (-zip-fill 1
                           (--map (string-to-number (s-replace "%" "" it))
                                  (split-string (pulseaudio-control--get-sink-input-volume id)
                                                " "))
                           '())))
        (volume (/ (car volume-pair)
                   (cdr volume-pair))))
    volume))

(defun sink-input-volume-bar ()
    (car (read-from-string (make-progress-bar (reduced-volume-of-sink-input pulseaudio-control--selected-sink-input-id)  :width "- [100%] [] +"))))

(defhydra pulseaudio-sink-input-hydra  
  (:body-pre (setq pulseaudio-control--selected-sink-input-id (pulseaudio-read-sink-input))
   :hint none
   :color amaranth)
  "
Volume of %(pulseaudio-control--get-current-sink-input-media)
_-_ [?=?] [%(sink-input-volume-bar)] _=_
_s_: Set
_m_: ?m?
_q_: Quit"
  ("=" (pulseaudio-control--inc-sink-input-volume pulseaudio-control--selected-sink-input-id)
   (format "%03d%%%%" (reduced-volume-of-sink-input pulseaudio-control--selected-sink-input-id)) :exit nil)
  ("-" (pulseaudio-control--dec-sink-input-volume pulseaudio-control--selected-sink-input-id)
   (format "%s" (pulseaudio-control--get-sink-input-volume pulseaudio-control--selected-sink-input-id)) :exit nil)
  ("s" (let ((set-to (format "%s" (read-string "Set volume: "))))
         (pulseaudio-control--set-sink-input-volume pulseaudio-control--selected-sink-input-id set-to))
   (format "%s" (pulseaudio-control--get-sink-input-volume pulseaudio-control--selected-sink-input-id)) :exit nil)
  ("m" (pulseaudio-control-toggle-sink-input-mute-by-index pulseaudio-control--selected-sink-input-id)
   (format "%s" (pulseaudio-control--get-sink-input-mute pulseaudio-control--selected-sink-input-id)) :exit nil)  
  ("q" (message "Done") "Quit" :exit t))

(cl-defun exwm-buffer-if-media-source-get-id (&optional (buffer nil))
  (when-let* ((buffer (or buffer (current-buffer)))
              (name (buffer-name buffer))
              (inputs (pulseaudio-control--get-sink-inputs))
              (id-and-media (--map
                             (cons (alist-get "media.name" (cdr it) nil nil #'string=) (car it))
                             inputs))
              (potential-candidates (--map
                                     (cons (concat (car it) " - " (cdr it)) (cdr it))
                                     (--filter
                                      (s-contains? (car it) name t)
                                      id-and-media)))
              (cand (if (< (cl-list-length potential-candidates) )
                        (caar potential-candidates)
                      (consult--read potential-candidates
                                     :prompt "Act on sink input: "
                                     :category 'sink-input
                                     :require-match t))))
    (cl-second (s-match ".* - \\([0-9]+\\)$" cand))))

(defun pulseaudio-control-inc-dwim ()
  (interactive)
  (if-let ((id (exwm-buffer-if-media-source-get-id)))
      (pulseaudio-control--inc-sink-input-volume id)
    (pulseaudio-control-increase-volume)))

(defun pulseaudio-control-dec-dwim ()
  (interactive)
  (if-let ((id (exwm-buffer-if-media-source-get-id)))
      (pulseaudio-control--dec-sink-input-volume id)
    (pulseaudio-control-decrease-volume)))

(defun pulseaudio-control-mute-dwim ()
  (interactive)
  (if-let ((id (exwm-buffer-if-media-source-get-id)))
      (pulseaudio-control-toggle-sink-input-mute-by-index id)
    (pulseaudio-control-toggle-current-sink-mute)))

(setq posframe-mouse-banish-function #'posframe-mouse-banish-simple)



(provide 'module-pulseaudio)
;;; module-pulseaudio.el ends here
