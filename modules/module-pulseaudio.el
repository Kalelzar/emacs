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
  :demand t
  :bind
  (("H-=" . pulseaudio-control-inc-dwim)
   ("H--" . pulseaudio-control-dec-dwim)
   ("H-m" . pulseaudio-control-mute-dwim)
   ("H-M" . pulseaudio-control-toggle-sink-input-mute-by-index)
   ("H-d" . pulseaudio-sink-input-hydra/body))
  :config
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "2%"
        pulseaudio-control--volume-maximum '(("percent" . 100) ("decibels" . 10) ("raw" . 98000)))
  (pulseaudio-control-default-keybindings))

(defun pulseaudio-control--get-current-description ()
  "Get description of currently-selected sink."
  (s-chomp (shell-command-to-string "pulsebinder machine print | cut -d ',' -f4")))


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
  
  (pulseaudio-control--set-sink-input-mute index "toggle")
  (pulseaudio-control--maybe-print-sink-input-status index))

(add-to-list 'marginalia-prompt-categories '("\\Sink input\\>" . sink-input))

(defun marginalia-annotate-sink-input (cand) 
 (let* ((cand-id (cl-second (s-match ".* - \\([0-9]+\\)$" cand)))
         (sink-input (alist-get cand-id (pulseaudio-control--get-sink-inputs) nil nil #'string=)))
    (marginalia--fields
     ((alist-get "Mute" sink-input nil nil #'string=) :truncate 3)
     ((pulseaudio-control--get-sink-input-active cand-id) :truncate 8)
     ((pulseaudio-volume-as-percentage (alist-get "Volume" sink-input nil nil #'string=)) :truncate 9)
     ((alist-get "application.name" sink-input "unknown" nil #'string=)
      :truncate 20))))



(add-to-list 'marginalia-annotator-registry '(sink-input marginalia-annotate-sink-input builtin none))

(defun pulseaudio-control--get-sink-input-active (id)
  (when-let (val (cl-second (s-split-words (pulseaudio-control--get-sink-input-prop id "Corked"))))
    (cond  
     ((string= val "yes") "inactive")
     ((string= val "no") "active"))))

(defun pulseaudio-volume-as-percentage (volume-string)
  (s-join " " (s-match "\\([0-9]+%\\)" volume-string)))

(defun pulseaudio-control--get-sink-input-prop (id prop)
  (let (beg)
    (with-temp-buffer
      (pulseaudio-control--call-pactl "list sink-inputs")
      (goto-char (point-min))
      (search-forward (concat "Sink Input #" id))
      (search-forward-regexp (format "%s\\(:\\| =\\)" (regexp-quote prop)))
      (beginning-of-line-text)
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
                                              " 100%")))
    (pulseaudio-control--maybe-print-sink-input-status id)))

(defun pulseaudio-control--maybe-print-sink-input-status (id)
  (when pulseaudio-control-volume-verbose
    (let* ((volume (reduced-volume-of-sink-input id))
           (header (format "[%03d%%]" volume))
           (width (min (- (frame-width) (length header)) (window-width) (round (frame-width) 2))))
      (exwm-show-msg (format "%s\n%s\n%s"
                             (cl-second (s-match ".* = \"\\(.*\\)\"$" (pulseaudio-control--get-sink-input-prop id "media.name")))
                             (if (string= (pulseaudio-control--get-sink-input-mute id)
                                          "Not Muted")
                                 (make-progress-bar volume
                                                    :width width
                                                    :label header)
                               (make-progress-bar volume
                                                  :width width
                                                  :fill-face 'error
                                                  :left-face 'success
                                                  :label header))
                             (pulseaudio-control-format-volume))
                     :center t
                     ))))

(defun pulseaudio-control-format-volume ()
  "Format volume of currently-selected Pulse sink."
  (interactive)
  (let* ((volumes-and-mute (s-split " "
                                (pulseaudio-control--get-current-volume-and-mute-with-pulsebinder)))
bv        (volume (cl-destructuring-bind (left right mute) volumes-and-mute
                    (round (+ (string-to-number left) (string-to-number right)) 2)))
(left (string-to-number (car volumes-and-mute)) )
(right (string-to-number (cadr volumes-and-mute)))
          
(mute (string= "true" (caddr volumes-and-mute)))
        (header (format "[%03d%%]" volume))
        (width (min (- (frame-width) (length header)) (window-width) (round (frame-width) 2))))
    (if (= left right)
        (format "%s\n%s"
                (pulseaudio-control--get-current-description)
                (if mute
                    (make-progress-bar volume
                                       :width width
                                       :fill-face 'error
                                       :left-face 'success
                                       :label header)
                  (make-progress-bar volume
                                     :width width
                                     :label header)))
      (format "%s\n%s\n L: %s\n R: %s"
              (pulseaudio-control--get-current-description)
              (if mute
                  (make-progress-bar volume
                                     :width width
                                     :fill-face 'error
                                     :left-face 'success
                                     :label header)
                (make-progress-bar volume
                                   :width width
                                   :label header))
              (if mute
                  (make-progress-bar left
                                     :width (- width 4)
                                     :fill-face 'error
                                     :left-face 'success
                                     :label (format "[%03d%%]" left))
                (make-progress-bar left
                                   :width (- width 4)
                                   :label (format "[%03d%%]" left)))
              (if mute
                  (make-progress-bar right
                                     :width (- width 4)
                                     :fill-face 'error
                                     :left-face 'success
                                     :label (format "[%03d%%]" right))
                (make-progress-bar right
                                   :width (- width 4)
                                   :label (format "[%03d%%]" right))))
      )
    ))


(defun pulseaudio-control-display-volume ()
  (if-let ((id (exwm-buffer-if-media-source-get-id)))
      (pulseaudio-control--maybe-print-sink-input-status id)
  (exwm-show-msg (pulseaudio-control-format-volume) :center t)))

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
                                (label nil)
                                (fill-face 'success)
                                (left-face 'error))
  (let* ((chars
         (cond
          ((numberp width) width)
          ((stringp width) (max (- (frame-width) (length width)) 0))))
         (percentage-fill-unrounded (* 100.0 (/ (* 1.0 (- value from)) (- to from))))
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
      (let* ((fills '((100 . "█") (75 . "▓") (50 . "▒") (25 . "░") (0 . "-")))
             (full-part (propertize (s-repeat chars-of-max-progress (get-best-fill 1 fills)) 'face fill-face))
             (partial (when (> leftover-progress 0) (propertize (get-best-fill (/ leftover-progress progress-per-char) fills) 'face fill-face)))
             (left (propertize (s-repeat (- chars chars-of-max-progress (if (> (length partial) 0) 1 0)) (get-best-fill 0 fills)) 'face left-face))
             (bar (concat
                   full-part
                   partial
                   left)))
        (if (null label)
            bar
          (let ((n (floor (- chars (length label)) 2)))
            (concat
             (s-left  n bar)
             (propertize (s-left (max 0 (- chars-of-max-progress n)) label) 'face `(:inherit ,fill-face :inverse-video t :background "#000"))
             (propertize (s-right (max 0(- (length label) (- chars-of-max-progress n))) label) 'face left-face)
             (s-right n bar))))
))))

(defun reduced-volume-of-sink-input (id)
  (let* ((volume-pair
          (-some--> (pulseaudio-control--get-sink-input-volume id)
            (split-string it " ")
            (--map (--> it
                        (s-replace "%" "" it)
                        (string-to-number it))
                   it)
            (-zip-fill 1
                       it
                       '())
            (--reduce
             (cons (+ (car it) (car acc))
                   (+ (cdr it) (cdr acc)))
             it)))
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
                                      (when (stringp (car it))(s-contains? (car it) name t))
                                      id-and-media)))
              (cand (if (< (cl-list-length potential-candidates) )
                        (caar potential-candidates)
                      (consult--read potential-candidates
                                     :prompt "Act on sink input: "
                                     :category 'sink-input
                                     :require-match t))))
    (cl-second (s-match ".* - \\([0-9]+\\)$" cand))))

(defun pulseaudio-control-inc-dwim (&optional step)
  (interactive)
  (if-let ((id (exwm-buffer-if-media-source-get-id)))
      (pulseaudio-control--inc-sink-input-volume id)
    (pulseaudio-control-increase-volume step)))

(defun pulseaudio-control-dec-dwim (&optional step)
  (interactive)
  (if-let ((id (exwm-buffer-if-media-source-get-id)))
      (pulseaudio-control--dec-sink-input-volume id)
    (pulseaudio-control-decrease-volume step)))

(defun pulseaudio-control-mute-dwim ()
  (interactive)
  (if-let ((id (exwm-buffer-if-media-source-get-id)))
      (pulseaudio-control-toggle-sink-input-mute-by-index id)
    (pulseaudio-control-toggle-current-sink-mute)))
      

;;; Overriding `pulseaudio-control' commands

(defun pulseaudio-control--get-default-sink ()
  "Get index of DEFAULT_SINK."
  (s-chomp (shell-command-to-string "pulsebinder machine print | cut -d, -f2")))

(defun pulseaudio-control--get-current-volume-with-pulsebinder ()
  "Get volume of currently-selected sink."
  (s-trim (s-chomp (shell-command-to-string "pulsebinder sink-by-default channel-volume"))))

(defun pulseaudio-control--get-current-volume-and-mute-with-pulsebinder ()
  "Get volume and mute status of currently-selected sink."
  (shell-command-to-string "pulsebinder sink-by-default channel-volume is-mute | tr -d '\n'"))

(defun pulseaudio-control--get-current-mute-with-pulsebinder ()
  "Get volume of currently-selected sink."
  (s-chomp (shell-command-to-string "pulsebinder sink-by-default is-mute")))

;;;###autoload
(defun pulseaudio-control-decrease-volume (&optional volume)
  "Decrease volume of currently-selected Pulse sink.

Amount of decrease is specified by `pulseaudio-control-volume-step'."
  (interactive)
  (setq volume (if volume
      (format "%d%%" (max (min volume 100) 0))
       pulseaudio-control-volume-step))
  (pulseaudio-control--maybe-update-current-sink)
  (pulseaudio-control--call-pactl
   (concat "set-sink-volume "
           pulseaudio-control--current-sink
           " -"
           volume))
  (if pulseaudio-control-volume-verbose
      (pulseaudio-control-display-volume)))

;;;###autoload
(defun pulseaudio-control-increase-volume (&optional volume)
  "Increase volume of currently-selected Pulse sink.

Amount of increase is specified by `pulseaudio-control-volume-step'."
  (interactive)
  (setq volume (if volume
      (format "%d%%" (max (min volume 100) 0))
       pulseaudio-control-volume-step))
  (pulseaudio-control--maybe-update-current-sink)
  (let* ((volume-step-unit
          (if (string-match "\\(%\\|dB\\)"
                            volume)
              (match-string 1 volume)
            nil))
         (volume-step
          (cond
           ((string-equal "%" volume-step-unit)
            (if (string-match "^\\([[:digit:]]+\\)%"
                              volume)
                (string-to-number
                 (match-string 1 volume))
              (user-error "Invalid step spec in `pulseaudio-control-volume-step'")))
           ((string-equal "dB" volume-step-unit)
            (if (string-match "^\\([[:digit:]]+\\)dB"
                              volume)
                (string-to-number
                 (match-string 1 pulseaudio-control-volume-step))
              (user-error "Invalid step spec in `pulseaudio-control-volume-step'")))
           ((if (string-match "^\\([[:digit:]]+\\.[[:digit:]]+\\)"
                              volume)
                (string-to-number volume)
              (user-error "Invalid step spec in `pulseaudio-control-volume-step'")))))
         (volume-max
          (cond
           ((string-equal "%" volume-step-unit)
            (cdr (assoc "percent" pulseaudio-control--volume-maximum)))
           ((string-equal "dB" volume-step-unit)
            (cdr (assoc "decibels" pulseaudio-control--volume-maximum)))
           (t
            (cdr (assoc "raw" pulseaudio-control--volume-maximum)))))
         (volumes-current (pulseaudio-control--get-current-volume))
         (volumes-re-component
          (concat
           "\\([[:digit:]]+\\)"
           "\\s-+/\\s-+"
           "\\([[:digit:]]+\\)%"
           "\\s-+/\\s-+"
           "\\(-?\\([[:digit:]]+\\(\\.[[:digit:]]+\\)?\\)\\|-inf\\) dB"))
         (volumes-re (concat volumes-re-component
                             "[^[:digit:]]+"
                             volumes-re-component))
         (volumes-alist
          (progn
            (string-match volumes-re volumes-current)
            `(("raw-left" . ,(string-to-number
                              (match-string 1 volumes-current)))
              ("percentage-left" . ,(string-to-number
                                     (match-string 2 volumes-current)))
              ("db-left" . ,(if (string=
                                 (match-string 3 volumes-current)
                                 "-inf")
                                pulseaudio-control--volume-minimum-db
                              (string-to-number
                               (match-string 3 volumes-current))))
              ("raw-right" . ,(string-to-number
                               (match-string 6 volumes-current)))
              ("percentage-right" . ,(string-to-number
                                      (match-string 7 volumes-current)))
              ("db-right" . ,(if (string=
                                  (match-string 8 volumes-current)
                                  "-inf")
                                 pulseaudio-control--volume-minimum-db
                               (string-to-number
                                (match-string 8 volumes-current))))))))
    (let ((clamp nil)
          (clamp-value nil))
      (cond
       ((string-equal "%" volume-step-unit)
        (if (or (> (+ (cdr (assoc "percentage-left" volumes-alist)) volume-step)
                   volume-max)
                (> (+ (cdr (assoc "percentage-right" volumes-alist)) volume-step)
                   volume-max))
            (progn
              (setq clamp t)
              (setq clamp-value (concat (number-to-string volume-max) "%")))))
       ((string-equal "dB" volume-step-unit)
        (if (or (> (+ (cdr (assoc "db-left" volumes-alist)) volume-step)
                   volume-max)
                (> (+ (cdr (assoc "db-right" volumes-alist)) volume-step)
                   volume-max))
            (progn
              (setq clamp t)
              (setq clamp-value (concat (number-to-string volume-max) "dB")))))
       (t
        (if (or (> (+ (cdr (assoc "raw-left" volumes-alist)) volume-step)
                   volume-max)
                (> (+ (cdr (assoc "raw-right" volumes-alist)) volume-step)
                   volume-max))
            (progn
              (setq clamp t)
              (setq clamp-value (number-to-string volume-max))))))

      (if clamp

          ;; Clamp volume to value of `pulseaudio-control--volume-maximum'.

          (pulseaudio-control--call-pactl
           (concat "set-sink-volume "
                   pulseaudio-control--current-sink
                   " "
                   clamp-value))

        ;; Increase volume by `pulseaudio-control-volume-step'.
        ;;
        ;; Once the PulseAudio volume becomes "0 / 0% / -inf dB", we can't:
        ;; * increase volume by x dB units, because -inf + x = -inf;
        ;; * specify an absolute dB value of -120, because pactl interprets
        ;;   this as "decrease volume by 120dB";
        ;; * scale by a linear factor of x, because 0 * x = 0.
        ;; So in this situation, when the user is using a dB value or
        ;; linear factor to increase volume, we set the volume to an arbitrary
        ;; small non-zero raw value, which subsequent volume increases can
        ;; act upon.

        (if (and (or (not volume-step-unit) ; `volume-step-unit' is nil
                     (string= "dB" volume-step-unit))
                 (or (= 0 (cdr (assoc "raw-left" volumes-alist)))
                     (= 0 (cdr (assoc "raw-right" volumes-alist)))))
            (pulseaudio-control--call-pactl
             (concat "set-sink-volume "
                     pulseaudio-control--current-sink
                     " 100"))
          (pulseaudio-control--call-pactl
           (concat "set-sink-volume "
                   pulseaudio-control--current-sink
                   " +"
                   volume)))))
    (if pulseaudio-control-volume-verbose
        (pulseaudio-control-display-volume))))


(provide 'module-pulseaudio)
;;; module-pulseaudio.el ends here
