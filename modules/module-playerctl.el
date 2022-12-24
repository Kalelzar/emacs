;;; module-playerctl.el --- Module for interaction with the playerctl command-line utility  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: convenience, tools

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

(require 'dash)
(require 's)
(require 'marginalia)
(require 'consult)
(require 'module-pulseaudio)

(defun playerctl-get-all-players-metadata ()
  "Get the metadata of all available players via `playerctl'.
This function calls the shell command 'playerctl -a metadata' and
parses the result.
TODO: It could be improved by changing the output format to something
more easily parseable such as a sexp or a json object.
"
  (-some--> (shell-command-to-string "playerctl -a metadata")
    (if (s-matches? "No players found\n" it) nil it)
    (s-split "\n" it t)
    (--map (cdr (s-match "\\([a-zA-Z]+\\)[[:space:]]+[a-zA-Z]+:\\([a-zA-Z]+\\)[[:space:]]+\\(.*\\)"  it)) it)
    (-group-by #'car it)
    (--map (--map (if (listp it) (cdr it) it)  it) it)
    (--map (append
            it
            (list (cons "status" (if  (-some-->
                                    (shell-command-to-string (format "playerctl -p '%s' status" (car it)))
                                  (s-split "\n" it t)
                                  (s-join "" it)
                                  (string= "Paused" it))
                               nil
                             t))))
           it)))

(defun playerctl-get-active-players-metadata ()
  "Return the metadata of all currently active players.
Active as in currently playing media."
  (--filter (alist-get "status" (cdr it) nil nil #'string=)
            (playerctl-get-all-players-metadata)))

(defun playerctl-get-active-players ()
  "Return all currently active players.
Active as in currently playing media."
  (-map #'car
        (playerctl-get-active-players-metadata)))


(cl-defun playerctl-select-player (&optional filter)
  "Interactively ask user to pick a player."
  (interactive)
  (when-let ((players (-map #'car (cond
                                   ((eq :active filter) (playerctl-get-active-players-metadata)) 
                                   (t (playerctl-get-all-players-metadata))))))
    (consult--read
     players
     :category 'mpris-player
     :prompt "Select player: ")))

(defun playerctl-get-metadata-ask-for-player ()
  "Return metadata for a interctively user-specified player.
Same as calling `playerctl-get-metadata-of-player' with
`playerctl-select-player' as argument."
  (let ((player (playerctl-select-player)))
    (playerctl-get-metadata-of-player player)))

(defun playerctl-get-metadata-of-player (player)
  "Return the metadata of the player called PLAYER."
  (cl-acons "player" player  (alist-get player (playerctl-get-all-players-metadata) nil nil #'string=)))


(defun playerctl-act-on-player (command player)
  "Applies COMMAND as an action to PLAYER.
Same as executing 'playerctl -p PLAYER COMMAND'

COMMAND is a string that matches any of the following:
'play' 
'pause'
'next'
'play-pause'
'previous'
'status'
'stop'
'metadata'
'loop' 
"
  (interactive (list
                (completing-read "Action"
                                 '("play"
                                   "pause"
                                   "next"
                                   "play-pause"
                                   "next"
                                   "previous"
                                   "status"
                                   "stop"
                                   "metadata"
                                   "loop"))
                (playerctl-select-player)))
  (message
   "%s"
   (shell-command-to-string (format "playerctl -p '%s' '%s'"
                                    player
                                    command))))


(with-eval-after-load 'module-exwm
  (cl-defun exwm-get-buffers-playing-media (&optional (media nil))   
    (--filter
     (when-let* ((name it)
                 (inputs (pulseaudio-control--get-sink-inputs))
                 (id-and-media (--map
                                (cons (alist-get "media.name"
                                                 (cdr it)
                                                 nil
                                                 nil
                                                 #'string=)
                                      (car it))
                                inputs))
                 (matching-media (--filter
                                  (if media
                                      (s-contains? media (car it))
                                    t)
                                  id-and-media))
                 (potential-candidates
                  (--map
                   (cons (concat (car it) " - " (cdr it)) (cdr it))
                   (--filter
                    (s-contains? (car it) name t)
                    matching-media)))
                 (cand
                  (if (< (cl-list-length potential-candidates) )
                      (caar potential-candidates)
                    (consult--read potential-candidates
                                   :prompt "Act on sink input: "
                                   :category 'sink-input
                                   :require-match t))))
       cand)
     (-map #'buffer-name (exwm-all-buffers))))

  (defun playerctl-goto-player-app (player)
    (interactive (list (playerctl-select-player :active)))
    (when-let* (player
                (media-buffers (playerctl-get-buffer-of player))
                (buffer (if (> (cl-list-length media-buffers) 1)
                            (consult--read
                             media-buffers
                             :prompt "Select buffer: "
                             :category 'buffer
                             :require-match t)
                          (car-safe media-buffers))))
      (exwm-workspace-switch-to-buffer buffer))) 

  (defun playerctl-get-active-buffers ()
    (-flatten (-map #'playerctl-get-buffer-of (playerctl-get-active-players))))

  (defun playerctl-get-buffer-of (player)
    (let 
        ((media (car (alist-get "title"
                                (playerctl-get-metadata-of-player player)
                                nil
                                nil
                                #'string=))))
      (exwm-get-buffers-playing-media media)))
)



(add-to-list 'marginalia-annotator-registry '(mpris-player marginalia-annotate-mpris-player builtin none))

(defun playerctl-get-volume (player)
  (let ((volume-or-error (shell-command-to-string (format "playerctl -p '%s' volume" player))))
    (if (s-matches-p "^[01][.][0-9]+$" volume-or-error)
        (round (* (string-to-number volume-or-error) 100))
      (when-let*
          ((media (car (alist-get "title"
                                  (playerctl-get-metadata-of-player player)
                                  nil
                                  nil
                                  #'string=)))
           (inputs (pulseaudio-control--get-sink-inputs))
           (id-and-media (--map
                          (cons (alist-get "media.name"
                                           (cdr it)
                                           nil
                                           nil
                                           #'string=)
                                (car it))
                          inputs))
           (matching-media (--filter
                            (if media
                                (s-contains? media (car it))
                              t)
                            id-and-media))
           (media (cdar matching-media))
           (volume (reduced-volume-of-sink-input  media))
           )
        volume
          )
        )
    ))

(defun playerctl-current-player-title ()
  (car (alist-get "title"
                  (playerctl-get-metadata-of-player playerctl-current-player)
                  nil
                  nil
                  #'string=)))

(defun playerctl-current-player-state-string ()
  (if (alist-get "status"
                  (playerctl-get-metadata-of-player playerctl-current-player)
                  nil
                  nil
                  #'string=)
      "playing"
    "paused"))

(defun playerctl-current-volume-bar ()
  (car (read-from-string (make-progress-bar (playerctl-get-volume playerctl-current-player)  :width "- [100%] [] +"))))


(defun playerctl (player)
  (interactive
   (list (if-let* ((pulseaudio-source (exwm-buffer-if-media-source-get-id))
                   (pulse-media (cl-second (s-match "media.name = \"\\(.*\\)\"" (pulseaudio-control--get-sink-input-prop pulseaudio-source "media.name"))))
                   (maybe-player (--find (s-contains-p (car (alist-get "title" it nil nil #'string=)) pulse-media)
                                  (playerctl-get-all-players-metadata)
                                  )))
             (car maybe-player)
           (playerctl-select-player))))
  (setq playerctl-current-player player)
  (playerctl-hydra/body)
  (setq playerctl-current-player nil)
  )

(defvar playerctl-current-player "firefox")
(defun playerctl-get-current-player () playerctl-current-player)

(defhydra playerctl-hydra  
  (:hint none
   :color amaranth)
  "
%(car (read-from-string playerctl-current-player)) is %(car (read-from-string (playerctl-current-player-state-string))) %(s-replace \"%\" \"%%\" (playerctl-current-player-title))
_-_ [?=?] [%(playerctl-current-volume-bar)] _=_
_p_: Pause/Play
_o_: Open
_._: Next
_,_: Previous
_q_: Quit"
  ("=" (pulseaudio-control--inc-sink-input-volume pulseaudio-control--selected-sink-input-id)
   (format "%03d%%%%" (playerctl-get-volume playerctl-current-player)) :exit nil)
  ("-" (pulseaudio-control--dec-sink-input-volume pulseaudio-control--selected-sink-input-id)
   "" :exit nil)
  ("p" (message "Not Implemented"))
  ("o" (message "Not Implemented"))
  ("." (message "Not Implemented"))
  ("," (message "Not Implemented"))
  ("q" (message "Done") "Quit" :exit t))


(provide 'module-playerctl)
;;; module-playerctl.el ends here
