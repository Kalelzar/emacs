;;; module-bufler.el --- Configuration module for bufler  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: tools, convenience

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

(require 'use-package)
(require 'consult)

(use-package bufler
  :after consult
  :demand t
  :init

  (defun bufler-disable-workspace ()
    "Reset the `frame-parameters' `bufler-workspace-path' and `bufler-workspace-path-formatted' to nil."
    (interactive)
    (set-frame-parameter nil 'bufler-workspace-path nil)
    (set-frame-parameter nil 'bufler-workspace-path-formatted nil))

  (defvar consult--bufler-workspace+
    `(:name "Workspace"
            :narrow ?w
            :category buffer
            :face consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :enabled  ,(lambda () (frame-parameter nil 'bufler-workspace-path))
            :items
            ,(lambda ()
               (let ((bufler-vc-state nil))
                 (mapcar #'buffer-name
                         (mapcar #'cdr
                                 (bufler-buffer-alist-at
                                  (frame-parameter nil 'bufler-workspace-path)
                                  :filter-fns bufler-filter-buffer-fns))))))
    "Bufler workspace buffers source for `consult-buffer'.")

  (add-to-list 'consult-buffer-sources 'consult--bufler-workspace+)

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
                                                       :as #'buffer-name)))))

(provide 'module-bufler)
;;; module-bufler.el ends here
