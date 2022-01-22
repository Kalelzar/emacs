;;; module-jq.el --- Library for jq integration      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: data, tools, terminals, convenience

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

(defun get-region-or-buffer-string ()
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))       
    (buffer-substring (buffer-end -1) (buffer-end 1))))

(defun act-on-region-or-buffer (action)
  (interactive (list (org-read-function "Action: ")))
  (let* ((str (get-region-or-buffer-string))
         (curried (-partial action str)))
    (if (region-active-p)
        (replace-region-contents
         (region-beginning)
         (region-end)
         curried)
      (replace-region-contents
       (buffer-end -1)
       (buffer-end 1)
       curried))))


(defun jq-on-region-or-buffer ()
  (interactive)
  (act-on-region-or-buffer  #'(lambda (data)
                                (shell-command-to-string
                                 (format
                                  "echo -e '%s' | jq '%s'"
                                  data
                                  (read-string "JQ query: "))))))

(defun jq-format ()
  (interactive)
  (act-on-region-or-buffer  #'(lambda (data)
                                (shell-command-to-string
                                 (format
                                  "echo -e '%s' | jq"
                                  data)))))

(provide 'module-jq)
;;; module-jq.el ends here
