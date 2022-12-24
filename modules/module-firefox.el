;;; module-firefox.el --- Convenience functions for working with firefox  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: convenience

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

(require 'marginalia)
(require 'vertico)
(require 'f)

(use-package emacsql)
(use-package emacsql-sqlite
  :after emacsql)

(defvar firefox-profile-path "~/.mozilla/firefox/auk2pako.default-release/")
(defvar firefox-places-db (f-join firefox-profile-path "places.sqlite"))

(defconst firefox-places-query [:select [visit_count url title frecency] :from moz_places])

;; (defconst firefox-places-query-string
;;   (let* ((db (emacsql-sqlite firefox-places-db))
;;          (expr (emacsql-compile db firefox-places-query)))
;;     (emacsql-close db)
;;     expr))

(defun dump-firefox-places-to-json ()
  (shell-command-to-string (concat "sqlite3 '"
                                   (f-expand firefox-places-db)
                                   ".unlocked' '"
                                   firefox-places-query-string
                                   "' | dump-firefox-places-to-json")))

(when (f-exists? firefox-profile-path)
  (defconst firefox-places-query-string
    (let* ((db (emacsql-sqlite firefox-places-db))
           (expr (emacsql-compile db firefox-places-query)))
      (emacsql-close db)
      expr))
  (defvar firefox-recent-urls (json-read-from-string (dump-firefox-places-to-json))))


;; (defvar firefox-recent-urls (json-read-from-string (dump-firefox-places-to-json)))

(defun firefox-recent-url-sort-fn (left right)
  (let ((lvisits (alist-get 'visits left))
        (rvisits (alist-get 'visits right))
        (lrecency (alist-get 'recency left))
        (rrecency (alist-get 'recency right)))
    (if (= lrecency rrecency)
        (> lvisits rvisits)
      (> lrecency rrecency))))

(add-to-list 'marginalia-prompt-categories '("\\<[fF]irefox [uU][rR][lL]\\>" . firefox-recent-url))

(defun marginalia-annotate-firefox-recent-url (cand)
  (let ((data (seq-find #'(lambda (x) (string= (alist-get 'url x) cand)) firefox-recent-urls)))
    (marginalia--fields
     ((alist-get 'title data) :truncate 60)
     ((format "%d" (alist-get 'visits data)) :truncate 8)
     ((format "%d" (alist-get 'recency data)) :truncate 8)
     )))

(add-to-list 'marginalia-annotator-registry '(firefox-recent-url marginalia-annotate-firefox-recent-url builtin none))

(defun firefox-browse-recent ()
  (interactive)
  (let ((vertico-sort-function #'identity))
    (browse-url-firefox (completing-read "Recent Firefox URL: " (seq-map #'(lambda (x) (alist-get 'url x)) (seq-sort #'firefox-recent-url-sort-fn  firefox-recent-urls))) t)))


(provide 'module-firefox)
;;; module-firefox.el ends here
