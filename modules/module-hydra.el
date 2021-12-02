;;; module-hydra.el --- Hydra config                 -*- lexical-binding: t; -*-

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

(require 'hydra)

(defhydra hydra-selection
  (:body-pre (set-mark (point))
             :color amaranth)
  ("w" previous-line "Point Up")
  ("s" next-line "Point Down" )
  ("a" backward-char "Point Left" )
  ("d" forward-char "Point Right")
  ("q" backward-sexp "Point Start")
  ("e" forward-sexp "Point End")
  ("i" (progn
         (exchange-point-and-mark)
         (previous-line)
         (exchange-point-and-mark)) "Mark Up")
  ("k" (progn
         (exchange-point-and-mark)
         (next-line)
         (exchange-point-and-mark)) "Mark Down" )
  ("j" (progn
         (exchange-point-and-mark)
         (backward-char)
         (exchange-point-and-mark)) "Mark Left" )
  ("l" (progn
         (exchange-point-and-mark)
         (forward-char)
         (exchange-point-and-mark)) "Mark Right")
  ("u" (progn
         (exchange-point-and-mark)
         (backward-sexp)
         (exchange-point-and-mark)) "Mark Start")
  ("o" (progn
         (exchange-point-and-mark)
         (forward-sexp)
         (exchange-point-and-mark)) "Mark End")
  ("<ESC>" (message "Done") "Quit" :exit t)
  )

(provide 'module-hydra)
;;; module-hydra.el ends here
