;;; module-vterm.el --- A libvterm configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: terminals

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
;; A simple libvterm configuration module.
;;; Code:

(require 'use-package)

;; This also requires some shell-side configuration
;; see 'https://github.com/akermu/emacs-libvterm#shell-side-configuration-files'
;; and 'https://github.com/Kalelzar/dotfiles/blob/master/.config/zsh/.zshrc'
(use-package vterm
  :config
  (setq vterm-buffer-name-string "%s - vterm"
        vterm-use-vterm-prompt-detection-method t
        vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no"
        vterm-copy-exclude-prompt t))



(provide 'module-vterm) 
;;; module-vterm.el ends here
