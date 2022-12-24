;;; module-eglot.el --- A module for setting up eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Borislav Atanasov

;; Author: Borislav Atanasov <natomanofglory@gmail.com>
;; Keywords: languages, convenience

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

(use-package project)

(use-package eglot
  :after package
  :ensure nil
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

;; (shell-command "mkdir -p ~/.config/emacs/oot && cd ~/.config/emacs/oot && git clone \"git@github.com:juanjosegarciaripoll/project-cmake.git\"")

(use-package project-cmake
  :after eglot
  :load-path "~/.config/emacs/oot/project-cmake/"
  :config
  (project-cmake-scan-kits)
  (project-cmake-eglot-integration))

(use-package yasnippet
  :config (yas-global-mode))

(use-package consult-eglot
  :after (consult eglot))

(use-package flymake-popon
  :config (global-flymake-popon-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; (shell-command "mkdir -p ~/.config/emacs/oot && cd ~/.config/emacs/oot && git clone \"git@github.com:orzechowskid/tsi.el.git\"")

(use-package tsi
  :after tree-sitter
  :load-path "~/.config/emacs/oot/tsi.el/"
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (require 'tsi-typescript)
  (require 'tsi-css)
  (require 'tsi-json)
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(provide 'module-eglot)
;;; module-eglot.el ends here
