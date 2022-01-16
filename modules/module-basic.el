;;; module-basic.el --- PL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

(setq comp-deferred-compilation t)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(setq use-package-enable-imenu-support t)

(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(setq use-package-always-ensure t)

;; TODO: Setup straight.el

;; Set up common variables
(setq user-full-name "Borislav Atanasov"
      user-mail-address "natomanofglory@gmail.com"
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      kill-whole-line t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-list/") t))
      sentence-end-double-space nil
      uniquify-buffer-name-style 'forward
      shell-command-switch "-ic"
      visible-bell t
      ring-bell-function 'ignore
      custom-safe-themes t
      read-process-output-max (* 1024 1024 4)
      display-time-format "[%H:%M %b %d]"
      display-time-string-forms '((if
                                      (and
                                       (not display-time-format)
                                       display-time-day-and-date)
                                      (format-time-string "%a %b %e " now)
                                    #1="")
                                  (propertize
                                   (format-time-string
                                    (or display-time-format
                                        (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                                    now)
                                   'help-echo
                                   (format-time-string "%a %b %e, %Y" now))))


(setq-default indent-tabs-mode nil
	      indicate-empty-lines t)

;; (add-to-list 'default-frame-alist
;; 	     (cond
;; 	      ((string-equal system-type "gnu/linux") '(font . "Fira Code-12"))))


(line-number-mode t)
(column-number-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

;(load custom-file)

(delete-selection-mode t)

(blink-cursor-mode -1)

(show-paren-mode t)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package smooth-scrolling)
(smooth-scrolling-mode)

(use-package bar-cursor
  :init
  (bar-cursor-mode 1)
  :diminish bar-cursor-mode)

(use-package doom-themes
  :init
  (load-theme 'doom-dark+ t)
  (enable-theme 'doom-dark+))

(use-package doom-modeline
  :init
  (doom-modeline-mode))

(with-eval-after-load 'consult
  (defun pick-module ()
    (when-let (modules (f-entries (f-join user-emacs-directory "modules")
                                  #'(lambda (entry) (message entry)
                                      (f-ext? entry "el"))))
      (consult--read modules
                     :prompt "Module: "
                     :category 'file)))
  (defun find-module (module)
    (interactive (list (pick-module)))
    (find-file-existing module)))
(provide 'module-basic)
;;; module-basic.el ends here
