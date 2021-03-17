;;; init.el -- Emacs config
;;; Commentary:
;;; init.el shamelessly borrowed from "https://github.com/danielmai/"
;;; Code:

(setq comp-deferred-compilation t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-buffer-choice
      #'(lambda ()
          (let ((buffer (get-buffer "*Magit Repositories*")))
            (if (buffer-live-p buffer)
                buffer
              (magit-list-repositories)))))

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(use-package diminish :ensure t)

(setq use-package-ensure-function 'quelpa)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(require 'bind-key)

;;; Load the config
(org-babel-load-file (concat user-emacs-directory "config.org"))

(message "Init done")
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(use-package-defaults
   '((:config
      (lambda
        (name args)
        (imsg "Config" name))
      t)
     (:init
      (lambda
        (name args)
        (imsg "Init" name))
      t)
     (:catch
      (lambda
        (name args)
        (ierr "Error" name))
      (lambda
        (name args)
        (not use-package-expand-minimally)))
     (:defer use-package-always-defer
             (lambda
               (name args)
               (and use-package-always-defer
                    (not
                     (plist-member args :defer))
                    (not
                     (plist-member args :demand)))))
     (:demand use-package-always-demand
              (lambda
                (name args)
                (and use-package-always-demand
                     (not
                      (plist-member args :defer))
                     (not
                      (plist-member args :demand)))))
     (:ensure
      (list use-package-always-ensure)
      (lambda
        (name args)
        (and use-package-always-ensure
             (not
              (plist-member args :load-path)))))
     (:pin use-package-always-pin use-package-always-pin))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
