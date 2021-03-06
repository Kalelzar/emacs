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
