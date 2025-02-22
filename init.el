;;; init.el --- PL -*- lexical-binding: t; -*-
;;; Commentary:
;;; Setup MELPA

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))

(require 'module-basic)
(require 'module-vterm)
(require 'module-consult)
(require 'framemove)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-allow-imprecise-window-fit nil))

(use-package windmove
  :init
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))


(use-package org :pin gnu
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (C . t)
     (shell . t)
     (lisp . t))))
(use-package org-contrib)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  :config
  ;; try the `completion-category-sort-function' first
  (advice-add #'vertico--sort-function
              :before-until #'completion-category-sort-function)

  (defun completion-category-sort-function ()
    (alist-get (vertico--metadata-get 'category)
               completion-category-sort-function-overrides))

  (defvar completion-category-sort-function-overrides
    '((file . directories-before-files))
    "Completion category-specific sorting function overrides.")

  (defun directories-before-files (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (let ((dir-then-files (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
				 (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))
      (nconc (seq-remove (lambda (x) (string-prefix-p "." x)) dir-then-files)
	     (seq-filter (lambda (x) (string-prefix-p "." x)) dir-then-files))
      ))

  
  (defun +completion-category-highlight-files (cand)
    (let ((len (length cand)))
      (when (and (> len 0)
		 (eq (aref cand (1- len)) ?/))
	(add-face-text-property 0 len 'dired-directory 'append cand)))
    cand)
  
  (defvar +completion-category-hl-func-overrides
    `((file . ,#'+completion-category-highlight-files))
    "Alist mapping category to highlight functions.")
  
  (advice-add #'vertico--arrange-candidates :around
              (defun vertico-format-candidates+ (func)
		(let ((hl-func (or (alist-get (vertico--metadata-get 'category)
                                              +completion-category-hl-func-overrides)
                                   #'identity)))
                  (cl-letf* (((symbol-function 'actual-vertico-format-candidate)
                              (symbol-function #'vertico--format-candidate))
                             ((symbol-function #'vertico--format-candidate)
                              (lambda (cand &rest args)
				(apply #'actual-vertico-format-candidate
                                       (funcall hl-func cand) args))))
                    (funcall func)))))

  
  (defadvice vertico-insert
      (after vertico-insert-add-history activate)
    "Make vertico-insert add to the minibuffer history."
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  
  )

(use-package orderless
  :demand t
  :config
   (defun +orderless-dispatch (pattern index _total)
     (cond
      ; Ensure that $ works with Consult commands, which add disambiguation suffixes
      ((string-suffix-p "$" pattern)
       `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
      ; File extensions
      ((and
        ; Completing filename or eshell
        (or minibuffer-completing-file-name
            (derived-mode-p 'eshell-mode))
        ; File extension
        (string-match-p "\\`\\.." pattern))
       `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
      ; Ignore single !
      ((string= "!" pattern) `(orderless-literal . ""))
      ))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)
	))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  :config
  (add-to-list 'marginalia-prompt-categories '("\\<buffer[s]?\\>" . buffer)))

(use-package corfu :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;;(corfu-echo-documentation t) ;; Do not show documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; (corfu-preview-current nil)    ;; Do not preview current candidate
  
  ;;Optionally use TAB for cycling, default is `corfu-complete'.
   :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous))

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun embark-vertico-indicator ()
  (let ((fr face-remapping-alist))
    (lambda (&optional keymap _targets prefix)
      (when (bound-and-true-p vertico--input)
        (setq-local face-remapping-alist
                    (if keymap
                        (cons '(vertico-current . embark-target) fr)
                      fr))))))
  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-vertico-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (setq embark-prompter #'embark-keymap-prompter)
  
  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(cl-defmacro key-closure (key-sequence docstring &body form)
  "Return a closure that will repeat the provided KEY-SEQUENCE when called."
  `(cl-flet ((key-closure-func ()
               ,docstring
               (interactive)
               (message "%s" (nth (- vertico--index 1) vertico--candidates))
               (let ((extended-key-sequence (format "%s " ,key-sequence vertico--index)))
                 (setq unread-command-events (listify-key-sequence (kbd extended-key-sequence)))
)))
     (let ((it (cl-function key-closure-func)))
     ,@form)))

(defmacro alias-key (key-sequence alias keymap docstring)
  "Treat KEY-SEQUENCE as if it were ALIAS in a KEYMAP."
  `(key-closure ,alias ,docstring (bind-key ,key-sequence it ,keymap)))

(setq bind-key-describe-special-forms t)

(alias-key "C-k"
           "C-u C-. k y"
           'vertico-map
           "Kill buffer at point")


(bind-key "C-x r v" #'register-to-point)
(bind-key "C-x x" #'replace-regexp) 

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . colorize-compilation-buffer))

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after marginalia
  :init (all-the-icons-completion-mode)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)


  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[Multiple] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;  (defalias 'completing-read-multiple 'magit-completing-read-multiple)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(with-eval-after-load 'exwm
  (defun marginalia-annotate-exwm-buffer (cand)
    (when-let (buffer (get-buffer cand))
      (with-current-buffer buffer
        (marginalia--fields
         ((propertize (format "%s" exwm-class-name) 'display '(:align-to left)) :truncate (/ marginalia-field-width 8))
         ((propertize " " 'display '(space :align-to center)))
         ((format "Workspace: %d " exwm--desktop))))))

  (defun marginalia-annotate-buffer (cand)
    "Annotate buffer CAND with modification status, file name and major mode."
    (when-let (buffer (get-buffer cand))
      (if (eq (buffer-local-value 'major-mode buffer) 'exwm-mode)
          (marginalia-annotate-exwm-buffer cand)      
        (marginalia--fields
         ((marginalia--buffer-status buffer))
         ((marginalia--buffer-file buffer)
          :truncate (/ marginalia-field-width 2)
          :face 'marginalia-file-name))))))

(require 'module-jq)
(require 'module-projectile)

(when (eql system-type 'gnu/linux)
  (require 'module-bluetooth)
  (require 'module-firefox)
  (require 'module-pulseaudio)
  (require 'module-iwctl)
  (require 'module-xbacklight)
  (require 'module-playerctl)
  (require 'module-exwm-quicklaunch)
  (require 'module-exwm)
  (require 'module-batch)
  (require 'module-eglot)

  (use-package pinentry)
  (pinentry-start))

(setq c-doc-comment-style
      '((java-mode . javadoc)
        (pike-mode . autodoc)
        (c-mode    . gtkdoc)
        (c++-mode  . doxygen)))

(defun xdg-open (what)
  (var cmd (format " xdg-open '%s'" what)
       (start-process "xdg-open" cmd "xdg-open" what)))

(defmacro with-xdg-open (command)
  (var new-name (intern (format "%s-open-dwim" (symbol-name command)))
       `(defun ,new-name ()
          (interactive)
          (xdg-open (,command)))))

;(with-xdg-open cme-pick-book)

(unbind-key "C-z")
(unbind-key "C-x C-z")

(defun buffer-major-mode-name (buffer)
  (with-current-buffer buffer
    (var major-mode-string (if (stringp mode-name) mode-name (or (and (eql (car mode-name) :eval) (eval (plist-get mode-name :eval))) (car mode-name)))
         (propertize major-mode-string 'face 'outline-1))))

(defun display-buffer-history ()
  (interactive)
  (let* ((next (-map #'buffer-name (window-next-buffers)))
        (current (buffer-name (current-buffer)))
        (prev (--remove
               (or (string= it current) (var -compare-fn #'string= (-contains? next it))) 
               (--map (buffer-name (car it)) (window-prev-buffers)))))

    (var max-length (max
                     (--reduce-from (max acc (length it)) 0 next)
                     (+ (length current) 2)
                     (--reduce-from (max acc (length it)) 0 prev))
    
    (let ((next-string (s-join "\n" (reverse (--map (format "%s %s" (s-pad-right max-length " " it) (buffer-major-mode-name it)) next))))
          (current-string (format "> %s %s" (propertize (s-pad-right (- max-length 2) " " current)  'face 'success) (buffer-major-mode-name current)))
          (prev-string (s-join "\n" (--map (format "%s %s" (s-pad-right max-length " " it) (buffer-major-mode-name it)) prev))))
      (exwm-show-msg (format "%s\n%s\n%s" next-string current-string prev-string)
                     :timeout 1)))))



(defun previous-buffer-with-history ()
  (interactive)
  (previous-buffer)
  (display-buffer-history))

(defun next-buffer-with-history ()
  (interactive)
  (next-buffer)
  (display-buffer-history))
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers.  You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(display-battery-mode)
;(iwctl-display-mode)

(put 'upcase-region 'disabled nil)
(provide 'init)
;;; init.el ends here
