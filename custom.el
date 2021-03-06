(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calibredb-library-alist '(("~/Documents/Library/")))
 '(custom-safe-themes
   '("2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(default-input-method "bulgarian-phonetic")
 '(display-buffer-alist
   '(("*Async Shell Command*" display-buffer-no-window
      (:allow-no-window . t))))
 '(doc-view-continuous t)
 '(doom-modeline-mode t)
 '(ede-project-directories '("/home/kalelzar/Code/cpp/pulsebind"))
 '(flycheck-stylelintrc "stylelint.json")
 '(global-undo-tree-mode t)
 '(global-whitespace-mode t)
 '(ivy-posframe-mode t nil (ivy-posframe))
 '(js-indent-level 2)
 '(org-agenda-files
   '("~/Documents/tickler.org" "~/Documents/notes.org" "~/Documents/agenda.org"))
 '(org-capture-templates
   '(("l" "Lecture" entry #'org-roam--capture-get-point "* ${title}
 %?" :unnarrowed t :org-roam
 (:head "#+title: ${title}
#+roam_alias: \"${title}\"
#+roam_tags: \"%^{prompt|General|Numeric Methods|Linear Algebra|Logic Programming|Computer Architectures|Design and Analysis of Algorithms|Discrete Structures|Geometry of Movement}\" \"Lecture\"
" :file-name "${title}"))
     ("d" "Definition" entry #'org-roam--capture-get-point "* ${title}
 %?" :unnarrowed t :org-roam
 (:head "#+title: ${title}
#+roam_alias: \"${title}\"
#+roam_tags: \"%^{prompt|General|Numeric Methods|Linear Algebra|Logic Programming|Computer Architectures|Design and Analysis of Algorithms|Discrete Structures|Geometry of Movement}\" \"Definition\"
" :file-name "${title}"))
     ("t" "Theorem" entry #'org-roam--capture-get-point "* ${title}
 %?" :unnarrowed t :org-roam
 (:head "#+title: ${title}
#+roam_alias: \"${title}\"
#+roam_tags: \"%^{prompt|General|Numeric Methods|Linear Algebra|Logic Programming|Computer Architectures|Design and Analysis of Algorithms|Discrete Structures|Geometry of Movement}\" \"Theorem\"
" :file-name "${title}"))))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-link-beautify-mode nil)
 '(org-ql-views
   '(("Overview: Agenda-like" :buffers-files org-agenda-files :query
      (and
       (not
        (done))
       (or
        (habit)
        (deadline auto)
        (scheduled :to today)
        (ts-active :on today)))
      :sort
      (date)
      :super-groups org-super-agenda-groups :title "Agenda-like")
     ("Overview: NEXT tasks" :buffers-files org-agenda-files :query
      (todo "NEXT")
      :sort
      (priority date)
      :super-groups org-super-agenda-groups :title "Overview: NEXT tasks")
     ("Calendar: Today" :buffers-files org-agenda-files :query
      (ts-active :on today)
      :title "Today" :super-groups org-super-agenda-groups :sort
      (priority))
     ("Calendar: This week" .
      #[0 "\301 \302\303\304\305\304\306\304\307\310\301 \311!>\204 \312\313\314D\"\210\211\315H\204\232 \211\315\316\317\320\311!>\2048 \312\313\314D\"\210\321H\204\223 \321\322H\323H	\324H
\325H\326H\327H\211
\211\203\213 \203\213 \203\213 \203\213 \203\213 \203\213 \330\331
&!\202\215 \330 \266\206\266\206I\210\321H\"!I\210\211\315H\262[
#&\302\303\332\305\333\306\333\307\310\327\301 \311!>\204\300 \312\313\314D\"\210\211\315H\204>\211\315\316\317\320\311!>\204\334 \312\313\314D\"\210\321H\2047\321\322H\323H	\324H
\325H\326H\327H\211
\211\203/\203/\203/\203/\203/\203/\330\331
&!\2021\330 \266\206\266\206I\210\321H\"!I\210\211\315H\262Z#&\334\335 \336\337\340\257\341\342\343\344\345\346&\207"
          [cl-struct-ts-tags ts-now ts-apply :hour 0 :minute :second ts-adjust day type-of signal wrong-type-argument ts 7 string-to-number format-time-string "%w" 17 3 2 1 4 5 6 float-time encode-time 23 59 org-ql-search org-agenda-files ts-active :from :to :title "This week" :super-groups org-super-agenda-groups :sort
                             (priority)]
          40 "Show items with an active timestamp during this calendar week." nil])
     ("Calendar: Next week" .
      #[0 "\301\302\303\304 #\305\306\307\310\307\311\307\301\302\304 \312!>\204  \313\314\315D\"\210\211\303H\204\236 \211\303\316\317\320\312!>\204< \313\314\315D\"\210\321H\204\227 \321\322H\323H	\324H
\325H\326H\327H\211
\211\203\217 \203\217 \203\217 \203\217 \203\217 \203\217 \330\331
&!\202\221 \330 \266\206\266\206I\210\321H\"!I\210\211\303H\262[
#&\305\306\332\310\333\311\333\301\302\327\304 \312!>\204\304 \313\314\315D\"\210\211\303H\204B\211\303\316\317\320\312!>\204\340 \313\314\315D\"\210\321H\204;\321\322H\323H	\324H
\325H\326H\327H\211
\211\2033\2033\2033\2033\2033\2033\330\331
&!\2025\330 \266\206\266\206I\210\321H\"!I\210\211\303H\262Z#&\334\335 \336\337\340\257\341\342\343\344\345\346&\207"
          [cl-struct-ts-tags ts-adjust day 7 ts-now ts-apply :hour 0 :minute :second type-of signal wrong-type-argument ts string-to-number format-time-string "%w" 17 3 2 1 4 5 6 float-time encode-time 23 59 org-ql-search org-agenda-files ts-active :from :to :title "Next week" :super-groups org-super-agenda-groups :sort
                             (priority)]
          40 "Show items with an active timestamp during the next calendar week." nil])
     ("Review: Recently timestamped" . org-ql-view-recent-items)
     (#("Review: Dangling tasks" 0 22
        (help-echo "Tasks whose ancestor is done"))
      :buffers-files org-agenda-files :query
      (and
       (todo)
       (ancestors
        (done)))
      :title
      #("Review: Dangling tasks" 0 22
        (help-echo "Tasks whose ancestor is done"))
      :sort
      (date priority todo)
      :super-groups
      ((:auto-parent t)))
     (#("Review: Stale tasks" 0 19
        (help-echo "Tasks without a timestamp in the past 2 weeks"))
      :buffers-files org-agenda-files :query
      (and
       (todo)
       (not
        (ts :from -14)))
      :title
      #("Review: Stale tasks" 0 19
        (help-echo "Tasks without a timestamp in the past 2 weeks"))
      :sort
      (date)
      :super-groups
      ((:auto-parent t)))
     (#("Review: Stuck projects" 0 22
        (help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
      :buffers-files org-agenda-files :query
      (and
       (todo)
       (descendants
        (todo))
       (not
        (descendants
         (todo "NEXT"))))
      :title
      #("Review: Stuck projects" 0 22
        (help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
      :sort
      (priority)
      :super-groups org-super-agenda-groups)))
 '(package-selected-packages
   '(ivy-posframe magit-section all-the-icons-ivy-rich x-path-walker hydra pretty-hydra ivy-hydra ninja-mode selectrum-prescient ivy-prescient company-prescient prescient orderless selectrum embark-consult embark marginalia consult-flycheck consult treemacs counsel-dash postcss-sorting scss-mode emms vue-mode edit-indirect ssass-mode vue-html-mode mmm-mode tss yaxception auto-complete typescript-mode math-symbol-lists ivy-yasnippet org-elp company-math quelpa flymake-sass sass-mode ripgrep simple-httpd delve lister graphviz-dot-mode emacsql-sqlite3 centered-window rust-mode yaml-imenu flycheck-yamllint yaml-mode calibredb cmake-mode cmake-project svg-tag-mode org-sidebar org-treeusage sbt-mode lsp-metals scala-mode scalariform cakecrumbs auto-rename-tag company-web web-beautify kaltask web-server json-mode json-snatcher json-reformat macrostep ghub treepy closql emacsql-sqlite emacsql git-commit with-editor transient swiper projectile helm-core popup async alert log4e gntp pkg-info epl shrink-path company-lsp company lsp-ui lsp-mode lv markdown-mode ht f s dash-functional dash framemove bar-cursor god-mode justify-kp slime magit counsel ivy helm novel-mode org-link-beautify all-the-icons memoize yasnippet quelpa-use-package treefactor rainbow-identifiers rainbow-blocks org-drill flymake-jslint org-wild-notifier org-alert all-the-icons-ivy yasnippet-snippets which-key wanderlust volatile-highlights use-package unicode-fonts undo-tree super-save stumpwm-mode srefactor smooth-scrolling smartparens slime-company scratch rainbow-mode rainbow-delimiters python-mode page-break-lines ox-epub org-superstar org-sticky-header org-plus-contrib nov hl-todo helm-swoop helm-projectile helm-ag forge flycheck fira-code-mode expand-region ewal-doom-themes elisp-slime-nav edit-server doom-modeline diminish crux counsel-projectile common-lisp-snippets cdlatex ccls c-eldoc avy auctex all-the-icons-ibuffer all-the-icons-gnus all-the-icons-dired activity-watch-mode))
 '(smooth-scrolling-mode t)
 '(svg-tag-default-line-width 2)
 '(svg-tag-horizontal-offset 9)
 '(svg-tag-tags '(("@[a-zA-Z0-9]+?@" . svg-tag-round)))
 '(svg-tag-vertical-offset -17)
 '(use-package-defaults
   '((:config
      '(t)
      t)
     (:init nil t)
     (:catch t
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
 '(cakecrumbs-attr ((t (:inherit font-lock-variable-name-face :extend t :inverse-video t :box (:line-width 4 :style released-button)))))
 '(cakecrumbs-class ((t (:inherit font-lock-type-face :inverse-video t :box (:line-width 4 :style released-button)))))
 '(cakecrumbs-ellipsis ((t (:inherit (default font-lock-comment-face) :inverse-video t :box (:line-width 4 :style released-button)))))
 '(cakecrumbs-id ((t (:inherit font-lock-keyword-face :inverse-video t :box (:line-width 4 :style released-button)))))
 '(cakecrumbs-preprocessor ((t (:inherit font-lock-preprocessor-face :inverse-video t :box (:line-width 4 :style released-button)))))
 '(cakecrumbs-pseudo ((t (:inherit font-lock-constant-face :inverse-video t :box (:line-width 4 :style released-button)))))
 '(cakecrumbs-separator ((t (:inherit (default font-lock-comment-face)))))
 '(cakecrumbs-tag ((t (:inherit font-lock-function-name-face :inverse-video t :box (:line-width 4 :style released-button)))))
 '(font-lock-keyword-face ((t (:foreground "#b75867"))))
 '(ivy-posframe-border ((t (:background "salmon3"))))
 '(svg-tag-default-face ((t (:background "purple" :foreground "white" :weight normal :height 140 :width expanded :family "Roboto Mono")))))
