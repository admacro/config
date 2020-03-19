(require 'cl-lib)
(deftheme adm-light)

(let ((colors '(;; Upstream theme color
                (adm-bg      "#ece4d0" nil)
                (adm-current "#E0D5B9") ; current-line/selection
                (adm-fg      "#2c2c54") ; 10.38
                (adm-comment "#B05423") ; 4
                (adm-red     "#b33939") ; 4.65
                (adm-purple  "#8A2B74") ; 6.18
                (adm-blue    "#474787") ; 6.57
                (adm-green   "#285928") ; 6.6

                ;; deplicates
                (adm-cyan    "#8A2B74")
                (adm-orange  "#B05423")
                (adm-yellow  "#285928")

                ;; bg
                (bg2 "#E8E0CB")
                (bg3 "#D3CBA7")
                (bg4 "#C0B082")

                ;; fg
                (fg2 "#35355F")
                (fg3 "#404072")
                (fg4 "#4D4D89")

                ;; other
                (other-blue "#2A69A7")  ; 4.5

                ;; good theme colour candidates
                (adm-darkblue  "#221199") ; 10.3
                (adm-orchid  "#770088") ; 7.74
                (adm-fire  "#aa1111") ; 5.92
                (adm-aqua  "#116644") ; 5.51
                (adm-slateblue  "#4c5bd3") ; 4.42
                (adm-chocolate  "#aa5500") ; 4.13
                (adm-rose  "#ce3262") ; 3.89
                ))

      (faces '(;; default
               (cursor :background ,fg3)
               (default :background ,adm-bg :foreground ,adm-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,adm-bg :foreground ,fg4)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,adm-current :extend t)
               (info-quoted-name :foreground ,adm-orange)
               (info-string :foreground ,adm-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg3)
               (link :foreground ,adm-green :underline t)
               (linum :foreground ,bg4 :background ,adm-bg)
               (line-number :foreground ,bg4 :background ,adm-bg)
               (minibuffer-prompt :weight bold :foreground ,adm-blue)
               (region :background ,bg4 :extend t)
               (trailing-whitespace :foreground nil :background ,adm-red)
               (vertical-border :foreground ,bg4)
               (success :foreground ,adm-cyan)
               (warning :foreground ,adm-orange)
               (error :foreground ,adm-red)
               (header-line :background ,adm-bg)

               ;; syntax
               (font-lock-function-name-face :foreground ,adm-purple)
               (font-lock-variable-name-face :foreground ,adm-fg)
               (font-lock-type-face :foreground ,adm-blue)
               (font-lock-keyword-face :foreground ,adm-blue :weight bold)
               (font-lock-constant-face :foreground ,adm-fg :slant italic)
               (font-lock-comment-face :foreground ,adm-comment)
               (font-lock-builtin-face :foreground ,adm-blue :weight bold)
               (font-lock-string-face :foreground ,adm-green)

               (font-lock-comment-delimiter-face :foreground ,adm-comment)
               (font-lock-doc-face :foreground ,adm-comment)
               (font-lock-negation-char-face :foreground ,adm-green)
               (font-lock-preprocessor-face :foreground ,adm-orange)
               (font-lock-reference-face :foreground ,adm-green)
               (font-lock-regexp-grouping-backslash :foreground ,adm-green)
               (font-lock-regexp-grouping-construct :foreground ,adm-purple)
               (font-lock-warning-face :foreground ,adm-red :background ,bg3)

               ;; highlight-numbers
               (highlight-numbers-number :foreground ,adm-green)

               ;; mode-line
               (mode-line :foreground nil :background ,adm-current :box ,adm-current)
               (mode-line-inactive :foreground ,bg4 :background ,bg2 :box ,bg2)

               ;; message
               ;; (message-mml :foreground ,adm-cyan :weight normal)
               ;; (message-header-xheader :foreground ,adm-green :weight normal)

               ;; org
               (org-agenda-date :foreground ,adm-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,adm-comment)
               (org-agenda-done :foreground ,adm-green)
               (org-agenda-structure :foreground ,adm-purple)
               (org-block :foreground ,fg4)
               (org-code :foreground ,fg4)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,adm-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,adm-comment)
               (org-document-title :weight bold :foreground ,adm-orange :height 1.44)
               (org-done :foreground ,adm-green)
               (org-ellipsis :foreground ,adm-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,adm-orange)
               (org-headline-done :foreground ,adm-comment :weight normal :strike-through t)
               (org-hide :foreground ,adm-bg :background ,adm-bg)
               (org-level-1 :inherit bold :foreground ,adm-blue :height 1.3)
               (org-level-2 :inherit bold :foreground ,adm-purple :height 1.1)
               (org-level-3 :weight normal :foreground ,adm-green :height 1.0)
               (org-level-4 :weight normal :foreground ,adm-orange)
               (org-level-5 :weight normal :foreground ,adm-red)
               (org-level-6 :weight normal :foreground ,other-blue)
               (org-level-7 :weight normal :foreground ,fg3)
               (org-level-8 :weight normal :foreground ,fg4)
               (org-link :foreground ,other-blue :underline t)
               (org-priority :foreground ,adm-cyan)
               (org-scheduled :foreground ,adm-green)
               (org-scheduled-previously :foreground ,adm-yellow)
               (org-scheduled-today :foreground ,adm-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,adm-yellow)
               (org-table :foreground ,adm-green)
               (org-tag :foreground ,adm-purple :weight bold :background ,bg2)
               (org-todo :foreground ,adm-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,adm-yellow)
               (org-warning :weight bold :foreground ,adm-red)

               ;; term
               ;; (term :foreground ,adm-fg :background ,adm-bg)
               ;; (term-color-black :foreground ,adm-bg :background ,adm-bg)
               ;; (term-color-blue :foreground ,adm-purple :background ,adm-purple)
               ;; (term-color-cyan :foreground ,adm-green :background ,adm-green)
               ;; (term-color-green :foreground ,adm-cyan :background ,adm-cyan)
               ;; (term-color-magenta :foreground ,adm-blue :background ,adm-blue)
               ;; (term-color-red :foreground ,adm-red :background ,adm-red)
               ;; (term-color-white :foreground ,adm-fg :background ,adm-fg)
               ;; (term-color-yellow :foreground ,adm-yellow :background ,adm-yellow)

               ;; company
               ;; (company-echo-common :foreground ,adm-bg :background ,adm-fg)
               ;; (company-preview :background ,adm-bg :foreground ,other-blue)
               ;; (company-preview-common :foreground ,bg2 :foreground ,fg3)
               ;; (company-preview-search :foreground ,adm-purple :background ,adm-bg)
               ;; (company-scrollbar-bg :background ,bg3)
               ;; (company-scrollbar-fg :foreground ,adm-blue)
               ;; (company-template-field :inherit region)
               ;; (company-tooltip :foreground ,fg2 :background ,bg2 :weight bold)
               ;; (company-tooltip-annotation :foreground ,adm-green)
               ;; (company-tooltip-common :foreground ,fg3)
               ;; (company-tooltip-common-selection :foreground ,adm-yellow)
               ;; (company-tooltip-mouse :inherit highlight)
               ;; (company-tooltip-selection :background ,bg3 :foreground ,fg3)

               ;; dired
               ;; (dired-directory :foreground ,adm-cyan :weight normal)
               ;; (dired-flagged :foreground ,adm-blue)
               ;; (dired-header :foreground ,fg3 :background ,adm-bg)
               ;; (dired-ignored :inherit shadow)
               ;; (dired-mark :foreground ,adm-fg :weight bold)
               ;; (dired-marked :foreground ,adm-orange :weight bold)
               ;; (dired-perm-write :foreground ,fg3 :underline t)
               ;; (dired-symlink :foreground ,adm-yellow :weight normal :slant italic)
               ;; (dired-warning :foreground ,adm-orange :underline t)
               ;; (diredp-compressed-file-name :foreground ,fg3)
               ;; (diredp-compressed-file-suffix :foreground ,fg4)
               ;; (diredp-date-time :foreground ,adm-fg)
               ;; (diredp-deletion-file-name :foreground ,adm-blue :background ,adm-current)
               ;; (diredp-deletion :foreground ,adm-blue :weight bold)
               ;; (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               ;; (diredp-dir-name :inherit dired-directory)
               ;; (diredp-dir-priv :inherit dired-directory)
               ;; (diredp-executable-tag :foreground ,adm-orange)
               ;; (diredp-file-name :foreground ,adm-fg)
               ;; (diredp-file-suffix :foreground ,fg4)
               ;; (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,adm-current)
               ;; (diredp-flag-mark :foreground ,fg2 :weight bold :background ,adm-current)
               ;; (diredp-ignored-file-name :foreground ,adm-fg)
               ;; (diredp-mode-line-flagged :foreground ,adm-orange)
               ;; (diredp-mode-line-marked :foreground ,adm-orange)
               ;; (diredp-no-priv :foreground ,adm-fg)
               ;; (diredp-number :foreground ,adm-green)
               ;; (diredp-other-priv :foreground ,adm-orange)
               ;; (diredp-rare-priv :foreground ,adm-orange)
               ;; (diredp-read-priv :foreground ,adm-purple)
               ;; (diredp-write-priv :foreground ,adm-blue)
               ;; (diredp-exec-priv :foreground ,adm-yellow)
               ;; (diredp-symlink :foreground ,adm-orange)
               ;; (diredp-link-priv :foreground ,adm-orange)
               ;; (diredp-autofile-name :foreground ,adm-yellow)
               ;; (diredp-tagged-autofile-name :foreground ,adm-yellow)

               ;; magit
               (magit-branch-local :foreground ,adm-green)
               (magit-branch-remote :foreground ,adm-cyan)
               (magit-tag :foreground ,adm-orange)
               (magit-section-heading :foreground ,adm-blue :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,adm-orange
                                            :background ,adm-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,adm-orange
                                                      :background ,bg3
                                                      :weight bold
                                                      :extend t)

               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,adm-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,adm-cyan)
               (magit-diffstat-removed :foreground ,adm-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,adm-orange :weight bold)
               (magit-process-ok :foreground ,adm-cyan :weight bold)

               ;; web-mode
               ;; (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               ;; (web-mode-comment-face :inherit ,font-lock-comment-face)
               ;; (web-mode-constant-face :inherit ,font-lock-constant-face)
               ;; (web-mode-doctype-face :inherit ,font-lock-comment-face)
               ;; (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               ;; (web-mode-html-attr-name-face :foreground ,adm-purple)
               ;; (web-mode-html-attr-value-face :foreground ,adm-cyan)
               ;; (web-mode-html-tag-face :foreground ,adm-blue :weight bold)
               ;; (web-mode-keyword-face :foreground ,adm-blue)
               ;; (web-mode-string-face :foreground ,adm-yellow)
               ;; (web-mode-type-face :inherit ,font-lock-type-face)
               ;; (web-mode-warning-face :inherit ,font-lock-warning-face)

               ;; whitespace
               ;; (whitespace-big-indent :background ,adm-red :foreground ,adm-red)
               ;; (whitespace-empty :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-hspace :background ,bg3 :foreground ,adm-comment)
               ;; (whitespace-indentation :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-line :background ,adm-bg :foreground ,adm-blue)
               ;; (whitespace-newline :foreground ,adm-comment)
               ;; (whitespace-space :background ,adm-bg :foreground ,adm-comment)
               ;; (whitespace-space-after-tab :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-space-before-tab :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-tab :background ,bg2 :foreground ,adm-comment)
               ;; (whitespace-trailing :inherit trailing-whitespace)
               )))

  (apply #'custom-theme-set-faces
         'adm-light
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (tty-colors (mapcar #'car (mapcar #'last colors))))
           (cl-flet* ((expand-for-tty (spec) (cl-progv color-names tty-colors
                                               (eval `(backquote ,spec))))
                      (expand-for-graphic (spec) (cl-progv color-names graphic-colors
                                                   (eval `(backquote ,spec)))))
             (cl-loop for (face . spec) in faces
                      collect `(,face
                                ((((min-colors 16777216))
                                  ,(expand-for-graphic spec))
                                 (t
                                  ,(expand-for-tty spec)))))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'adm-light)
