;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Face-Customization.html
;; https://www.w3.org/WAI/WCAG21/Understanding/contrast-minimum.html
;; https://colorshark.io
;; https://accessible-colors.com/
;; https://userway.org/contrast/
;; https://webaim.org/resources/contrastchecker/
(require 'cl-lib)
(deftheme adm-aaa)

(let ((colors '(
                (adm-aaa-bg "#f2f2f2")
                (adm-aaa-fg "#2f2f2f") ; 18.00 (against adm-aaa-bg)

                ;; colors (contrast ratio against adm-aaa-bg: 8)
                (adm-aaa-red     "#980101") ; H0
                (adm-aaa-orange  "#61440A") ; H40
                (adm-aaa-green   "#065606") ; H120
                (adm-aaa-cyan    "#024E74") ; H200
                (adm-aaa-blue    "#3838AD") ; H240
                (adm-aaa-purple  "#702097") ; H280
                (adm-aaa-magenta "#8B0E61") ; H300 品红/洋红

                ;; bg (against adm-aaa-fg)
                (bg1 "#EBE5D0")                 ; 16.00
                (bg2 "#E1D7B7")                 ; 14.03
                (bg3 "#D5C79F")                 ; 12.01
                (bg4 "#CAB57D")                 ; 10.01

                ;; fg (against adm-aaa-bg)
                (fg1 "#0C0C4B")                 ; 16.01
                (fg2 "#13136C")                 ; 14.01
                (fg3 "#151593")                 ; 12.00
                (fg4 "#2121AB")                 ; 10.01

                ;; other
                (other-red "#BB0202")  ; red2
                (other-blue "#2222DD")  ; H240 8.0
                (adm-aaa-blue-aa "#4A4AC9")  ; H240 8.0
                ))

      (faces '(;; default
               (cursor :background ,fg3)
               (default :background ,adm-aaa-bg :foreground ,adm-aaa-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,adm-aaa-bg :foreground ,fg4)
               (highlight :foreground nil :background ,bg3)
               (hl-line :background ,bg1 :extend t)
               (info-quoted-name :foreground ,adm-aaa-orange)
               (info-string :foreground ,adm-aaa-cyan)
               (lazy-highlight :foreground ,fg2 :background ,bg3)
               (link :foreground ,adm-aaa-green :underline t)
               (linum :foreground ,bg4 :background ,adm-aaa-bg)
               (line-number :foreground ,bg4 :background ,adm-aaa-bg)
               (minibuffer-prompt :weight bold :foreground ,adm-aaa-blue)
               (region :background ,bg4 :extend t)
               (trailing-whitespace :foreground nil :background ,adm-aaa-red)
               (vertical-border :foreground ,bg4)
               (success :foreground ,adm-aaa-purple)
               (warning :foreground ,adm-aaa-orange)
               (error :foreground ,adm-aaa-red)
               (header-line :background ,adm-aaa-bg)

               ;; syntax
               (font-lock-function-name-face :foreground ,adm-aaa-purple)
               (font-lock-variable-name-face :foreground ,adm-aaa-red)
               (font-lock-type-face :foreground ,adm-aaa-magenta)
               (font-lock-keyword-face :foreground ,adm-aaa-blue :weight bold)
               (font-lock-constant-face :foreground ,adm-aaa-red :slant italic)
               (font-lock-builtin-face :foreground ,adm-aaa-blue :weight normal :slant italic)
               (font-lock-string-face :foreground ,adm-aaa-green)

               (font-lock-comment-face :foreground ,adm-aaa-orange)
               (font-lock-comment-delimiter-face :foreground ,adm-aaa-orange)
               (font-lock-doc-face :foreground ,adm-aaa-orange)
               (font-lock-negation-char-face :foreground ,adm-aaa-red)
               (font-lock-preprocessor-face :foreground ,adm-aaa-blue)
               (font-lock-reference-face :foreground ,adm-aaa-green)
               (font-lock-regexp-grouping-backslash :foreground ,adm-aaa-red)
               (font-lock-regexp-grouping-construct :foreground ,adm-aaa-purple)
               (font-lock-warning-face :foreground ,other-red :background ,bg2)

               ;; highlight-numbers
               (highlight-numbers-number :foreground ,adm-aaa-green)

               ;; mode-line
               (mode-line :foreground ,adm-aaa-fg :background ,bg3 :box ,bg3)
               (mode-line-inactive :foreground ,bg4 :background ,bg2 :box ,bg2)

               ;; ido-mode
               (ido-only-match :foreground ,adm-aaa-green)
               (ido-subdir :foreground ,adm-aaa-purple)

               ;; show-paren
               (show-paren-match :background ,bg4)
               (show-paren-mismatch :weight bold :background ,other-red)

               ;; message
               ;; (message-mml :foreground ,adm-aaa-purple :weight normal)
               ;; (message-header-xheader :foreground ,adm-aaa-green :weight normal)

               ;; gotest
               (go-test--standard-face :foreground ,adm-aaa-orange)
               (go-test--ok-face :foreground ,adm-aaa-green)
               (go-test--error-face :foreground ,adm-aaa-red)
               (go-test--warning-face :foreground ,adm-aaa-cyan)

               ;; treemacs
               (treemacs-root-face :foreground ,adm-aaa-orange :weight bold)
               (treemacs-directory-face :foreground ,adm-aaa-purple)
               (treemacs-git-untracked-face :foreground ,adm-aaa-green :slant italic :underline t)
               (treemacs-git-added-face :foreground ,adm-aaa-green)
               (treemacs-git-modified-face :foreground ,adm-aaa-cyan)
               (treemacs-tags-face :foreground ,adm-aaa-blue)

               ;; company
               (company-echo-common :foreground ,adm-aaa-bg :background ,adm-aaa-fg)
               (company-preview :foreground ,other-blue :background ,adm-aaa-bg)
               (company-preview-common :foreground ,fg3 :background ,bg2)
               (company-preview-search :foreground ,adm-aaa-magenta :background ,adm-aaa-bg)
               (company-scrollbar-bg :background ,bg1)
               (company-scrollbar-fg :background ,bg3)
               (company-template-field :inherit region)
               (company-tooltip :foreground ,adm-aaa-fg :background ,bg1)
               (company-tooltip-annotation :foreground ,fg4 :slant italic)
               (company-tooltip-common :foreground ,adm-aaa-fg :background ,bg4)
               (company-tooltip-common-selection :foreground ,adm-aaa-red :background ,bg4)
               (company-tooltip-mouse :inherit highlight)
               (company-tooltip-selection :foreground ,adm-aaa-red :background ,bg2)

               ;; diff
               (diff-refine-added :background ,adm-aaa-green)
               (diff-refine-removed :background ,adm-aaa-red)

               ;; magit
               (magit-branch-local :foreground ,adm-aaa-green)
               (magit-branch-remote :foreground ,adm-aaa-purple)
               (magit-tag :foreground ,adm-aaa-orange)
               (magit-section-heading :foreground ,adm-aaa-blue :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,adm-aaa-orange
                                            :background ,adm-aaa-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,adm-aaa-orange
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
               (magit-diff-file-heading :foreground ,adm-aaa-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,adm-aaa-purple)
               (magit-diffstat-removed :foreground ,adm-aaa-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,adm-aaa-orange :weight bold)
               (magit-process-ok :foreground ,adm-aaa-purple :weight bold)

               ;; org
               (org-agenda-date :foreground ,adm-aaa-purple :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,adm-aaa-orange)
               (org-agenda-done :foreground ,adm-aaa-green)
               (org-agenda-structure :foreground ,adm-aaa-magenta)
               (org-block :foreground ,fg4)
               (org-code :foreground ,fg4)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,adm-aaa-purple :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,adm-aaa-orange)
               (org-document-title :weight bold :foreground ,adm-aaa-orange :height 1.44)
               (org-done :foreground ,adm-aaa-green)
               (org-ellipsis :foreground ,adm-aaa-orange)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,adm-aaa-orange)
               (org-headline-done :foreground ,adm-aaa-orange :weight normal :strike-through t)
               (org-hide :foreground ,adm-aaa-bg :background ,adm-aaa-bg)
               (org-level-1 :inherit bold :foreground ,adm-aaa-blue :height 1.3)
               (org-level-2 :inherit bold :foreground ,adm-aaa-magenta :height 1.1)
               (org-level-3 :weight normal :foreground ,adm-aaa-green :height 1.0)
               (org-level-4 :weight normal :foreground ,adm-aaa-orange)
               (org-level-5 :weight normal :foreground ,adm-aaa-red)
               (org-level-6 :weight normal :foreground ,other-blue)
               (org-level-7 :weight normal :foreground ,fg3)
               (org-level-8 :weight normal :foreground ,fg4)
               (org-link :foreground ,other-blue :underline t)
               (org-priority :foreground ,adm-aaa-purple)
               (org-scheduled :foreground ,adm-aaa-green)
               (org-scheduled-previously :foreground ,adm-aaa-cyan)
               (org-scheduled-today :foreground ,adm-aaa-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,adm-aaa-cyan)
               (org-table :foreground ,adm-aaa-green)
               (org-tag :foreground ,adm-aaa-magenta :weight bold :background ,bg2)
               (org-todo :foreground ,adm-aaa-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,adm-aaa-cyan)
               (org-warning :weight bold :foreground ,adm-aaa-red)

               ;; dired
               (dired-directory :foreground ,adm-aaa-purple :weight normal)
               (dired-flagged :foreground ,adm-aaa-blue)
               (dired-header :foreground ,fg3 :background ,adm-aaa-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,adm-aaa-fg :weight bold)
               (dired-marked :foreground ,adm-aaa-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,adm-aaa-cyan :weight normal :slant italic)
               (dired-warning :foreground ,adm-aaa-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,adm-aaa-fg)
               (diredp-deletion-file-name :foreground ,adm-aaa-blue :background ,bg1)
               (diredp-deletion :foreground ,adm-aaa-blue :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,adm-aaa-orange)
               (diredp-file-name :foreground ,adm-aaa-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,bg1)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,bg1)
               (diredp-ignored-file-name :foreground ,adm-aaa-fg)
               (diredp-mode-line-flagged :foreground ,adm-aaa-orange)
               (diredp-mode-line-marked :foreground ,adm-aaa-orange)
               (diredp-no-priv :foreground ,adm-aaa-fg)
               (diredp-number :foreground ,adm-aaa-green)
               (diredp-other-priv :foreground ,adm-aaa-orange)
               (diredp-rare-priv :foreground ,adm-aaa-orange)
               (diredp-read-priv :foreground ,adm-aaa-magenta)
               (diredp-write-priv :foreground ,adm-aaa-blue)
               (diredp-exec-priv :foreground ,adm-aaa-cyan)
               (diredp-symlink :foreground ,adm-aaa-orange)
               (diredp-link-priv :foreground ,adm-aaa-orange)
               (diredp-autofile-name :foreground ,adm-aaa-cyan)
               (diredp-tagged-autofile-name :foreground ,adm-aaa-cyan)

               ;; term
               ;; (term :foreground ,adm-aaa-fg :background ,adm-aaa-bg)
               ;; (term-color-black :foreground ,adm-aaa-bg :background ,adm-aaa-bg)
               ;; (term-color-blue :foreground ,adm-aaa-magenta :background ,adm-aaa-magenta)
               ;; (term-color-cyan :foreground ,adm-aaa-green :background ,adm-aaa-green)
               ;; (term-color-green :foreground ,adm-aaa-purple :background ,adm-aaa-purple)
               ;; (term-color-magenta :foreground ,adm-aaa-blue :background ,adm-aaa-blue)
               ;; (term-color-red :foreground ,adm-aaa-red :background ,adm-aaa-red)
               ;; (term-color-white :foreground ,adm-aaa-fg :background ,adm-aaa-fg)
               ;; (term-color-yellow :foreground ,adm-aaa-cyan :background ,adm-aaa-cyan)

               ;; web-mode
               ;; (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               ;; (web-mode-comment-face :inherit ,font-lock-comment-face)
               ;; (web-mode-constant-face :inherit ,font-lock-constant-face)
               ;; (web-mode-doctype-face :inherit ,font-lock-comment-face)
               ;; (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               ;; (web-mode-html-attr-name-face :foreground ,adm-aaa-magenta)
               ;; (web-mode-html-attr-value-face :foreground ,adm-aaa-purple)
               ;; (web-mode-html-tag-face :foreground ,adm-aaa-blue :weight bold)
               ;; (web-mode-keyword-face :foreground ,adm-aaa-blue)
               ;; (web-mode-string-face :foreground ,adm-aaa-cyan)
               ;; (web-mode-type-face :inherit ,font-lock-type-face)
               ;; (web-mode-warning-face :inherit ,font-lock-warning-face)

               ;; whitespace
               ;; (whitespace-big-indent :background ,adm-aaa-red :foreground ,adm-aaa-red)
               ;; (whitespace-empty :background ,adm-aaa-orange :foreground ,adm-aaa-red)
               ;; (whitespace-hspace :background ,bg3 :foreground ,adm-aaa-orange)
               ;; (whitespace-indentation :background ,adm-aaa-orange :foreground ,adm-aaa-red)
               ;; (whitespace-line :background ,adm-aaa-bg :foreground ,adm-aaa-blue)
               ;; (whitespace-newline :foreground ,adm-aaa-orange)
               ;; (whitespace-space :background ,adm-aaa-bg :foreground ,adm-aaa-orange)
               ;; (whitespace-space-after-tab :background ,adm-aaa-orange :foreground ,adm-aaa-red)
               ;; (whitespace-space-before-tab :background ,adm-aaa-orange :foreground ,adm-aaa-red)
               ;; (whitespace-tab :background ,bg2 :foreground ,adm-aaa-orange)
               ;; (whitespace-trailing :inherit trailing-whitespace)
               )))

  (apply #'custom-theme-set-faces
         'adm-aaa
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

(provide-theme 'adm-aaa)
