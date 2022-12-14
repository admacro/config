;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Face-Customization.html
;; https://www.w3.org/WAI/WCAG21/Understanding/contrast-minimum.html
;; https://colorshark.io
;; https://webaim.org/resources/contrastchecker/
(require 'cl-lib)
(deftheme adm)

(let ((colors '(
                (adm-bg      "#E8E3D9")
                (adm-fg      "#000000")

                ;; foreground colors
                (adm-red     "#970000")
                (adm-magenta "#8E005F")
                (adm-orange  "#733900")
                (adm-green   "#005600")
                (adm-cyan    "#004D74")
                (adm-blue    "#2F2FC6")
                (adm-purple  "#7300AC")

                ;; background colors
                (adm-red-bg     "#E8D9D9")
                (adm-magenta-bg "#E8D9E3")
                (adm-orange-bg  "#E8E0D9")
                (adm-green-bg   "#D9E8D9")
                (adm-cyan-bg    "#D9E3E8")
                (adm-blue-bg    "#D9D9E8")
                (adm-purple-bg  "#E3D9E8")

                ;; bg
                (bg1 "#E0D9CC")
                (bg2 "#D7CEBC")
                (bg3 "#CDC2AC")
                (bg4 "#C2B59E")

                ;; fg
                (fg1 "#0F0F0F")
                (fg2 "#1C1C1C")
                (fg3 "#262626")
                (fg4 "#303030")

                ;; grayed colors
                (adm-red-gray     "#7E3E3E")
                (adm-magenta-gray "#7F3F6A")
                (adm-orange-gray  "#806040")
                (adm-green-gray   "#408040")
                (adm-cyan-gray    "#406A7F")
                (adm-blue-gray    "#40407F")
                (adm-purple-gray  "#6A407F")
                (adm-bg-gray "#816B40")
                (adm-fg-gray "#6C6C6C")
                ))

      (faces '(;; default
               (cursor :background ,fg3)
               (default :background ,adm-bg :foreground ,adm-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,adm-bg :foreground ,fg4)
               (highlight :foreground nil :background ,bg3)
               (hl-line :background ,bg1 :extend t)
               (info-quoted-name :foreground ,adm-orange)
               (info-string :foreground ,adm-blue)
               (lazy-highlight :foreground ,fg2 :background ,bg3)
               (link :foreground ,adm-green :underline t)
               (linum :foreground ,bg4 :background ,adm-bg)
               (line-number :foreground ,bg4 :background ,adm-bg)
               (minibuffer-prompt :weight bold :foreground ,adm-cyan)
               (region :background ,bg3 :extend t)
               (trailing-whitespace :foreground nil :background ,adm-red)
               (vertical-border :foreground ,bg4)
               (success :foreground ,adm-green)
               (warning :foreground ,adm-orange)
               (error :foreground ,adm-red)
               (header-line :background ,adm-bg)

               ;; syntax
               (font-lock-variable-name-face :foreground ,adm-fg)
               (font-lock-constant-face :foreground ,adm-purple :slant italic)
               (font-lock-string-face :foreground ,adm-green)
               (font-lock-keyword-face :foreground ,adm-cyan :weight bold)
               (font-lock-type-face :foreground ,adm-red)
               (font-lock-function-name-face :foreground ,adm-magenta)
               (font-lock-builtin-face :foreground ,adm-cyan :slant italic)

               (font-lock-comment-face :foreground ,adm-orange)
               (font-lock-comment-delimiter-face :foreground ,adm-orange)
               (font-lock-doc-face :foreground ,adm-orange :slant italic)

               (font-lock-negation-char-face :foreground ,adm-red)
               (font-lock-preprocessor-face :foreground ,adm-blue)
               (font-lock-regexp-grouping-backslash :foreground ,adm-red)
               (font-lock-regexp-grouping-construct :foreground ,adm-purple)
               (font-lock-warning-face :foreground ,adm-red :background ,bg3)

               ;; minibuffer
               (completions-common-part :foreground ,adm-green)
               (completions-first-difference :foreground ,adm-red)

               ;; highlight-numbers
               (highlight-numbers-number :foreground ,adm-green)

               ;; mode-line
               (mode-line :foreground ,adm-fg :background ,bg4 :box ,bg4)
               (mode-line-inactive :foreground ,bg4 :background ,bg1 :box ,bg1)

               ;; ido-mode
               (ido-only-match :foreground ,adm-green)
               (ido-subdir :foreground ,adm-purple)

               ;; show-paren
               (show-paren-match :background ,bg4)
               (show-paren-mismatch :weight bold :background ,adm-red)

               ;; message
               ;; (message-mml :foreground ,adm-purple :weight normal)
               ;; (message-header-xheader :foreground ,adm-green :weight normal)

               ;; gotest
               (go-test--standard-face :foreground ,adm-fg)
               (go-test--ok-face :foreground ,adm-green)
               (go-test--error-face :foreground ,adm-red)
               (go-test--warning-face :foreground ,adm-orange)

               ;; shell script
               (sh-quoted-exec :foreground ,adm-magenta)

               ;; company
               (company-echo-common :foreground ,adm-bg :background ,adm-fg)
               (company-preview :foreground ,adm-cyan :background ,adm-bg)
               (company-preview-common :foreground ,fg3 :background ,bg2)
               (company-preview-search :foreground ,adm-magenta :background ,adm-bg)
               (company-scrollbar-bg :background ,bg1)
               (company-scrollbar-fg :background ,bg3)
               (company-template-field :inherit region)
               (company-tooltip :foreground ,adm-fg :background ,bg1)
               (company-tooltip-annotation :foreground ,fg4 :slant italic)
               (company-tooltip-common :foreground ,adm-fg :background ,bg4)
               (company-tooltip-common-selection :foreground ,adm-red :background ,bg4)
               (company-tooltip-mouse :inherit highlight)
               (company-tooltip-selection :foreground ,adm-red :background ,bg2)

               ;; diff
               (diff-refine-added :background ,adm-green)
               (diff-refine-removed :background ,adm-red)

               ;; magit
               (magit-branch-local :foreground ,adm-green)
               (magit-branch-remote :foreground ,adm-cyan)
               (magit-tag :foreground ,adm-orange)
               (magit-section-heading :foreground ,adm-orange :weight bold)
               (magit-section-highlight :background ,bg1 :extend t)
               (magit-diff-context-highlight :background ,bg1 :extend t)
               (magit-diff-added :background ,adm-green-bg :foreground ,fg4 :extend t)
               (magit-diff-added-highlight :background ,adm-green-bg :foreground ,adm-green :extend t)
               (magit-diff-removed :background ,adm-red-bg :foreground ,fg4 :extend t)
               (magit-diff-removed-highlight :background ,adm-red-bg :foreground ,adm-red  :extend t)
               (magit-diff-file-heading :background ,bg3 :weight bold)
               (magit-diff-file-heading-highlight :foreground ,adm-orange :background ,bg3)
               (magit-diff-hunk-heading :background ,bg2)
               (magit-diff-hunk-heading-highlight :foreground ,adm-orange :background ,bg3)
               ;; (magit-diff-revision-summary :foreground ,adm-orange :background ,adm-bg :weight bold)
               ;; (magit-diff-revision-summary-highlight :foreground ,adm-orange :background ,bg3 :weight bold :extend t)
               (magit-diffstat-added :foreground ,adm-green)
               (magit-diffstat-removed :foreground ,adm-red)
               (magit-hash :foreground ,adm-fg-gray)
               (magit-log-author :foreground ,adm-orange)
               (magit-process-ng :foreground ,adm-red :weight bold)
               (magit-process-ok :foreground ,adm-green :weight bold)

               ;; org
               (org-agenda-date :foreground ,adm-purple :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,adm-orange)
               (org-agenda-done :foreground ,adm-green)
               (org-agenda-structure :foreground ,adm-magenta)
               (org-block :background ,bg1 :foreground ,fg1)
               (org-code :background ,bg1 :foreground ,fg1)
               (org-verbatim :background ,bg1 :foreground ,fg1)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,adm-purple :underline t)
               (org-document-info :foreground ,adm-blue)
               (org-document-info-keyword :foreground ,adm-orange)
               (org-document-title :weight bold :foreground ,adm-orange :height 1.44)
               (org-done :foreground ,adm-green)
               (org-ellipsis :foreground ,adm-orange)
               (org-footnote :foreground ,adm-blue)
               (org-formula :foreground ,adm-orange)
               (org-headline-done :foreground ,adm-orange :weight normal :strike-through t)
               (org-hide :foreground ,adm-bg :background ,adm-bg)
               (org-level-1 :inherit bold :foreground ,adm-red :height 1.3)
               (org-level-2 :inherit bold :foreground ,adm-cyan :height 1.1)
               (org-level-3 :weight normal :foreground ,adm-magenta :height 1.0)
               (org-level-4 :weight normal :foreground ,adm-green)
               (org-level-5 :weight normal :foreground ,adm-orange)
               (org-level-6 :weight normal :foreground ,adm-purple)
               (org-level-7 :weight normal :foreground ,adm-blue)
               (org-level-8 :weight normal :foreground ,adm-red)
               (org-link :foreground ,adm-blue :underline t)
               (org-priority :foreground ,adm-purple)
               (org-scheduled :foreground ,adm-green)
               (org-scheduled-previously :foreground ,adm-blue)
               (org-scheduled-today :foreground ,adm-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,adm-blue)
               (org-table :foreground ,adm-green)
               (org-tag :foreground ,adm-magenta :weight bold :background ,bg2)
               (org-todo :foreground ,adm-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,adm-blue)
               (org-warning :weight bold :foreground ,adm-red)

               ;; dired
               (dired-directory :foreground ,adm-cyan :weight normal)
               (dired-flagged :foreground ,adm-purple)
               (dired-header :foreground ,fg3 :background ,adm-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,adm-fg :weight bold)
               (dired-marked :foreground ,adm-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,adm-blue :weight normal :slant italic)
               (dired-warning :foreground ,adm-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,adm-fg)
               (diredp-deletion-file-name :foreground ,adm-cyan :background ,bg1)
               (diredp-deletion :foreground ,adm-cyan :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,adm-orange)
               (diredp-file-name :foreground ,adm-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,bg1)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,bg1)
               (diredp-ignored-file-name :foreground ,adm-fg)
               (diredp-mode-line-flagged :foreground ,adm-orange)
               (diredp-mode-line-marked :foreground ,adm-orange)
               (diredp-no-priv :foreground ,adm-fg)
               (diredp-number :foreground ,adm-green)
               (diredp-other-priv :foreground ,adm-orange)
               (diredp-rare-priv :foreground ,adm-orange)
               (diredp-read-priv :foreground ,adm-magenta)
               (diredp-write-priv :foreground ,adm-cyan)
               (diredp-exec-priv :foreground ,adm-blue)
               (diredp-symlink :foreground ,adm-orange)
               (diredp-link-priv :foreground ,adm-orange)
               (diredp-autofile-name :foreground ,adm-blue)
               (diredp-tagged-autofile-name :foreground ,adm-blue)

               ;; treemacs
               (treemacs-root-face :foreground ,adm-fg :weight bold)
               (treemacs-directory-face :foreground ,adm-fg)
               (treemacs-git-untracked-face :foreground ,adm-green :slant italic :underline t)
               (treemacs-git-added-face :foreground ,adm-green)
               (treemacs-git-modified-face :foreground ,adm-orange)
               (treemacs-git-ignored-face :foreground ,adm-fg-gray)
               (treemacs-tags-face :foreground ,adm-cyan)

               ;; term
               ;; (term :foreground ,adm-fg :background ,adm-bg)
               ;; (term-color-black :foreground ,adm-bg :background ,adm-bg)
               ;; (term-color-blue :foreground ,adm-magenta :background ,adm-magenta)
               ;; (term-color-cyan :foreground ,adm-green :background ,adm-green)
               ;; (term-color-green :foreground ,adm-purple :background ,adm-purple)
               ;; (term-color-magenta :foreground ,adm-cyan :background ,adm-cyan)
               ;; (term-color-red :foreground ,adm-red :background ,adm-red)
               ;; (term-color-white :foreground ,adm-fg :background ,adm-fg)
               ;; (term-color-yellow :foreground ,adm-blue :background ,adm-blue)

               ;; web-mode
               ;; (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               ;; (web-mode-comment-face :inherit ,font-lock-comment-face)
               ;; (web-mode-constant-face :inherit ,font-lock-constant-face)
               ;; (web-mode-doctype-face :inherit ,font-lock-comment-face)
               ;; (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               ;; (web-mode-html-attr-name-face :foreground ,adm-magenta)
               ;; (web-mode-html-attr-value-face :foreground ,adm-purple)
               ;; (web-mode-html-tag-face :foreground ,adm-cyan :weight bold)
               ;; (web-mode-keyword-face :foreground ,adm-cyan)
               ;; (web-mode-string-face :foreground ,adm-blue)
               ;; (web-mode-type-face :inherit ,font-lock-type-face)
               ;; (web-mode-warning-face :inherit ,font-lock-warning-face)

               ;; whitespace
               ;; (whitespace-big-indent :background ,adm-red :foreground ,adm-red)
               ;; (whitespace-empty :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-hspace :background ,bg3 :foreground ,adm-orange)
               ;; (whitespace-indentation :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-line :background ,adm-bg :foreground ,adm-cyan)
               ;; (whitespace-newline :foreground ,adm-orange)
               ;; (whitespace-space :background ,adm-bg :foreground ,adm-orange)
               ;; (whitespace-space-after-tab :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-space-before-tab :background ,adm-orange :foreground ,adm-red)
               ;; (whitespace-tab :background ,bg2 :foreground ,adm-orange)
               ;; (whitespace-trailing :inherit trailing-whitespace)
               )))

  (apply #'custom-theme-set-faces
         'adm
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

(provide-theme 'adm)
