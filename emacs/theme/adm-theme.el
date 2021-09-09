;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Face-Customization.html
;; https://colorshark.io
(require 'cl-lib)
(deftheme adm)

(let ((colors '(
				(adm-bg		   "#F6F2E4")
				(adm-fg		   "#020227") ; 18.00

				;; colors
				(adm-red	"#790C0C") ; H360
				(adm-maroon "#6D0D5D") ; H310 栗色
				(adm-purple "#461E94") ; H260
				(adm-blue	"#183877") ; H220 like royalblue in emacs
				(adm-green	"#0C450C") ; H120
				(adm-yellow "#3D3D0F") ; H60 like olivegreen in emacs
				(adm-orange "#5B3006") ; H30 like darkorange4 in emacs

				;; bg
				(bg1 "#F2ECD9")			; 17.08
				(bg2 "#EBE5D0")			; 16.00
				(bg3 "#E1D7B7")			; 14.03
				(bg4 "#D5C79F")			; 12.01

				;; fg
				(fg1 "#06063D")			; 17.01
				(fg2 "#0C0C4B")			; 16.01
				(fg3 "#13136C")			; 14.01
				(fg4 "#1F1F7F")			; 12.00

				;; other
				(other-blue "#2E2E8A")	; H240 like slateblue in emacs
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
			   (info-string :foreground ,adm-yellow)
			   (lazy-highlight :foreground ,fg2 :background ,bg3)
			   (link :foreground ,adm-green :underline t)
			   (linum :foreground ,bg4 :background ,adm-bg)
			   (line-number :foreground ,bg4 :background ,adm-bg)
			   (minibuffer-prompt :weight bold :foreground ,adm-blue)
			   (region :background ,bg4 :extend t)
			   (trailing-whitespace :foreground nil :background ,adm-red)
			   (vertical-border :foreground ,bg4)
			   (success :foreground ,adm-purple)
			   (warning :foreground ,adm-orange)
			   (error :foreground ,adm-red)
			   (header-line :background ,adm-bg)

			   ;; syntax
			   (font-lock-function-name-face :foreground ,adm-maroon)
			   (font-lock-variable-name-face :foreground ,adm-fg)
			   (font-lock-type-face :foreground ,adm-purple)
			   (font-lock-keyword-face :foreground ,adm-blue :weight bold)
			   (font-lock-constant-face :foreground ,adm-fg :slant italic)
			   (font-lock-comment-face :foreground ,adm-orange)
			   (font-lock-builtin-face :foreground ,adm-blue :weight normal :slant italic)
			   (font-lock-string-face :foreground ,adm-green)

			   (font-lock-comment-delimiter-face :foreground ,adm-orange)
			   (font-lock-doc-face :foreground ,adm-orange)
			   (font-lock-negation-char-face :foreground ,adm-green)
			   (font-lock-preprocessor-face :foreground ,adm-orange)
			   (font-lock-reference-face :foreground ,adm-green)
			   (font-lock-regexp-grouping-backslash :foreground ,adm-green)
			   (font-lock-regexp-grouping-construct :foreground ,adm-maroon)
			   (font-lock-warning-face :foreground ,adm-red :background ,bg3)

			   ;; highlight-numbers
			   (highlight-numbers-number :foreground ,adm-green)

			   ;; mode-line
			   (mode-line :foreground ,adm-fg :background ,bg3 :box ,bg3)
			   (mode-line-inactive :foreground ,bg4 :background ,bg2 :box ,bg2)

			   ;; ido-mode
			   (ido-only-match :foreground ,adm-green)
			   (ido-subdir :foreground ,adm-purple)

			   ;; message
			   ;; (message-mml :foreground ,adm-purple :weight normal)
			   ;; (message-header-xheader :foreground ,adm-green :weight normal)

			   ;; gotest
			   (go-test--standard-face :foreground ,adm-orange)
			   (go-test--ok-face :foreground ,adm-green)
			   (go-test--error-face :foreground ,adm-red)
			   (go-test--warning-face :foreground ,adm-yellow)

			   ;; treemacs
			   (treemacs-root-face :foreground ,adm-orange :weight bold)
			   (treemacs-directory-face :foreground ,adm-purple)
			   (treemacs-git-untracked-face :foreground ,adm-green :slant italic :underline t)
			   (treemacs-git-added-face :foreground ,adm-green)
			   (treemacs-git-modified-face :foreground ,adm-yellow)
			   (treemacs-tags-face :foreground ,adm-blue)

			   ;; company
			   (company-echo-common :foreground ,adm-bg :background ,adm-fg)
			   (company-preview :foreground ,other-blue :background ,adm-bg)
			   (company-preview-common :foreground ,fg3 :background ,bg2)
			   (company-preview-search :foreground ,adm-maroon :background ,adm-bg)
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
			   (magit-branch-remote :foreground ,adm-purple)
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
			   (magit-diffstat-added :foreground ,adm-purple)
			   (magit-diffstat-removed :foreground ,adm-red)
			   (magit-hash :foreground ,fg2)
			   (magit-hunk-heading :background ,bg3)
			   (magit-hunk-heading-highlight :background ,bg3)
			   (magit-item-highlight :background ,bg3)
			   (magit-log-author :foreground ,fg3)
			   (magit-process-ng :foreground ,adm-orange :weight bold)
			   (magit-process-ok :foreground ,adm-purple :weight bold)

			   ;; org
			   (org-agenda-date :foreground ,adm-purple :underline nil)
			   (org-agenda-dimmed-todo-face :foreground ,adm-orange)
			   (org-agenda-done :foreground ,adm-green)
			   (org-agenda-structure :foreground ,adm-maroon)
			   (org-block :foreground ,fg4)
			   (org-code :foreground ,fg4)
			   (org-column :background ,bg4)
			   (org-column-title :inherit org-column :weight bold :underline t)
			   (org-date :foreground ,adm-purple :underline t)
			   (org-document-info :foreground ,other-blue)
			   (org-document-info-keyword :foreground ,adm-orange)
			   (org-document-title :weight bold :foreground ,adm-orange :height 1.44)
			   (org-done :foreground ,adm-green)
			   (org-ellipsis :foreground ,adm-orange)
			   (org-footnote :foreground ,other-blue)
			   (org-formula :foreground ,adm-orange)
			   (org-headline-done :foreground ,adm-orange :weight normal :strike-through t)
			   (org-hide :foreground ,adm-bg :background ,adm-bg)
			   (org-level-1 :inherit bold :foreground ,adm-blue :height 1.3)
			   (org-level-2 :inherit bold :foreground ,adm-maroon :height 1.1)
			   (org-level-3 :weight normal :foreground ,adm-green :height 1.0)
			   (org-level-4 :weight normal :foreground ,adm-orange)
			   (org-level-5 :weight normal :foreground ,adm-red)
			   (org-level-6 :weight normal :foreground ,other-blue)
			   (org-level-7 :weight normal :foreground ,fg3)
			   (org-level-8 :weight normal :foreground ,fg4)
			   (org-link :foreground ,other-blue :underline t)
			   (org-priority :foreground ,adm-purple)
			   (org-scheduled :foreground ,adm-green)
			   (org-scheduled-previously :foreground ,adm-yellow)
			   (org-scheduled-today :foreground ,adm-green)
			   (org-sexp-date :foreground ,fg4)
			   (org-special-keyword :foreground ,adm-yellow)
			   (org-table :foreground ,adm-green)
			   (org-tag :foreground ,adm-maroon :weight bold :background ,bg2)
			   (org-todo :foreground ,adm-orange :weight bold :background ,bg2)
			   (org-upcoming-deadline :foreground ,adm-yellow)
			   (org-warning :weight bold :foreground ,adm-red)

			   ;; dired
			   (dired-directory :foreground ,adm-purple :weight normal)
			   (dired-flagged :foreground ,adm-blue)
			   (dired-header :foreground ,fg3 :background ,adm-bg)
			   (dired-ignored :inherit shadow)
			   (dired-mark :foreground ,adm-fg :weight bold)
			   (dired-marked :foreground ,adm-orange :weight bold)
			   (dired-perm-write :foreground ,fg3 :underline t)
			   (dired-symlink :foreground ,adm-yellow :weight normal :slant italic)
			   (dired-warning :foreground ,adm-orange :underline t)
			   (diredp-compressed-file-name :foreground ,fg3)
			   (diredp-compressed-file-suffix :foreground ,fg4)
			   (diredp-date-time :foreground ,adm-fg)
			   (diredp-deletion-file-name :foreground ,adm-blue :background ,bg1)
			   (diredp-deletion :foreground ,adm-blue :weight bold)
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
			   (diredp-read-priv :foreground ,adm-maroon)
			   (diredp-write-priv :foreground ,adm-blue)
			   (diredp-exec-priv :foreground ,adm-yellow)
			   (diredp-symlink :foreground ,adm-orange)
			   (diredp-link-priv :foreground ,adm-orange)
			   (diredp-autofile-name :foreground ,adm-yellow)
			   (diredp-tagged-autofile-name :foreground ,adm-yellow)

			   ;; term
			   ;; (term :foreground ,adm-fg :background ,adm-bg)
			   ;; (term-color-black :foreground ,adm-bg :background ,adm-bg)
			   ;; (term-color-blue :foreground ,adm-maroon :background ,adm-maroon)
			   ;; (term-color-cyan :foreground ,adm-green :background ,adm-green)
			   ;; (term-color-green :foreground ,adm-purple :background ,adm-purple)
			   ;; (term-color-magenta :foreground ,adm-blue :background ,adm-blue)
			   ;; (term-color-red :foreground ,adm-red :background ,adm-red)
			   ;; (term-color-white :foreground ,adm-fg :background ,adm-fg)
			   ;; (term-color-yellow :foreground ,adm-yellow :background ,adm-yellow)

			   ;; web-mode
			   ;; (web-mode-builtin-face :inherit ,font-lock-builtin-face)
			   ;; (web-mode-comment-face :inherit ,font-lock-comment-face)
			   ;; (web-mode-constant-face :inherit ,font-lock-constant-face)
			   ;; (web-mode-doctype-face :inherit ,font-lock-comment-face)
			   ;; (web-mode-function-name-face :inherit ,font-lock-function-name-face)
			   ;; (web-mode-html-attr-name-face :foreground ,adm-maroon)
			   ;; (web-mode-html-attr-value-face :foreground ,adm-purple)
			   ;; (web-mode-html-tag-face :foreground ,adm-blue :weight bold)
			   ;; (web-mode-keyword-face :foreground ,adm-blue)
			   ;; (web-mode-string-face :foreground ,adm-yellow)
			   ;; (web-mode-type-face :inherit ,font-lock-type-face)
			   ;; (web-mode-warning-face :inherit ,font-lock-warning-face)

			   ;; whitespace
			   ;; (whitespace-big-indent :background ,adm-red :foreground ,adm-red)
			   ;; (whitespace-empty :background ,adm-orange :foreground ,adm-red)
			   ;; (whitespace-hspace :background ,bg3 :foreground ,adm-orange)
			   ;; (whitespace-indentation :background ,adm-orange :foreground ,adm-red)
			   ;; (whitespace-line :background ,adm-bg :foreground ,adm-blue)
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
