;; xah fly keys (default layout is dvorak)
(cp 'xah-fly-keys
    (lambda()
      (require 'xah-fly-keys)
      (xah-fly-keys 1)
      (dk xah-fly-t-keymap (kbd "h") 'xah-close-current-buffer)
      (dk xah-fly-leader-key-map (kbd "v") 'xah-goto-matching-bracket)
      (lk xah-fly-leader-key-map (kbd "g")
          (lambda() "close-current-buffer-and-delete-window"
            (interactive)
            (xah-close-current-buffer)
            (delete-window)))))

;; magit
(cp 'magit
    (lambda()
      ;; (setq magit-diff-refine-hunk 'all)
      ;; show changes made to current git repo (magit-status)
      (dk xah-fly-dot-keymap (kbd "c") 'magit-status)))

;; exec-path-from-shell
(cp 'exec-path-from-shell
    (lambda()
      (when (memq window-system '(mac ns x))
        (exec-path-from-shell-initialize))))

;; find file in repository
(cp 'find-file-in-repository
    (lambda() (gsk (kbd "<f7>") 'find-file-in-repository)))

;; lsp-mode
(cp 'lsp-mode
    (lambda()
      (autoload 'lsp-mode "lsp-mode")
      (setq lsp-enable-links nil)
      (setq lsp-enable-snippet nil)

      ;; performance
      ;; ** Experimenting **
      ;; Configuration to improve lsp-mode performance
      ;;
      ;; The default setting is too low for lsp-mode's needs
      ;; due to the fact that client/server communication
      ;; generates a lot of memory/garbage. You have two options:
      ;; - Set it to big number(100mb) like most of the popular starter kits like
      ;; Spacemacs/Doom/Prelude, etc do
      ;; - Follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii:
      ;; "My suggestion is to repeatedly multiply gc-cons-threshold by 2 until
      ;; you stop seeing significant improvements in responsiveness, and in any
      ;; case not to increase by a factor larger than 100 or somesuch. If even a
      ;; 100-fold increase doesn't help, there's some deeper problem with the
      ;; Lisp code which produces so much garbage, or maybe GC is not the reason
      ;; for slowdown."
      ;; Source: https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/
      (setq gc-cons-threshold gc-con-threshold-value)

      ;; Increase the amount of data which Emacs reads from the process. Again the
      ;; emacs default is too low 4k considering that the some of the language
      ;; server responses are in 800k - 3M range.
      (setq read-process-output-max (* 1024 1024)) ; 1MB

      ;; lsp-mode's company-capf does caching by default
      (setq lsp-prefer-capf t)

      ;; This variable determines how often lsp-mode will refresh the highlights,
      ;; lenses, links, etc while you type.
      (setq lsp-idle-delay 1)

      (dk xah-fly-comma-keymap (kbd "n") 'lsp-rename)
      (dk xah-fly-comma-keymap (kbd "s") 'xref-find-apropos)))

;; lsp-treemacs
(cp 'lsp-treemacs
    (lambda()
      ;; enable bidirectional synchronization of lsp workspace folders and treemacs projects
      (lsp-treemacs-sync-mode 1)
      (dk xah-fly-dot-keymap (kbd "s") 'lsp-treemacs-symbols)
      (lk xah-fly-comma-keymap (kbd "r")
          (lambda() "show-lsp-references-in-treemacs"
            (interactive)
            (lsp-treemacs-references t)))
      (lk xah-fly-comma-keymap (kbd "m")
          (lambda() "show-lsp-implementations-in-treemacs"
            (interactive)
            (lsp-treemacs-implementations t)))))

;; treemacs
(cp 'treemacs
    (lambda()
      (setq treemacs-no-png-images t)
      (setq treemacs-width 30)
      (setq treemacs-show-cursor 1)
      (setq treemacs-fringe-indicator-mode nil)
      ;; on small screens, make treemacs window deletable by delete-other-windows
      ;; on big screens, it might be worthy to keep it around
      ;; TODO use display-pixel-width/display-pixel-height to detect display size
      ;; call external tool to get pixel density to get the diagonal size of the display
      (setq treemacs-no-delete-other-windows nil)
      ;; no spacing betweewn root nodes (this is to fix *LSP Symbols List*)
      (setq treemacs-space-between-root-nodes nil)
      ;; hide line numbers and mode line in treemacs
      (add-hook `treemacs-mode-hook
                (lambda ()
                  (interactive)
                  (display-line-numbers-mode 0)
                  (variable-pitch-mode 1)))
      (dk xah-fly-dot-keymap (kbd "p") 'treemacs)))

;; company
(cp 'company
    (lambda()
      (setq company-require-match nil)
      (setq company-tooltip-align-annotations t); Align annotation to the right side.
      (setq company-minimum-prefix-length 1)))

;; company-lsp
(cp 'company-lsp
    (lambda()
      (autoload 'company-lsp "company-lsp")
      (with-eval-after-load 'company
        (push 'company-lsp company-backends))))

;; go-mode
(cp 'go-mode
    (lambda()
      ;; fontify only function declarations, not function calls
      (setq go-fontify-function-calls nil)
      ;; format code and reorganize imports before saving buffer
      ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
      (add-hook 'go-mode-hook
                (lambda()
                  (add-hook 'before-save-hook #'lsp-format-buffer t t)
                  (add-hook 'before-save-hook #'lsp-organize-imports t t)))
      ;; go-run and go-test
      (setq go-test-verbose t)
      (setq go-test-args "-count=1")    ; bypass test caching
      (dk xah-fly-dot-keymap (kbd "r") 'go-run)
      (dk xah-fly-dot-keymap (kbd "t") 'go-test-current-test)
      (dk xah-fly-dot-keymap (kbd "T") 'go-test-current-file)
      ;; enable lsp-mode in go-mode
      (cp 'lsp-mode (lambda() (add-hook 'go-mode-hook 'lsp-deferred)))
      ;; register jump implmentation
      (puthash 'go-mode 'lsp-find-definition jump-map)
      ;; show eldoc
      (cp 'go-eldoc (lambda() (add-hook 'go-mode-hook 'go-eldoc-setup)))
      ;; use gogetdoc to show doc (go got )
      (setq godoc-at-point-function 'godoc-gogetdoc)
      (dk xah-fly-comma-keymap (kbd "d") 'godoc-at-point)))

;; hl-todo
(cp 'hl-todo
    (lambda()
      (global-hl-todo-mode)
      (setq hl-todo-keyword-faces
            '(("TODO"   . "#C40233")
              ("FIXME"  . "#C40233")
              ("DEBUG"  . "#A020F0")
              ("GOTCHA" . "#FF4500")
              ("STUB"   . "#1E90FF")))))

;; highlight numbers
(cp 'highlight-numbers
    (lambda()
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'highlight-numbers-mode-hook
                (lambda()
                  "symbol-start and symbol-end excludes dot (.)
                  so numbers start or end with dots must be matched separately"
                  (puthash 'go-mode
                           (rx
                            (or
                             ;; match numbers end with .
                             (and
                              symbol-start
                              (or
                               (and (+ (any digit "_"))
                                    "."
                                    (? (and (any "eE")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               ))

                             ;; match numbers start with .
                             (and
                              (or
                               (and "."
                                    (+ (any digit "_"))
                                    (? (and (any "eE")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               )
                              symbol-end)

                             ;; match all the rest number formats
                             (and
                              symbol-start
                              (or
                               (and "0"
                                    (or (and (any "bB")
                                             (+ (any "0_1")))
                                        (and (any "oO")
                                             (+ (any "0-7_")))
                                        (and (any "xX")
                                             (+ (any hex-digit "_."))))
                                    (? (and (any "pP")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               (and digit
                                    (* (any digit "_"))
                                    (? ".")
                                    (* (any digit "_"))
                                    (? (and (any "eE")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               )
                              symbol-end)
                             ))
                           highlight-numbers-modelist)))))

;; markdown-mode
(cp 'markdown-mode
    (lambda()
      ;; make writing in markdown-mode less distracting
      (add-hook 'markdown-mode-hook
                (lambda ()
                  ;; sum of left and right margin cannot be greater than 67
                  ;; was 35 35, now 33 33
                  ;; see https://github.com/Alexander-Miller/treemacs/issues/669
                  ;; for now, only cell unit (40 chars) is support when sitting window margin
                  ;; check up visual-fill-column for support for ratio unit (0.3 (30%))
                  (setq left-margin-width 33)
                  (setq right-margin-width 33)
                  (visual-line-mode 1)
                  (display-line-numbers-mode 0)
                  (setq mode-line-format nil)
                  (setq buffer-face-mode-face
                        ;; '(:family "IM FELL English PRO" :height 220 :width regular))
                        '(:family "Bradley Hand" :height 220 :width regular))
                        ;; '(:family "caveat" :height 300 :width regular))
                  (buffer-face-mode)))))

;; Ruby robe
(cp 'robe
    (lambda()
      (setq ruby-insert-encoding-magic-comment nil)
      (setq inf-ruby-console-environment "development")
      (add-hook 'ruby-mode-hook 'robe-mode)
      (add-hook 'ruby-mode-hook 'robe-start)
      ;; register jump implmentation
      (puthash 'ruby-mode 'robe-jump jump-map)
      ;; ruby code navigation
      (gsk (kbd "s-r r") 'inf-ruby)
      (gsk (kbd "s-r c") 'inf-ruby-console-auto)))

;; Web Mode
(cp 'web-mode
    (lambda()
      (autoload 'web-mode "web-mode")
      (atl 'auto-mode-alist '("\\.html\\'" . web-mode))
      (atl 'auto-mode-alist '("\\.erb\\'" . web-mode))))

;; YAML
(cp 'yaml-mode
    (lambda()
      (autoload 'yaml-mode "yaml-mode")
      (atl 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
      (atl 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))))

;; Rest client
(cp 'restclient
    (lambda()
      (autoload 'restclient "restclient")
      (atl 'auto-mode-alist '("\\.http\\'" . restclient-mode))
      (atl 'auto-mode-alist '("\\.rest\\'" . restclient-mode))))

;; Apib-mode (major mode for editing API blueprint file)
(cp 'apib-mode
    (lambda()
      (autoload 'apib-mode "apib-mode")
      (atl 'auto-mode-alist '("\\.api\\'" . apib-mode))
      (atl 'auto-mode-alist '("\\.apib\\'" . apib-mode))
      (atl 'auto-mode-alist '("\\.blueprint\\'" . apib-mode))))

;; json mode
(cp 'json-mode
    (lambda()
      (add-hook 'json-mode-hook
                (lambda ()
                  (make-local-variable 'js-indent-level)
                  (setq js-indent-level 2)))))
