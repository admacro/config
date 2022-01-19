(require 'string-inflection)
(define-key xah-fly-dot-keymap (kbd "-") 'string-inflection-cycle)

;; magit
(config-pkg 'magit
            (lambda()
              ;; (setq magit-diff-refine-hunk 'all)

              ;; show changes made to current git repo (magit-status)
              (define-key xah-fly-dot-keymap (kbd "s") 'magit-status)

              ;; commit (C-c C-c)
              (define-key xah-fly-dot-keymap (kbd "c") 'with-editor-finish)
              ))

;; exec-path-from-shell
(config-pkg 'exec-path-from-shell
            (lambda()
              (when (memq window-system '(mac ns x))
                (exec-path-from-shell-initialize))))

;; find file in repository
(config-pkg 'find-file-in-repository
            (lambda()
              (define-key xah-fly-leader-key-map (kbd "7") 'find-file-in-repository)
              (define-key xah-fly-leader-key-map (kbd "&") 'find-file-in-repository)))

;; lsp-mode
(config-pkg 'lsp-mode
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
              (setq gc-cons-threshold (* 1024 1024 100)) ; 50MB

              ;; Increase the amount of data which Emacs reads from the process. Again the
              ;; emacs default is too low 4k considering that the some of the language
              ;; server responses are in 800k - 3M range.
              (setq read-process-output-max (* 1024 1024 20)) ; 10MB

              ;; lsp-mode's company-capf does caching by default
              (setq lsp-prefer-capf t)

              ;; This variable determines how often lsp-mode will refresh the highlights,
              ;; lenses, links, etc while you type.
              (setq lsp-idle-delay 1)

              ;; code navigation
              (global-set-key (kbd "<f6>") 'lsp-find-definition)
              (global-set-key (kbd "<f5>") 'xref-pop-marker-stack) ; go back to previous jump mark

              (define-key xah-fly-comma-keymap (kbd "n") 'lsp-rename)
              (define-key xah-fly-comma-keymap (kbd ",") 'xref-find-apropos)))

;; company
;; in-buffer code completion framework
(config-pkg 'company
            (lambda()
              (setq company-require-match nil)
              (setq company-tooltip-align-annotations t); Align annotation to the right side.
              (setq company-minimum-prefix-length 1)))

(defun lsp-reorganize-code-before-save()
  "reformat buffer and reorganize imports in lsp mode before saving the buffer"
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; lsp-java
(config-pkg 'lsp-java
            (lambda()
              (require 'lsp-java) ; use 'lsp-java-boot for sprint boot projects

              (add-hook 'java-mode-hook 'lsp-deferred)
              (add-hook 'java-mode-hook 'lsp-reorganize-code-before-save)

              ;; organize imports when saving buffer
              (setq lsp-java-save-actions-organize-imports t)
              (setq lsp-java-vmargs
                    (list
                     "-noverify"
                     "-Xms1G"
                     "-Xmx2G"
                     "-XX:+UseG1GC"
                     "-XX:+UseStringDeduplication"))))

;; go-mode
(config-pkg 'go-mode
            (lambda()
              ;; fontify only function declarations, not function calls
              (setq go-fontify-function-calls nil)

              ;; format code and reorganize imports before saving buffer
              ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
              (add-hook 'go-mode-hook 'lsp-reorganize-code-before-save)

              ;; go-run and go-test
              (setq go-test-verbose t)
              (setq go-test-args "-count=1")    ; bypass test caching

              ;; enable lsp-mode in go-mode
              (config-pkg 'lsp-mode (lambda() (add-hook 'go-mode-hook 'lsp-deferred)))

              ;; show eldoc
              (config-pkg 'go-eldoc (lambda() (add-hook 'go-mode-hook 'go-eldoc-setup)))

              ;; use gogetdoc to show doc (go got )
              (setq godoc-at-point-function 'godoc-gogetdoc)

              (define-key xah-fly-dot-keymap (kbd "r") 'go-run)
              (define-key xah-fly-dot-keymap (kbd "t") 'go-test-current-test)
              (define-key xah-fly-dot-keymap (kbd "f") 'go-test-current-file)
              (define-key xah-fly-comma-keymap (kbd "d") 'godoc-at-point)
              ))

;; treemacs
;; a tree layout file explorer
(config-pkg 'treemacs
            (lambda()
              ;; (setq treemacs-no-png-images t) ; use text ui instead of icons

              (setq treemacs-show-cursor 1)
              (setq treemacs-fringe-indicator-mode nil)

              ;; set indentation to default icon size (22px)
              (setq treemacs-indentation '(22 px))

              ;; when using a big screen
              ;; 1. keep treemacs window around
              ;; 2. use slightly wider column
              (unless (> (display-pixel-width) 1600)
                (progn (setq treemacs-width 35)
                       (setq treemacs-no-delete-other-windows nil))
                (setq treemacs-width 30))

              ;; no spacing between root nodes (this is to fix *LSP Symbols List*)
              (setq treemacs-space-between-root-nodes nil)
              ;; hide line numbers and mode line in treemacs
              (add-hook `treemacs-mode-hook
                        (lambda ()
                          (interactive)
                          (display-line-numbers-mode 0)
                          (variable-pitch-mode 1)))

              (define-key xah-fly-dot-keymap (kbd ".") 'treemacs)
              ))

;; lsp-treemacs
(config-pkg 'lsp-treemacs
            (lambda()
              ;; enable bidirectional synchronization of lsp workspace folders and treemacs projects
              (lsp-treemacs-sync-mode 1)
              (define-key xah-fly-comma-keymap (kbd "s") 'lsp-treemacs-symbols)
              (define-key xah-fly-comma-keymap (kbd "r") 'lsp-treemacs-references)
              (define-key xah-fly-comma-keymap (kbd "m") 'lsp-treemacs-implementations)))

;; hl-todo
(config-pkg 'hl-todo
            (lambda()
              (global-hl-todo-mode)
              (setq hl-todo-keyword-faces
                    '(("TODO"   . "#C40233")
                      ("FIXME"  . "#C40233")
                      ("DEBUG"  . "#A020F0")
                      ("GOTCHA" . "#FF4500")
                      ("STUB"   . "#1E90FF")))))

;; highlight numbers
(config-pkg 'highlight-numbers
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

;; Ruby robe
(config-pkg 'robe
            (lambda()
              (setq ruby-insert-encoding-magic-comment nil)
              (setq inf-ruby-console-environment "development")
              (add-hook 'ruby-mode-hook 'robe-mode)
              (add-hook 'ruby-mode-hook 'robe-start)
              ;; register jump implmentation
              (puthash 'ruby-mode 'robe-jump jump-map)
              ;; ruby code navigation
              (global-set-key (kbd "s-r r") 'inf-ruby)
              (global-set-key (kbd "s-r c") 'inf-ruby-console-auto)))

;; Web Mode
(config-pkg 'web-mode
            (lambda()
              (autoload 'web-mode "web-mode")
              (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))))

;; YAML
(config-pkg 'yaml-mode
            (lambda()
              (autoload 'yaml-mode "yaml-mode")
              (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
              (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))))

;; Rest client
(config-pkg 'restclient
            (lambda()
              (autoload 'restclient "restclient")
              (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
              (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))))

;; Apib-mode (major mode for editing API blueprint file)
(config-pkg 'apib-mode
            (lambda()
              (autoload 'apib-mode "apib-mode")
              (add-to-list 'auto-mode-alist '("\\.api\\'" . apib-mode))
              (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))
              (add-to-list 'auto-mode-alist '("\\.blueprint\\'" . apib-mode))))

;; json mode
(config-pkg 'json-mode
            (lambda()
              (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
              (add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonc-mode))
              (add-hook 'json-mode-hook
                        (lambda ()
                          (make-local-variable 'js-indent-level)
                          (setq js-indent-level 2)))))
