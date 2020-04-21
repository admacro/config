;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; xah fly keys (default layout is dvorak)
(require 'xah-fly-keys)
(xah-fly-keys 1)


;; find file in repository
(global-set-key (kbd "<f7>") 'find-file-in-repository)


;; org-mode
(setq org-startup-indented t)

;; Programming
;; no magit coding comment
(setq ruby-insert-encoding-magic-comment nil)

;; hl-todo
(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#C40233")
        ("FIXME"  . "#C40233")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))

;; highlight numbers in most programming mode
(defun highlight-numbers-go-mode ()
  "symbol-start and symbol-end excludes dot (.) so numbers start or end with dots must be matched separately"
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
               (and (+ (any digit "_"))
                    (? ".")
                    (* (any digit "_"))
                    (? (and (any "eE")
                            (? (any "-+"))
                            (+ (any digit "_"))))
                    (? "i"))
               )
              symbol-end)
             ))
           highlight-numbers-modelist))
(add-hook 'highlight-numbers-mode-hook 'highlight-numbers-go-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; call different jump implementation in differernt major-mode
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive) ; global-set-key expects an interactive command
                  (cond
                   ((string-equal "go-mode" major-mode)
                    (lsp-find-definition))
                   ((string-equal "ruby-mode" major-mode)
                    (robe-jump)))))
(global-set-key (kbd "<f5>") 'pop-tag-mark) ; go back to previous jump mark


;; flycheck
;; (add-hook 'go-mode-hook #'flycheck-mode) ; auto configured by lsp-mode


;; lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/lsp-mode.html
;; enable language server integration with gopls
(require 'lsp-mode)
(add-hook 'go-mode-hook 'lsp-deferred)
(setq lsp-enable-links nil)
(setq lsp-enable-snippet nil)


;; lsp-treemacs
;; enable bidirectional synchronization of lsp workspace folders and treemacs projects
(lsp-treemacs-sync-mode 1)

;; Use cursor instead of fringe indicator in treemacs side window, and less width
(setq treemacs-show-cursor 1)
(setq treemacs-fringe-indicator-mode nil)
(setq treemacs-width 25)

;; no spacing betweewn root nodes (this is to fix *LSP Symbols List*)
(setq treemacs-space-between-root-nodes nil)

;; hide line numbers in treemacs
(add-hook `treemacs-mode-hook (lambda ()
                                (display-line-numbers-mode 0)))

;; keymap for lsp-treemacs
(define-key xah-fly-dot-keymap (kbd "t") 'treemacs)
(define-key xah-fly-dot-keymap (kbd "s") 'lsp-treemacs-symbols)


;; company
(setq company-require-match nil)
(setq company-tooltip-align-annotations t)  ; Align annotation to the right side.
(setq company-minimum-prefix-length 1)

;; company-lsp
(require 'company-lsp)
(push 'company-lsp company-backends)

;; go-mode
;; fontify only function declarations, not function calls
(setq go-fontify-function-calls nil)

;; format code and reorganize imports before saving buffer
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook 'lsp-go-install-save-hooks)

;; show eldoc
(add-hook 'go-mode-hook 'go-eldoc-setup)


;; Ruby
;; rbenv
;; integrate robe
;; (require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (setq inf-ruby-console-environment "development")
;; (add-hook 'ruby-mode-hook 'robe-start)

;; ruby code navigation
;; (global-set-key (kbd "s-r r") 'inf-ruby)
;; (global-set-key (kbd "s-r c") 'inf-ruby-console-auto)


;; Web Mode
(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)) ; html
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) ; eRuby

;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))


;; Rest client
(autoload 'restclient "restclient")
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))


;; Apib-mode (major mode for editing API blueprint file)
(autoload 'apib-mode "apib-mode")
(add-to-list 'auto-mode-alist '("\\.api\\'" . apib-mode))
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))
(add-to-list 'auto-mode-alist '("\\.blueprint\\'" . apib-mode))


;; Sh Mode
(add-to-list 'auto-mode-alist '("\\Procfile.*\\'" . sh-mode)) ; forego foreman/procfile

;; json mode
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
