;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; xah fly keys (default layout is dvorak)
(require 'xah-fly-keys)
(xah-fly-keys 1)


;; find file in repository
(global-set-key (kbd "<f7>") 'find-file-in-repository)


;; Programming
;; no magit coding comment
(setq ruby-insert-encoding-magic-comment nil)

;; highlight numbers in most programming mode
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


;; company-lsp
(require 'company-lsp)
(push 'company-lsp company-backends)


;; golang
;; simplify code while formatting
;; (setq gofmt-args '("-s"))

;; format Go code and organize imports
;; see https://github.com/dominikh/go-mode.el/issues/340

;; the goimports approach
;; (setq gofmt-command "goimports") ; goimports also does code formatting
;; (add-hook 'before-save-hook 'gofmt-before-save)

;; the lsp approach
(add-hook 'before-save-hook
          (lambda ()
            (lsp-format-buffer)
            (lsp-organize-imports)))

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
