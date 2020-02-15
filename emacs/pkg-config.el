;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; xah fly keys (default layout is dvorak)
(require 'xah-fly-keys)
(xah-fly-keys 1)


;; find file in repository
(global-set-key (kbd "<f7>") 'find-file-in-repository)


;; Programming Config
;; no magit coding comment
(setq ruby-insert-encoding-magic-comment nil)


;; golang env setup
;; simplify code while formatting
(setq gofmt-args '("-s"))

;; format Go code with gofmt before saving
;; (defun go-tidy-up ()
  ;; (gofmt-before-save)
  ;; (go-remove-unused-imports))
;; (add-hook 'before-save-hook 'go-tidy-up)
(add-hook 'before-save-hook 'gofmt-before-save)

;; lsp-mode
;; enable language server integration with gopls
(require 'lsp-mode)
(add-hook 'go-mode-hook 'lsp-deferred)


;; rbenv env setup
;; integrate robe
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(setq inf-ruby-console-environment "development")
(add-hook 'ruby-mode-hook 'robe-start)

;; ruby code navigation
(global-set-key (kbd "s-r r") 'inf-ruby)
(global-set-key (kbd "s-r c") 'inf-ruby-console-auto)
(global-set-key (kbd "<f6>") 'robe-jump)
(global-set-key (kbd "<f5>") 'pop-tag-mark)


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
