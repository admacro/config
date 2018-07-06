;; MELPA Stable
(require 'package)
(add-to-list 'package-archives
	           '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)


;; xah fly keys (default layout is dvorak)
(require 'xah-fly-keys)
(xah-fly-keys 1)


;; find file in repository
(global-set-key (kbd "<f7>") 'find-file-in-repository)


;; Programming Config
;; rbenv env setup
(setq rbenv-home (concat (getenv "HOME") "/.rbenv"))
(if (file-directory-p rbenv-home)
    (progn
      (setenv "PATH" 
	            (concat rbenv-home "/shims:" 
		                  rbenv-home "/bin:"
		                  (getenv "PATH")))
      (add-to-list 'exec-path (concat rbenv-home "/shims"))))


;; integrate robe
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(setq inf-ruby-console-environment "development")
(add-hook 'ruby-mode-hook 'inf-ruby-console-auto)

;; support common ruby script file formats
;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))


;; start an irb process
(global-set-key (kbd "s-r r") 'inf-ruby)
(global-set-key (kbd "s-r c") 'inf-ruby-console-auto)
(global-set-key (kbd "<f6>") 'robe-jump)
(global-set-key (kbd "<f5>") 'pop-tag-mark)

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) ; eRuby


;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))


;; Rest client
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))
