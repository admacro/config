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
;; (add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Enhanced Ruby mode
;; (setq enh-ruby-program (concat rbenv-home "/shims/ruby")) ; use ruby from rbenv
;; (setq enh-ruby-add-encoding-comment-on-save nil) ; stop enh-ruby from adding utf-8 encoding
;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("Rakefile" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . enh-ruby-mode))


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
