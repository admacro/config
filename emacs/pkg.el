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

;; (define-key xah-fly-key-map (kbd "x") 'robe-jump)

;; Programming Config
;; rbenv env setup
(setq rbenv-home (concat (getenv "HOME") "/.rbenv"))
(if (file-directory-p rbenv-home)
    (progn
      (setenv "PATH" 
	      (concat rbenv-home "/shims:" 
		      rbenv-home "/bin:"
		      (getenv "PATH")))
      (add-to-list 'exec-path (concat rbenv-home "/shims"))
      )
  )

;; Enhanced Ruby mode
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . enh-ruby-mode))
(setq enh-ruby-program (concat rbenv-home "/shims/ruby"))

;; rinari
(require 'rinari)

;; integrate robe
;; (require 'robe)
;; (add-hook 'enh-ruby-mode-hook 'robe-mode)

;; stop enh-ruby from adding utf-8 encoding
(setq enh-ruby-add-encoding-comment-on-save nil)

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) ; eRuby

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Rest client
;; (require 'restclient-mode)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
