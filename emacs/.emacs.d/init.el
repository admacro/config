;;_UTF-8_as_default_encoding
(set-language-environment "UTF-8")

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
(setenv "PATH" 
	(concat rbenv-home "/shims:" 
		rbenv-home "/bin:"
		(getenv "PATH")))
(add-to-list 'exec-path (concat rbenv-home "/shims"))

;; Enhanced Ruby mode
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . enh-ruby-mode))
(setq enh-ruby-program (concat rbenv-home "/shims/ruby"))

;; integrate robe and company
(require 'robe)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
;; stop enh-ruby from adding utf-8 encoding
(setq enh-ruby-add-encoding-comment-on-save nil)

;; (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)


;; (global-company-mode t)
;; (push 'company-robe company-backends)
;; (add-hook 'ruby-mode-hook 'ruby-electric-mode)

;; rbenv
;; (setq rbenv-home (concat (getenv "HOME") "~/.rbenv"))
;; (require 'rbenv)
;; (global-rbenv-mode)
;; (rbenv-use-global) ;; will activate global ruby
;; (rbenv-use "2.2.1") ;; allows you to choose what ruby version you want to use
;; (rbenv-use-corresponding) ;; searches for .ruby-version and activates the corresponding ruby
;; (setq rbenv-modeline-function 'rbenv--modeline-plain) ;; this will remove the colors


;; projectile rails
;; (projectile-global-mode)
;; (projectile-rails-global-mode)
;; (setq projectile-keymap-prefix (kbd ""))


;; Emacs GC config for flx-ido 
;; https://github.com/lewang/flx
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) ; eRuby

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Rest client
;; (require 'restclient-mode)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; Global config
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq column-number-mode t) ; display column number in the mode line
(global-auto-revert-mode t) ; auto refresh files

;; Setup font
(cond ((string-equal system-type "windows-nt") ; Windows
       (set-default-font "Consolas-11"))
      ((string-equal system-type "darwin") ; Mac
       (set-default-font "Inconsolata-16"))
      ;;      ((String-equal system-type "gnu/linux") ; Linux
      ;;     (set-default-font "Inconsolata-16"))
      )

;; Set default window size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 155) ; chars
              (height . 45) ; lines
	      ;; (background-color . "WhiteSmoke")
              ))

      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 150)
              (height . 40)
	      ;; (background-color . "WhiteSmoke")
              )))
  )

;; Set cursor type
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; turn on highlighting current line
(global-hl-line-mode 1)

(setq show-paren-delay 0)
(show-paren-mode 1)

(electric-pair-mode 1)


;; enable downcase command
(put 'downcase-region 'disabled nil)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("41f90b83fae6e57d37617a9998424cb78fa064fc79706442e677201084ee181d" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "db73e672b32ebfe06b8bee3ca51769a56688fc6a7baed75bf88ef8ebf091c797" "eae43024404a1e3c4ea853a9cf7f6f2ad5f091d17234ec3478a23591f25802eb" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "778ccbc6e871da902e4761243a4cb73cd77a007be6841308e459a9f722a7c9e2" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" "242527ce24b140d304381952aa7a081179a9848d734446d913ca8ef0af3cef21" "9fcac3986e3550baac55dc6175195a4c7537e8aa082043dcbe3f93f548a3a1e0" "a4df5d4a4c343b2712a8ed16bc1488807cd71b25e3108e648d4a26b02bc990b3" "f5516b1e20aa2c9edc3d4ed151702a48a574d8aab91ab38721714e1d4a25688d" "1473f4ea26c61b23bc0931088503ba339d4ae0d9ba126cd9001df208f1dfa85e" "f0c817a3706ac81717ac86feb8c17c649fae70a6d49cb639aebfb28409772eb3" "1e918d6e3fb6ecf1f5860a84f30028a5352700e640ea40553c8f32d0d4b7a91f" "94a84f52916d89bcfc8df7900a2fbb9ea6f555ced94edfc6af3f43adfbd830e3" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (graphql-mode dracula-theme restclient xah-find eink-theme eziam-theme faff-theme grayscale-theme greymatters-theme nord-theme nordless-theme northcode-theme ruby-electric robe find-file-in-repository inf-ruby enh-ruby-mode xah-fly-keys magit web-mode markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Check if time is between 6 PM and 6 AM, aka night time
(defun nightp ()
  (set 'hour-str 
       (car (split-string
	     (nth 3
		  (split-string (current-time-string)))
	     ":")))
  (set 'hour (string-to-number hour-str))
  (message hour-str)
  (or (> hour 16) (< hour 6)))

;; use dark theme for night and light theme for day
(if (nightp)
    (progn
      (message "It's night. Loading dark theme")  
      (load-theme 'dracula))
  (progn
    (message "It's day. Loading light theme")  
    (load-theme 'faff)))


(progn
  ;; make buffer switch command do suggestions, also for find-file command
  (require 'ido)
  (ido-mode 1)
  ;; show choices vertically
  (if (version< emacs-version "25")
      (progn
        (make-local-variable 'ido-separator)
        (setq ido-separator "\n"))
    (progn
      (make-local-variable 'ido-decorations)
      (setf (nth 2 ido-decorations) "\n")))

  ;; enable ido everywhere
  (setq ido-everywhere t)
  ;; show any name that has the chars you typed
  (setq ido-enable-flex-matching t)
  ;; use current pane for newly opened file
  (setq ido-default-file-method 'selected-window)
  ;; use current pane for newly switched buffer
  (setq ido-default-buffer-method 'selected-window)
  ;; stop ido from suggesting when naming new file
  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
)

;; big minibuffer height, for ido to show choices vertically
(setq max-mini-window-height 0.5)

;; (defun xah-html-decode-percent-encoded-url ()
;;   "Decode percent encoded URI of URI under cursor or selection.

;; Example:
;;     http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_%28D%C3%BCrer%29
;; becomes
;;     http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)
    
;; Example:
;;     http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
;; becomes
;;     http://zh.wikipedia.org/wiki/文本编辑器
    
;; For string version, see `xah-html-url-percent-decode-string'.
;; To encode, see `xah-html-encode-percent-encoded-url'.
;; URL `http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html'
;; Version 2015-09-14."
;;   (interactive)
;;   (let ($boundaries $p1 $p2 $input-str)
;;     (if (use-region-p)
;;         (progn
;;           (setq $p1 (region-beginning))
;;           (setq $p2 (region-end)))
;;       (progn
;;         (setq $boundaries (bounds-of-thing-at-point 'url))
;;         (setq $p1 (car $boundaries))
;;         (setq $p2 (cdr $boundaries))))
;;     (setq $input-str (buffer-substring-no-properties $p1 $p2))
;;     (require 'url-util)
;;     (delete-region $p1 $p2)
;;     (insert (decode-coding-string (url-unhex-string $input-str) 'utf-8))))
