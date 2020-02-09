;;_UTF-8_as_default_encoding
(set-language-environment "UTF-8")


;; Set cursor type
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

(global-hl-line-mode 1) ; highlight current line
(electric-pair-mode 1) ; auto insert matching paren
(global-display-line-numbers-mode 1) ; display line numbers
(global-auto-revert-mode t) ; auto refresh files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq column-number-mode t) ; display column number in the mode line

;; show matching paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;; enable downcase command
(put 'downcase-region 'disabled nil)


;; Emacs GC config for flx-ido 
;; https://github.com/lewang/flx
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; load custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


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




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "68b7d8301bf8121abb8a92bbe7c247fbc3e64a0adfdda534daefd18f18c44a55" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "1012cf33e0152751078e9529a915da52ec742dabf22143530e86451ae8378c1a" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" "3a69621a68c2d3550a4c777ffc000e1ea66f5bc2f61112814c591e1bda3f5704" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "cde05ed51346d6925d29311fb131511115ae7612764297077ca1b61371e6b047" "c30d153e623dfe32184857790a0cad243b979e8b1104e057c4a6ffe2210856f7" "08f5da7e1f5064a2917af94f0dab946adfb25665b25450168ded749ec78a1145" "fc524ddf651fe71096d0012b1c34d08e3f20b20fb1e1b972de4d990b2e793339" "4455435a66dba6e81d55a843c9c7e475a7a935271bf63a1dfe9f01ed2a4d7572" "f9567e839389f2f0a1ede73d1c3e3bd2c9ed93adaf6bb7d13e579ea2b15fcef8" "b7d967c53f4e3dfc1f847824ffa3f902de44d3a99b12ea110e0ec2fcec24501d" "c4d3cbd4f404508849e4e902ede83a4cb267f8dff527da3e42b8103ec8482008" "9076ed00a3413143191cb9324d9426df38d83fb6dba595afbd43983db1015ef4" "b8bb8a91752c68df1de3b790fe5bbe5e39441488d19851654ee0d2875bc6f94b" "f72ccaa311763cb943de5f9f56a0d53b0009b772f4d05f47835aa08011797aa8" "7e362b29da8aa9447b51c2b354d8df439db33b3612ddd5baa34ad3de32206d83" "0e33022384e4db1374827f51e3d9e9a2d56282c2e3568c22f1c12ad80e20cf0c" "41f90b83fae6e57d37617a9998424cb78fa064fc79706442e677201084ee181d" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "db73e672b32ebfe06b8bee3ca51769a56688fc6a7baed75bf88ef8ebf091c797" "eae43024404a1e3c4ea853a9cf7f6f2ad5f091d17234ec3478a23591f25802eb" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "778ccbc6e871da902e4761243a4cb73cd77a007be6841308e459a9f722a7c9e2" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" "242527ce24b140d304381952aa7a081179a9848d734446d913ca8ef0af3cef21" "9fcac3986e3550baac55dc6175195a4c7537e8aa082043dcbe3f93f548a3a1e0" "a4df5d4a4c343b2712a8ed16bc1488807cd71b25e3108e648d4a26b02bc990b3" "f5516b1e20aa2c9edc3d4ed151702a48a574d8aab91ab38721714e1d4a25688d" "1473f4ea26c61b23bc0931088503ba339d4ae0d9ba126cd9001df208f1dfa85e" "f0c817a3706ac81717ac86feb8c17c649fae70a6d49cb639aebfb28409772eb3" "1e918d6e3fb6ecf1f5860a84f30028a5352700e640ea40553c8f32d0d4b7a91f" "94a84f52916d89bcfc8df7900a2fbb9ea6f555ced94edfc6af3f43adfbd830e3" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (pastelmac-theme soft-morning-theme soft-stone-theme sublime-themes anti-zenburn-theme autumn-light-theme graphql-mode dracula-theme restclient xah-find faff-theme ruby-electric robe find-file-in-repository inf-ruby enh-ruby-mode xah-fly-keys magit web-mode markdown-mode))))
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
      (load-theme 'brin))
  (progn
    (message "It's day. Loading light theme")  
    (load-theme 'snow)))


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
