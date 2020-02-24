;; custom functions

;; theme
;; good font sizes for "Go Mono": 7,8,9,12,14,17,19,22,24,25 (H/W ratio: 2)
;; refer to font-size.md for ratios of other sizes
(defun light-theme ()
  "adm-light-theme"
  (defalias 'sfa 'set-face-attribute)
  (sfa 'font-lock-function-name-face nil :foreground "#833471")
  (sfa 'font-lock-variable-name-face nil :foreground "#2c2c54")
  (sfa 'default nil :foreground "#2c2c54" :background "#ece4d0")
  (sfa 'font-lock-type-face nil :foreground "#833471")
  (sfa 'font-lock-keyword-face nil :foreground "#474787" :weight 'bold)
  (sfa 'font-lock-constant-face nil :foreground "#006266" :slant 'italic)
  (sfa 'font-lock-comment-face nil :foreground "#bf6139" :background nil)
  (sfa 'font-lock-builtin-face nil :foreground "#474787" :weight 'bold)
  (sfa 'font-lock-string-face nil :foreground "#006266")
  (sfa 'fringe nil :background "#ECE4D0")
  (sfa 'vertical-border nil :foreground "gray50")
  )

(defun adm-default-theme ()
  (set-default-font "Go Mono-14")
  (load-theme 'adm-light-theme t)
  ;; this will overwrite background in colour theme
  ;; other color options: #EFE6E0, #F1EAE2/#F4EEE6/#F8F3EB, #EBE3E1/#F8F5F4
  ;; old paper colors: #E5D8B2, #ECE4D0
  ;; (set-background-color "#ECE4D0")
  )
(adm-default-theme)

;; Check if everybody is sleeping except me (from 0 to 5 AM)
(defun nightp ()
  (set 'hour-str
       (car (split-string
             (nth 3
                  (split-string (current-time-string)))
             ":")))
  (set 'hour (string-to-number hour-str))
  (message hour-str)
  (or (> hour 23) (< hour 5)))

(defun random-theme ()
  (let ((themes-list (list 'leuven 'tsdh-light nil)))
    (nth (random (length themes-list)) themes-list)))

(defun load-random-theme ()
  (interactive)
  (let ((theme (random-theme)))
    (if (not (equal theme nil))
        (progn
          (message (concat "Loading theme " (symbol-name theme)))
          (load-theme theme)
          ))))

;; 2019-12-09
;; Theming is fun, when you are at it. But today I think I'm done
;; with it after fiddlling with it now and then for the past few years.
;; Thus, end it for now.
;; use dark theme for night and some random light theme for day
;; (if (nightp)
;;     (progn
;;       (message "It's night. Loading dark theme")
;;       (load-theme 'deeper-blue))
;;   (progn
;;     (message "It's day. Loading light theme")
;;     (load-random-theme)))

;; Use variable width font faces in current buffer
(defun writing-mode ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Gabriele Dark Ribbon FG" :height 188 :width regular))
  (buffer-face-mode)
  (global-display-line-numbers-mode 0)	; disable line numbers everywhere
  (turn-on-auto-fill)
  (load-theme 'leuven)
  )
(define-key xah-fly-dot-keymap (kbd "w") 'writing-mode)

;; unhighlight all hi-lock highlights in current buffer
(defun unhighlight-all-in-buffer()
  "Remove all highlights made by `hi-lock' from the current buffer."
  (interactive)
  (unhighlight-regexp t))
(define-key xah-fly-dot-keymap (kbd "u") 'unhighlight-all-in-buffer)

;; show changes made to current git repo (magit-status)
(define-key xah-fly-dot-keymap (kbd "c") 'magit-status)

;; find file in repository
(define-key xah-fly-dot-keymap (kbd ".") 'find-file-in-repository)

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2018-12-23"
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal $sort-by "name") (setq $arg "-Al "))
     ((equal $sort-by "date") (setq $arg "-Al -t"))
     ((equal $sort-by "size") (setq $arg "-Al -S"))
     (t (error "logic error 09535" )))
    (dired-sort-other $arg )))
(require 'dired )
(define-key dired-mode-map (kbd "s") 'xah-dired-sort)
