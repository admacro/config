;; custom functions

;; Appearance
;; good font sizes for "Go Mono": 7,8,9,12,14,17,19,22,24,25 (H/W ratio: 2)
;; refer to font-size.md for ratios of other sizes
(defun adm-default-theme ()
  (set-default-font "Go Mono-17")
  (load-theme 'adm-light t))
(adm-default-theme)


;; Display hex colour code in its corresponding colour
;; https://www.emacswiki.org/emacs/HexColour
(defvar hexcolour-keywords-light
  '(("#[[:xdigit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :foreground 
                                       (match-string-no-properties 0)
                                       :background "white"
                                       ;; :box "black"
                                       ))))))
(defvar hexcolour-keywords-dark
  '(("#[[:xdigit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background 
                                       (match-string-no-properties 0)
                                       :foreground "black"
                                       ;; :box "black"
                                       ))))))

(defvar hexcolour-keywords hexcolour-keywords-light)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil hexcolour-keywords)))

(defun hexcolour-toggle-light-or-dark ()
  (interactive)
  (if (eq hexcolour-keywords hexcolour-keywords-light)
      (setq hexcolour-keywords hexcolour-keywords-dark)
    (setq hexcolour-keywords hexcolour-keywords-light))
  (revert-buffer t t))


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
