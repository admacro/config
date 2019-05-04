;; custom functions

;; Check if time is between 6 PM and 6 AM, aka night time
(defun nightp ()
  (set 'hour-str
       (car (split-string
	           (nth 3
		              (split-string (current-time-string)))
	           ":")))
  (set 'hour (string-to-number hour-str))
  (message hour-str)
  (or (> hour 21) (< hour 5)))

;; use dark theme for night and light theme for day
(if (nightp)
    (progn
      (message "It's night. Loading dark theme")
      (load-theme 'deeper-blue))
  (progn
    (message "It's day. Loading light theme")
    (load-theme 'leuven)))

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
