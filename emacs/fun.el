;; fun.el
;; fun is fun in `defun'
;; all custom functions reside here

(defun random-font ()
  "Random-font returs one of these good fonts randomly.
  The following font sizes all have the same width 10.
  `go mono-16' and `gabriele bad ah-16' have the same H/W ratio of 1.8.
  `calling code-18' and `pointfree-16` have the same H/W ratio of 2.2."
  (let ((font-list (list "go mono-16"
                         "gabriele bad ah-16"
                         "calling code-18"
                         "pointfree-16")))
    (nth (random (length font-list)) font-list)))

(defun load-adm-theme ()
  "Use dark theme for night and light theme for day.
Day starts from 6AM to 6PM. The rest is night. :D"
  (let* ((hour-str
	  (car (split-string
		(nth 3 (split-string (current-time-string)))
		":")))
	 (hour (string-to-number hour-str)))
    (if (or (>= hour 18) (<= hour 6))
	(load-theme 'adm-dark t)
      (load-theme 'adm-light t))))

(defun window-margin-toggle()
  "Toggle the left and right margins of current window.
Always cycle the margin width in this order: 20 cells, 0 (no margin)."
  (interactive)
  (if (get 'window-margin-enabled 'state)
      (progn
        (set-window-margins nil 0 0)
        (put 'window-margin-enabled 'state nil))
    (progn
        (set-window-margins nil 20 20)
        (put 'window-margin-enabled 'state t))))

;; hex colour
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
  "toggle between colour"
  (interactive)
  (if (eq hexcolour-keywords hexcolour-keywords-light)
      (setq hexcolour-keywords hexcolour-keywords-dark)
    (setq hexcolour-keywords hexcolour-keywords-light))
  (revert-buffer t t))

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
