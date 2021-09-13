(defun fontspec (family size)
  (format "%s-%s" family size))

(defun set-default-font ()
  (set-font-size default-font-size))

(defun set-font-size (size)
  (let ((mono-font (fontspec monospaced-font-family size))
        (prop-font (fontspec proportional-font-family size)))
    ;; set default font for all frames
    (add-to-list 'default-frame-alist `(font . ,mono-font))
    ;; override font lock faces (must be executed after theme has loaded)
    (set-face-font 'font-lock-comment-face prop-font)
    (set-face-font 'font-lock-doc-face prop-font)
    (set-face-font 'variable-pitch prop-font)))

(defun cycle-font-size()
  "Cycle font size in this order: 14 -> 17 -> 21 -> 14..."
  (interactive)
  (let ((font-size-list '(14 17 21))
        (current-size-index (get 'current-size-index 'state))
        (current-font-size (/ (face-attribute 'default :height) 10)))
    (cl-flet ((set-fonts (font-size-index)
                         (let ((font-size (nth font-size-index font-size-list)))
                           (set-font-size font-size)
                           (put 'current-size-index 'state font-size-index))))
	  (cond
	   ((or (equal current-size-index 1) ) (set-fonts 2))
	   ((equal current-size-index 2) (set-fonts 0))
	   ((equal current-size-index 0) (set-fonts 1))
	   ((equal current-size-index nil)
	    (set-fonts (cl-position current-font-size font-size-list :test '<)))
	   ))))

;; default font families and size
(setq proportional-font-family "SF Pro Text")
(setq monospaced-font-family "SF Mono")
(setq default-font-size 14)

(set-default-font)

;; set font for chinese characters
(set-fontset-font
 t
 'han
 (if (member "Songti SC" (font-family-list)) "Songti SC"))
