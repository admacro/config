(defalias 'perl-mode 'cperl-mode)

;; Setup font
(cond ((string-equal system-type "windows-nt") ; Windows
       (set-default-font "Consolas-11"))
      ((string-equal system-type "darwin") ; Mac
       (set-default-font "Inconsolata-16"))
;;      ((string-equal system-type "gnu/linux") ; Linux
;;     (set-default-font "Inconsolata-16"))
      )

;; Set default window size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 120) ; chars
              (height . 50) ; lines
	      (background-color . "white smoke")
              ))

      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 120)
              (height . 50)
     	      (background-color . "white smoke")
              ;;
              )))
  )

;;
