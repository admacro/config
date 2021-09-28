(load "~/prog/config/emacs/theme/adm-theme.el")
(load "~/prog/config/emacs/theme/adm-light-theme.el")
(load "~/prog/config/emacs/theme/adm-dark-theme.el")

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

(load-theme 'adm t)
