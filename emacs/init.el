(defun get-full-path(relative-path)
  "http://ergoemacs.org/emacs/organize_your_dot_emacs.html"
  (concat (file-name-directory (or load-file-name buffer-file-name))
          relative-path)
  )
(defalias 'fp 'get-full-path)

(load (fp "basic"))
(load (fp "theme/theme"))
(load (fp "font"))
(load (fp "pkg/pkg"))
(load (fp "org/org"))
(load (fp "abbrevs"))
(load (fp "misc"))
