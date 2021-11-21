(defun get-full-path(relative-path)
  "http://ergoemacs.org/emacs/organize_your_dot_emacs.html"
  (concat (file-name-directory (or load-file-name buffer-file-name))
          relative-path))

(load (get-full-path "xfk"))
(load (get-full-path "basic"))
(load (get-full-path "theme/theme"))
(load (get-full-path "font"))
(load (get-full-path "pkg/pkg"))
(load (get-full-path "org/org"))
(load (get-full-path "abbrevs"))
(load (get-full-path "misc"))
(load (get-full-path "xfk-config"))
