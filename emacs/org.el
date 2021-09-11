;; org-mode
(require 'ox-publish)

(setq org-project-notes-name "notes")
(setq org-project-notes-path "~/prog/notes")
(setq org-project-www-name "www")
(setq org-project-www-path "~/prog/admacro.github.io")

(setq org-startup-indented t)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-html-preamble nil)
(setq org-html-postamble t)
(setq org-html-postamble-format '(("en" "<p>By %a | Date: %d | Last Updated: %C</p><p>© 2021 James Ni</p>")))
(setq org-descriptive-links nil)
(setq org-publish-project-alist
      (list (list "notes"
                  :base-directory (concat org-project-notes-path "/org/")
                  :base-extension "org"
                  :publishing-directory org-project-notes-path
                  :recursive t
                  :publishing-function 'org-html-publish-to-html
                  :section-numbers nil)
            (list "www"
                  :base-directory (concat org-project-www-path "/org/")
                  :base-extension "org"
                  :publishing-directory org-project-www-path
                  :publishing-function 'org-html-publish-to-html
                  :recursive t
                  :section-numbers nil)
            (list org-project-notes-name :components '("notes"))
            (list org-project-www-name :components '("www"))
            ))
