;; org-mode
(require 'ox-publish)

(setq org-startup-indented t)
(add-hook 'org-mode-hook 'visual-line-mode)


(setq org-descriptive-links nil)

(defun config-project(name path)
  (setq org-html-preamble nil)
  (setq org-html-postamble t)
  (setq org-html-postamble-format
        '(("en" "<p>By %a | Date: %d | Last Updated: %C</p><p>© 2022 James Ni</p>")))
  (setq org-publish-project-alist
        (list (list name
                    :base-directory (concat path "/org/")
				    :base-extension "org"
				    :publishing-directory path
				    :recursive t
				    :publishing-function 'org-html-publish-to-html
				    :section-numbers nil)
			  (list name :components (quote name))
              )))

(defun org-publish-by-name(org-project-name)
  "Publish org by project name. This can overwrite existing html files."
  (if (y-or-n-p "Force republish all? ")
      (org-publish org-project-name t)
    (org-publish org-project-name)))

(defun publish-org-project(name path)
  (config-project name path)
  (org-publish-by-name name))

(defun publish-notes()
  (interactive)
  (publish-org-project "notes" "~/prog/notes"))

(defun publish-www()
  (interactive)
  (publish-org-project "www" "~/prog/admacro.github.io"))
