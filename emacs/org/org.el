;; org-mode
(setq org-startup-indented t)
(setq org-descriptive-links nil)
(add-hook 'org-mode-hook 'visual-line-mode)

;; org publish
(require 'ox-publish)
(defun config-project(name src-path &optional pub-path)
  (setq org-html-preamble nil)
  (setq org-html-postamble t)
  (setq org-html-postamble-format
        '(("en" "<p>By %a | Date: %d | Last Updated: %C</p><p>© 2022 %a</p>")))
  (setq org-publish-project-alist
        (list (list name
                    :base-directory (concat src-path "/org/")
				    :base-extension "org"
				    :publishing-directory (if pub-path pub-path src-path)
				    :recursive t
				    :publishing-function 'org-html-publish-to-html
				    :section-numbers nil)
              )))

(defun publish-project(name src-path &optional pub-path)
  "Configure and publish org project with name and path. This can overwrite existing html files."
  (config-project name src-path pub-path)
  (if (y-or-n-p "Force republish all? ")
      (org-publish name t)
    (org-publish name)))

(defun publish-notes()
  (interactive)
  (publish-project "notes" "~/prog/notes" "~/prog/notes/html"))

(defun publish-www()
  (interactive)
  (publish-project "www" "~/prog/blog"))
