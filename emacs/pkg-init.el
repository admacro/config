;; MELPA
(require 'package)
(add-to-list 'package-archives
	           '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Refresh package list to include melpa packages
;; Otherwise package-install cannot find melpa packages
(package-refresh-contents)

;; Packages for basic editing, file navigation, search, etc.
(setq package-list-essential
      '(xah-fly-keys
        magit
        xah-find
        find-file-in-repository
        smex
        markdown-mode))

;; Packages for Ruby and Rails development
(setq package-list-rails
      '(robe
        web-mode
        yaml-mode
        json-mode
        restclient))

;; Packages for Go programming
(setq package-list-go
      '(go-mode
        go-playground))

(defun install-packages(package-list)
  "Install missing packages in the package-list"
  (message "=====================================")
  (message "=== Start package installation ===")
  (setq missing-package-list (list))
  (dolist (package package-list)
    (if (package-installed-p package)
        (message "✔ [%s] is installed" package)
      (progn
        (setq missing-package-list
              (append missing-package-list (list package)))
        (message "✘ [%s] is not installed" package)
        (message "> Start installing %s" package)
        (package-install package)
        (message "> End installing %s" package)
        )
      ))
  (if (eq nil missing-package-list)
      (message "No missing packages")
    (message "Packages installed: %S" missing-package-list))
  )

(install-packages package-list-essential)
(install-packages package-list-rails)
(install-packages package-list-go)