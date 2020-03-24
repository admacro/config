;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Packages for basic editing, file navigation, search, etc.
(setq package-list-essential
      '(xah-fly-keys
        smex
        magit
        xah-find
        find-file-in-repository
        markdown-mode))

;; Packages for programming
(setq package-list-prog
      '(exec-path-from-shell
        highlight-numbers
        hl-todo
        lsp-mode
        flycheck
        company-lsp
        lsp-treemacs
        htmlize))

;; Packages for Go programming
(setq package-list-go
      '(go-mode
        go-playground
        flycheck-golangci-lint
        go-eldoc
        ob-go))

;; Packages for Ruby and Rails development
(setq package-list-rails
      '(robe
        web-mode
        yaml-mode
        json-mode
        restclient))

;; On launch, melpa packages are not included in the package search list
(setq package-contents-refreshed nil)

(defun install-packages(package-list category)
  "Install missing packages in the package-list"
  (message "=====================================")
  (message "=== Check package installation ===")
  (message "=== Category: [%s] ===" category)
  (setq missing-package-list (list))
  (dolist (package package-list)
    (if (package-installed-p package)
        (message "✔ [%s] is installed" package)
      (progn
        (if (not package-contents-refreshed)
            ;; Refresh package list to include melpa packages
            ;; Otherwise package-install cannot find melpa packages
            ;; Performe only once
            (progn
              (package-refresh-contents)
              (setq package-contents-refreshed t)
              ))
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

(install-packages package-list-essential "Essential")
(install-packages package-list-prog "Programming")
(install-packages package-list-go "Go")
(install-packages package-list-rails "Ruby & Rails")

;; (setq package-list-test '(xah-fly-keys))
;; (install-packages package-list-test "test")
;; (require 'xah-fly-keys)
;; (xah-fly-keys 1)

