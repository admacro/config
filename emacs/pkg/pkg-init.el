;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Packages for basic editing, file navigation, search, etc.
(setq package-list-essential
      '(xah-fly-keys
        smex
        xah-find
        find-file-in-repository
        markdown-mode
        magit))

;; Packages for programming
(setq package-list-prog
      '(exec-path-from-shell
        highlight-numbers
        hl-todo
        flycheck
        lsp-mode
        company
        lsp-treemacs
        htmlize
        restclient))

;; Packages for Java programming
(setq package-list-java
      '(lsp-java))

;; Packages for Go programming
(setq package-list-go
      '(go-mode
        gotest
        go-playground
        go-eldoc
        ob-go))

;; Packages for extra language support
(setq package-list-extra-lang
      '(yaml-mode
        json-mode))

;; Packages for Web development
(setq package-list-web
      '(web-mode))

;; Packages for Ruby and Rails development
(setq package-list-ruby
      '(robe))

;; Package category name to package list mapping
;; Comment or uncomment an entry to uninstall or install a category
(setq pkgHash (make-hash-table :test 'equal))
(puthash "Essential" package-list-essential pkgHash)
(puthash "Programming" package-list-prog pkgHash)
(puthash "Java" package-list-java pkgHash)
(puthash "Go" package-list-go pkgHash)
(puthash "Extra Language Support" package-list-extra-lang pkgHash)
;; (puthash "Web Development" package-list-web pkgHash)
;; (puthash "Ruby & Rails" package-list-ruby pkgHash)

(defun missing-packages ()
  "Return a list of packages that are not installed"
  (message "=====================================")
  (message "=== Check package installation ===")
  (let ((missing-package-list (list)))
    (maphash
     (lambda (category package-list)
       (message "=== Category: [%s] ===" category)
       (dolist (package package-list)
         (if (package-installed-p package)
             (message "✔ [%s] is installed" package)
           (progn
             (message "✘ [%s] is not installed" package)
             (setq missing-package-list
                   (append missing-package-list (list package)))))))
     pkgHash)
    missing-package-list))

(defun init-packages ()
  "Check package installation and install missing ones according to the configured package list"
  (let ((new-packages (missing-packages)))
    (if new-packages
        (progn
          ;; Refresh package list to include melpa packages
          ;; Otherwise package-install cannot find melpa packages
          (message "Refreshing package content...")
          (package-refresh-contents)
          (dolist (package new-packages)
            (message "> Start installing %s -----" package)
            (package-install package)
            (message "> End installing %s -----" package)))
      (message "No missing packages"))))

(init-packages)
