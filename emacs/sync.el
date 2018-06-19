(package-initialize)
(setq package-list '(heml-mode web-mode enh-ruby-mode))

(defun emacs-sync()
  "synchronize packages and config"
  (dolist (package package-list)
    (if (package-installed-p package)
	(message "%s is installed" package)
      (message "%s is not installed" package)
      ))
  )

(emacs-sync)

(defun test-package(package)
  (if (package-installed-p package)
      (message "%s is installed" package)
    (message "%s is not installed" package)
    )
  )

(test-package 'web-mode)
(test-package 'test-mode)
