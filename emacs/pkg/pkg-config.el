(defun config-pkg(packages function)
  "config-pkg configures the packages passed in by
calling the function passed in"
  (if (symbolp packages) (setq packages (list packages)))
  (catch 'nodep
    (message "Configuring packages [%s]" packages)
    (dolist (package packages)
      (if (not (package-installed-p package))
          (progn
            (message "\t> Package [%s] is not installed." package)
            (throw 'nodep t))))
    (funcall function)))

(load (get-full-path "pkg-config-builtin"))
(load (get-full-path "pkg-config-melpa"))
