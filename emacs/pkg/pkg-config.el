(defun config-package(packages function)
  "config-package configures the packages passed in by
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

(defun lambda-key (keymap key def)
  "Wrap`define-key' to provide documentation."
  (set 'sym (make-symbol (documentation def)))
  (fset sym def)
  (define-key keymap key sym))

;; function aliases
(defalias 'cp 'config-package)
(defalias 'cpd 'config-package-with-dependencies)
(defalias 'dk 'define-key)
(defalias 'lk 'lambda-key)
(defalias 'gsk 'global-set-key)
(defalias 'atl 'add-to-list)

(load "~/prog/config/emacs/pkg/pkg-config-builtin.el")
(load "~/prog/config/emacs/pkg/pkg-config-melpa.el")
