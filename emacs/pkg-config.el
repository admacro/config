(defun config-package(packages function)
  "config-package configures the packages passed in by
calling the function passed in"
  (if (symbolp packages) (setq packages (list packages)))
  (catch 'nodep
    (message "Configuring packages [%s]" packages)
    (dolist (package packages)
      (if (not (package-installed-p package))
          (progn
            (message "\tPackage [%s] is not installed." package)
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

;; ---------------------------------------------
;; TODO figure out what the following belongs to
;; ---------------------------------------------

;; code navigation
(setq jump-map (make-hash-table :test 'eq))
(gsk (kbd "<f6>")
     (lambda ()
       (interactive) ; global-set-key expects an interactive command
       (funcall (gethash major-mode jump-map))))
(gsk (kbd "<f5>") 'xref-pop-marker-stack) ; go back to previous jump mark

;; ---------------------------------
;; package configuration starts here
;; ---------------------------------

;; xah fly keys (default layout is dvorak)
(cp 'xah-fly-keys
    (lambda()
      (require 'xah-fly-keys)
      (xah-fly-keys 1)
      (dk xah-fly-t-keymap (kbd "h") 'xah-close-current-buffer)
      (dk xah-fly-leader-key-map (kbd "v") 'xah-goto-matching-bracket)
      (lk xah-fly-leader-key-map (kbd "g")
          (lambda() "close-current-buffer-and-delete-window"
            (interactive)
            (xah-close-current-buffer)
            (delete-window)))))

;; exec-path-from-shell
(cp 'exec-path-from-shell
    (lambda()
      (when (memq window-system '(mac ns x))
        (exec-path-from-shell-initialize))))

;; find file in repository
(cp 'find-file-in-repository
    (lambda() (gsk (kbd "<f7>") 'find-file-in-repository)))

;; lsp-mode
(cp 'lsp-mode
    (lambda()
      (autoload 'lsp-mode "lsp-mode")
      (setq lsp-enable-links nil)
      (setq lsp-enable-snippet nil)
      (dk xah-fly-comma-keymap (kbd "n") 'lsp-rename)))

;; lsp-treemacs
(cp 'lsp-treemacs
    (lambda()
      ;; enable bidirectional synchronization of lsp workspace folders and treemacs projects
      (lsp-treemacs-sync-mode 1)
      (dk xah-fly-dot-keymap (kbd "s") 'lsp-treemacs-symbols)
      (lk xah-fly-comma-keymap (kbd "r")
          (lambda() "show-lsp-references-in-treemacs"
            (interactive)
            (lsp-treemacs-references t)))
      (lk xah-fly-comma-keymap (kbd "m")
          (lambda() "show-lsp-implementations-in-treemacs"
            (interactive)
            (lsp-treemacs-implementations t)))))

;; treemacs
(cp 'treemacs
    (lambda()
      (setq treemacs-no-png-images t)
      (setq treemacs-width 33)
      (setq treemacs-show-cursor 1)
      (setq treemacs-fringe-indicator-mode nil)
      ;; on small screens, make treemacs window deletable by delete-other-windows
      ;; on big screens, it might be worthy to keep it around
      ;; TODO use display-pixel-width/display-pixel-height to detect display size
      ;; call external tool to get pixel density to get the diagonal size of the display
      (setq treemacs-no-delete-other-windows nil)
      ;; no spacing betweewn root nodes (this is to fix *LSP Symbols List*)
      (setq treemacs-space-between-root-nodes nil)
      ;; hide line numbers and mode line in treemacs
      (add-hook `treemacs-mode-hook
                (lambda ()
                  (display-line-numbers-mode 0)
                  (setq mode-line-format nil)))
      (dk xah-fly-dot-keymap (kbd "p") 'treemacs)))

;; company
(cp 'company
    (lambda()
      (setq company-require-match nil)
      (setq company-tooltip-align-annotations t); Align annotation to the right side.
      (setq company-minimum-prefix-length 1)))

;; company-lsp
(cp 'company-lsp
    (lambda()
      (autoload 'company-lsp "company-lsp")
      (with-eval-after-load 'company
        (push 'company-lsp company-backends))))

;; go-mode
(cp 'go-mode
    (lambda()
      ;; fontify only function declarations, not function calls
      (setq go-fontify-function-calls nil)
      ;; format code and reorganize imports before saving buffer
      ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
      (add-hook 'go-mode-hook
                (lambda()
                  (add-hook 'before-save-hook #'lsp-format-buffer t t)
                  (add-hook 'before-save-hook #'lsp-organize-imports t t)))
      ;; go-run and go-test
      (setq go-test-verbose t)
      (dk xah-fly-dot-keymap (kbd "r") 'go-run)
      (dk xah-fly-dot-keymap (kbd "t") 'go-test-current-test)
      (dk xah-fly-dot-keymap (kbd "T") 'go-test-current-file)
      ;; enable lsp-mode in go-mode
      (cp 'lsp-mode (lambda() (add-hook 'go-mode-hook 'lsp-deferred)))
      ;; register jump implmentation
      (puthash 'go-mode 'lsp-find-definition jump-map)
      ;; show eldoc
      (cp 'go-eldoc (lambda() (add-hook 'go-mode-hook 'go-eldoc-setup)))))

;; hl-todo
(cp 'hl-todo
    (lambda()
      (global-hl-todo-mode)
      (setq hl-todo-keyword-faces
            '(("TODO"   . "#C40233")
              ("FIXME"  . "#C40233")
              ("DEBUG"  . "#A020F0")
              ("GOTCHA" . "#FF4500")
              ("STUB"   . "#1E90FF")))))

;; highlight numbers
(cp 'highlight-numbers
    (lambda()
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'highlight-numbers-mode-hook
                (lambda()
                  "symbol-start and symbol-end excludes dot (.)
                  so numbers start or end with dots must be matched separately"
                  (puthash 'go-mode
                           (rx
                            (or
                             ;; match numbers end with .
                             (and
                              symbol-start
                              (or
                               (and (+ (any digit "_"))
                                    "."
                                    (? (and (any "eE")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               ))

                             ;; match numbers start with .
                             (and
                              (or
                               (and "."
                                    (+ (any digit "_"))
                                    (? (and (any "eE")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               )
                              symbol-end)

                             ;; match all the rest number formats
                             (and
                              symbol-start
                              (or
                               (and "0"
                                    (or (and (any "bB")
                                             (+ (any "0_1")))
                                        (and (any "oO")
                                             (+ (any "0-7_")))
                                        (and (any "xX")
                                             (+ (any hex-digit "_."))))
                                    (? (and (any "pP")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               (and digit
                                    (* (any digit "_"))
                                    (? ".")
                                    (* (any digit "_"))
                                    (? (and (any "eE")
                                            (? (any "-+"))
                                            (+ (any digit "_"))))
                                    (? "i"))
                               )
                              symbol-end)
                             ))
                           highlight-numbers-modelist)))))

;; markdown-mode
(cp 'markdown-mode
    (lambda()
      ;; make writing in markdown-mode less distracting
      (add-hook 'markdown-mode-hook
                (lambda ()
                  ;; sum of left and right margin cannot be greater than 67
                  ;; was 35 35, now 33 33
                  ;; see https://github.com/Alexander-Miller/treemacs/issues/669
                  ;; for now, only cell unit (40 chars) is support when sitting window margin
                  ;; check up visual-fill-column for support for ratio unit (0.3 (30%))
                  (setq left-margin-width 33)
                  (setq right-margin-width 33)
                  (visual-line-mode 1)
                  (display-line-numbers-mode 0)
                  (setq mode-line-format nil)
                  (setq buffer-face-mode-face
                        ;; '(:family "IM FELL English PRO" :height 220 :width regular))
                        '(:family "caveat" :height 300 :width regular))
                  (buffer-face-mode)))))

;; Ruby robe
(cp 'robe
    (lambda()
      (setq ruby-insert-encoding-magic-comment nil)
      (setq inf-ruby-console-environment "development")
      (add-hook 'ruby-mode-hook 'robe-mode)
      (add-hook 'ruby-mode-hook 'robe-start)
      ;; register jump implmentation
      (puthash 'ruby-mode 'robe-jump jump-map)
      ;; ruby code navigation
      (gsk (kbd "s-r r") 'inf-ruby)
      (gsk (kbd "s-r c") 'inf-ruby-console-auto)))

;; Web Mode
(cp 'web-mode
    (lambda()
      (autoload 'web-mode "web-mode")
      (atl 'auto-mode-alist '("\\.html\\'" . web-mode))
      (atl 'auto-mode-alist '("\\.erb\\'" . web-mode))))

;; YAML
(cp 'yaml-mode
    (lambda()
      (autoload 'yaml-mode "yaml-mode")
      (atl 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
      (atl 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))))

;; Rest client
(cp 'restclient
    (lambda()
      (autoload 'restclient "restclient")
      (atl 'auto-mode-alist '("\\.http\\'" . restclient-mode))
      (atl 'auto-mode-alist '("\\.rest\\'" . restclient-mode))))

;; Apib-mode (major mode for editing API blueprint file)
(cp 'apib-mode
    (lambda()
      (autoload 'apib-mode "apib-mode")
      (atl 'auto-mode-alist '("\\.api\\'" . apib-mode))
      (atl 'auto-mode-alist '("\\.apib\\'" . apib-mode))
      (atl 'auto-mode-alist '("\\.blueprint\\'" . apib-mode))))

;; json mode
(cp 'apib-mode
    (lambda()
      (add-hook 'json-mode-hook
                (lambda ()
                  (make-local-variable 'js-indent-level)
                  (setq js-indent-level 2)))))

;; org-mode
(setq org-startup-indented t)

;; Sh Mode
(atl 'auto-mode-alist '("\\Procfile.*\\'" . sh-mode)) ; forego foreman/procfile

