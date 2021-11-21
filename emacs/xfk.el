;; xah fly keys (default layout is dvorak)
(add-to-list 'load-path "~/prog/xah-fly-keys/")

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)

;; by default, <f8> activates xah-fly-command-mode-activate-no-hook
;; <f8> seems cannot be bound to xah-fly-command-mode-activate
(global-set-key (kbd "<f9>") 'xah-fly-command-mode-activate)

(defun close-current-buffer-and-delete-window()
  (interactive)
  (xah-close-current-buffer)
  (delete-window))
 (define-key xah-fly-t-keymap (kbd "h") 'close-current-buffer-and-delete-window)

;; Dumang keyboard customization (dedicated symbol keys replacing number row keys)
;; 1 2 3 4 5 6 7 8 9 0
;; ! @ # $ % ^ & * ( )
(define-key xah-fly-key-map (kbd "#") 'delete-other-windows)
(define-key xah-fly-key-map (kbd "$") 'split-window-below)
(define-key xah-fly-key-map (kbd "%") 'delete-char)
(define-key xah-fly-key-map (kbd "^") 'xah-select-block)
(define-key xah-fly-key-map (kbd "&") 'xah-select-line)
(define-key xah-fly-key-map (kbd "*") 'xah-extend-selection)
(define-key xah-fly-key-map (kbd "(") 'xah-select-text-in-quote)

(define-key xah-fly-leader-key-map (kbd "#") 'delete-window)
(define-key xah-fly-leader-key-map (kbd "$") 'split-window-right)
(define-key xah-fly-leader-key-map (kbd "%") 'balance-windows)
(define-key xah-fly-leader-key-map (kbd "^") 'xah-upcase-sentence)
(define-key xah-fly-leader-key-map (kbd "(") 'ispell-word)

(define-key xah-fly-t-keymap (kbd "!") 'xah-append-to-register-1)
(define-key xah-fly-t-keymap (kbd "@") 'xah-clear-register-1)
(define-key xah-fly-t-keymap (kbd "#") 'xah-copy-to-register-1)
(define-key xah-fly-t-keymap (kbd "$") 'xah-paste-from-register-1)
(define-key xah-fly-t-keymap (kbd "&") 'xah-append-to-register-1)
(define-key xah-fly-t-keymap (kbd "*") 'xah-clear-register-1)

(define-key xah-fly-r-keymap (kbd "#") 'number-to-register)
(define-key xah-fly-r-keymap (kbd "$") 'increment-register)

(define-key xah-fly-n-keymap (kbd ")") 'shell-command-on-region)
(define-key xah-fly-n-keymap (kbd "!") 'set-input-method)
(define-key xah-fly-n-keymap (kbd "@") 'global-hl-line-mode)
(define-key xah-fly-n-keymap (kbd "$") 'global-display-line-numbers-mode)
(define-key xah-fly-n-keymap (kbd "%") 'visual-line-mode)
(define-key xah-fly-n-keymap (kbd "^") 'calendar)
(define-key xah-fly-n-keymap (kbd "&") 'calc)
(define-key xah-fly-n-keymap (kbd "(") 'shell-command)

(define-key xah-fly--tab-key-map (kbd ")") 'expand-jump-to-next-slot)
(define-key xah-fly--tab-key-map (kbd "!") 'abbrev-prefix-mark)
(define-key xah-fly--tab-key-map (kbd "@") 'edit-abbrevs)
(define-key xah-fly--tab-key-map (kbd "#") 'expand-abbrev)
(define-key xah-fly--tab-key-map (kbd "$") 'expand-region-abbrevs)
(define-key xah-fly--tab-key-map (kbd "%") 'unexpand-abbrev)
(define-key xah-fly--tab-key-map (kbd "^") 'add-global-abbrev)
(define-key xah-fly--tab-key-map (kbd "&") 'add-mode-abbrev)
(define-key xah-fly--tab-key-map (kbd "*") 'inverse-add-global-abbrev)
(define-key xah-fly--tab-key-map (kbd "(") 'inverse-add-mode-abbrev)
