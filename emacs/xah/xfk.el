;; xah fly keys (default layout is dvorak)
(add-to-list 'load-path "~/prog/xah-fly-keys/")

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)

(defun close-current-buffer-and-delete-window()
  (interactive)
  (xah-close-current-buffer)
  (delete-window))
(define-key xah-fly-leader-key-map (kbd "t h") 'close-current-buffer-and-delete-window)

(defun set-input-source(source)
  ;; shell-command creates a buffer *Shell Command Output*
  ;; shell-command-to-string does not, but catches the output
  ;; http://ergoemacs.org/emacs/elisp_call_shell_command.html
  (shell-command-to-string (format "issw %s" source))
  (message "Input source is set to %s" source))

(defun switch-to-last-input-source(source)
  "Switch input source to last input source saved while entering chinese insert mode.
External command util: https://github.com/vovkasm/input-source-switcher"
  (if (equal "com.apple.inputmethod.SCIM.ITABC" source)
      (set-input-source "com.apple.keylayout.ABC")
    (set-input-source source))
  (remove-hook 'xah-fly-command-mode-activate-hook 'switch-to-last-input-source))

(defun chinese-insert-mode-activate()
  "Switch Mac OS input source to Chinese Pinyin when activating insert mode"
  (interactive)
  (let ((last-input-source (replace-regexp-in-string "\n$" "" (shell-command-to-string "issw"))))
    (add-hook 'xah-fly-command-mode-activate-hook
              ;; apply-partially lets you add function call with parameters as a hook
              (apply-partially #'switch-to-last-input-source last-input-source)))
  (xah-fly-insert-mode-activate)
  (set-input-source "com.apple.inputmethod.SCIM.ITABC"))

;; ;; use DEL as leader key
;; (define-key key-translation-map (kbd "DEL") (kbd "SPC"))

(define-key xah-fly-leader-key-map (kbd "DEL") 'chinese-insert-mode-activate)

;; make 【leader . r】 run certain command in certain mode
(setq runInModeHash (make-hash-table :test 'equal))
(defun run-command-in-mode()
  "Run command in mode according to runInModeHash mapping"
  (interactive)
  (maphash
   (lambda (mode cmd) (if (equal major-mode mode) (funcall cmd)))
   runInModeHash))
(define-key xah-fly-leader-key-map (kbd ". r") 'run-command-in-mode)

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

;; SPACE
(define-key xah-fly-leader-key-map (kbd "#") 'delete-window)
(define-key xah-fly-leader-key-map (kbd "$") 'split-window-right)
(define-key xah-fly-leader-key-map (kbd "%") 'balance-windows)
(define-key xah-fly-leader-key-map (kbd "^") 'xah-upcase-sentence)
(define-key xah-fly-leader-key-map (kbd "(") 'ispell-word)

;; dvorat t
(define-key xah-fly-leader-key-map (kbd "t !") 'xah-append-to-register-1)
(define-key xah-fly-leader-key-map (kbd "t @") 'xah-clear-register-1)
(define-key xah-fly-leader-key-map (kbd "t #") 'xah-copy-to-register-1)
(define-key xah-fly-leader-key-map (kbd "t $") 'xah-paste-from-register-1)
(define-key xah-fly-leader-key-map (kbd "t &") 'xah-append-to-register-1)
(define-key xah-fly-leader-key-map (kbd "t *") 'xah-clear-register-1)

;; dvorat r
(define-key xah-fly-leader-key-map (kbd "r #") 'number-to-register)
(define-key xah-fly-leader-key-map (kbd "r $") 'increment-register)

;; dvorat n
(define-key xah-fly-leader-key-map (kbd "n )") 'shell-command-on-region)
(define-key xah-fly-leader-key-map (kbd "n !") 'set-input-method)
(define-key xah-fly-leader-key-map (kbd "n @") 'global-hl-line-mode)
(define-key xah-fly-leader-key-map (kbd "n $") 'global-display-line-numbers-mode)
(define-key xah-fly-leader-key-map (kbd "n %") 'visual-line-mode)
(define-key xah-fly-leader-key-map (kbd "n ^") 'calendar)
(define-key xah-fly-leader-key-map (kbd "n &") 'calc)
(define-key xah-fly-leader-key-map (kbd "n (") 'shell-command)

;; dvorat TAB
(define-key xah-fly-leader-key-map (kbd "TAB )") 'expand-jump-to-next-slot)
(define-key xah-fly-leader-key-map (kbd "TAB !") 'abbrev-prefix-mark)
(define-key xah-fly-leader-key-map (kbd "TAB @") 'edit-abbrevs)
(define-key xah-fly-leader-key-map (kbd "TAB #") 'expand-abbrev)
(define-key xah-fly-leader-key-map (kbd "TAB $") 'expand-region-abbrevs)
(define-key xah-fly-leader-key-map (kbd "TAB %") 'unexpand-abbrev)
(define-key xah-fly-leader-key-map (kbd "TAB ^") 'add-global-abbrev)
(define-key xah-fly-leader-key-map (kbd "TAB &") 'add-mode-abbrev)
(define-key xah-fly-leader-key-map (kbd "TAB *") 'inverse-add-global-abbrev)
(define-key xah-fly-leader-key-map (kbd "TAB (") 'inverse-add-mode-abbrev)
