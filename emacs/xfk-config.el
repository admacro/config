(defvar current-input-source
  (let ((source (replace-regexp-in-string "\n$" "" (shell-command-to-string "issw"))))
    (if (string= source "com.apple.inputmethod.SCIM.ITABC")
        "com.apple.keylayout.US"
      source)))

(defun cycle-input-source()
  (interactive)
  (if (string= current-input-source "com.apple.keylayout.Dvorak")
      (setq current-input-source "com.apple.keylayout.US")
      (setq current-input-source "com.apple.keylayout.Dvorak"))
  (set-input-source current-input-source))

(defun set-input-source(source)
  (shell-command-to-string (format "issw %s" source))
  (message "Input source is set to %s" source))

(defun english-insert-mode-activate()
  "Switch Mac OS input source to English, US or Dvorak
External command util: https://github.com/vovkasm/input-source-switcher"
  ;; shell-command creates a buffer *Shell Command Output*
  ;; shell-command-to-string does not, but catches the output
  ;; http://ergoemacs.org/emacs/elisp_call_shell_command.html
  (set-input-source current-input-source))

(defun chinese-insert-mode-activate()
  "Switch Mac OS input source to Chinese Pinyin when activating insert mode"
  (interactive)
  (set-input-source "com.apple.inputmethod.SCIM.ITABC")
  (xah-fly-insert-mode-activate))

;; auto input source switching when mode changess
(add-hook 'xah-fly-command-mode-activate-hook 'english-insert-mode-activate)
(define-key xah-fly-leader-key-map (kbd "DEL") 'chinese-insert-mode-activate)
