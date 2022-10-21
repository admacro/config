;; -*- coding: utf-8; lexical-binding: t; -*-

(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '())

;; define abbrev for specific major mode
;; the first part of the name should be the value of the variable major-mode of that mode
;; e.g. for go-mode, name should be go-mode-abbrev-table

(when (boundp 'go-mode-abbrev-table)
  (clear-abbrev-table go-mode-abbrev-table))

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("mn" "package main\n
import \"fmt\"\n
func main() {
    fmt.Println(\"\")
}")

    ("pf" "fmt.Printf(\"%v\\n\", abc)")
    ("pl" "fmt.Println(abc)")
    ("fn" "func FuncName(i InputType) (o ReturnType) {
    return nil
}")
    ("iferr" "if err != nil { panic(err) }")
    ("fori" "for i := 0; i < len(abc); i++ {}")
    ("forr" "for k, v := range abc {}")
    ))

(when (boundp 'org-mode-abbrev-table)
  (clear-abbrev-table org-mode-abbrev-table))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("isrc" "#+BEGIN_SRC lang\n
#+END_SRC")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
