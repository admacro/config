;; -*- coding: utf-8; lexical-binding: t; -*-

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; programing
    ("eq" "==" )
    ("r" "return" )
    ))

;; define abbrev for specific major mode
;; the first part of the name should be the value of the variable major-mode of that mode
;; e.g. for go-mode, name should be go-mode-abbrev-table

(when (boundp 'go-mode-abbrev-table)
  (clear-abbrev-table go-mode-abbrev-table))

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("go" "package main\n
import \"fmt\"\n
func main() {
    fmt.Println(\"\")
}")

    ("p" "fmt.Printf(\"%v\\n\", hh▮)")
    ("pl" "fmt.Println(hh▮)")
    ("r" "return")
    ("st" "string")
    ("eq" "==")
    ("v" "var x = 3")
    ("df" "x := 3")
    ("c" "const x = 3")
    ("f" "func f() {}")
    ("if" "if true {}")
    ("ie" "if err != nil { panic(err) }")
    ("ei" "else if true {}")
    ("else" "else {}")
    ("for" "for i := 0; i < len(a); i++ {}")
    ("fr" "for k, v := range xxx {}")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
