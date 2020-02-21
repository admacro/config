(defalias 'sfa 'set-face-attribute)

(defun reset ()
  (sfa 'font-lock-function-name-face nil :foreground "black" :weight 'normal :slant 'normal)
  (sfa 'font-lock-variable-name-face nil :foreground "black" :weight 'normal :slant 'normal)
  (sfa 'default nil :foreground "black" :background "#F4EEE6" :weight 'normal :slant 'normal)
  (sfa 'font-lock-type-face nil :foreground "black":weight 'normal :slant 'normal)
  (sfa 'font-lock-keyword-face nil :foreground "black" :weight 'normal :slant 'normal)
  (sfa 'font-lock-constant-face nil :foreground "black" :weight 'normal :slant 'normal)
  (sfa 'font-lock-comment-face nil :foreground "black" :weight 'normal :slant 'normal)
  (sfa 'font-lock-builtin-face nil :foreground "black" :weight 'normal :slant 'normal)
  (sfa 'font-lock-string-face nil :foreground "black" :weight 'normal :slant 'normal)
  )

(defun rainbow ()
  (sfa 'font-lock-function-name-face nil :foreground "purple4") ; 9.55
  (sfa 'font-lock-variable-name-face nil :foreground "red4") ; 8.68
  (sfa 'default nil :foreground "brown4") ; 5.11
  (sfa 'font-lock-type-face nil :foreground "orange4") ; 7.71
  (sfa 'font-lock-keyword-face nil :foreground "gold4") ; 3.92
  (sfa 'font-lock-constant-face nil :foreground "yellow4") ; 3.14
  (sfa 'font-lock-comment-face nil :foreground "blue4") ; 3.88
  (sfa 'font-lock-builtin-face nil :foreground "cyan4") ; 3.59
  (sfa 'font-lock-string-face nil :foreground "green4") ; 13.27
  )

(defun rainbow-aaa ()
  (sfa 'font-lock-function-name-face nil :foreground "#941508")
  (sfa 'font-lock-variable-name-face nil :foreground "#801300")
  (sfa 'default nil :foreground "#6D4000")
  (sfa 'font-lock-type-face nil :foreground "#604525")
  (sfa 'font-lock-keyword-face nil :foreground "#574A00")
  (sfa 'font-lock-constant-face nil :foreground "#4E4D00")
  (sfa 'font-lock-comment-face nil :foreground "#004B00")
  (sfa 'font-lock-builtin-face nil :foreground "#005354")
  (sfa 'font-lock-string-face nil :foreground "#042BDB")
  )

(defun colourful-aaa ()
  (sfa 'font-lock-function-name-face nil :foreground "#A30000")
  (sfa 'font-lock-variable-name-face nil :foreground "#80390A")
  (sfa 'default nil :foreground "#684208")
  (sfa 'font-lock-type-face nil :foreground "#514A0B")
  (sfa 'font-lock-keyword-face nil :foreground "#245144")
  (sfa 'font-lock-constant-face nil :foreground "#22593A")
  (sfa 'font-lock-comment-face nil :foreground "#33535B")
  (sfa 'font-lock-builtin-face nil :foreground "#3D4A66")
  (sfa 'font-lock-string-face nil :foreground "#4A4A4A")
  )

(defun contrast-six ()
  (sfa 'font-lock-function-name-face nil :foreground "#B2171A")
  (sfa 'font-lock-variable-name-face nil :foreground "#8B4805")
  (sfa 'default nil :foreground "#6D5539")
  (sfa 'font-lock-type-face nil :foreground "#705604")
  (sfa 'font-lock-keyword-face nil :foreground "#15681A")
  (sfa 'font-lock-constant-face nil :foreground "#006465")
  (sfa 'font-lock-comment-face nil :foreground "#094ED1")
  (sfa 'font-lock-builtin-face nil :foreground "#862BB6")
  (sfa 'font-lock-string-face nil :foreground "#5A5A5A")
  )

(defun contrast-seven ()
  (sfa 'font-lock-function-name-face nil :foreground "#9E1517")
  (sfa 'font-lock-variable-name-face nil :foreground "#7E3F00")
  (sfa 'default nil :foreground "#624C34")
  (sfa 'font-lock-type-face nil :foreground "#634D04")
  (sfa 'font-lock-keyword-face nil :foreground "#135C18")
  (sfa 'font-lock-constant-face nil :foreground "#00595B")
  (sfa 'font-lock-comment-face nil :foreground "#0846BA")
  (sfa 'font-lock-builtin-face nil :foreground "#7726A3")
  (sfa 'font-lock-string-face nil :foreground "#50504F")
  )

(defun contrast-eight ()
  (sfa 'font-lock-function-name-face nil :foreground "#8F1315")
  (sfa 'font-lock-variable-name-face nil :foreground "#713800")
  (sfa 'default nil :foreground "#58432F")
  (sfa 'font-lock-type-face nil :foreground "#594403")
  (sfa 'font-lock-keyword-face nil :foreground "#105314")
  (sfa 'font-lock-constant-face nil :foreground "#005052")
  (sfa 'font-lock-comment-face nil :foreground "#073FA6")
  (sfa 'font-lock-builtin-face nil :foreground "#6A2291")
  (sfa 'font-lock-string-face nil :foreground "#474746")
  )

(defun contrast-nine ()
  (sfa 'font-lock-function-name-face nil :foreground "#801214")
  (sfa 'font-lock-variable-name-face nil :foreground "#653200")
  (sfa 'default nil :foreground "#503C2B")
  (sfa 'font-lock-type-face nil :foreground "#503D03")
  (sfa 'font-lock-keyword-face nil :foreground "#0E4A13")
  (sfa 'font-lock-constant-face nil :foreground "#004849")
  (sfa 'font-lock-comment-face nil :foreground "#073895")
  (sfa 'font-lock-builtin-face nil :foreground "#611F81")
  (sfa 'font-lock-string-face nil :foreground "#40403F")
  )

;; #0A2744 13.13
;; #632948 9.38
;; #1F4D86 7.39
;; #706253 5.11
;; #D83501 4.11
;; #AC6D31 3.65
;; #A28563 3
;; #6991D3 2.76
;; #F88200 2.22
;; #C9A87C 1.94
;; #91B4C7 1.9
;; #A2B2C2 1.88
;; #D3B0AD 1.72
;; #F3CFA4 1.27
;; #F9EAB2 1.04
(defun solar ()
  (sfa 'font-lock-function-name-face nil :foreground "#D83501")
  (sfa 'font-lock-variable-name-face nil :foreground "#632948")
  (sfa 'default nil :foreground "whitesmoke" :background "#0A2744")
  (sfa 'font-lock-type-face nil :foreground "#632948")
  (sfa 'font-lock-keyword-face nil :foreground "#632948")
  (sfa 'font-lock-constant-face nil :foreground "#C9A87C")
  (sfa 'font-lock-comment-face nil :foreground "#A28563")
  (sfa 'font-lock-builtin-face nil :foreground "#6991D3")
  (sfa 'font-lock-string-face nil :foreground "#D83501")
  )

(defun adm-default-theme ()
  (sfa 'font-lock-function-name-face nil :foreground "red4")
  (sfa 'font-lock-variable-name-face nil :foreground "blue2")
  (sfa 'default nil :foreground "" :background "#F4EEE6")
  (sfa 'font-lock-type-face nil :foreground "violet")
  (sfa 'font-lock-keyword-face nil :foreground "gray20")
  (sfa 'font-lock-constant-face nil :foreground "darkviolet" :slant 'italic)
  (sfa 'font-lock-comment-face nil :foreground "gray50")
  (sfa 'font-lock-builtin-face nil :foreground "gray20")
  (sfa 'font-lock-string-face nil :foreground "darkgreen")
  ;; #F8F3EB/#F4EEE6/#F1EAE2
  ;; #EBE3E1/#F8F5F4
  )

;; (reset) 

;; (rainbow)

;; (rainbow-aaa)
;; (colourful-aaa)

;; (contrast-six)
;; (contrast-seven)
;; (contrast-eight)
;; (contrast-nine)

;; (solar)

;; (adm-default-theme)
