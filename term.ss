(define foreground-color-table
  '((default . 39)
    (black . 30)
    (red . 31)
    (green . 32)
    (yellow . 33)
    (blue . 34)
    (magenta . 35)
    (cyan . 36)
    (light-gray . 37)
    (dark-gray . 90)
    (light-red . 91)
    (light-green . 92)
    (light-yellow . 93)
    (light-blue . 94)
    (light-magenta . 95)
    (light-cyan . 96)
    (white . 97)))

(define (foreground-color-escape color)
  (cond ((assq color foreground-color-table) => cdr)
        (else 39)))

(define (put-term-code code)
  (display "\033[") (display code) (display "m"))

(define (clear-term-code)
  (display "\033[0m"))

(define (with-foreground color)
  (let ((escape (foreground-color-escape color)))
    (lambda (thunk)
      (put-term-code escape)
      (thunk)
      (clear-term-code))))

(define (display-with-foreground color object)
  (let ((escape (foreground-color-escape color)))
    (put-term-code escape)
    (display object)
    (clear-term-code)))




