(import (charset)
        (brzozowski))

(load "load.ss")

(define display-length
  (make-parameter 3))

(define url "https://www.dailycribbagehand.org/")
(define hand-file
  (let ((now (time-utc->date (current-time))))
    (format "daily-cribbage-hand/~a-~a-~a"
            (date-day now)
            (date-month now)
            (date-year now))))

(define lang-card
  (re:. (re:string "normal/")
        (re:charset "atjqk23456789")
        (re:charset "sdch")))

(define lang-score
  (re:. (re:string "<div id=\"score\">")
        ;;        lang-number
        (re:? #\*)
        #\-
        ;;        lang-number
        (re:? #\*)))

(define (fetch-daily-hand)
  (unless (file-exists? hand-file)
    (system (format "wget ~a -O ~a" url hand-file))))

(define (read-hand)
  (with-input-from-file hand-file
    (lambda ()
      (let loop ((x (read-char)) (xs '()))
        (if (eof-object? x)
            (list->string (reverse xs))
            (loop (read-char) (cons x xs)))))))

(define (read-cards)
  (map (lambda (c)
         (substring c (- (string-length c) 2) (string-length c)))
       (show-matches lang-card (read-hand))))

(define (char->rank char)
  (let ((table '((#\t . 9)
                 (#\j . 10)
                 (#\q . 11)
                 (#\k . 12)
                 (#\a . 0))))
    (cond ((assoc char table)
           => cdr)
          (else (char- char #\1)))))

(define (char->suit char)
  (length (cdr (memq char '(#\s #\h #\d #\c)))))

(define (parse-match s)
  (+ (char->rank (string-ref s 0))
     (* 13 (char->suit (string-ref s 1)))))

(define (main)
  (fetch-daily-hand)
  (let ((cards (map parse-match (read-cards))))
    (display-discard-strategy deal-maximize-points-heuristic cards (display-length))
    (newline) (newline)
    (display-discard-strategy pone-maximize-points-heuristic cards (display-length))
    (newline) (newline)
    (display-discard-strategy pone-minimize-crib-heuristic cards (display-length))
    (newline) (newline)))
