(import (charset)
        (brzozowski))

(load "load.ss")

(define display-length
  (make-parameter 3))

(define url "https://www.dailycribbagehand.org/")

(define hand-file
  (make-parameter
   (let ((now (time-utc->date (current-time))))
     (format "daily-cribbage-hand/~a-~a-~a"
             (date-day now)
             (date-month now)
             (date-year now)))))

;;; regular expressions
(define re:card
  (re:. (re:string "normal/")
        (re:charset "atjqk23456789")
        (re:charset "sdch")))

(define re:number
  (re:1+ (re:charset "0123456789")))

(define re:score
  (re:. (re:string "<div id=\"score\">")
        re:number
        (re:? #\*)
        #\-
        re:number
        (re:? #\*)
        #\&))

;;; parse cards
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

(define (parse-card s)
  (+ (char->rank (string-ref s 0))
     (* 13 (char->suit (string-ref s 1)))))

(define (parse-score s)
  (map string->number (show-matches re:number s)))

(define (fetch-daily-hand)
  (unless (file-exists? (hand-file))
    (system (format "wget ~a -O ~a" url (hand-file)))))

(define (read-hand)
  (with-input-from-file (hand-file)
    (lambda ()
      (let loop ((x (read-char)) (xs '()))
        (if (eof-object? x)
            (list->string (reverse xs))
            (loop (read-char) (cons x xs)))))))

(define (read-cards)
  (map (lambda (c)
         (substring c (- (string-length c) 2) (string-length c)))
       (show-matches re:card (read-hand))))

(define (read-puzzle)
  (let* ((puzzle (read-hand))
         (hand (map (lambda (c)
                      (parse-card (substring c
                                             (- (string-length c) 2)
                                             (string-length c))))
                    (show-matches re:card puzzle)))
         (score-string (car (show-matches re:score puzzle)))
         (scores (map string->number (show-matches re:number score-string))))
    ;;; * indicates dealer, our score is listed first. We match the &, 
    ;;; so - 2 for index
    (make-state-discard (not (char=? #\*
                                     (string-ref score-string
                                                 (- (string-length score-string)
                                                    2))))
                        (list-ref scores 0)
                        (list-ref scores 1)
                        hand)))


(define (main)
  (fetch-daily-hand)
  (let* ((situation (read-puzzle))
         (cards (state-discard-hand situation)))
    (cond ((state-discard-dealer? situation)
           (display-discard-strategy deal-maximize-points-heuristic cards (display-length))
           (newline) (newline)
           ;;           (display-discard-strategy deal-maximize-crib-heuristic cards (display-length))
           ;;           (newline) (newline)
           )
          (else
           (display-discard-strategy pone-maximize-points-heuristic cards (display-length))
           (newline) (newline)
           ;;
           (display-discard-strategy pone-minimize-crib-heuristic cards (display-length))
           ;;           (newline) (newline)
           ))
    (display-ln situation)))


