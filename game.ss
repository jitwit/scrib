
(define (crib-change-turn crib)
  (crib-update-turn crib opposite-player))

(define (update-score crib who dx)
  (case who
    ((A) (crib-update-scoreA crib (curry + dx)))
    ((B) (crib-update-scoreB crib (curry + dx)))))

(define (execute-discard crib card+)
  (case (crib-turn crib)
    ((A) (crib-update-handA crib (curry discard card+)))
    ((B) (crib-update-handB crib (curry discard card+)))))

(define (execute-discard-to-crib crib cards)
  (crib-update-crib (execute-discard crib cards) (curry append cards)))

(define (execute-discard-peg crib card)
  (crib-update-board (execute-discard crib (list card)) (curry cons* card)))

(define (execute-peg crib card)
  (let ((new-crib (execute-discard crib (list card))))
    (crib-change-turn
     (update-score new-crib
                   who
                   (curry + (score-peg (crib-board new-crib)))))))

(define (score-cut crib)
  (if (jack? (crib-cut crib))
      (update-score crib (crib-dealer crib) 2)
      crib))

(define (score-go crib who)
  (update-score crib who 1))

(define (make-peg crib who card)
  (let ((board (cons card (crib-board crib)))
        (hand (crib-cards crib who)))
    (case who
      ((A) (update-score crib )
       (make-crib (crib-dealer crib)
                  (+ (score-peg board)
                     (crib-scoreA crib))
                  (crib-scoreB crib)
                  (remv card hand)
                  (crib-handB crib)
                  (crib-crib crib)
                  (crib-cut crib)
                  board
                  (crib-board* crib)))
      ((B) (make-crib (crib-dealer crib)
                      (crib-scoreA crib)
                      (+ (score-peg board)
                         (crib-scoreB crib))
                      (crib-handA crib)
                      (remv card hand)
                      (crib-crib crib)
                      (crib-cut crib)
                      board
                      (crib-board* crib))))))

(define (make-go crib)
  (make-crib (crib-dealer crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             '()
             (cons (crib-board crib)
                   (crib-board* crib))))

;;; Mechanics
(define (discarded? crib)
  (= 4 (length (crib-crib crib))))

(define (view-crib crib who)
  (if (discarded? crib)
      (list (eq? (crib-dealer crib) who)
            (crib-scoreA crib)
            (crib-scoreB crib)
            (crib-cards crib who)
            (crib-cut crib)
            (crib-board crib)
            (crib-board* crib))
      (list (eq? (crib-dealer crib) who)
            (crib-scoreA crib)
            (crib-scoreB crib)
            (crib-cards crib who))))

(define (deal-crib scoreA scoreB dealer)
  (let ((cards (list-head (deck) 13)))
    (make-crib dealer
               (opposite-player dealer)
               scoreA
               scoreB
               (sort-on (list-head (cdr cards) 6) rank)
               (sort-on (list-tail (cdr cards) 6) rank)
               '()
               (car cards)
               '()
               '())))

(define (valid-discard? who crib cards)
  ;; not completely valid. doesn't prevent double discarding
  (or (and (eq? who 'A) (= 4 (length (discard cards (crib-handA crib)))))
      (and (eq? who 'B) (= 4 (length (discard cards (crib-handB crib)))))))

(define (make-discard who crib cards)
  (case who
    ((A) (make-crib (crib-dealer crib)
                    (crib-scoreA crib)
                    (crib-scoreB crib)
                    (discard cards (crib-handA crib))
                    (crib-handB crib)
                    (append cards (crib-crib crib))
                    (crib-cut crib)
                    (crib-board crib)
                    (crib-board* crib)))
    ((B) (make-crib (crib-dealer crib)
                    (crib-scoreA crib)
                    (crib-scoreB crib)
                    (crib-handA crib)
                    (discard cards (crib-handB crib))
                    (append cards (crib-crib crib))
                    (crib-cut crib)
                    (crib-board crib)
                    (crib-board* crib)))))

(define (crib-make-discard who crib cards)
  (if (valid-discard? who crib cards)
      (make-discard who crib cards)
      'illegal-move))

(define (crib-cards crib who)
  (if (eq? who 'A)
      (crib-handA crib)
      (crib-handB crib)))

(define (game-won? crib)
  (<= 121 (max (scoreA crib)
               (scoreB crib))))

(define (opposite-player x)
  (case x
    ((A) 'B)
    ((B) 'A)))

(define (make-game playerA playerB)
  (letrec* ((dealer (if (zero? (random 2)) 'A 'B))
            (crib (deal-crib 0 0 dealer))
            (next (opposite-turn dealer)))
    (lambda (m)
      (case m
        ((dealer) dealer)
        ((crib) crib)
        ((A B) (view-crib crib m))
        ))))
