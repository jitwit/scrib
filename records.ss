(define-record-type crib
  (fields dealer
          turn
          scoreA
          scoreB
          handA
          handB
          crib
          cut
          last-peg
          board
          board*))

(define-record-type state-discard
  (fields dealer?
          scoreA
          scoreB
          hand))

(define-record-type state-peg
  (fields dealer?
          scoreA
          scoreB
          cut
          hand
          last-peg
          board
          board*))

(define-record-type cribbage-agent
  (fields discard-strategy
          pegging-strategy))

(define (crib->discard crib)
  (case (crib-turn crib)
    ((A) (make-state-discard (eq? 'A (crib-dealer crib))
                             (crib-scoreA crib)
                             (crib-scoreB crib)
                             (crib-handA crib)))
    ((B) (make-state-discard (eq? 'B (crib-dealer crib))
                             (crib-scoreB crib)
                             (crib-scoreA crib)
                             (crib-handB crib)))))

(define (crib->peg crib)
  (case (crib-turn crib)
    ((A) (make-state-peg (eq? 'A (crib-dealer crib))
                         (crib-scoreA crib)
                         (crib-scoreB crib)
                         (crib-cut crib)
                         (crib-handA crib)
                         (let ((lp (crib-last-peg crib)))
                           (case lp
                             ((B) 'opponent)
                             ((A) 'you)
                             (else #f)))
                         (crib-board crib)
                         (map (lambda (c.id)
                                (cons (car c.id)
                                      (if (eq? 'A (cdr c.id))
                                          'you
                                          'opponent)))
                              (crib-board* crib))))
    ((B) (make-state-peg (eq? 'B (crib-dealer crib))
                         (crib-scoreB crib)
                         (crib-scoreA crib)
                         (crib-cut crib)
                         (crib-handB crib)
                         (let ((lp (crib-last-peg crib)))
                           (case lp
                             ((A) 'opponent)
                             ((B) 'you)
                             (else #f)))
                         (crib-board crib)
                         (map (lambda (c.id)
                                (cons (car c.id)
                                      (if (eq? 'B (cdr c.id))
                                          'you
                                          'opponent)))
                              (crib-board* crib))))))

(define (crib->sexp crib)
  `((dealer . ,(crib-dealer crib))
    (turn . ,(crib-turn crib))
    (scoreA . ,(crib-scoreA crib))
    (scoreB . ,(crib-scoreB crib))
    (handA . ,(crib-handA crib))
    (handB . ,(crib-handB crib))
    (crib . ,(crib-crib crib))
    (cut . ,(crib-cut crib))
    (board . ,(crib-board crib))
    (board* . ,(crib-board* crib))))

(define (crib-update-dealer crib f)
  (make-crib (f (crib-dealer crib))
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-turn crib f)
  (make-crib (crib-dealer crib)
             (f (crib-turn crib))
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-scoreA crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (f (crib-scoreA crib))
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-scoreB crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (f (crib-scoreB crib))
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-handA crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (f (crib-handA crib))
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-handB crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (f (crib-handB crib))
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-crib crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (f (crib-crib crib))
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-cut crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (f (crib-cut crib))
             (crib-last-peg crib)
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-board crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (f (crib-board crib))
             (crib-board* crib)))

(define (crib-update-last-peg crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (f (crib-last-peg crib))
             (crib-board crib)
             (crib-board* crib)))

(define (crib-update-board* crib f)
  (make-crib (crib-dealer crib)
             (crib-turn crib)
             (crib-scoreA crib)
             (crib-scoreB crib)
             (crib-handA crib)
             (crib-handB crib)
             (crib-crib crib)
             (crib-cut crib)
             (crib-last-peg crib)
             (crib-board crib)
             (f (crib-board* crib))))

(define-record-type cribbage-table
  (fields location recipe))

(define-record-type cribbot
  (fields identity strategy))
