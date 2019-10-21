;;;; Encoding Cribbage

(define (crib:pair? xy)
  (apply = (map rank xy)))

(define (crib:rank card)
  (fx1+ (fxmin (rank card) 9)))

;;; Hand scores.
;; Expects argument variable hand to be the cut cons'ed to the hand.

(define (score-pairs hand)
  (fxsll (count crib:pair? (combinations hand 2)) 1))

(define (score-fifteens hand)
  (let ((V (make-fxvector 16)))
    (fxvector-set! V 0 1)
    (for-each (lambda (r)
                (do ((i 15 (fx1- i)))
                    ((fx< i r))
                  (fxvector-set! V
                                 i
                                 (fx+ (fxvector-ref V i)
                                      (fxvector-ref V (fx- i r))))))
              (map crib:rank hand))
    (fxsll (fxvector-ref V 15) 1)))

(define (score-flush hand)
  (let ((cut (car hand))
        (s0 (suit (cadr hand))))
    (if (andmap (lambda (c)
                  (fx= s0 (suit c)))
                (cddr hand))
        (fx+ 4 (if (fx= s0 (suit cut)) 1 0))
        0)))

(define (score-crib-flush hand)
  (let ((cut (car hand))
        (s0 (suit (cadr hand))))
    (if (and (andmap (lambda (c)
                       (fx= s0 (suit c)))
                     (cddr hand))
             (fx= s0 (suit cut)))
        5
        0)))

(define (score-runs hand)
  ;; 8-5-t-j-q gets 1 for runs
  (let ((runs (group-with (lambda (x y)
                            (fx= (fx1+ (car x))
                                 (car y)))
                          (sort-on (eq-histogram
                                    (map rank hand))
                                   car))))
    (cond ((find (lambda (run)
                   (fx< 2 (length run)))
                 runs)
           =>
           (lambda (run)
             (fx* (fold-left fx* 1 (map cdr run))
                  (length run))))
          (else 0))))

(define (score-heels hand)
  (if (jack? (car hand)) 2 0))

(define (score-nobs hand)
  (let ((jack-suits (map suit (filter jack? (cdr hand))))
        (nob-suit (suit (car hand))))
    (if (memq nob-suit jack-suits)
        1
        0)))

(define (score-pone hand)
  (fx+ (score-runs hand)
       (score-fifteens hand)
       (score-pairs hand)
       (score-flush hand)
       (score-nobs hand)))

(define (score-deal hand)
  (fx+ (score-runs hand)
       (score-fifteens hand)
       (score-pairs hand)
       (score-flush hand)
       (score-nobs hand)))

(define (score-hand hand)
  (fx+ (score-runs hand)
       (score-fifteens hand)
       (score-pairs hand)
       (score-flush hand)
       (score-nobs hand)))

(define (score-crib hand)
  (fx+ (score-runs hand)
       (score-fifteens hand)
       (score-pairs hand)
       (score-crib-flush hand)
       (score-nobs hand)))

;;; Peg Scores
;; Assumes the board is lifo stack
(define (crib-board-total board)
  (fold-left (lambda (total card)
               (fx+ total (crib:rank card)))
             0
             board))

(define (score-peg-count board)
  (case (crib-board-total board)
    ((15 31) 2)
    (else 0)))

(define (score-peg-pairs board)
  (if (pair? board)
      (let ((n (length (car (group-with = (map rank board))))))
        ;;; 2 * (choose n 2) pairs out of top grouping
        (fxsll (s:choose n 2) 1))
      0))

(define (simple-run? ranks n)
  (let ((ranks (sort < ranks)))
    (andmap (lambda (x y)
              (fx= (fx1+ x) y))
            (list-head ranks (fx1- n))
            (cdr ranks))))

(define (score-peg-runs board)
  (let ((n (length board)))
    (let loop ((prefixes (map reverse
                              (suffixes
                               (reverse
                                (map rank board)))))
               (n n))
      (if (fx< n 3)
          0
          (if (simple-run? (car prefixes) n)
              n
              (loop (cdr prefixes) (fx1- n)))))))

(define (score-peg board)
  (fx+ (score-peg-count board) ;; fifteens scored by count
       (score-peg-pairs board)
       (score-peg-runs board)))

(define (valid-pegs board hand)
  (let ((total (crib-board-total board)))
    (filter (lambda (card)
              (fx<= (fx+ total (crib:rank card)) 31))
            hand)))

;;; Board States := count | peg | discard | won

(define (game-won? state)
  (fx<= 121 (fxmax (crib-scoreA state)
                   (crib-scoreB state))))

(define (discard-complete? crib)
  (fx= 4 (length (crib-crib crib))))

(define (peg-complete? crib)
  (and (null? (crib-handA crib))
       (null? (crib-handB crib))))

(define (no-pegs-left? crib)
  (let ((board (crib-board crib)))
    (and (null? (valid-pegs board (crib-handA crib)))
         (null? (valid-pegs board (crib-handB crib))))))

(define (game-phase crib)
  (cond ((game-won? crib) 'won)
        ((peg-complete? crib) 'count)
        ((discard-complete? crib) 'peg)
        (else 'discard)))

;;; Move checks
(define (valid-discard? state turn move)
  (and (fx= 2 (length move))
       (eq? turn (crib-turn state))
       (andmap (lambda (card)
                 (memq card
                       (if (eq? turn 'A)
                           (crib-handA state)
                           (crib-handB state))))
               move)))

(define (current-players-hand cribbage)
  (if (eq? 'A (crib-turn cribbage))
      (crib-handA cribbage)
      (crib-handB cribbage)))

(define (actions cribbage)
  (case (game-phase cribbage)
    ((discard) (combinations (current-players-hand cribbage) 2))))




