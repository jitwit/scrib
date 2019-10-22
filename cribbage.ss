;;;; Encoding Cribbage

(define (crib:pair? xy)
  (apply = (map rank xy)))

(define (crib:rank card)
  (fx1+ (fxmin (rank card) 9)))

;;; Hand scores.
;; Expects argument variable hand to be the cut cons'ed to the hand.

(define pair-table
  (let ((table '#vfx(0 0 2 6 12 20)))
    (lambda (count)
      (fxvector-ref table count))))

(define (score-pairs&fifteens hand)
  (let ((pairs (make-fxvector 13))
        (fifteens (make-fxvector 16)))
    (fxvector-set! fifteens 0 1)
    (for-all (lambda (card)
               (fxvector-inc! pairs (rank card))
               (let ((r (crib:rank card)))
                 (do ((i 15 (fx1- i)))
                     ((fx< i r))
                   (let ((fifteens-i-r (fxvector-ref fifteens (fx- i r))))
                     (when (fx< 0 fifteens-i-r)
                       (fxvector-set! fifteens
                                      i
                                      (fx+ (fxvector-ref fifteens i)
                                           fifteens-i-r)))))))
             hand)
    (do ((i 0 (fx1+ i))
         (count 0 (fx+ count (pair-table (fxvector-ref pairs i)))))
        ((fx= i 13)
         (fx+ count (fxsll (fxvector-ref fifteens 15) 1))))))

(define (score-runs-from-frequencies ranks)
  (define (loop1 i k r)
    (cond ((fx< 12 i)
           (if (fx< 2 k) (fx* k r) 0))
          ((fxzero? (fxvector-ref ranks i))
           (if (fx< 2 k) (fx* k r) (loop2 (fx1+ i))))
          (else (loop1 (fx1+ i) (fx1+ k) (fx* r (fxvector-ref ranks i))))))
  (define (loop2 i)
    (cond ((fx< 12 i) 0)
          ((fx< 0 (fxvector-ref ranks i)) (loop1 (fx1+ i) 1 (fxvector-ref ranks i)))
          (else (loop2 (fx1+ i)))))
  (loop2 0))

(define (score-pairs&fifteens&runs hand)
  (let ((ranks (make-fxvector 13))
        (fifteens (make-fxvector 16)))
    (fxvector-set! fifteens 0 1)
    (for-all (lambda (card)
               (fxvector-inc! ranks (rank card))
               (let ((r (crib:rank card)))
                 (do ((i 15 (fx1- i)))
                     ((fx< i r))
                   (let ((fifteens-i-r (fxvector-ref fifteens (fx- i r))))
                     (when (fx< 0 fifteens-i-r)
                       (fxvector-set! fifteens
                                      i
                                      (fx+ (fxvector-ref fifteens i)
                                           fifteens-i-r)))))))
             hand)
    (do ((i 0 (fx1+ i))
         (count 0 (fx+ count (pair-table (fxvector-ref ranks i)))))
        ((fx= i 13)
         (fx+ count
              (score-runs-from-frequencies ranks)
              (fxsll (fxvector-ref fifteens 15) 1))))))

(define (score-pairs hand)
  (let ((V (make-fxvector 13)))
    (for-all (lambda (card)
               (fxvector-inc! V (rank card)))
             hand)
    (do ((i 0 (fx1+ i))
         (count 0 (fx+ count (pair-table (fxvector-ref V i)))))
        ((fx= i 13) count))))

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
  (let ((V (make-fxvector 4)))
    (for-all (lambda (card)
               (fxvector-inc! V (suit card)))
             (cdr hand))
    (let loop ((i 0))
      (cond ((fx= 4 (fxvector-ref V i))
             (if (fx= (suit (car hand)) i) 5 0))
            ((fx< 0 (fxvector-ref V i) 4) 0)
            (else (loop (fx1+ i)))))))

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
  (let ((nob-suit (suit (car hand))))
    (if (ormap (lambda (card)
                 (and (jack? card)
                      (fx= nob-suit (suit card))))
               (cdr hand))
        1
        0)))

(define (score-hand hand)
  (fx+ (score-pairs&fifteens&runs hand)
       (score-flush hand)
       (score-nobs hand)))

(define (score-deal hand)
  (fx+ (score-runs hand)
       (score-fifteens hand)
       (score-pairs hand)
       (score-flush hand)
       (score-nobs hand)))

(define (score-pone hand)
  (fx+ (score-runs hand)
       (score-pairs&fifteens hand)
       (score-flush hand)
       (score-nobs hand)))

(define (score-crib hand)
  (fx+ (score-runs hand)
       (score-pairs&fifteens hand)
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

;;; Board States := count | peg | discard | won
(define (cribbage-actions state)
  (case (game-phase state)
    ((discard) (combinations (current-players-hand state) 2))
    ((peg)
     (let ((discards (valid-pegs (crib-board state) (current-players-hand state))))
       (if (null? discards)
           '(go)
           discards)))))

(define (valid-pegs board hand)
  (let ((total (crib-board-total board)))
    (filter (lambda (card)
              (fx<= (fx+ total (crib:rank card)) 31))
            hand)))

