;;;;; Encoding Cribbage

;;;; Scoring Cribbage

(define (crib:pair? xy)
  (apply = (map rank xy)))

(define (crib:rank card)
  (fx1+ (fxmin (rank card) 9)))

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
    ;; this disgusting-ness is to only pass over hand once... 
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
  (let ((suit0 (suit (cadr hand))))
    (if (andmap (lambda (card)
                  (= suit0 (suit card)))
                (cddr hand))
        (+ 4 (if (= suit0 (suit (car hand))) 1 0))
        0)))

(define (score-crib-flush hand)
  (let ((suit0 (suit (car hand))))
    (if (andmap (lambda (card)
                  (= suit0 (suit card)))
                (cdr hand))
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
      (let ((r0 (rank (car board))))
        (let loop ((n 1) (board (cdr board)))
          (if (and (pair? board)
                   (fx= r0 (rank (car board))))
              (loop (fx1+ n) (cdr board))
              (pair-table n))))
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

(define (game-won? state)
  (fx<= 121 (fxmax (crib-scoreA state)
                   (crib-scoreB state))))

(define (discard-complete? crib)
  (fx= 4 (length (crib-crib crib))))

(define (peg-complete? crib)
  (and (null? (crib-handA crib))
       (null? (crib-handB crib))
       (not (crib-last-peg crib))))

(define (no-pegs-left? crib)
  (let ((board (crib-board crib)))
    (and (null? (valid-pegs board (crib-handA crib)))
         (null? (valid-pegs board (crib-handB crib))))))

(define (game-phase crib)
  (cond ((game-won? crib) 'won)
        ((not (discard-complete? crib)) 'discard)
        ((peg-complete? crib) 'count)
        (else 'peg)))

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

;;; Board States := count | peg | discard | won
(define (valid-pegs board hand)
  (let ((total (crib-board-total board)))
    (filter (lambda (card)
              (fx<= (fx+ total (crib:rank card)) 31))
            hand)))

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

;;; helper-ish things
(define (opposite-player x)
  (if (eq? x 'A) 'B 'A))

(define (random-dealer)
  (if (zero? (random 2)) 'A 'B))

(define (current-players-hand cribbage)
  (if (eq? 'A (crib-turn cribbage))
      (crib-handA cribbage)
      (crib-handB cribbage)))

(define (crib-cards crib who)
  (if (eq? who 'A)
      (crib-handA crib)
      (crib-handB crib)))

;;;; Cribbage Mechanics

;;; dealing
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
               #f
               '()
               '())))

(define (redeal-crib crib)
  (let ((cards (list-head (deck) 13)))
    (deal-crib (crib-scoreA crib)
               (crib-scoreB crib)
               (opposite-player (crib-dealer crib)))))

;;; updates
(define (change-turn crib)
  (crib-update-turn crib opposite-player))

(define (move-board crib)
  (crib-update-turn
   (crib-update-last-peg
    (crib-update-board
     crib
     (const '()))
    (const #f))
   (const (opposite-player (crib-last-peg crib)))))

(define (update-score state who dx)
  (if (game-won? state)
      state
      (case who
        ((A) (let ((score (+ dx (crib-scoreA state))))
               (crib-update-scoreA state (const (min score 121)))))
        ((B) (let ((score (+ dx (crib-scoreB state))))
               (crib-update-scoreB state (const (min score 121))))))))

;;; moves
(define (execute-discard crib card+)
  (case (crib-turn crib)
    ((A) (crib-update-handA crib (lambda (handA) (discard card+ handA))))
    ((B) (crib-update-handB crib (lambda (handB) (discard card+ handB))))))

(define (execute-discard-crib state cards)
  (change-turn
   (crib-update-crib (execute-discard state cards) (lambda (crib) (append cards crib)))))

(define (execute-peg crib card/go)
  (if (eq? card/go 'go)
      (execute-peg-go crib)
      (execute-peg-discard crib card/go)))

;;; pretty severe bug, a lot of final go's aren't being counted. I think this is fixed
;;; by forcing a final go if last-peg is non-false...
(define (execute-peg-go state)
  (if (no-pegs-left? state)
      (move-board (update-score state (crib-last-peg state) 1))
      (change-turn state)))

(define (execute-peg-discard state card)
  (let ((turn (crib-turn state))
        (board (cons card (crib-board state))))
    (let ((state* (update-score
                   (crib-update-last-peg
                    (crib-update-board*
                     (crib-update-board
                      (execute-discard state (list card))
                      (lambda (board) (cons* card board)))
                     (lambda (board*) (cons* (cons card turn) board*)))
                    (const turn))
                   turn
                   (score-peg board))))
      (if (= 31 (crib-board-total board))
          (move-board state*)
          (change-turn state*)))))

(define (board*->hand state who)
  (filter-map (lambda (c.id)
                (and (eq? (cdr c.id) who)
                     (car c.id)))
              (crib-board* state)))

(define (execute-count crib)
  (let ((cut (crib-cut crib))
        (handA (board*->hand crib 'A))
        (handB (board*->hand crib 'B)))
    (let ((scoreA (score-hand (cons cut handA)))
          (scoreB (score-hand (cons cut handB)))
          (scoreC (score-hand (cons cut (crib-crib crib))))
          (crib (crib-update-handB
                 (crib-update-handA
                  crib
                  (const handA))
                 (const handB))))
      (redeal-crib
       (cond
        ((eq? 'A (crib-dealer crib))
         (update-score (update-score (update-score crib 'B scoreB) 'A scoreA) 'A scoreC))
        (else
         (update-score
          (update-score
           (update-score crib 'A scoreA) 'B scoreB) 'B scoreC)))))))

;;; executing moves
(define (run-discard state action)
  (let ((state* (execute-discard-crib state action)))
    (if (and (jack? (crib-cut state*))
             (discard-complete? state*))
        (update-score state* (crib-dealer state) 2)
        state*)))

(define (valid-peg? state turn move)
  (if (eq? move 'go)
      (valid-go? state turn)
      (valid-peg-discard? state turn move)))

(define (valid-go? state turn)
  (and (eq? turn (crib-turn state))
       (null? (valid-pegs (crib-board state)
                          (crib-cards state turn)))))

(define (valid-peg-discard? state turn card)
  (and (eq? turn (crib-turn state))
       (memq card (valid-pegs (crib-board state)
                              (crib-cards state turn)))))

(define (run-peg state action)
  (if (eq? action 'go)
      (execute-peg-go state)
      (execute-peg-discard state action)))

(define (run-cribbage state action)
  (case (game-phase state)
    ((discard) (run-discard state action))
    ((peg) (run-peg state action))
    ((count) (execute-count state))
    ((won) state)))
