
;;; Turns

;;; Run moves
;;; Game Phase predicates

(define (current-score state)
  (list (crib-dealer state)
        (crib-scoreA state)
        (crib-scoreB state)))

(define (winning-player state)
  (let ((score (current-score state)))
    (cond ((= 121 (list-ref score 1)) 'A)
          ((= 121 (list-ref score 2)) 'B)
          (else #f))))

(define (trace-game state A B)
  (let ((agent-A (cribbot-strategy A))
        (agent-B (cribbot-strategy B))
        (positions (list (current-score state))))
    (let loop ((state state))
      (let ((turn (crib-turn state)))
        (case (game-phase state)
          ((discard)
           (let ((score (current-score state)))
             (unless (equal? (car positions) score)
               (push! score positions)))
           (if (eq? turn 'A)
               (loop (run-cribbage state (agent-A (crib->discard state))))
               (loop (run-cribbage state (agent-B (crib->discard state))))))
          ((peg)
           (if (eq? turn 'A)
               (loop (run-cribbage state (agent-A (crib->peg state))))
               (loop (run-cribbage state (agent-B (crib->peg state))))))
          ((count) (loop (execute-count state)))
          ((won)
           ;; not sure if we want to include this or not. 
           ;;           (push! (current-score state) positions)
           (set! winner (winning-player state))))))
    (cons winner positions)))

(define (record-trace dealer-table dealer-total pone-table pone-total trace)
  (let ((winner (car trace)))
    (for-all (lambda (dealer x y) ;; change in wins from dealer perspective
               (cond ((and (eq? dealer 'A) (eq? winner 'A))
                      (matrix-update! dealer-total x y 1+)
                      (matrix-update! pone-total y x 1+)
                      (matrix-update! dealer-table x y 1+)
                      (matrix-update! pone-table y x 1-))
                     ((and (eq? dealer 'A) (eq? winner 'B))
                      (matrix-update! dealer-total x y 1+)
                      (matrix-update! pone-total y x 1+)
                      (matrix-update! dealer-table x y 1-)
                      (matrix-update! pone-table y x 1+))
                     ((and (eq? dealer 'B) (eq? winner 'A))
                      (matrix-update! dealer-total y x 1+)
                      (matrix-update! pone-total x y 1+)
                      (matrix-update! dealer-table y x 1-)
                      (matrix-update! pone-table x y 1+))
                     (else
                      (matrix-update! dealer-total y x 1+)
                      (matrix-update! pone-total x y 1+)
                      (matrix-update! dealer-table y x 1+)
                      (matrix-update! pone-table x y 1-))))
             (map car (cdr trace))
             (map cadr (cdr trace))
             (map caddr (cdr trace)))))

;;; iters-all to get whole board lit up, probably want 1 or 2.
;;; iterations for starting games form zero
(define (bootstrap-win-table alice bob iterations-arbitrary iterations-normal-start)
  (let ((dealer-table (matrix 122 122))
        (dealer-total (matrix 122 122))
        (pone-table (matrix 122 122))
        (pone-total (matrix 122 122)))
    (do ((k 0 (1+ k)))
        ((= k iterations-normal-start))
      (simple-progress-bar 'Bootstrapping-Normal k iterations-normal-start)
      (record-trace dealer-table
                    dealer-total
                    pone-table
                    pone-total
                    (trace-game (deal-crib 0 0 (random-dealer))
                                alice
                                bob)))
    (do ((k 0 (1+ k)))
        ((= k iterations-arbitrary)
         (matrix-tabulate dealer-table
                          (lambda (i j)
                            (cond ((= i 121) 1)
                                  ((= j 121) -1)
                                  (else (/ (matrix-ref dealer-table i j)
                                           (max 1 (matrix-ref dealer-total i j)))))))
         (matrix-tabulate pone-table
                          (lambda (i j)
                            (cond ((= i 121) 1)
                                  ((= j 121) -1)
                                  (else (/ (matrix-ref pone-table i j)
                                           (matrix-ref pone-total i j))))))
         (for-each save-thing-to-file
                   (list dealer-table
                         dealer-total
                         pone-table
                         pone-total)
                   '("calculations/dealer-table"
                     "calculations/dealer-total"
                     "calculations/pone-table"
                     "calculations/pone-total")))
      (format #t "iteration ~a~%" k)
      (do ((i 0 (1+ i)))
          ((= i 121))
        (simple-progress-bar 'Bootstrapping-Arbitrary i 121)
        (do ((j 0 (1+ j)))
            ((= j 121))
          (record-trace dealer-table
                        dealer-total
                        pone-table
                        pone-total
                        (trace-game (deal-crib i j 'A) alice bob)))))))

(define (run-agents state A B)
  (let* ((now (current-time))
         (end #f)
         (out (open-output-file (format "games/~a-~a-~a-~a"
                                        (cribbot-identity A)
                                        (cribbot-identity B)
                                        (time-second now)
                                        (time-nanosecond now))))
         (agent-A (cribbot-strategy A))
         (agent-B (cribbot-strategy B)))
    (let loop ((state state))
      (write (crib->sexp state) out) (newline out)
      (let ((turn (crib-turn state)))
        (case (game-phase state)
          ((discard)
           (if (eq? turn 'A)
               (loop (run-cribbage state (agent-A (crib->discard state))))
               (loop (run-cribbage state (agent-B (crib->discard state))))))
          ((peg)
           (if (eq? turn 'A)
               (loop (run-cribbage state (agent-A (crib->peg state))))
               (loop (run-cribbage state (agent-B (crib->peg state))))))
          ((count) (loop (execute-count state)))
          ((won) (set! end state)))))
    (close-output-port out)
    end))

(define (compare-agents A B trials)
  (let ((wins-a 0)
        (wins-b 0)
        (N trials))
    (do ((trials trials (1- trials)))
        ((zero? trials)
         (format #t "~a won ~,2f% over ~a~%" (cribbot-identity A) (/ wins-a N 1/100) (cribbot-identity B)))
      (let ((result (time (run-agents (deal-crib 0 0 (random-dealer))
                                      A
                                      B))))
        (if (eq? (crib-scoreA result) 121)
            (inc! wins-a)
            (inc! wins-b))
        (format #t "~a v ~a~%" (cribbot-identity A) (cribbot-identity B))
        (format #t "~a - ~a out of ~a~%" wins-a wins-b N)
        (display-progress-bar wins-a wins-b N)))))

;;; terminal game 
(define (run-game machine)
  (let* ((now (current-time))
         (end #f)
         (out (open-output-file (format "games/~a-~a-~a-~a"
                                        (cribbot-identity machine)
                                        (cribbot-identity Jitwit)
                                        (time-second now)
                                        (time-nanosecond now))))
         (machine (cribbot-strategy machine))
         (jitwit-id 'B)
         (jitwit (cribbot-strategy Jitwit)))
    (let loop ((state (deal-crib 0 0 (random-dealer))))
      (write (crib->sexp state) out) (newline out)
      (let ((turn (crib-turn state)))
        (case (game-phase state)
          ((discard)
           (cond ((eq? turn 'A)
                  (loop (run-cribbage state (machine (crib->discard state)))))
                 (else
                  (display-cribbage-state jitwit-id state 'discard)
                  (loop (run-cribbage state (jitwit (crib->discard state)))))))
          ((peg)
           (cond ((eq? turn 'A)
                  (loop (run-cribbage state (machine (crib->peg state)))))
                 (else
                  (display-cribbage-state jitwit-id state 'peg)                  
                  (loop (run-cribbage state (jitwit (crib->peg state)))))))
          ((count)
           (display-cribbage-state jitwit-id state 'count)
           (loop (execute-count state)))
          ((won)
           (display-cribbage-state jitwit-id state 'won)
           (set! end state)))))
    (close-output-port out)
    end))
