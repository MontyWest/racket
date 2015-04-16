#lang racket

(require "tiles.rkt")

(struct tile-snapshot (before tile after changed?) #:transparent)

(struct board-snapshot (board changed?) #:transparent)

;Iterates through the board applying f to each tile, returns board and whether it changed.
(define (board-iter f board)
  (let iter ([tried '()]
             [to-try board]
             [bchange? #f])
    (if (null? to-try)
        (board-snapshot tried bchange?)
        (let ((return (f tried 
                         (car to-try) 
                         (cdr to-try))))
          (iter (append (tile-snapshot-before return) 
                        (list (tile-snapshot-tile return)))
                (tile-snapshot-after return)
                (tile-snapshot-changed? return))))))

;Iterates through board and applys f to each tile, returns concatenated list of all f returns.
(define (board-acc f board)
  (let iter ([tried '()]
             [to-try board]
             [acc '()])
    (if (null? to-try)
        acc
        (let* ((trying (car to-try)) (return (f trying)))
          (iter (append tried (list trying))
                (cdr to-try)
                (append acc return))))))

; Removes the val from list of values of any tile that is relevant to the row col box (Strategy 1).
(define (remove row col box val)
  (lambda (before oldtile after)
    (if (tile-relevant? oldtile row col box)
        (let ((newtile (tile 
                        (tile-row oldtile)
                        (tile-col oldtile)
                        (tile-box oldtile)
                        (set-remove (tile-values oldtile) val))))
             (tile-snapshot before
                       newtile
                       after
                       (not (equal? (tile-values oldtile) (tile-values newtile)))))
        (tile-snapshot before oldtile after #f))))

;Takes a tile, extracts the row col box and iterates through board and applys the remove function.
(define (remove-iter before focustile after)
  (if (equal? 1 (set-count (tile-values focustile)))
  (let ((removelamb (remove (tile-row focustile)
                            (tile-col focustile)
                            (tile-box focustile)
                            (set-first (tile-values focustile)))))
     (let ((before-bc (board-iter removelamb before))
           (after-bc (board-iter removelamb after)))
       (tile-snapshot (board-snapshot-board before-bc)
                 focustile
                 (board-snapshot-board after-bc)
                 (or (board-snapshot-changed? before-bc) (board-snapshot-changed? after-bc)))))
  (tile-snapshot before focustile after #f)))

;(define (uniqueval row col box values)

;focuses on a tile applies remove-iter to if it only has one value, uniqueval-iter otherwise.
(define (focus before focustile after)
  (if (equal? 1 (set-count (tile-values focustile)))
  (remove-iter before focustile after)
  (tile-snapshot before focustile after #f)))

(provide tile-snapshot board-snapshot board-iter board-acc remove focus)