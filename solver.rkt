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
                (or (tile-snapshot-changed? return) bchange?))))))

;Iterates through board and applys f to each tile, returns concatenated list of all f returns.
(define (board-acc f init board)
  (let iter ([tried '()]
             [to-try board]
             [acc init])
    (if (null? to-try)
        acc
        (let* ((trying (car to-try)) (return (f trying acc)))
          (iter (append tried (list trying))
                (cdr to-try)
                return)))))

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

;Holder for possible values across row col and box
(struct uniqueval-struct (row-values col-values box-values) #:transparent)

;If tile is in same row, col or box then removes it's values from the uv struct
(define (uniqueval row col box)
  (lambda (tile uv)
    (if (tile-relevant? tile row col box)
        (uniqueval-struct 
         (set-subtract (uniqueval-struct-row-values uv) 
                       (if (equal? (tile-row tile) row) (tile-values tile) (set)))
         (set-subtract (uniqueval-struct-col-values uv) 
                       (if (equal? (tile-col tile) col) (tile-values tile) (set)))
         (set-subtract (uniqueval-struct-box-values uv) 
                       (if (equal? (tile-box tile) box) (tile-values tile) (set))))
        uv)))

;Uses board acc to accumulate a struct of unique vals along row col and box of focustile,
;if one value is returned then changes tile to that value
(define (uniqueval-iter before focustile after)
  (if (<= 1 (set-count (tile-values focustile)))
      (let* ((uniques (board-acc (uniqueval (tile-row focustile)
                                           (tile-col focustile)
                                           (tile-box focustile))
                                (uniqueval-struct (tile-values focustile)
                                                  (tile-values focustile)
                                                  (tile-values focustile))
                                (append before after)))
             (flatuniques (set-union (uniqueval-struct-row-values uniques) 
                                     (uniqueval-struct-col-values uniques) 
                                     (uniqueval-struct-box-values uniques))))
           (if (equal? (set-count flatuniques) 1)
             (tile-snapshot before 
                            (tile (tile-row focustile)
                                  (tile-col focustile)
                                  (tile-box focustile)
                                  flatuniques)
                            after
                            #t)
             (tile-snapshot before focustile after #f)))
      (tile-snapshot before focustile after #f)))
    
  

;focuses on a tile applies remove-iter to if it only has one value, uniqueval-iter otherwise.
(define (focus before focustile after)
  (if (equal? 1 (set-count (tile-values focustile)))
  (remove-iter before focustile after)
  (uniqueval-iter before focustile after)))

;takes a board, passes it to board iter with focus, recurses on board if changed, otherwise returns board.
(define (solve-board board)
  (let ((ret (board-iter focus board)))
    (if (board-snapshot-changed? ret)
        (solve-board (board-snapshot-board ret))
        (board-snapshot-board ret))))

;solver for matrix, transforms to board, uses solve-board, untransforms back to matrix
(define (solve matrix)
  (untransform-tile 
   (solve-board (transform-tile matrix))))



(provide tile-snapshot board-snapshot board-iter board-acc remove remove-iter uniqueval uniqueval-iter uniqueval-struct uniqueval-struct-row-values uniqueval-struct-col-values uniqueval-struct-box-values focus solve-board solve)