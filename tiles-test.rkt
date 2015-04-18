#lang racket

(require rackunit "tiles.rkt")

(check-equal? all (set 1 2 3 4 5 6 7 8 9) "Test all")
(check-equal? (tile-solved? all) #f "Testing that more that one option returns not solved")
(check-equal? (tile-solved? (set 3)) #t "Testing singleton returns solved")


(check-equal? (transform (list (list 1 2 0) (list 0 5 0) (list 7 0 9))) 
              (list (list (set 1) (set 2) (set 1 2 3 4 5 6 7 8 9))
                    (list (set 1 2 3 4 5 6 7 8 9) (set 5) (set 1 2 3 4 5 6 7 8 9))
                    (list (set 7) (set 1 2 3 4 5 6 7 8 9) (set 9)))
              "Testing transforming small grid into nest lists of sets")

(check-equal? (transformflat (list (list 1 2 0) (list 0 5 0) (list 7 0 9))) 
              (list (set 1) (set 2) (set 1 2 3 4 5 6 7 8 9)
                    (set 1 2 3 4 5 6 7 8 9) (set 5) (set 1 2 3 4 5 6 7 8 9)
                    (set 7) (set 1 2 3 4 5 6 7 8 9) (set 9))
              "Testing transforming small grid into flat list of sets")

(check-equal? (get-row 80) 8 "Testing tile 80 is in row 8")
(check-equal? (get-row 54) 6 "Testing tile 54 is in row 6")
(check-equal? (get-row 4) 0 "Testing tile 4 is in row 0")

(check-equal? (get-col 80) 8 "Testing tile 80 is in col 8")
(check-equal? (get-col 54) 0 "Testing tile 55 is in col 0")
(check-equal? (get-col 4) 4 "Testing tile 4 is in col 4")

(check-equal? (get-box 80) 8 "Testing tile 80 is in box 8")
(check-equal? (get-box 54) 6 "Testing tile 55 is in box 6")
(check-equal? (get-box 4) 1 "Testing tile 4 is in box 1")

(check-equal? (make-tile 0 54) (tile 6 0 6 all) "Testing make tile deals with 0")
(check-equal? (make-tile 3 4) (tile 0 4 1 (set 3)) "Testing make tile deals with values")

(check-equal? (transform-tile (test-matrix)) (test-board) "Testing matrix to tile board")

(define (test-matrix-sets)
  (list
   (list all 2 all 1 7 8 all 3 all)
   (list all 4 all 3 all 2 all 9 all)
   (list 1 all all all all all all all 6)
   (list all all 8 6 all 3 5 all all)
   (list 3 all all all all all all all 4)
   (list all all 6 7 all 9 2 all all)
   (list 9 all all all all all all all 2)
   (list all 8 all 9 all 1 all 6 all)
   (list all 1 all 4 3 6 all 5 all)))

(check-equal? (untransform-tile (test-board))
              (test-matrix-sets)
              "Checking that untransform words")

(check-equal? (untransform-tile (transform-tile (test-matrix)))
              (test-matrix-sets)
              "Checking untransform is converse to transform")