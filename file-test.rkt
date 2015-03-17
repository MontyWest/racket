#lang racket

(require rackunit "file.rkt")

(check-equal? all (set 1 2 3 4 5 6 7 8 9) "Test all")
(check-equal? (tile-solved? all) #f "Testing that more that one option returns not solved")
(check-equal? (tile-solved? (set 3)) #t "Testing singleton returns solved")
