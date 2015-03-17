#lang racket

(define all (set 1 2 3 4 5 6 7 8 9))

(define (tile-solved? t) (= 1 (set-count t)))





(provide all tile-solved?)