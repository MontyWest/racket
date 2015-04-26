# racket
## Sudoku solver in Racket

### Author:
Monty West - mwest06

### About:

(transform matrix) function can be found in tiles.rkt, unit test in tiles-test.rkt (using sudoku puzzle in spec)

(solve matrix) function can be found in solver.rkt, unit test in solver-test.rkt (using sudoku puzzle found in spec)

### Running:

- Clone the repo and move to the directory

- Run:
```
$ racket
> (require (file "tiles.rkt"))
> (require (file "solver.rkt"))
```

- Then the following commands are available
```
; Prints an example sudoku
> (test-matrix)

; Solves a sudoku, if it can't gives a set of possibilities for the tile
> (solve <sudoku-matrix>)
; Using example board
> (solve (test-matrix))
```

- You can define your own sudoku. Use a list of lists that represent the rows, with unknowns as '0' e.g.
```
'((2 4 0 0 3 0 0 0 8)
  (0 0 0 5 8 4 0 0 0)
	...)

; Solved as follow:
(solve '((2 4 0 0 3 0 0 0 8)
  		 (0 0 0 5 8 4 0 0 0)
			...))
```
