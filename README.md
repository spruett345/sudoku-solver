Sudoku puzzle solver
====================

Simple backtracking based Sudoku solver, written in Haskell.

Boards are represented as a Vector of BitSets, with each set holding
the possible values for the cell. Propogates constraints and then
backtracks over all possibilities until a proper solution is found.

Input
-----

Takes input from standard in, with one puzzle per line, until EOF.

For example:

```
$ cabal run
4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......

 4 1 7 | 3 6 9 | 8 2 5
 6 3 2 | 1 5 8 | 9 4 7
 9 5 8 | 7 2 4 | 3 1 6
----------------------
 8 2 5 | 4 3 7 | 1 6 9
 7 9 1 | 5 8 6 | 4 3 2
 3 4 6 | 9 1 2 | 7 5 8
----------------------
 2 8 9 | 6 4 3 | 5 7 1
 5 7 3 | 2 9 1 | 6 8 4
 1 6 4 | 8 7 5 | 2 9 3
```
