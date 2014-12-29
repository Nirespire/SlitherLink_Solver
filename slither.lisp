;;;; Sanjay Nair
;;;; University of Florida
;;;; Fall 2014


;; load all support files
(load "boards.lisp")
(load "validators.lisp")
(load "solver.lisp")
(load "solverUtil.lisp")



(defun compileProject()
	(compile-file "slither.lisp")
	(compile-file "validators.lisp")
	(compile-file "boards.lisp")
	(compile-file "solver.lisp")
	(compile-file "solverUtil.lisp")
)


;; NOTE: will only work if compiled in CLISP runtime environment
(defun loadProject()
	(load "slither.fas")
	(load "validators.fas")
	(load "boards.fas")
	(load "solver.fas")
	(load "solverUtil.fas")
)



;; function to convert a board representation into a string and return it
(defun boardToString(board &optional height width)
	(let ((output "") (y) (x))
		(setq output (concatenate 'string output (format nil "~C~C" #\return #\linefeed)))	; pad output with some newlines (only tested with windows)
		(if (not(null height))
			(setf y height)
			(setf y (getBoardHeight board))
		)
		(loop named outer
			for i from 1 to y
			do
			(if (not(null width))
				(setf x width)
				(setf x (getRowWidth (car board)))
			)
			(loop for j from 1 to x
				do
				
				(cond
					((equal (getElement board i j) '_)
						(setq output (concatenate 'string output " "))
					)

					((null (getElement board i j))
						(setq output (concatenate 'string output "x"))
					)
					((equal (getElement board i j) 'x)
						(setq output (concatenate 'string output " "))
					)
					((equal (getElement board i j) '+)
						(setq output (concatenate 'string output "+"))
					)

					((equal (getElement board i j) '-)
						(if (oddp i)
							(setq output (concatenate 'string output "-"))	; vertical or horizontal line depending on the row
							(setq output (concatenate 'string output "|"))
						)
					)
					(T
						(setq output (concatenate 'string output (write-to-string(getElement board i j)) ""))	; dirty string conversion
					)
				)	
			)
			(setq output (concatenate 'string output (format nil "~C"  #\linefeed)))
		)
		(setq output (concatenate 'string output (format nil "~C~C" #\return #\linefeed)))
		;;(princ output)	; debug
	)
)



;; prints a board to the console with row and column numbers
(defun printBoard(brd)
	(princ "     ")
	(setf b 0)
	(loop for i from 1 to (second (getBoardDimen brd))
			for j from 1 to 9
		do(
			progn
				(princ ".")
				(princ j)
				(setf b (1+ b))
				;;(princ " ")
		)
	)
	(setf b (1+ b))
	(loop for i from b to (second (getBoardDimen brd))
		do(
			progn
				(princ ".")
				(princ (mod i 10))
				;;(princ " ")
		)
	)
	(princ ".")
	(terpri)
	(terpri)
	(printBoard2 brd T 1)
)



;; helper function for printBoard. oddRow is either T or nil and determines
;; if lines should be printed vertically or horizontally
;; count keeps track of the row number
(defun printBoard2(brd oddRow count)
	(cond
		((null brd)
			T
		)
		(T
			(if(null oddRow)
				(progn
					(princ ".")
					(princ count)
					(if (< count 10)
						(princ ".  ")
						(princ ". ")
					)
					(setq count (1+ count))
				)
				(progn
					(princ "     ")
				)
			)
			(printRow (car brd) oddRow)
			(terpri)
			(printBoard2 (cdr brd) (not oddRow) count)
		)
	)
)



;; helper function for printBoard. Recursively prints a row, whether odd or even
(defun printRow(row oddRow)
	(cond
		((null row)
			T
		)
		
		;; odd row
		;; row with only dots and horizontal bars
		((equal T oddRow)
			(cond
				;; dot
				((equal (car row) '+)
					(princ "+")
				)
				;; no line
				((equal (car row) '_)						
					(princ " ")
				)
				;; horizontal line
				((equal (car row) '-)
					(princ "-")
				)
				;; if printing visited array
				((null (car row))
					(princ "x")
				)
				
				(T
					(princ (car row));
				)
			)
			(printRow (cdr row) oddRow)
		)
		
		;; even row
		;; row with only vertical bars and numbers
		(T
			(cond
				;; no line
				((equal (car row) '_)
					(princ " ")
				)
				;; no number
				((equal (car row) 'x)
					(princ " ")
				)
				;; vertical line
				((equal (car row) '-)
					(princ "|")
				)
				;; if printing visited array
				((null (car row))
					(princ "x")
				)
				;; number
				(T
					(princ (car row));
				)
			)
			(printRow (cdr row) oddRow)
		)
	)
)



(defun printMenu()
	(terpri)
	(princ "Enter a row, column and position to toggle a line ([R]ight,[L]eft,[T]op,[B]ottom): ")
	(terpri)
)



;; returns a true copy of the nested list passed in that can be independently modified
(defun copyBoard (brd)
    (if (null brd)
        nil
        (cons (copy-list (car brd)) (copyBoard (rest brd)))
    )
)



;; get the playable dimensions of the board, the dimensions the player cares about
(defun getBoardDimen(brd)
	(mapcar #' (lambda (x) (/ x 2)) (mapcar #'(lambda (x) (1- x)) (getRealBoardDimen brd)))
)



;; get the array size of the board object, the dimensions the programmer cares about
(defun getRealBoardDimen(brd)
	(list (getBoardHeight brd) (getRowWidth (car brd)))
)



;; get board array height
(defun getBoardHeight(brd)
	(cond
		((null (car brd))
			0
		)
		(T
			(1+ (getBoardHeight (cdr brd)))
		)
	)
)



;; get board array width
(defun getRowWidth(row)
	(cond
		((null (car row))
			0
		)
		(T
			(1+ (getRowWidth (cdr row)))
		)
	)
)


;; TODO
;; get the element at the square specified by row, col
;; row and col are specified with respect to the gameboard, not the board array
(defun getNumber(brd row col)
	(setq tempRow (* 2 row))
	(setq tempCol (* 2 col))
	(cond
		((and (mod tempRow 2) (mod tempCol 2))
			(getElement brd tempRow tempCol)
		)
		(T
			nil
		)
	)
)



;; return the element at row,col
;; row and col are specified with respect to the board array
(defun getElement(brd row col)
	(if (or (minusp (1- col)) (minusp (1- row)))
		nil
		
		(nth (1- col) (nth (1- row) brd))
	)	
)



;; set the element at row,col
;; row and col are specified with respect to the board array
(defun setElement(brd row col val)
	(if (or (minusp (1- col)) (minusp (1- row)) (<= (getBoardHeight brd) (1- row)) (<= (getRowWidth brd) (1- col)))
		nil
		(setf (nth (1- col)(nth (1- row) brd)) val)
	)
)



;; for use when the user is interacting with the gameboard
;; toggle a line around square row, col and position T, B, L or R
;; row and col are specified with respect to the gameboard, not the board array
(defun toggleLine (brd row col pos)
	(cond
		((not(inBounds  brd (* 2 col) (* 2 row)))
			nil
		)
		((containsLine brd row col pos)
			(clearLine brd row col pos)
		)
		(T
			(setLine brd row col pos)
		)
	)
)



;; returns T if the playable location row, col contains a set line '-
(defun containsLine (brd row col pos)
	(setq tempRow (* 2 row))
	(setq tempCol (* 2 col))
	
	(cond
		((equal pos 'L)
			(equal (getElement brd tempRow (1- tempCol)) '-)
		)
		((equal pos 'R)
			(equal (getElement brd tempRow (1+ tempCol)) '-)
		)
		((equal pos 'T)
			(equal (getElement brd (1- tempRow) tempCol) '-)
		)
		((equal pos 'B)
			(equal (getElement brd (1+ tempRow) tempCol) '-)
		)
		(T
			nil
		)
	)
)



;; return T if the index provided is within bounds of the gameboard array
;; should be depreciated
(defun inBounds (brd row col)
	(cond
		((null (getElement brd row col))
			nil
		)
		(T
			T
		)
	)
)



;; helper function for toggleLine
(defun setLine(brd row col pos)
	(setq tempRow (* 2 row))
	(setq tempCol (* 2 col))
	
	(cond
		((not (inBounds brd tempRow tempCol))
			nil
		)
		((equal pos 'L)
			(setElement brd tempRow (1- tempCol) '-)
		)
		((equal pos 'R)
			(setElement brd tempRow (1+ tempCol) '-)
		)
		((equal pos 'T)
			(setElement brd (1- tempRow) tempCol '-)
		)
		((equal pos 'B)
			(setElement brd (1+ tempRow) tempCol '-)
		)
		(T
			nil
		)
	)
)



;; helper function for toggleLine
(defun clearLine(brd row col pos)
	(setq tempRow (* 2 row))
	(setq tempCol (* 2 col))
	
	(cond
		((not (inBounds brd tempRow tempCol))
			nil
		)
		((equal pos 'L)
			(setElement brd tempRow (1- tempCol) '_)
		)
		((equal pos 'R)
			(setElement brd tempRow (1+ tempCol) '_)
		)
		((equal pos 'T)
			(setElement brd (1- tempRow) tempCol '_)
		)
		((equal pos 'B)
			(setElement brd (1+ tempRow) tempCol '_)
		)
	)
)



;; clears all lines from a board
(defun clearBoard(brd)
	(cond
		((null brd)
			T
		)
		(T
			(clearRow (car brd))
			(clearBoard (cdr brd))
		)
	)
)



;; helper function for clearBoard
(defun clearRow(row)
	(cond
		((null row)
			T
		)
		(T

			(if(equal (car row) '-)
				(setf (car row) '_)
			)

			(clearRow (cdr row))
		)
	)
)



;; main user function
;; the user may select from a list of preprogrammed boards
;; the user may choose to solve the board themselves or have the AI subroutine solve the board for them
(defun slither()
	(princ "WELCOME TO SLITHER")
	(terpri)
	(princ "Select a board and begin placing bars on the board")
	(terpri)
	(princ "by typing in the coordinate of the square and which location")
	(terpri)
	(princ "you would like to place the bar (ex t,b,l,r)")
	(terpri)
	(princ "NOTE: if a board has > 9 columns, still enter coordinates according to column")
	(terpri)
	(princ "number. Ignore repeating 1 digit numbering for larger boards.")
	(terpri)
	(princ "The game is over when a closed loop is formed and each number square has")
	(terpri)
	(princ "a correct number of bars around it")
	(terpri)
	(princ "------------------------------------------------------------------------")
	(terpri)

	(loop
		(princ "Choose which board you would like to play on")
		(terpri)

		(princ "Enter 1-15 for tournament boards 1-15 respectively")
		(terpri)
	
		(setq choice (read))
	
		(cond
			((equal choice 1)
				(setq gameBoard tourney1)
			)
			((equal choice 2)
				(setq gameBoard tourney2)
			)
			((equal choice 3)
				(setq gameBoard tourney3)
			)
			((equal choice 4)
				(setq gameBoard tourney4)
			)
			((equal choice 5)
				(setq gameBoard tourney5)
			)
			((equal choice 6)
				(setq gameBoard tourney6)
			)
			((equal choice 7)
				(setq gameBoard tourney7)
			)
			((equal choice 8)
				(setq gameBoard tourney8)
			)
			((equal choice 9)
				(setq gameBoard tourney9)
			)
			((equal choice 10)
				(setq gameBoard tourney10)
			)
			((equal choice 11)
				(setq gameBoard tourney11)
			)
			((equal choice 12)
				(setq gameBoard tourney12)
			)
			((equal choice 13)
				(setq gameBoard tourney13)
			)
			((equal choice 14)
				(setq gameBoard tourney14)
			)
			((equal choice 15)
				(setq gameBoard tourney15)
			)
			(T
				(setq gameBoard nil)
				(princ "Invalid choice")
				(terpri)
			)
		)
		(terpri)
		(princ "Board Chosen:")
		(terpri)
		(printBoard gameBoard)
		(terpri)

		(princ "(1) Would you like to solve or (2) have the puzzle solved for you?")
		(terpri)
		(setq choice (read))

		;; user chooses to solve the board
		(if (equal choice 1)
			(progn
				(setq visited (createVisited gameBoard))

				(cond 
					((not(null gameBoard))
						(loop
							(princ "---------------------------------")
							(terpri)
							(printBoard gameBoard)
							(terpri)
							(princ "---------------------------------")
							(printMenu)
							(setq row (read))
							(cond 
								((equal row 'exit)
									(princ "Goodbye")
									(return)
								)
							)
							(setq col (read))
							(setq pos (read))
				
							(toggleLine gameBoard row col pos)

							(cond
								((and(checkBoard1 gameBoard) (checkBoard2 gameBoard))
									(cond
										((checkBoard3 gameBoard)
											(princ "---------------------------------")
											(terpri)
											(printBoard gameBoard)
											(princ "You won!")
											(terpri)
											(princ "Play again? (Y/N)")
											(terpri)
											(setq choice (read))
											(return)
										)
									)
								)
								(T
									;; nada
								)
							)
						)

						(if(or(equal choice 'N) (equal row 'exit))
							(return)
						)
					)
				)

				(if(or(equal choice 'N) (equal row 'exit))
					(return T)
				)

				(clearBoard gameBoard)
			)
			
			;; else, AI will solve board
			(progn
				(initAI gameBoard)
				(clearBoard gameBoard)

				(princ "Play again? (Y/N)")
				(terpri)
				(setq choice (read))
				
				(if (equal choice 'N)
					(return-from slither T)
				)
			)
		)
	)
)