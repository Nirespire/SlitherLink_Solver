;;;; Sanjay Nair
;;;; University of Florida
;;;; Fall 2014


;; main AI subroutine
(defun initAI(board)
	(setq final nil)
	(initialPlacement board)	; place initial lines on the board
	(initialInvalid board)		; place initial invalid marks on the valid board
	

	;; debug
	;;(printBoard board)
	;;(terpri)
	;;(printBoard valid)
	;;(terpri)

	;; debug : write each board state to a text file to trace execution
	;;(let ((stream (open "test.txt" :direction :output :if-exists :supersede)))(write-line "-" stream)(close stream))

	;;(findFirstLine board)

	(princ "Solving...")
	(terpri)

	(if (not(null (findFirstLine board)))
		(time (mainsearch (copyboard board) (copyboard valid) (first (findFirstLine board)) (second (findFirstLine board)) (third (findFirstLine board))))
		(time (mainsearch (copyboard board) (copyboard valid) 1 1 'r))
	)
)



;; create a copy of the board to be used to mark invalid edges
(defun createValid(brd)
	(setq valid (copyBoard brd))
)




;; loop that repeats rules based on relative line placements to place additional lines on the current board configuration
(defun linePlacementLoop(board valid)
	(loop named outer
		for i from 1 to (getBoardHeight board)
		do
		(loop for j from 1 to (getRowWidth (car board))
			do

			;; for number squares
			(if (numberp (getElement board i j))
				(progn
					;; if num invalid edges around num == (4 - num), fill remaining edges with lines
					(if (equal (getElement board i j) (- 4 (countInvalidAround board valid i j)))
						(progn

							(if (and (equal (top board i j) '_) (not(null (top valid i j))))
								(setElement board (- i 1) j '-)
								(setElement valid (- i 1) j '-)
							)

							(if (and (equal (bottom board i j) '_) (not(null (bottom valid i j))))
								(setElement board (+ i 1) j '-)
								(setElement valid (+ i 1) j '-)
							)

							(if (and (equal (left board i j) '_) (not(null (left valid i j))))
								(setElement board i (- j 1) '-)
								(setElement valid i (- j 1) '-)
							)

							(if (and (equal (right board i j) '_) (not(null (right valid i j))))
								(setElement board i (+ j 1) '-)
								(setElement valid i (+ j 1) '-)
							)
						)
					)

					
					
					(if (equal (getElement board i j) '3)
						
						(progn
							;; 2 invalids on corner edges of 3
							;; bottom right
							(if (and (null(getElement valid (+ i 1) (+ j 2))) 
								(null(getElement valid (+ i 2) (+ j 1))))
								(progn
									(setElement board i (+ j 1) '-)
									(setElement valid i (+ j 1) '-)
									(setElement board (+ i 1) j '-)
									(setElement valid (+ i 1) j '-)
								)
							)

							;; bottom left
							(if (and (null(getElement valid (+ i 1) (- j 2))) 
								(null(getElement valid (+ i 2) (- j 1))))
								(progn
									(setElement board i (- j 1) '-)
									(setElement valid i (- j 1) '-)
									(setElement board (+ i 1) j '-)
									(setElement valid (+ i 1) j '-)
								)
							)

							;; top right
							(if (and (null(getElement valid (- i 1) (+ j 2))) 
								(null(getElement valid (- i 2) (+ j 1))))
								(progn
									(setElement board i (+ j 1) '-)
									(setElement valid i (+ j 1) '-)
									(setElement board (- i 1) j '-)
									(setElement valid (- i 1) j '-)
								)
							)

							;; top left
							(if (and (null(getElement valid (- i 1) (- j 2))) 
								(null(getElement valid (- i 2) (- j 1))))
								(progn
									(setElement board i (- j 1) '-)
									(setElement valid i (- j 1) '-)
									(setElement board (- i 1) j '-)
									(setElement valid (- i 1) j '-)
								)
							)


							;; lines on diagonal edges to 3
							;; +  +
							;;  3
							;; +  +
							;;    |
							;; +  +
							;; bottom right
							(if (equal (getElement board (+ i 2) (+ j 1)) '-)
								(progn
									(setElement board (- i 1) j '-)
									(setElement valid (- i 1) j '-)
									(setElement board i (- j 1) '-)
									(setElement valid i (- j 1) '-)
								)
							)

							;; bottom left
							(if (equal (getElement board (+ i 2) (- j 1)) '-)
								(progn
									(setElement board (- i 1) j '-)
									(setElement valid (- i 1) j '-)
									(setElement board i (+ j 1) '-)
									(setElement valid i (+ j 1) '-)
								)
							)

							;; top right
							(if (equal (getElement board (- i 2) (+ j 1)) '-)
								(progn
									(setElement board (+ i 1) j '-)
									(setElement valid (+ i 1) j '-)
									(setElement board i (- j 1) '-)
									(setElement valid i (- j 1) '-)
								)
							)

							;; top left
							(if (equal (getElement board (- i 2) (- j 1)) '-)
								(progn
									(setElement board (+ i 1) j '-)
									(setElement valid (+ i 1) j '-)
									(setElement board i (+ j 1) '-)
									(setElement valid i (+ j 1) '-)
								)
							)
						)
					)
				)
			)
		)
	)
)



;; loop that repeats rules based on relative line placements to place additional invalid marks on the valid board
(defun invalidPlacementLoop (board valid)
	(loop named outer
		for i from 1 to (getBoardHeight board)
		do
		(loop for j from 1 to (getRowWidth (car board))
			do

			;; for numbered squared
			(if (numberp (getElement board i j))
				(progn
					;; if num lines around a number = that number, remaining edges are invalid
					(if (equal (getElement board i j) (countAdjLines board i j 0))
						(progn

							(if (and (equal (top board i j) '_) (not(null (top valid i j))))
								(setElement valid (- i 1) j nil)
							)

							(if (and (equal (bottom board i j) '_) (not(null (bottom valid i j))))
								(setElement valid (+ i 1) j nil)
							)

							(if (and (equal (left board i j) '_) (not(null (left valid i j))))
								(setElement valid i (- j 1) nil)
							)

							(if (and (equal (right board i j) '_) (not(null (right valid i j))))
								(setElement valid i (+ j 1) nil)
							)
						)
					)

					;; if a 3 has lines on a corner, edges outside that corner are invalid
					(if (equal (getElement board i j) '3)
						(progn
							;; top left corner
							(if (and (equal (left board i j) '-) (equal (top board i j) '-))
								(progn
									(setElement valid (- i 1) (- j 2) nil)
									(setElement valid (- i 2) (- j 1) nil)
								)
							)

							;; top right corner
							(if (and (equal (right board i j) '-) (equal (top board i j) '-))
								(progn
									(setElement valid (- i 1) (+ j 2) nil)
									(setElement valid (- i 2) (+ j 1) nil)
								)
							)

							;; bottom left corner
							(if (and (equal (left board i j) '-) (equal (bottom board i j) '-))
								(progn
									(setElement valid (+ i 1) (- j 2) nil)
									(setElement valid (+ i 2) (- j 1) nil)
								)
							)

							;; bottom right corner
							(if (and (equal (right board i j) '-) (equal (bottom board i j) '-))
								(progn
									(setElement valid (+ i 1) (+ j 2) nil)
									(setElement valid (+ i 2) (+ j 1) nil)
								)
							)

							;; lines on diagonal edges to 3
							;; +  +
							;;  3
							;; +  +
							;;    |
							;; +  +

							;; bottom right
							(if (equal (getElement board (+ i 2) (+ j 1)) '-)
								(progn
									(setElement valid (+ i 1) (+ j 2) '-)
								)
							)

							;; bottom left
							(if (equal (getElement board (+ i 2) (- j 1)) '-)
								(progn
									(setElement valid (+ i 1) (- j 2) '-)
								)
							)

							;; top right
							(if (equal (getElement board (- i 2) (+ j 1)) '-)
								(progn
									(setElement valid (- i 1) (+ j 2) '-)
								)
							)

							;; top left
							(if (equal (getElement board (- i 2) (- j 1)) '-)
								(progn
									(setElement valid (- i 1) (- j 2) '-)
								)
							)
						)
					)
				)
			)
			
			
			;; for blank squares
			(if (equal (getElement board i j) 'x)
				(progn
					;; if blank square has 3 lines around it, the fourth edge is invalid
					(if(equal(countAdjLines board i j 0) 3)
						(progn
							(cond
								((equal (left board i j) '_)
									(setElement valid i (- j 1) nil)
								)

								((equal (right board i j) '_)
									(setElement valid i (+ j 1) nil)
								)

								((equal (top board i j) '_)
									(setElement valid (- i 1) j nil)
								)

								((equal (bottom board i j) '_)
									(setElement valid (+ i 1) j nil)
								)
							)
						)
					)
				)
			)


			;; for lines relations
			(if (equal (getElement board i j) '-)
				(progn
					;; consecutive vertical lines
					;;	|
					;; x+x
					;;	|
					(if(oddp j)
						(if (equal (getElement board (- i 2) j) '-)
							(progn
								(setElement valid (- i 1) (+ j 1) nil)
								(setElement valid (- i 1) (- j 1) nil)
							)
						)
					)

					;; consecutive horizontal lines
					;;  x
					;; -+-
					;;  x
					(if (oddp i)
						(if (equal (getElement board i (- j 2)) '-)
							(progn
								(setElement valid (- i 1) (- j 1) nil)
								(setElement valid (+ i 1) (- j 1) nil)
							)
						)
					)
				)
			)


			;; for vertices
			(if (equal (getElement board i j) '+)
				(progn
					;; if there are 3 invalid edges around a vertex, the last edge is also invalid
					(if (equal(countInvalidAround board valid i j) 3)
						(cond
							((equal (left valid i j) '_)
								(setElement valid i (- j 1) nil)
							)
							((equal (right valid i j) '_)
								(setElement valid i (+ j 1) nil)
							)
							((equal (top valid i j) '_)
								(setElement valid (- i 1) j nil)
							)
							((equal (bottom valid i j) '_)
								(setElement valid (+ i 1) j nil)
							)
						)
					)
				)
			)
		)
	)
)




;; main recursive search
;; pass in a board and run placement and invalid marking loops on it
;; recursively copy and call the search out of every valid edges expanding out of the current location denoted by row, col in the direction of dir
(defun mainSearch (board valid row col dir)

	;; debug, write all board states to a text file
	#|
	(let 
		((stream (open "test.txt" :direction :output :if-exists :append)))
		;;(write-line (boardToString board) stream) 
		(write-line (boardToString valid (getBoardHeight board) (getRowWidth (car board))) stream)
		(write-line (write-to-string (findFirstLine board)) stream)
		(close stream)
	)
	|#

	;;(printBoard board)

	;; if the current board is solved, print and save it
	(if (and (checkBoard1 board)
			(checkBoard2 board)
			(checkBoard3 board)
		)
		(progn
			(princ "SOLVED")
			(terpri)
			(setq final (copyBoard board))
			(printBoard final)
			(return-from mainSearch T)
		)
	)

	;; check if a solution was found, don't continue
	(if (not(null final))
		(return-from mainSearch T)
	)

	;; if the board is not solved and there is no line to expand from. discard
	(if (null (findFirstLine board))
		(return-from mainSearch nil)
	)

	;; If the current board has a vertex with more than 2 lines, its invalid. discard
	(if(not(checkBoard1AI board))
		(return-from mainSearch nil)
	)

	;;(if (checkBoard3 board)
	;;	(progn
			(setf row (first (findFirstLine board)))
			(setf col (second (findFirstLine board)))
			(setf dir (third (findFirstLine board)))
	;;	)
	;;)

	;; copy the current board and modify the copy
	(let ( 	(newBoard (copyBoard board))
			(newValid (copyBoard valid))
		)

		;; add any immediate lines and mark invalid locations for this configuration
		(linePlacementLoop newBoard newValid)
		(invalidPlacementLoop newBoard newValid)

		(cond
			((equal dir 'r)
				;; right is valid
				(if (and (not(null (right valid row col)))
						(not(null (right board row col)))
						(not(equal (right board row col) '-))
					)
					(progn
						(setElement newboard row (+ col 1) '-)
						(setElement newValid row (+ col 1) '-)
						(mainSearch newboard newValid row (+ col 2) 'r)
						(setElement newboard row (+ col 1) '_)
						(setElement newValid row (+ col 1) '_)
					)
				)

				;; top is valid
				(if (and (not(null (top valid row col)))
						(not(null (top board row col)))
						(not(equal (top board row col) '-))
					)
					(progn
						(setElement newboard (- row 1) col '-)
						(setElement newValid (- row 1) col '-)
						(mainSearch newboard newValid (- row 2) col 't)
						(setElement newboard (- row 1) col '_)
						(setElement newValid (- row 1) col '_)
					)

				)
				;; bottom is valid
				(if (and (not(null (bottom valid row col)))
						(not(null (bottom board row col)))
						(not(equal (bottom board row col) '-))
					)
					(progn
						(setElement newboard (+ row 1) col '-)
						(setElement newValid (+ row 1) col '-)
						(mainSearch newboard newValid (+ row 2) col 'b)
						(setElement newboard (+ row 1) col '_)
						(setElement newValid (+ row 1) col '_)
					)

				)
			)

			((equal dir 'l)
				(if (and (not(null (top valid row col)))
						(not(null (top board row col)))
						(not(equal (top board row col) '-))
					)
					(progn
						(setElement newboard (- row 1) col '-)
						(setElement newValid (- row 1) col '-)
						(mainSearch newboard newValid (- row 2) col 't)
						(setElement newboard (- row 1) col '_)
						(setElement newValid (- row 1) col '_)
					)
				)

				(if (and (not(null (left valid row col)))
						(not(null (left board row col)))
						(not(equal (left board row col) '-))
					)
					(progn
						(setElement newboard row (- col 1) '-)
						(setElement newValid row (- col 1) '-)
						(mainSearch newboard newValid row (- col 2) 'l)
						(setElement newboard row (- col 1) '_)
						(setElement newValid row (- col 1) '_)
					)

				)
				(if (and (not(null (bottom valid row col)))
						(not(null (bottom board row col)))
						(not(equal (bottom board row col) '-))
					)
					(progn
						(setElement newboard (+ row 1) col '-)
						(setElement newvalid (+ row 1) col '-)
						(mainSearch newboard newValid (+ row 2) col 'b)
						(setElement newboard (+ row 1) col '_)
						(setElement newvalid (+ row 1) col '_)
					)

				)
			)
			((equal dir 't)
				(if (and (not(null (right valid row col)))
						(not(null (right board row col)))
						(not(equal (right board row col) '-))
					)
					(progn
						(setElement newboard row (+ col 1) '-)
						(setElement newvalid row (+ col 1) '-)
						(mainSearch newboard newValid row (+ col 2) 'r)
						(setElement newboard row (+ col 1) '_)
						(setElement newvalid row (+ col 1) '_)
					)
				)

				(if (and (not(null (left valid row col)))
						(not(null (left board row col)))
						(not(equal (left board row col) '-))
					)
					(progn
						(setElement newboard row (- col 1) '-)
						(setElement newvalid row (- col 1) '-)
						(mainSearch newboard newValid row (- col 2) 'l)
						(setElement newboard row (- col 1) '_)
						(setElement newvalid row (- col 1) '_)
					)

				)
				(if (and (not(null (top valid row col)))
						(not(null (top board row col)))
						(not(equal (top board row col) '-))
					)
					(progn
						(setElement newboard (- row 1) col '-)
						(setElement newvalid (- row 1) col '-)
						(mainSearch newboard newValid (- row 2) col 't)
						(setElement newboard (- row 1) col '_)
						(setElement newvalid (- row 1) col '_)
					)

				)
			)

			((equal dir 'b)
				(if (and (not(null (right valid row col)))
						(not(null (right board row col)))
						(not(equal (right board row col) '-))
					)
					(progn
						(setElement newboard row (+ col 1) '-)
						(setElement newvalid row (+ col 1) '-)
						(mainSearch newboard newValid row (+ col 2) 'r)
						(setElement newboard row (+ col 1) '_)
						(setElement newvalid row (+ col 1) '_)
					)
				)

				(if (and (not(null (left valid row col)))
						(not(null (left board row col)))
						(not(equal (left board row col) '-))
					)
					(progn
						(setElement newboard row (- col 1) '-)
						(setElement newvalid row (- col 1) '-)
						(mainSearch newboard newValid row (- col 2) 'l)
						(setElement newboard row (- col 1) '_)
						(setElement newvalid row (- col 1) '_)
					)

				)
				(if (and (not(null (bottom valid row col)))
						(not(null (bottom board row col)))
						(not(equal (bottom board row col) '-))
					)
					(progn
						(setElement newboard (+ row 1) col '-)
						(setElement newvalid (+ row 1) col '-)
						(mainSearch newboard newValid (+ row 2) col 'b)
						(setElement newboard (+ row 1) col '_)
						(setElement newvalid (+ row 1) col '_)
					)
				)
			)
		)
	)
)




;; place initial lines on the board based on static rules of relative number placements
(defun initialPlacement(board)
	;;; place initial bars on the board
	(loop named outer
		for i from 0 to (getBoardHeight board)
		do
		(loop for j from 0 to (getRowWidth (car board))
			do

			;; places bars in bewteen and on the sides of adjacent 3's
			(if (equal '3 (getElement board i j))
				(progn
					;; bottom
					(if (equal (getElement board (+ i 2) j) '3)
						(progn
							(setElement board (+ i 3) j '-)
							(if (not(null(getElement board (+ i 4) j)))
								(setElement board (+ i 1) j '-)
							)
							(setElement board (- i 1) j '-)
						)
					)

					;; top
					(if (equal (getElement board (- i 2) j) '3)
						(progn
							(setElement board (- i 3) j '-)
							(if (not(null(getElement board (- i 4) j)))
								(setElement board (- i 1) j '-)
							)
							(setElement board (+ i 1) j '-)
						)
					)

					;; right
					(if (equal (getElement board i (+ j 2)) '3)
						(progn
							(setElement board i (+ j 3) '-)
							(if (not(null(getElement board i (+ j 4))))
								(setElement board i (+ j 1) '-)
							)
							(setElement board i (- j 1) '-)
						)
					)

					;; left
					(if (equal (getElement board i (- j 2)) '3)
						(progn
							(setElement board i (- j 3) '-)
							(if (not(null(getElement board i (- j 4))))
								(setElement board i (- j 1) '-)
							)
							(setElement board i (+ j 1) '-)
						)
					)
				)
			)

			;; place bars around the 3 if there is a 0 adjacent to it
			(if (equal '3 (getElement board i j))
				(progn

					;; bottom
					(if (equal (getElement board (+ i 2) j) '0)
						(progn
							(setElement board (- i 1) j '-)
							(setElement board i (- j 1) '-)
							(setElement board i (+ j 1) '-)

							(setElement board (+ i 1)  (- j 2) '-)
							(setElement board (+ i 1)  (+ j 2) '-)
						)
					)

					;; top
					(if (equal (getElement board (- i 2) j) '0)
						(progn
							(setElement board (+ i 1) j '-)
							(setElement board i (- j 1) '-)
							(setElement board i (+ j 1) '-)

							(setElement board (- i 1) (- j 2) '-)
							(setElement board (- i 1) (+ j 2) '-)
						)
					)

					;; right
					(if (equal (getElement board i (+ j 2)) '0)
						(progn
							(setElement board (+ i 1) j '-)
							(setElement board (- i 1) j '-)
							(setElement board i (- j 1) '-)

							(setElement board (- i 2) (+ j 1) '-)
							(setElement board (+ i 2) (+ j 1) '-)
						)
					)

					;; left
					(if (equal (getElement board i (- j 2)) '0)
						(progn
							(setElement board (+ i 1) j '-)
							(setElement board (- i 1) j '-)
							(setElement board i (+ j 1) '-)

							(setElement board (- i 2) (- j 1) '-)
							(setElement board (+ i 2) (- j 1) '-)
						)
					)
				)
			)

			;;place bars on corners of diagonal 3's
			(if (equal '3 (getElement board i j))
				(progn

					;; top left
					(if (equal (getElement board (- i 2) (- j 2)) '3)
						(progn
							(setElement board i (+ j 1) '-)
							(setElement board (+ i 1) j '-)

							(setElement board (- i 2) (- j 3) '-)
							(setElement board (- i 3) (- j 2) '-)
						)
					)

					;; top right
					(if (equal (getElement board (- i 2) (+ j 2)) '3)
						(progn
							(setElement board i (- j 1) '-)
							(setElement board (+ i 1) j '-)

							(setElement board (- i 2) (+ j 3) '-)
							(setElement board (- i 3) (+ j 2) '-)
						)
					)

					;; bottom left
					(if (equal (getElement board (+ i 2) (- j 2)) '3)
						(progn
							(setElement board i (+ j 1) '-)
							(setElement board (- i 1) j '-)

							(setElement board (+ i 2) (- j 3) '-)
							(setElement board (+ i 3) (- j 2) '-)
						)
					)

					;; bottom left
					(if (equal (getElement board (+ i 2) (+ j 2)) '3)
						(progn
							(setElement board i (- j 1) '-)
							(setElement board (- i 1) j '-)

							(setElement board (+ i 2) (+ j 3) '-)
							(setElement board (+ i 3) (+ j 2) '-)
						)
					)
				)
			)
		

			;; place bars on the corner common to a 3 and 0 diagonal to each other
			;; 0 _ x
			;; _ * -
			;; x - 3
			(if (equal '3 (getElement board i j))
				(progn

					;; top left
					(if (equal (getElement board (- i 2) (- j 2)) '0)
						(progn
							(setElement board (- i 1) j '-)
							(setElement board i (- j 1) '-)
						)
					)

					;; top right
					(if (equal (getElement board (- i 2) (+ j 2)) '0)
						(progn
							(setElement board (- i 1) j '-)
							(setElement board i (+ j 1) '-)
						)
					)

					;; bottom left
					(if (equal (getElement board (+ i 2) (- j 2)) '0)
						(progn
							(setElement board (+ i 1) j '-)
							(setElement board i (- j 1) '-)
						)
					)

					;; bottom left
					(if (equal (getElement board (+ i 2) (+ j 2)) '0)
						(progn
							(setElement board (+ i 1) j '-)
							(setElement board i (+ j 1) '-)
						)
					)
				)
			)


			;; place bars on boundary corners of the board with 3's
			(if (equal '3 (getElement board i j))
				(progn

					;; top left corner
					(if (and (null (getElement board (- i 2) j)) (null (getElement board i (- j 2))))
						(progn
							(setElement board (- i 1) j '-)
							(setElement board i (- j 1) '-)
						)
					)

					;; top right
					(if (and (null (getElement board (- i 2) j)) (null (getElement board i (+ j 2))))
						(progn
							(setElement board (- i 1) j '-)
							(setElement board i (+ j 1) '-)
						)
					)

					;; bottom left
					(if (and (null (getElement board (+ i 2) j)) (null (getElement board i (- j 2))))
						(progn
							(setElement board (+ i 1) j '-)
							(setElement board i (- j 1) '-)
						)
					)

					;; bottom right
					(if (and (null (getElement board (+ i 2) j)) (null (getElement board i (+ j 2))))
						(progn
							(setElement board (+ i 1) j '-)
							(setElement board i (+ j 1) '-)
						)
					)
				)
			)

			;; place bars on boundary corners of board with 2's
			(if (equal '2 (getElement board i j))
				(progn

					;; top left corner
					(if (and (null (getElement board (- i 2) j)) (null (getElement board i (- j 2))))
						(progn
							(setElement board (+ i 2) (- j 1) '-)
							(setElement board (- i 1) (+ j 2) '-)
						)
					)

					;; top right
					(if (and (null (getElement board (- i 2) j)) (null (getElement board i (+ j 2))))
						(progn
							(setElement board (+ i 2) (+ j 1) '-)
							(setElement board (- i 1) (- j 2) '-)
						)
					)

					;; bottom left
					(if (and (null (getElement board (+ i 2) j)) (null (getElement board i (- j 2))))
						(progn
							(setElement board (- i 2) (- j 1) '-)
							(setElement board (+ i 1) (+ j 2) '-)
						)
					)

					;; bottom right
					(if (and (null (getElement board (+ i 2) j)) (null (getElement board i (+ j 2))))
						(progn
							(setElement board (- i 2) (+ j 1) '-)
							(setElement board (+ i 1) (- j 2) '-)
						)
					)
				)
			)
		)
	)
)



;; place initial invalid marks on the valid board by static rules based on relative number placement
(defun initialInvalid(board)
	(createValid board)
	(loop named outer
		for i from 0 to (getBoardHeight board)
		do

		(loop for j from 0 to (getRowWidth (car board))
			do

			;; all edges around a 0 are invalid
			(if (equal '0 (getElement board i j))
				(progn
					(setElement valid (+ i 1) j nil)
					(setElement valid (- i 1) j nil)
					(setElement valid i (+ j 1) nil)
					(setElement valid i (- j 1) nil)

					;; check if 0 is on a boundary

					;; bottom
					(if (null (getElement board (+ i 2) j))
						(progn
							(setElement valid (+ i 1) (+ j 2) nil)
							(setElement valid (+ i 1) (- j 2) nil)
						)
					)

					;; top
					(if (null (getElement board (- i 2) j))
						(progn
							(setElement valid (- i 1) (+ j 2) nil)
							(setElement valid (- i 1) (- j 2) nil)
						)
					)

					;; right
					(if (null (getElement board i (+ j 2)))
						(progn
							(setElement valid (- i 2) (+ j 1) nil)
							(setElement valid (+ i 2) (+ j 1) nil)
						)
					)

					;; left
					(if (null (getElement board i (- j 2)))
						(progn
							(setElement valid (- i 2) (- j 1) nil)
							(setElement valid (+ i 2) (- j 1) nil)
						)
					)
				)	
			)

			;; ends of bars in bewteen adjacent 3's are invalid
			(if (equal '3 (getElement board i j))
				(progn
					;; bottom
					(if (equal (getElement board (+ i 2) j) '3)
						(progn
							(setElement valid (+ i 1) (- j 2) nil)
							(setElement valid (+ i 1) (+ j 2) nil)
						)
					)

					;; top
					(if (equal (getElement board (- i 2) j) '3)
						(progn
							(setElement valid (- i 1) (- j 2) nil)
							(setElement valid (- i 1) (+ j 2) nil)
						)
					)

					;; right
					(if (equal (getElement board i (+ j 2)) '3)
						(progn
							(setElement valid (+ i 2) (+ j 1) nil)
							(setElement valid (- i 2) (+ j 1) nil)
						)
					)

					;; left
					(if (equal (getElement board i (- j 2)) '3)
						(progn
							(setElement valid (+ i 2) (- j 1) nil)
							(setElement valid (- i 2) (- j 1) nil)
						)
					)
				)
			)

			;; lines outside corners of diagonal 3's are invalid
			(if (equal '3 (getElement board i j))
				(progn

					;; top left
					(if (equal (getElement board (- i 2) (- j 2)) '3)
						(progn
							(setElement valid (+ i 1) (+ j 2) nil)
							(setElement valid (+ i 2) (+ j 1) nil)

							(setElement valid (- i 3) (- j 4) nil)
							(setElement valid (- i 4) (- j 3) nil)
						)
					)

					;; top right
					(if (equal (getElement board (- i 2) (+ j 2)) '3)
						(progn
							(setElement valid (+ i 1) (- j 2) nil)
							(setElement valid (+ i 2) (- j 1) nil)

							(setElement valid (- i 3) (+ j 4) nil)
							(setElement valid (- i 4) (+ j 3) nil)
						)
					)

					;; bottom left
					(if (equal (getElement board (+ i 2) (- j 2)) '3)
						(progn
							(setElement valid (- i 1) (+ j 2) nil)
							(setElement valid (- i 2) (+ j 1) nil)

							(setElement valid (+ i 3) (- j 4) nil)
							(setElement valid (+ i 4) (- j 3) nil)
						)
					)

					;; bottom left
					(if (equal (getElement board (+ i 2) (+ j 2)) '3)
						(progn
							(setElement valid (- i 1) (- j 2) nil)
							(setElement valid (- i 2) (- j 1) nil)

							(setElement valid (+ i 3) (+ j 4) nil)
							(setElement valid (+ i 4) (+ j 3) nil)
						)
					)
				)
			)

			;; 1's in corners, lines  touching board boundary are invalid
			(if (equal '1 (getElement board i j))
				(progn

					;; top left corner
					(if (and (null (getElement board (- i 2) j)) (null (getElement board i (- j 2))))
						(progn
							(setElement valid i (- j 1) nil)
							(setElement valid (- i 1) j nil)
						)
					)

					;; top right
					(if (and (null (getElement board (- i 2) j)) (null (getElement board i (+ j 2))))
						(progn
							(setElement valid i (+ j 1) nil)
							(setElement valid (- i 1) j nil)
						)
					)

					;; bottom left
					(if (and (null (getElement board (+ i 2) j)) (null (getElement board i (- j 2))))
						(progn
							(setElement valid i (- j 1) nil)
							(setElement valid (+ i 1) j nil)
						)
					)

					;; bottom right
					(if (and (null (getElement board (+ i 2) j)) (null (getElement board i (+ j 2))))
						(progn
							(setElement valid i (+ j 1) nil)
							(setElement valid (+ i 1) j nil)
						)
					)
				)
			)

			;; 3 adjacent to 0, lines around outside of 3 invalid
			(if (equal '3 (getElement board i j))
				(progn
					;; bottom
					(if (equal (getElement board (+ i 2) j) '0)
						(progn
							(setElement valid (- i 1) (- j 2) nil)
							(setElement valid (- i 1) (+ j 2) nil)

							(setElement valid (- i 2) (- j 1) nil)
							(setElement valid (- i 2) (+ j 1) nil)
						)
					)

					;; top
					(if (equal (getElement board (- i 2) j) '0)
						(progn
							(setElement valid (+ i 1) (- j 2) nil)
							(setElement valid (+ i 1) (+ j 2) nil)

							(setElement valid (+ i 2) (- j 1) nil)
							(setElement valid (+ i 2) (+ j 1) nil)
						)
					)

					;; right
					(if (equal (getElement board i (+ j 2)) '0)
						(progn
							(setElement valid (- i 2) (- j 1) nil)
							(setElement valid (+ i 2) (- j 1) nil)

							(setElement valid (- i 1) (- j 2) nil)
							(setElement valid (+ i 1) (- j 2) nil)
						)
					)

					;; left
					(if (equal (getElement board i (- j 2)) '0)
						(progn
							(setElement valid (- i 2) (+ j 1) nil)
							(setElement valid (+ i 2) (+ j 1) nil)

							(setElement valid (- i 1) (+ j 2) nil)
							(setElement valid (+ i 1) (+ j 2) nil)
						)
					)
				)
			)
		)
	)
)