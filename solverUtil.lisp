;;;; Sanjay Nair
;;;; University of Florida
;;;; Fall 2014

;; -------------------------------------------------------------------------
;; set of helper functions to return one of 4 adjacent location from i, j
(defun top(board i j)
	(getElement board (- i 1) j)
)

(defun bottom(board i j)
	(getElement board (+ i 1) j)
)

(defun left(board i j)
	(getElement board i (- j 1))
)

(defun right(board i j)
	(getElement board i (+ j 1))
)
;; -------------------------------------------------------------------------



;; returns the number of set lines adjacent to index i, j
(defun countLinesAround(board i j)
	(let ((count 0))
		(if (equal (top board i j) '-)
			(setq count (1+ count))
		)
		(if (equal (bottom board i j) '-)
			(setq count (1+ count))
		)
		(if (equal (left board i j) '-)
			(setq count (1+ count))
		)
		(if (equal (right board i j) '-)
			(setq count (1+ count))
		)
		count
	)
)



;; returns the number of invalid marks on the valid board adjacent to index i, j
(defun countInvalidAround (board valid i j)
	(let ((count 0))
		(if (equal (top valid i j) 'nil)
			(setq count (1+ count))
		)
		(if (equal (bottom valid i j) 'nil)
			(setq count (1+ count))
		)
		(if (equal (left valid i j) 'nil)
			(setq count (1+ count))
		)
		(if (equal (right valid i j) 'nil)
			(setq count (1+ count))
		)
		count
	)
)



;; returns the index of the vertex of the first open line found on the board starting from the top left and iterating right
;; also returns the direction that the line is pointing out
(defun findFirstLine(board)
	(loop named outer
		for i from 0 to (getBoardHeight board)
		do
		(loop for j from 0 to (getRowWidth (car board))
			do
			(if (equal (getElement board i j) '+)
				(progn
					;; line coming in from left
					(if (equal (left board i j) '-)
						(progn
							;;(princ "FROM THE LEFT")
							(if(and (or 
									(and (equal (top board i j) '_) (equal (top valid i j) '_))
									(and (equal (right board i j) '_) (equal (right valid i j) '_))
									(and (equal (bottom board i j) '_) (equal (bottom valid i j) '_))
								) (equal(countAdjLines board i j 0) 1))
								(return-from findFirstLine (list i j 'r))
							)
						)
					)

					;; coming in from right
					(if (equal (right board i j) '-)
						(progn
							;;(princ "FROM THE RIGHT")
							(if (and(or 
									(and (equal (top board i j) '_) (equal (top valid i j) '_))
									(and (equal (left board i j) '_) (equal (left valid i j) '_))
									(and (equal (bottom board i j) '_) (equal (bottom valid i j) '_))
								)(equal(countAdjLines board i j 0) 1))
								(return-from findFirstLine (list i j 'l))
							)
						)
					)

					;; coming in from top
					(if (equal (top board i j) '-)
						(progn
							;;(princ "FROM THE TOP")
							(if(and(or 
									(and (equal (left board i j) '_) (equal (left valid i j) '_))
									(and (equal (right board i j) '_) (equal (right valid i j) '_))
									(and (equal (bottom board i j) '_) (equal (bottom valid i j) '_))
								)(equal(countAdjLines board i j 0) 1))
								(return-from findFirstLine (list i j 'b))
							)
						)
					)

					;; coming in from bottom
					(if (equal (bottom board i j) '-)
						(progn
							;;(princ "FROM THE BOTTOM")
							(if(and(or 
									(and (equal (top board i j) '_) (equal (top valid i j) '_))
									(and (equal (right board i j) '_) (equal (right valid i j) '_))
									(and (equal (left board i j) '_) (equal (left valid i j) '_))
								)(equal(countAdjLines board i j 0) 1))
								(return-from findFirstLine (list i j 't))
							)
						)
					)
				)
			)
		)
	)
)