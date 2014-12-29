;;;; Sanjay Nair
;;;; University of Florida
;;;; Fall 2014


;; check if each vertex has 2 or 0 lines coming out of them
(defun checkBoard1(brd)
	(checkLinesOut brd 1 1 t)
)



;; check if each vertex has 1, 2 or 0 lines coming out of them
;; this is to account for intermediate states of potential board solutions
(defun checkBoard1AI (brd)
	(checkLinesOutAI brd 1 1 t)
)



;; check if each number has the correct number of lines around them
(defun checkBoard2(brd)
	(checkLinesAroundNum brd 1 1 t)
)



;; check if there is a single, connected loop and no stray lines
(defun checkBoard3(brd)
	(createVisited brd)
	(setq visited (markLoop brd visited 1 1))
	(checkSingleLoop brd visited)
)



;; sets the visited board to the variable visited
(defun createVisited(brd)
	(setq visited nil)
	(setq visited (createVisited2 brd))
)


;; helper function that recursively copies the board to the variable visited
(defun createVisited2(brd)
	(cond
		((null brd)
			visited
		)
		(T
			(setq visited (cons (make-list(second (getRealBoardDimen brd))) visited))
			;;(print (make-list(second (getRealBoardDimen brd)) :initial-element 0))
			(createVisited2 (cdr brd))
		)
	)
)



;; check the number of lines around a number square are correct
(defun checkLinesAroundNum(brd row col valid)
	(cond
		((not valid)
			nil
		)
		(T
			(cond 
				((equal (1+ (nth 1 (getRealBoardDimen brd))) col)
					(checkLinesAroundNum brd (1+ row) 1 valid)
				)
		
				((equal (1+ (nth 0 (getRealBoardDimen brd))) row)
					valid
				)
		
				(T
					(cond 

						((numberp (getElement brd row col))
							(checkLinesAroundNum brd row (1+ col) (equal (getElement brd row col) (countAdjLines brd row col 0)))

						)

						(T
							(checkLinesAroundNum brd row (1+ col) valid)
						)
					)
				)
			)
		)
	)
)



;; check from index row col to end of matrix to see if
;; each dot has exactly 2 or 0 lines coming out of it
(defun checkLinesOut(brd row col valid)
	(cond
		((not valid)
			nil
		)
		(T
			(cond 
				((equal (1+ (nth 1 (getRealBoardDimen brd))) col)
					(checkLinesOut brd (1+ row) 1 valid)
				)
		
				((equal (1+ (nth 0 (getRealBoardDimen brd))) row)
					valid
				)
		
				(T
					(if (equal (getElement brd row col) '+)
						(checkLinesOut brd row (1+ col) (checkAdjLines brd row col 0))
				
						(checkLinesOut brd row (1+ col) valid)
					)
				)
			)
		)
	)
)



;; alternate used for AI
(defun checkLinesOutAI(brd row col valid)
	(cond
		((not valid)
			nil
		)
		(T
			(cond 
				((equal (1+ (nth 1 (getRealBoardDimen brd))) col)
					(checkLinesOutAI brd (1+ row) 1 valid)
				)
		
				((equal (1+ (nth 0 (getRealBoardDimen brd))) row)
					valid
				)
		
				(T
					(if (equal (getElement brd row col) '+)
						(checkLinesOutAI brd row (1+ col) (checkAdjLinesAI brd row col 0))
				
						(checkLinesOutAI brd row (1+ col) valid)
					)
				)
			)
		)
	)
)



;; call with count at 0 initially
;; T if num lines around an index is 2 or 0, else nil
(defun checkAdjLines(brd row col count)
	(setq top (getElement brd (1- row) col))
	(setq bottom (getElement brd (1+ row) col))
	(setq left (getElement brd row (1- col)))
	(setq right (getElement brd row (1+ col)))
	
	(if(equal top '-)
		(setq count (1+ count))		
	)
	
	(if(equal bottom '-)
		(setq count (1+ count))
	)
	
	(if(equal left '-)
		(setq count (1+ count))
	)
	
	(if(equal right '-)
		(setq count (1+ count))
	)
		
	(if (and(not(equal count 2)) (not(equal count 0)))
		nil
		T
	)
)



;; alternate function that checks if a vertex has 0, 1, or 2 lines
;; coming out of it
(defun checkAdjLinesAI(brd row col count)
	(let (
		(top (getElement brd (1- row) col))
		(bottom (getElement brd (1+ row) col))
		(left (getElement brd row (1- col)))
		(right (getElement brd row (1+ col)))
		)
	
		(if(equal top '-)
			(setq count (1+ count))		
		)
		
		(if(equal bottom '-)
			(setq count (1+ count))
		)
		
		(if(equal left '-)
			(setq count (1+ count))
		)
		
		(if(equal right '-)
			(setq count (1+ count))
		)
			
		(if (and(not(equal count 2)) (not(equal count 0)) (not(equal count 1)))
			nil
			T
		)
	)
)



;; call with count at 0 initially
;; returns the number of lines around an index
(defun countAdjLines(brd row col count)
	(setq top (getElement brd (1- row) col))
	(setq bottom (getElement brd (1+ row) col))
	(setq left (getElement brd row (1- col)))
	(setq right (getElement brd row (1+ col)))
	
	(if(equal top '-)
		(setq count (1+ count))		
	)
	
	(if(equal bottom '-)
		(setq count (1+ count))
	)
	
	(if(equal left '-)
		(setq count (1+ count))
	)
	
	(if(equal right '-)
		(setq count (1+ count))
	)
		
	count
)



;; mark t in the visited array from the first line found
;; along a continuous way till a full loop or end
(defun markLoop (brd visited row col)
	(cond
	
		((not(inBounds brd row col))
			;;(print "out of bounds")
			(cond
				((inBounds brd row (1+ col))
					(markLoop brd visited row (1+ col))
				)
				
				((inBounds brd (1+ row) 1)
					(markLoop brd visited (1+ row) 1)
				)
				(T
					visited
				)
			)
		)
		
		((equal (getElement brd row col) '-)
			(setElement visited row col t)

			;;(princ "visited ")
			;;(princ row)
			;;(princ " ")
			;;(princ col)
			;;(terpri)
			
				;; check each potential way we can trace the current line
				
			;; right down
			(if(and (equal (getElement brd (1+ row) (1+ col)) '-) (null (getElement visited (1+ row) (1+ col))))
				(markLoop brd visited (1+ row) (1+ col))
			)

			;; right up
			(if(and (equal (getElement brd (1- row) (1+ col)) '-) (null (getElement visited (1- row) (1+ col))))
				(markLoop brd visited (1- row) (1+ col))
			)

			;; left down
			(if(and (equal (getElement brd (1+ row) (1- col)) '-) (null (getElement visited (1+ row) (1- col))))
				(markLoop brd visited (1+ row) (1- col))
			)

			;; left up
			(if(and (equal (getElement brd (1- row) (1- col)) '-) (null (getElement visited (1- row) (1- col))))
				(markLoop brd visited (1- row) (1- col))
			)
				
			(cond
				;; even row (need to go up and down)
				((equal (mod row 2) 0)
					;;(princ "even row")
					;;(terpri)
					;; up
					(if(and (equal (getElement brd (- row 2) col) '-) (null (getElement visited (- row 2) col)))
						(markLoop brd visited (- row 2) col)
					)

					;; down
					(if(and (equal (getElement brd (+ row 2) col) '-) (null (getElement visited (+ row 2) col)))
						(markLoop brd visited (+ row 2) col)
					)
				)

				;; odd row (need to go right and left)
				(T
					;; right
					(if(and (equal (getElement brd row (+ col 2)) '-) (null (getElement visited row (+ col 2))))
						(markLoop brd visited row (+ col 2))
					)
				
					;; left
					(if(and (equal (getElement brd row (- col 2)) '-) (null (getElement visited row (- col 2))))
						(markLoop brd visited row (- col 2))
					)
				)	
			)

			;; if no lines around or all visited
			(if T
				visited
			)
			
		)

		(T
			(markLoop brd visited row (1+ col))
		)
	)
)



(defun checkSingleLoop(brd visited)
	(cond 
		((null (car brd))
			T
		)

		(T
			(if (null(checkSingleLoopRow (car brd) (car visited)))
				nil
				(checkSingleLoop (cdr brd) (cdr visited))
			)
		)
	)
)


;; helper function for checkSingleLoop
(defun checkSingleLoopRow (bRow vRow)
	(cond
		((null(car bRow))
			T
		)
		(T
			(cond
				((equal (car bRow) '-)
					;;(princ "hit line")
					;;(terpri)
					(if (equal(car vRow) T)
						(checkSingleLoopRow (cdr bRow) (cdr vRow))
						nil
					)
				)
				(T
					(checkSingleLoopRow (cdr bRow) (cdr vRow))
				)
			)
		)
	)
)