;;;; Sanjay Nair
;;;; University of Florida
;;;; Fall 2014

;; -------------------------------------------------------------------------------------------------------------------------------

;; Each board follows the following representation
;; each sublist represents a row of the board
;;		+ 		 		vertex
;;		_ 		 		edge without a line
;; 		-				edge with a line
;;		x				square without a number
;; 						a square with a number would be represented by that number
;; the valid board is represented exactly the same, except edges have an additional possible value
;;		nil				edge that in an invalid location for a line to be placed
;;
;; additional boards can be added to the file in the same manner as the ones included
;; currently, the user can play with all tournament boards

;; -------------------------------------------------------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;
;; TOURNAMENT BOARDS ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq tourney1
	'(
		(+ _ + _ + _ + _ + _ +) 
		(_ x _ 3 _ 3 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 1 _ x _ 0 _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ 1 _ 1 _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq tourney2
	'(
		(+ _ + _ + _ + _ + _ +) 
		(_ x _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 0 _ 2 _ 0 _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ x _ 1 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq tourney3
	'(
		(+ _ + _ + _ + _ + _ +) 
		(_ 3 _ x _ 3 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ 0 _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ 3 _ x _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 1 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq tourney4
	'(
		(+ _ + _ + _ + _ + _ +) 
		(_ x _ x _ 1 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ 1 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ 1 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 0 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq tourney5
	'(
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 1 _ 3 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ 1 _ x _ 0 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ 2 _ 2 _ 3 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ 1 _ 2 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ x _ 2 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
	)
)

(setq tourney6
	'(
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ 2 _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 3 _ 1 _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 1 _ 1 _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 1 _ x _ 0 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 3 _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
	)
)

(setq tourney7
	'(
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ 3 _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 1 _ 1 _ x _ 1 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 1 _ 3 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 3 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ 2 _ 0 _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ 3 _ x _ 3 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
	)
)

(setq tourney8
	'(
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 1 _ x _ 2 _ x _ 3 _ 3 _ 3 _ 3 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ x _ 1 _ 2 _ x _ 0 _ 2 _ 1 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 3 _ 1 _ 1 _ x _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ 1 _ x _ x _ 1 _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ 3 _ x _ x _ x _ x _ 3 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ x _ 1 _ 3 _ x _ 2 _ x _ 0 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ 2 _ x _ 2 _ x _ 2 _ x _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 1 _ 2 _ x _ 3 _ 2 _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ 2 _ x _ 2 _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ x _ 3 _ 2 _ 3 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)
)


(setq tourney9
	'(	
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 3 _ 3 _ x _ x _ 3 _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 0 _ x _ x _ x _ 3 _ 1 _ x _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 2 _ 2 _ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ x _ x _ 2 _ 3 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 1 _ x _ x _ 1 _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ x _ 1 _ x _ x _ 3 _ 1 _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ x _ x _ 2 _ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ x _ 3 _ x _ 2 _ 1 _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ 2 _ x _ x _ 1 _ 3 _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ x _ x _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)	
)

(setq tourney10
	'(	
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 3 _ 2 _ 2 _ 3 _ x _ 3 _ 2 _ x _ x _ x _ 1 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ x _ 1 _ 3 _ x _ x _ 2 _ 2 _ 2 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 3 _ x _ x _ 1 _ 2 _ 1 _ x _ x _ 2 _ 1 _ 3 _ 2 _ 3 _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ 2 _ x _ 2 _ 2 _ x _ 2 _ x _ 2 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ 1 _ x _ x _ 3 _ 2 _ x _ x _ x _ x _ 0 _ 1 _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 3 _ 2 _ 3 _ 1 _ 1 _ x _ x _ x _ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ x _ 2 _ x _ 2 _ x _ 2 _ x _ 2 _ x _ 1 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ 3 _ 2 _ 3 _ x _ x _ x _ 3 _ 2 _ 2 _ 2 _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 1 _ x _ 1 _ x _ x _ x _ x _ 1 _ x _ x _ 0 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ x _ x _ x _ x _ 2 _ x _ x _ x _ 2 _ x _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 2 _ x _ x _ 2 _ x _ x _ 3 _ 0 _ 3 _ 2 _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ 1 _ 3 _ 1 _ 2 _ 2 _ x _ x _ x _ 3 _ x _ 3 _ 1 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ x _ x _ x _ x _ x _ x _ 1 _ 1 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ 2 _ 1 _ 2 _ x _ x _ x _ 2 _ 2 _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 1 _ x _ x _ x _ 2 _ x _ x _ 3 _ 2 _ x _ 2 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)	
)

(setq tourney11
	'(	
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ x _ x _ x _ 2 _ 2 _ x _ x _ 3 _ 2 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ x _ 1 _ x _ x _ 2 _ 2 _ 2 _ x _ x _ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ 2 _ x _ 2 _ x _ 2 _ 3 _ 1 _ x _ 1 _ 2 _ x _ 2 _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ 1 _ 1 _ x _ 1 _ 2 _ 3 _ 2 _ x _ 3 _ 2 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ 2 _ 2 _ 2 _ 2 _ 2 _ x _ x _ x _ 2 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ x _ x _ x _ x _ x _ 2 _ 2 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ x _ 2 _ x _ 2 _ x _ 2 _ 2 _ 3 _ 3 _ 1 _ x _ 2 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ 2 _ 1 _ x _ x _ 2 _ x _ x _ 3 _ x _ 3 _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ 3 _ x _ x _ x _ 3 _ x _ 2 _ 0 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 1 _ 1 _ x _ 2 _ x _ x _ 1 _ x _ x _ x _ x _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ 3 _ 2 _ 2 _ 2 _ x _ 2 _ x _ x _ x _ 3 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ 1 _ x _ 2 _ x _ 2 _ x _ x _ 1 _ 3 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 0 _ x _ 2 _ 3 _ 2 _ 1 _ x _ x _ 1 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ 2 _ x _ x _ x _ 2 _ 2 _ 2 _ 3 _ x _ x _ 2 _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ x _ x _ x _ x _ x _ 2 _ x _ x _ x _ 3 _ x _ 3 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)	
)

(setq tourney12
	'(	
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 1 _ x _ x _ x _ x _ x _ x _ x _ x _ x _ x _ 3 _ x _ 1 _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ 2 _ x _ x _ x _ 2 _ 3 _ x _ 2 _ 2 _ x _ x _ x _ 2 _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 3 _ 2 _ 3 _ x _ x _ 1 _ 3 _ 2 _ 2 _ x _ 1 _ x _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ x _ 1 _ x _ 1 _ x _ x _ x _ 1 _ 2 _ 1 _ x _ x _ x _ 1 _ x _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ x _ x _ x _ x _ 3 _ x _ 2 _ 2 _ x _ x _ x _ 1 _ x _ 3 _ 2 _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 3 _ 2 _ 1 _ x _ x _ 3 _ 2 _ 3 _ x _ x _ 1 _ x _ x _ 2 _ x _ 1 _ 2 _ 1 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ x _ x _ x _ 1 _ 1 _ 2 _ 2 _ 3 _ x _ x _ 1 _ x _ 3 _ 2 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ 2 _ 2 _ x _ 3 _ 2 _ x _ x _ 2 _ 3 _ 1 _ x _ 2 _ x _ x _ 2 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ 2 _ x _ x _ x _ 3 _ x _ x _ 0 _ 2 _ 2 _ x _ 2 _ 1 _ x _ 1 _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 1 _ x _ 2 _ x _ x _ x _ x _ x _ x _ x _ 0 _ x _ 1 _ 3 _ 2 _ 3 _ x _ x _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 3 _ x _ x _ 1 _ 2 _ x _ x _ 1 _ 2 _ x _ x _ x _ x _ 2 _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 1 _ x _ x _ 2 _ x _ 3 _ 2 _ 3 _ 1 _ 2 _ 1 _ 2 _ x _ 2 _ x _ x _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ x _ 2 _ 3 _ x _ x _ x _ 3 _ x _ x _ x _ x _ x _ x _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ x _ 2 _ x _ 2 _ x _ 1 _ 2 _ x _ 2 _ x _ 2 _ x _ 2 _ x _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ x _ 2 _ 1 _ x _ 2 _ 1 _ x _ 2 _ 3 _ 2 _ 2 _ 1 _ 2 _ 2 _ 2 _ 1 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ 2 _ 2 _ x _ 2 _ 2 _ x _ 1 _ x _ 2 _ x _ 1 _ x _ 2 _ x _ 1 _ 2 _ 1 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ x _ 2 _ 1 _ 1 _ 3 _ x _ x _ x _ x _ x _ x _ 1 _ 2 _ 2 _ x _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 1 _ x _ 2 _ 2 _ x _ 2 _ 3 _ x _ 1 _ 2 _ 2 _ x _ x _ x _ 1 _ 1 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 2 _ 2 _ 0 _ 2 _ x _ x _ x _ 1 _ 1 _ 2 _ x _ x _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 3 _ 2 _ 3 _ 3 _ 3 _ 2 _ 2 _ 3 _ 2 _ 2 _ 2 _ 2 _ 2 _ 3 _ x _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)
)

(setq tourney13
	'(	
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 2 _ x _ x _ 3 _ 2 _ 3 _ 3 _ 3 _ x _ x _ x _ x _ x _ 1 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ 3 _ x _ x _ x _ 2 _ x _ 2 _ x _ 3 _ x _ 1 _ x _ x _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 3 _ 1 _ 2 _ 1 _ x _ 2 _ 2 _ 2 _ 2 _ x _ 1 _ 3 _ 1 _ x _ 3 _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ 1 _ x _ x _ x _ 1 _ x _ x _ x _ x _ 3 _ x _ x _ x _ x _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 2 _ 2 _ 1 _ 3 _ 2 _ x _ 1 _ x _ x _ x _ 2 _ x _ 3 _ 1 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 1 _ x _ x _ x _ 3 _ x _ 2 _ x _ x _ 2 _ x _ x _ 3 _ 2 _ 1 _ x _ x _ x _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ x _ 2 _ x _ x _ 3 _ x _ x _ x _ 3 _ x _ x _ x _ x _ 3 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ x _ x _ x _ 1 _ x _ x _ x _ x _ x _ 3 _ 1 _ x _ x _ 3 _ x _ 1 _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ 1 _ 1 _ x _ 3 _ x _ 3 _ x _ x _ x _ 2 _ x _ x _ 2 _ 1 _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 2 _ x _ x _ x _ 2 _ x _ x _ x _ 3 _ 0 _ 2 _ 3 _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ x _ 3 _ 1 _ x _ x _ x _ x _ x _ x _ x _ x _ x _ 2 _ x _ x _ 2 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ x _ x _ 2 _ 2 _ x _ 2 _ 1 _ x _ x _ 2 _ 2 _ x _ 2 _ 0 _ 1 _ 1 _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ 1 _ x _ 2 _ x _ x _ 3 _ x _ 1 _ 2 _ 2 _ x _ 3 _ x _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ 3 _ x _ x _ 2 _ 3 _ x _ x _ x _ 2 _ 3 _ 2 _ 2 _ x _ x _ 3 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 1 _ x _ x _ 1 _ 1 _ 2 _ x _ x _ x _ 2 _ 1 _ x _ x _ 1 _ x _ x _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 1 _ x _ x _ 1 _ 3 _ x _ x _ x _ 2 _ x _ x _ x _ 1 _ x _ 2 _ 3 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 3 _ x _ 2 _ x _ 2 _ 1 _ x _ x _ x _ x _ 3 _ x _ x _ 2 _ 1 _ 1 _ x _ 1 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 1 _ 2 _ 1 _ x _ 2 _ x _ 2 _ 3 _ 3 _ 2 _ 1 _ x _ x _ x _ 2 _ 1 _ x _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ x _ x _ x _ x _ 1 _ x _ x _ x _ 3 _ 2 _ x _ 2 _ x _ 2 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ x _ x _ x _ x _ x _ x _ 2 _ 1 _ 3 _ x _ x _ x _ x _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)	
)

(setq tourney14
	'(	
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ 2 _ 2 _ x _ x _ x _ x _ x _ x _ x _ 3 _ 2 _ x _ x _ x _ x _ x _ 2 _ 0 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ x _ 1 _ 3 _ 3 _ x _ x _ x _ x _ x _ 2 _ x _ x _ x _ 3 _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ 3 _ x _ x _ x _ 2 _ 1 _ 3 _ x _ x _ x _ x _ 0 _ x _ 2 _ 1 _ 3 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ 2 _ x _ x _ x _ x _ x _ 3 _ 2 _ 3 _ 2 _ x _ x _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ 1 _ 3 _ 1 _ 2 _ x _ x _ x _ 2 _ x _ 2 _ x _ x _ x _ 1 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ 1 _ x _ x _ x _ 3 _ x _ 3 _ x _ 3 _ 0 _ x _ 1 _ x _ x _ x _ 1 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 1 _ 2 _ x _ 2 _ x _ x _ x _ 2 _ x _ x _ x _ x _ x _ x _ x _ 2 _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ x _ 1 _ 3 _ x _ x _ x _ x _ x _ x _ 2 _ x _ 2 _ 2 _ 3 _ 1 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 1 _ 2 _ x _ 2 _ x _ 1 _ 2 _ x _ x _ x _ 2 _ x _ 3 _ x _ 2 _ x _ 2 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ 1 _ 2 _ 1 _ x _ x _ 2 _ x _ 3 _ x _ x _ 1 _ 3 _ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ 2 _ x _ 3 _ x _ x _ 3 _ 1 _ x _ x _ x _ 1 _ 1 _ 2 _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 3 _ 1 _ 2 _ 1 _ 1 _ 2 _ x _ 2 _ 3 _ 2 _ x _ x _ 2 _ 2 _ 1 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ 3 _ x _ x _ x _ x _ 3 _ x _ x _ 2 _ 1 _ 1 _ 2 _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ x _ x _ 2 _ 2 _ 1 _ 2 _ x _ x _ x _ x _ 2 _ x _ 2 _ x _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ 1 _ 1 _ x _ 1 _ x _ x _ 3 _ 2 _ x _ 1 _ x _ 1 _ x _ x _ 1 _ 3 _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ 1 _ 1 _ 2 _ x _ x _ 2 _ 1 _ 1 _ x _ x _ x _ x _ 2 _ 2 _ 3 _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ x _ x _ 1 _ x _ x _ x _ 2 _ x _ 1 _ x _ 3 _ x _ x _ x _ 1 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ 3 _ x _ 3 _ x _ 2 _ x _ 3 _ x _ x _ 3 _ 2 _ x _ 2 _ 3 _ 2 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ x _ x _ 2 _ 2 _ x _ x _ 2 _ x _ 3 _ x _ x _ 2 _ x _ x _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 3 _ x _ x _ x _ x _ 2 _ 1 _ 3 _ x _ x _ x _ x _ 2 _ x _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)	
)

(setq tourney15
	'(	
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ 2 _ 3 _ x _ x _ 2 _ x _ x _ x _ 2 _ 2 _ x _ x _ x _ x _ x _ x _ 2 _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 2 _ 2 _ x _ x _ x _ x _ 2 _ x _ x _ 1 _ x _ 2 _ x _ x _ 3 _ 2 _ 3 _ 1 _ x _ x _ 3 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 1 _ 3 _ x _ x _ x _ 2 _ x _ x _ x _ 2 _ x _ x _ 3 _ x _ 3 _ x _ 2 _ x _ x _ x _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 1 _ 2 _ 3 _ 2 _ 3 _ 0 _ x _ x _ 3 _ x _ x _ x _ 2 _ x _ 0 _ 3 _ x _ x _ 0 _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ x _ 1 _ x _ 2 _ x _ 3 _ x _ x _ x _ 3 _ x _ 3 _ 2 _ x _ x _ x _ x _ 3 _ x _ 3 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ x _ 3 _ 2 _ x _ 2 _ x _ 2 _ x _ x _ x _ 3 _ 2 _ x _ x _ x _ 2 _ 1 _ x _ x _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ 3 _ x _ x _ 2 _ x _ x _ 2 _ x _ 3 _ x _ 3 _ x _ 1 _ x _ 2 _ x _ 1 _ 2 _ x _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 1 _ 3 _ x _ x _ 2 _ x _ 2 _ 1 _ 2 _ x _ x _ 2 _ x _ x _ 1 _ x _ x _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 1 _ x _ x _ x _ x _ 1 _ 2 _ x _ 1 _ 3 _ x _ x _ 3 _ 2 _ x _ x _ x _ x _ 1 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ 1 _ x _ x _ x _ x _ 2 _ 1 _ 2 _ x _ 1 _ 2 _ x _ 1 _ 2 _ x _ 3 _ x _ 2 _ 2 _ x _ 3 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ 1 _ x _ x _ 2 _ x _ x _ 2 _ x _ x _ x _ 1 _ x _ 2 _ 2 _ 2 _ 3 _ 2 _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ 2 _ 3 _ 2 _ x _ 1 _ 3 _ x _ 2 _ 2 _ 2 _ 3 _ x _ x _ x _ 2 _ 2 _ 2 _ x _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 3 _ x _ 1 _ 1 _ x _ x _ x _ x _ 3 _ x _ 2 _ x _ x _ x _ x _ x _ 3 _ x _ 2 _ x _ 3 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ 1 _ 1 _ 2 _ 2 _ x _ x _ 2 _ x _ 2 _ 2 _ 2 _ 2 _ x _ 2 _ x _ x _ x _ x _ 1 _ 1 _ 3 _ x _ 1 _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 3 _ x _ x _ x _ x _ 3 _ 2 _ x _ x _ x _ x _ x _ x _ x _ x _ 1 _ x _ x _ x _ x _ 2 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 2 _ 2 _ 3 _ x _ x _ x _ x _ x _ x _ x _ 0 _ 3 _ 2 _ x _ x _ 2 _ x _ 1 _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 1 _ x _ 2 _ x _ 2 _ x _ 1 _ 2 _ x _ 2 _ 3 _ x _ 2 _ x _ x _ x _ 1 _ 1 _ x _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 3 _ x _ x _ 3 _ 3 _ x _ 3 _ 2 _ x _ 1 _ x _ 2 _ 3 _ x _ x _ x _ 2 _ x _ 3 _ x _ x _ 3 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ 2 _ x _ x _ 2 _ 1 _ x _ x _ x _ 1 _ 3 _ 3 _ x _ 1 _ 2 _ 2 _ x _ x _ 1 _ 2 _ 0 _ 2 _ 0 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ x _ x _ 2 _ 2 _ x _ x _ x _ x _ x _ x _ 3 _ x _ x _ x _ x _ x _ x _ x _ x _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ 2 _ x _ 1 _ x _ 2 _ x _ x _ x _ 3 _ x _ 2 _ 2 _ x _ 2 _ 3 _ x _ 1 _ x _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ x _ x _ 3 _ x _ 3 _ x _ 2 _ x _ x _ x _ x _ x _ x _ 2 _ 1 _ x _ x _ 2 _ x _ x _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ 2 _ x _ 3 _ x _ 2 _ x _ x _ x _ 3 _ 3 _ x _ 2 _ x _ x _ x _ 3 _ x _ 3 _ x _ x _ 2 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ x _ 2 _ 1 _ 2 _ 1 _ 1 _ x _ 1 _ 2 _ 2 _ x _ 3 _ 2 _ x _ x _ 1 _ 1 _ x _ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ x _ x _ x _ x _ 1 _ 2 _ 3 _ 2 _ x _ x _ x _ 3 _ x _ 3 _ x _ 2 _ x _ 3 _ x _ x _ 1 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ 3 _ x _ 2 _ x _ 2 _ 1 _ x _ 2 _ 1 _ 1 _ x _ x _ x _ x _ x _ 2 _ 2 _ x _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ x _ 2 _ x _ 1 _ 1 _ x _ x _ 2 _ x _ x _ x _ x _ 2 _ x _ x _ 3 _ 2 _ 2 _ x _ x _ 1 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ 3 _ x _ x _ 3 _ x _ x _ 3 _ 2 _ 2 _ x _ x _ x _ 2 _ 1 _ 2 _ 3 _ 2 _ x _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 2 _ x _ x _ 2 _ 2 _ 1 _ x _ x _ x _ 2 _ x _ x _ 2 _ x _ x _ x _ x _ x _ x _ 2 _ x _ 3 _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 0 _ 2 _ x _ x _ 2 _ 2 _ 2 _ 2 _ 1 _ x _ 3 _ 3 _ x _ x _ x _ x _ x _ x _ x _ 3 _ 2 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ +)
	)	
)


;; -------------------------------------------------------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOARDS USED FOR TESTING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq 5board
	'(
		(+ _ + _ + _ + _ + _ +) 
		(_ x _ 1 _ 2 _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ 0 _ 2 _ 1 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ 1 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq 5board2
	'(
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ 0 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 3 _ 2 _ x _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 1 _ 2 _ x _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq 5board3
	'(
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ 2 _ 1 _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 1 _ 2 _ 0 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 3 _ 3 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq 5board4
	'(
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ 2 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ 2 _ 1 _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 0 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 3 _ 3 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq 5board5
	'(
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 1 _ x _ 1 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 3 _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 1 _ x _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ 3 _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ 1 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq 5board6
	'(
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 1 _ x _ x _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ 2 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ 1 _ 2 _ 3 _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ 3 _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq 7board
	'(
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ x _ x _ 2 _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ 2 _ x _ 3 _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 1 _ 3 _ 0 _ 1 _ 1 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ x _ 2 _ x _ x _ 2 _ 3 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 1 _ x _ x _ 2 _ 1 _ 1 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 2 _ x _ 1 _ x _ 2 _ x _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
		(_ 3 _ x _ x _ 2 _ 2 _ 2 _ x _)
		(+ _ + _ + _ + _ + _ + _ + _ +)
	)
)

(setq 3board
	'(
		(+ _ + _ +)
		(_ 2 _ x _)
		(+ _ + _ +)
		(_ x _ 2 _)
		(+ _ + _ +)	
	)
)

(setq 3board2
	'(
		(+ _ + _ + )
		(_ 3 _ 3 _)
		(+ _ + _ +)
		(_ x _ x _)
		(+ _ + _ + )
		
	)
)

(setq closeBoard
	'(
		(+ - + - + - + _ + _ +)
		(- x _ x _ x - 2 _ 0 _)
		(+ _ + - + _ + - + _ +)
		(- 2 - 3 - 2 _ x - 2 _)
		(+ _ + _ + - + _ + - +)
		(- 2 - 1 _ x - x _ 3 -)
		(+ _ + _ + _ + _ + - +)
		(- x - 1 _ 2 - x - 3 _)
		(+ _ + _ + - + _ + - +)
		(- x - 2 - x _ x _ x -)
		(+ - + _ + - + - + - +)
	)
)

(setq test
	'(
		(+ _ + _ + - + _ + _ +)
		(_ x _ x - x - x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x - 3 _ x _ x _)
		(+ _ + _ + - + _ + _ +)
		(_ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
		(_ x _ x _ x _ x _ x _)
		(+ _ + _ + _ + _ + _ +)
	)
)

(setq sbrd2
	'(
		(+ - + - + - + _ + _ +)
		(- x _ x _ x - 2 _ 0 _)
		(+ _ + - + _ + - + _ +)
		(- 2 - 3 - 2 _ x - 2 _)
		(+ _ + _ + - + _ + - +)
		(- 2 - 1 _ x - x _ 3 -)
		(+ _ + _ + _ + _ + - +)
		(- x - 1 _ 2 - x - 3 _)
		(+ _ + _ + - + _ + - +)
		(- x - 2 - x _ x _ x -)
		(+ - + _ + - + - + - +)
	)
)

(setq falseBoard
	'(
		(+ - + - + )
		(- 3 - 3 _)
		(+ _ + _ +)
		(- x - x -)
		(+ _ + _ + )
		
	)
)

(setq trueBoard
	'(
		(+ - + - + )
		(- 3 _ 3 -)
		(+ - + - +)
		(_ x _ x _)
		(+ _ + _ + )
		
	)
)