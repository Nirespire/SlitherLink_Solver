SlitherLink_Solver
==================

[![ghit.me](https://ghit.me/badge.svg?repo=Nirespire/SlitherLink_Solver)](https://ghit.me/repo/Nirespire/SlitherLink_Solver)

Sanjay Nair

CAP 5635

Fall 2014

Final Submission for SlitherLink project.

LISP implementation of a playable and self-solving game of SlitherLink
Developed for the CLISP runtime environment

Files Included:
* slither.lisp
* boards.lisp
* validators.lisp
* solver.lisp
* solverUtil.lisp

##Instructions to Execute:

- Load slither.lisp into your lisp environment. I will automatically load all
	dependent files.

- If you would like to run the compiled version of the code, run (compileProject) then (loadProject)
	which should load all the compiled .fas versions of all the files.
	
- Run the function (slither). You will be provided with instructions to play, which board
	to play on, and whether you would like the board solved for you. By default, Tournament board 1
	and Tournament board 4 are choices to play on.
	
- If you choose for the program to solve, it will begin executing the subroutine and
	print the solution to the board when it is found, then ask if you would
	like to play again.

-  If you choose to solve the board yourself, you can enter a coordinate and a 
	position (T,B,L,R) to TOGGLE a bar on the board. If an edge is blank, it will 
	place a bar there. If a bar exists at the specified location, it will remove it.
	The game will continue to ask for bar locations until the board is solved, then will
	ask the user if they would like to play again.

- Any time during solving a board, the user may type "exit" to close the program
	
- NOTE: if you would like to run the solver on other boards, simply execute (initai) with
	the name of the board as the only parameter. All the boards have been defined in boards.lisp
	under the "Tournament" section and additional boards may be added using the same representation.

	For example, if I wanted to run the AI on tournement board #3, I would execute (initAI tourney3)
