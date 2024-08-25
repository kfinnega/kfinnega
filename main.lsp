( load "board.lsp" )
( load "pieces.lsp" )
( load "moves.lsp" )
(ql:quickload "split-sequence")


( defun r () (load "main.lsp") )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;-----------------------;;;;
;;;;  RANDOM MOVE METHODS  ;;;;
;;;;-----------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defun random-black-piece ()
  ( nth ( random ( length *black-pieces* ) ) *black-pieces* )
)

( defun random-black-move ()
  ( random-move ( get-all-color-moves 'b ) )
) 

( defun random-white-move ()
  ( random-move ( get-all-color-moves 'w ) )
) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;-------------------------------;;;;
;;;;  MATERIAL SCORE MOVE METHODS  ;;;;
;;;;-------------------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defun compute-wscore ( temp-pieces &aux score )
  ( setf score 0 ) 
  ( dolist ( wpiece temp-pieces )
    ( if ( not ( typep wpiece 'king ) )
      ( setf score ( + score ( val wpiece ) ) )
    )
  )
	score
)

( defun compute-bscore ( temp-pieces &aux score )
  ( setf score 0 ) 
  ( dolist ( bpiece temp-pieces )
    ( if ( not ( typep bpiece 'king ) )
      ( setf score ( + score ( val bpiece ) ) )
    )
  )
	score
)

( defun compute-score ( color temp-pieces )
  ( cond  
    ( ( eq color 'b ) ( compute-bscore temp-pieces ) )
    ( ( eq color 'w ) ( compute-wscore temp-pieces ) )
  )
)

( defun moves-with-lowest-score (color &aux temp-pieces  best-moves opposite-color-moves opposite-color all-move-pairs min-score source destination occupier-source occupier-destination score)
 ( setf opposite-color ( oppo-color color ) )
 ( setf opposite-color-moves ( oppo-pieces-of-color color ) )
 ( setf all-move-pairs ( get-all-move-pair-list opposite-color-moves ) )
 ( setf min-score 100000 ) 
 ( setf best-moves all-move-pairs )
    ( dolist ( move-pair all-move-pairs )
      ( setf source ( car move-pair ) )
      ( setf destination ( car ( cdr move-pair ) ) )
      ( setf occupier-source ( occupier source ) )
      ( setf occupier-destination ( occupier destination ) )  
	  ( if ( not ( null occupier-destination ) )
        ( progn
		  ( if ( eq ( type occupier-destination ) 'king )
	        ( progn 
		      ( setf best-moves '() )
		      ( push move-pair best-moves )
		      ( return )
	 	    )
	      )
          ( setf temp-pieces ( remove occupier-destination ( pieces-of-color color )) )
		  ( setf ( occupier destination ) occupier-source )		  
          ( setf ( cs occupier-source ) destination )
          ( setf ( occupier source ) nil )
	      ( setf score ( compute-score color temp-pieces ) )
	      ( if ( = score min-score ) 
		    ( push move-pair best-moves )
	      )	  
	     ( if ( < score min-score )
	       ( progn 
		     ( setf min-score score )
      	     ( setf best-moves '() )
		     ( push move-pair best-moves )
		   )
	     )
        )
	  )
         ( setf ( occupier source ) occupier-source )
	     ( setf ( cs occupier-source ) source )
	     ( setf ( occupier destination ) occupier-destination )
		 
		 
	)
	best-moves
)

( defun material-move ( move-pairs &aux curr-square dest square selected ) 
  ( setf selected ( nth ( random ( length move-pairs ) ) move-pairs ) )
  ( setf curr-square ( car selected ) )
  ( setf dest-square ( car ( cdr selected ) ) )
  ( move curr-square dest-square )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;--------------------------;;;;
;;;;  LOCATION SCORE METHODS  ;;;;
;;;;--------------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defmethod pawn-lscore ( ( square square ) ( pawn pawn ) )
  ( cond 
    ( ( eq ( color pawn ) 'b ) ( aref *bpawn-table* ( rank square ) ( file square ) ) ) 
    ( ( eq ( color pawn ) 'w ) ( aref *wpawn-table* ( rank square ) ( file square ) ) ) 
  )
)

( defmethod king-lscore ( ( square square ) ( king king ) &aux score )
  ( cond 
    ( ( eq ( color king ) 'b ) ( aref *bking-table* ( rank square ) ( file square ) ) ) 
    ( ( eq ( color king ) 'w ) ( aref *wking-table* ( rank square ) ( file square ) ) ) 
  )
)

( defmethod rook-lscore ( ( square square ) ( rook rook ) )
  ( cond 
    ( ( eq ( color rook ) 'b ) ( aref *brook-table* ( rank square ) ( file square ) ) ) 
    ( ( eq ( color rook ) 'w ) ( aref *wrook-table* ( rank square ) ( file square ) ) ) 
  )
)

( defmethod bishop-lscore ( ( square square ) ( bishop bishop ) )
  ( cond 
    ( ( eq ( color bishop ) 'b ) ( aref *bbishop-table* ( rank square ) ( file square ) ) ) 
    ( ( eq ( color bishop ) 'w ) ( aref *wbishop-table* ( rank square ) ( file square ) ) ) 
  )
)

( defmethod queen-lscore ( ( square square ) ( queen queen ) ) 
  ( aref *queen-table* ( rank square ) ( file square ) ) 
)

( defmethod knight-lscore ( ( square square ) ( knight knight ) ) 
  ( aref *knight-table* ( rank square ) ( file square ) ) 
)


( defun compute-location-score ( temp-pieces &aux score )
  ( setf score 0 )
  ( dolist ( piece temp-pieces )
    ( cond 
	  ( ( eq ( type piece ) 'pawn ) ( setf score ( + score ( pawn-lscore ( cs piece ) piece ) ) ) )
      ( ( eq ( type piece ) 'queen ) ( setf score ( + score ( queen-lscore ( cs piece ) piece ) ) ) )
      ( ( eq ( type piece ) 'king ) ( setf score ( + score ( king-lscore ( cs piece ) piece ) ) ) )
      ( ( eq ( type piece ) 'rook ) ( setf score ( + score ( rook-lscore ( cs piece ) piece ) ) ) )
      ( ( eq ( type piece ) 'knight ) ( setf score ( + score ( knight-lscore ( cs piece ) piece ) ) ) )
      ( ( eq ( type piece ) 'bishop ) ( setf score ( + score ( bishop-lscore ( cs piece ) piece ) ) ) )
    )
  )
	score
)

( defun highest-location-score ( color &aux temp-pieces  best-moves pieces opposite-color all-move-pairs max-score source destination occupier-source occupier-destination score )
 ( setf pieces( pieces-of-color color ) )
 ( setf all-move-pairs ( get-all-move-pair-list pieces ) )
 ( setf max-score -10000 ) 
 ( setf best-moves all-move-pairs )
    ( dolist ( move-pair all-move-pairs )
      ( setf source ( car move-pair ) )
      ( setf destination ( car ( cdr move-pair ) ) )
      ( setf occupier-source ( occupier source ) )
      ( setf occupier-destination ( occupier destination ) ) 
      ( setf temp-pieces ( remove occupier-destination ( pieces-of-color color )) )
	  ( setf ( occupier destination ) occupier-source )		  
      ( setf ( cs occupier-source ) destination )
      ( setf ( occupier source ) nil )
	  ( setf score ( compute-location-score temp-pieces ) )
;	  ( format t "Score: ~A~%" score )
	  ( if ( = score max-score ) 
	    ( push move-pair best-moves )
	  )	  
	  ( if ( > score max-score )
	    ( progn 
          ( setf max-score score )
    	  ( setf best-moves '() )
	      ( push move-pair best-moves )
		)
	  )
	  ( setf ( occupier source ) occupier-source )
	  ( setf ( cs occupier-source ) source )
	  ( setf ( occupier destination ) occupier-destination )
  
	)
	best-moves
)

( defun location-move ( move-pairs &aux curr-square dest square selected ) 
  ( setf selected ( nth ( random ( length move-pairs ) ) move-pairs ) )
  ( setf curr-square ( car selected ) )
  ( setf dest-square ( car ( cdr selected ) ) )
  ( move curr-square dest-square )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;--------------------;;;;
;;;;  END GAME METHODS  ;;;;
;;;;--------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Check if the black king has been taken ;; 
( defun bking-in-playp ()
  ( square-in-list-p bking *black-pieces* )
)

;; Check if the white king has been taken ;;
( defun wking-in-playp ()
  ( square-in-list-p wking *white-pieces* )
)


;; Check if either king has been taken ;;
( defun game-overp ()
  ( cond 
    ( ( and ( bking-in-playp ) ( wking-in-playp ) )
  	  nil 
	)
    ( ( bking-in-playp ) 'b )
    ( ( wking-in-playp ) 'w )
    ( t nil )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;----------------------;;;;
;;;;  PLAY TURN METHODS   ;;;;
;;;;----------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; HUMAN PLAYER METHODS ;;;;

;; Play a human player turn ;;
( defun play-turn--h ( color &aux curr-square csr csf dest-square dsr dsf ) 
   ( format t "It is the ~A player's turn~%" color )
   ( format t "Enter start square: " )
   ( setf curr-square ( parse-square ( string-trim " " (read-line))))
   ( setf csr ( car curr-square ) )
   ( setf csf ( car ( cdr curr-square ) ) )
   ( format t "Enter end square: " )
   ( setf dest-square (parse-square (string-trim " " (read-line))))
   ( setf dsr ( car dest-square ) )
   ( setf dsf ( car ( cdr dest-square ) ) )
   ( setf curr-square ( aref ( board *gameboard* ) csf csr ) ) 
   ( setf dest-square ( aref ( board *gameboard* ) dsf dsr ) )
   ( move curr-square dest-square )
)


;;;; RANDOM PLAYER METHODS ;;;;

;; Plays a random black turn ;;
( defun play-turn--rb () 
  ( random-black-move )
)


;; Plays a random white turn ;;
( defun play-turn--rw ()
  ( random-white-move )
)


;; Plays a turn for a random player of the given color ;;
( defun play-turn--r ( color )
  ( cond 
    ( ( eq color 'w )
	  ( play-turn--rw )
	)
	( ( eq color 'b )
	  ( play-turn--rb )
	)
  )
)


;;;; MATERIAL SCORE PLAYER ;;;;
( defun play-turn--m ( color &aux best-moves )
  ( setf best-moves ( moves-with-lowest-score (oppo-color color ) ) )
  ( material-move best-moves )
)

;;;; LOCATION SCORE PLAYER ;;;;
( defun play-turn--l ( color &aux best-moves )
  ( setf best-moves ( highest-location-score color ) ) 
  ( location-move best-moves )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;----------------------------------;;;;
;;;;  HUMAN VS AI PLAY GAME METHODS   ;;;;
;;;;----------------------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  HUMAN PLAYER VS HUMAN PLAYER  ;;
( defun play-game--hh ( &aux color )
  ( new-game )
  ( display-board *gameboard*)
  ( setf color 'w )
  ( play-turn--h color )
  ( loop while ( null ( game-overp ) )
        do ( progn
             ( setf color ( oppo-color color ) )
			 ( d )
             ( play-turn--h color )
		   )
  )
  ( format t "GAME OVER~%PLAYER ~A WINS~%" ( game-overp ) )
)


;;  HUMAN PLAYER VS RANDOM PLAYER  ;;
( defun play-game--hr ()
  (new-game )
  ( display-board *gameboard*)
  ( loop while ( null ( game-overp ) ) 
    do ( progn
	     ( d )
         ( play-turn--h 'w )
         ( when ( null ( game-overp ) ) 
  		   ( play-turn--rb ) 
		 ) 
	   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( format t "GAME OVER~%PLAYER HUMAN PLAYER WINS~%" ) 
    ( format t "GAME OVER~%PLAYER RANDOM PLAYER WINS~%" )
  ) 
)




( defun play-n-turns ( n &aux color )
  ( new-game )
  ( display-board *gameboard* )
  ( setf color 'w )
  ( play-turn--r color )
  ( loop while ( > n 0 )
        do ( progn
             ( setf color ( oppo-color color ) )
             ( play-turn--r color ) 
			 ( setf n ( - n 1 ) )
		   ) 
  )
)


;;   HUMAN PLAYER VS MATERIAL PLAYER ;;
( defun play-game--hm ()
  (new-game )
  ( display-board *gameboard*)
  ( loop while ( null ( game-overp ) ) 
    do ( progn
	     ( d ) 
         ( play-turn--h 'w )
         ( when ( null ( game-overp ) ) 
  		   ( play-turn--m 'b ) 
		 ) 
	   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( format t "GAME OVER~%PLAYER HUMAN PLAYER WINS~%" ) 
    ( format t "GAME OVER~%PLAYER MATERIAL PLAYER WINS~%" )
  )
)

;;   HUMAN PLAYER VS LOCATION PLAYER ;;
( defun play-game--hl ()
  ( new-game )
  ( display-board *gameboard*)
  ( loop while ( null ( game-overp ) ) 
    do ( progn
	     ( d ) 
         ( play-turn--h 'w )
         ( when ( null ( game-overp ) ) 
  		   ( play-turn--l 'b ) 
		 ) 
	   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( format t "GAME OVER~%PLAYER HUMAN PLAYER WINS~%" ) 
    ( format t "GAME OVER~%PLAYER MATERIAL PLAYER WINS~%" )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;-------------------------------;;;;
;;;;  AI vs AI PLAY GAME METHODS   ;;;;
;;;;-------------------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  RANDOM PLAYER VS RANDOM PLAYER  ;;
( defun play-game--rr ( &aux color turns )
  ( new-game )
  ( display-board *gameboard* )
  ( setf turns 1 )
  ( setf color 'w )
  ( play-turn--r color )
  ( loop while ( null ( game-overp ) )
        do ( progn
             ( setf color ( oppo-color color ) )
			 ( incf turns )
             ( play-turn--r color ) 
		   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( progn 
      ;( format t "GAME OVER~%PLAYER RANDOM PLAYER 1 WINS~%" )
	  'a
	)
	( progn 
      ;( format t "GAME OVER~%PLAYER RANDOM PLAYER 2 WINS~%" )
	  'r
	)
  )
)


;;  MATERIAL PLAYER VS MATERIAL PLAYER  ;;
( defun play-game--mm ( &aux color )
  ( new-game )
  ( display-board *gameboard* )
  ( setf color 'w )
  ( play-turn--m color )
  ( loop while ( null ( game-overp ) )
        do ( progn
             ( setf color ( oppo-color color ) )
             ( play-turn--m color ) 
		   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( progn 
      ;( format t "GAME OVER~%PLAYER MATERIAL PLAYER 1 WINS~%" )
	  'm
	)
	( progn 
      ;( format t "GAME OVER~%PLAYER MATERIAL PLAYER 2 WINS~%" )
	  'a
	)
  )
)



;;  MATERIAL PLAYER VS RANDOM PLAYER  ;;
( defun play-game--mr ( &aux color )
  ( new-game )
  ( setf color 'w )
  ( play-turn--m color )
  ( loop while ( null ( game-overp ) )
        do ( progn
             ( setf color ( oppo-color color ) )
             ( play-turn--r color ) 
		   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( progn 
      ;( format t "GAME OVER~%PLAYER MATERIAL PLAYER WINS~%" )
	  'm
	)
	( progn 
      ;( format t "GAME OVER~%PLAYER RANDOM PLAYER WINS~%" )
	  'r
	)
  )
)


;;  LOCATION PLAYER VS LOCATION PLAYER  ;;
( defun play-game--rr ( &aux color turns )
  ( new-game )
  ( display-board *gameboard* )
  ( setf turns 1 )
  ( setf color 'w )
  ( play-turn--l color )
  ( loop while ( null ( game-overp ) )
        do ( progn
             ( setf color ( oppo-color color ) )
			 ( incf turns )
             ( play-turn--l color ) 
		   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( progn 
      ;( format t "GAME OVER~%PLAYER RANDOM PLAYER 1 WINS~%" )
	  'a
	)
	( progn 
      ;( format t "GAME OVER~%PLAYER RANDOM PLAYER 2 WINS~%" )
	  'r
	)
  )
)


;;  LOCATION PLAYER VS RANDOM PLAYER  ;;
( defun play-game--lr ( &aux color )
  ( new-game )
  ( setf color 'w )
  ( play-turn--l color )
  ( loop while ( null ( game-overp ) )
        do ( progn
             ( setf color ( oppo-color color ) )
             ( play-turn--r color ) 
		   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( progn 
      ;( format t "GAME OVER~%PLAYER MATERIAL PLAYER WINS~%" )
	  'l
	)
	( progn 
      ;( format t "GAME OVER~%PLAYER RANDOM PLAYER WINS~%" )
	  'r
	)
  )
)

;;  MATERIAL PLAYER VS LOCATION PLAYER  ;;
( defun play-game--ml ( &aux color )
  ( new-game )
  ( setf color 'w )
  ( play-turn--l color )
  ( loop while ( null ( game-overp ) )
        do ( progn
             ( setf color ( oppo-color color ) )
             ( play-turn--r color ) 
		   ) 
  )
  ( if ( eq 'w ( game-overp ) )
    ( progn 
      ;( format t "GAME OVER~%PLAYER MATERIAL PLAYER WINS~%" )
	  'm
	)
	( progn 
      ;( format t "GAME OVER~%PLAYER RANDOM PLAYER WINS~%" )
	  'l
	)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;------------------;;;;
;;;;  STATS METHODS   ;;;;
;;;;------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

( defun play-mr-games ( n &aux material-wins random-wins result )
  ( setf material-wins 0 )
  ( setf random-wins 0 )
  ( loop repeat n
        do ( progn
             ( setf result ( play-game--mr ) )
             ( cond ( ( eq result 'm ) ( incf material-wins ) )
                    ( ( eq result 'r ) ( incf random-wins ) )
			 )
		   )
  )
  ( format t "Material Player wins: ~A times~%" material-wins )
  ( format t "Random Player wins: ~A times~%" random-wins )
  ( list location-wins random-wins )  
)

( defun play-ml-games ( n &aux material-wins random-wins result )
  ( setf material-wins 0 )
  ( setf location-wins 0 )
  ( loop repeat n
        do ( progn
             ( setf result ( play-game--ml ) )
             ( cond ( ( eq result 'm ) ( incf material-wins ) )
                    ( ( eq result 'l ) ( incf location-wins ) )
			 )
		   )
  )
  ( format t "Material Player wins: ~A times~%" material-wins )
  ( format t "Loction Player wins: ~A times~%" location-wins )
  ( list location-wins random-wins )  
)

( defun play-lr-games ( n &aux material-wins random-wins result )
  ( setf location-wins 0 )
  ( setf random-wins 0 )
  ( loop repeat n
        do ( progn
             ( setf result ( play-game--lr ) )
             ( cond ( ( eq result 'l ) ( incf location-wins ) )
                    ( ( eq result 'r ) ( incf random-wins ) )
			 )
		   )
  )
  ( format t "Location Player wins: ~A times~%" location-wins )
  ( format t "Random Player wins: ~A times~%" random-wins )
  ( list location-wins random-wins )  
)

;; Method for all AI players to play each other 1000 times ;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;-------------------;;;;
;;;;  HELPER METHODS   ;;;;
;;;;-------------------;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Create a new game ;;
( defun new-game ()
  ( make-gameboard )
  ( make-wpieces )
  ( make-bpieces )
)


;; Combines 2 lists ;;
( defun combine ( lists )
  ( reduce #'append lists )
)

;; Get all the pieces of a certain color ;;
( defun pieces-of-color ( color ) 
  ( cond 
    ( ( eq color 'w ) *white-pieces* )
	( ( eq color 'b ) *black-pieces* )
  )
)

;; Get the pieces of the opposite color
( defun oppo-pieces-of-color ( color ) 
  ( cond 
    ( ( eq color 'b ) *white-pieces* )
	( ( eq color 'w ) *black-pieces* )
  )
)

;; Get all the moves of a color ;;
( defun get-all-color-moves ( color &aux pieces) 
  ( setf pieces ( pieces-of-color color ) )
  ( combine ( remove nil ( mapcar #'get-move-pair-list pieces ) ) ) 
)

;; Get all the moves of the opposite color ;;
(defun get-oppo-color-moves (color)
  (cond 
    ( ( eq color 'w ) ( get-all-color-moves 'b ) )
	( ( eq color 'b ) ( get-all-color-moves 'w ) )
  )
)

;; Get the opposite color ;;
( defun oppo-color ( color )
  ( cond 
    ( ( eq color 'w ) 'b )
	( ( eq color 'b ) 'w )
  )
)

;; Get the king of a given color ;;
( defun get-king ( color )
  ( cond 
    ( ( eq color 'w ) wking )
	( ( eq color 'b ) bking )
  )
)


( defun parse-square ( square-string )
  ( list 
    ( - ( char-code ( aref square-string 0 ) ) ( char-code #\a ) )
    ( 1- ( parse-integer ( subseq square-string 1 ) ) )
  )
)

;; Creates a new game for tests ;;
( defun testgame ()
  ( new-game )
  ( d )
)

;; less typing display method ;;
( defun d () 
  ( display-board *gameboard* )
)







 


  









