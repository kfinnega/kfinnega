;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions and objects used to create the chessboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( defclass chessboard () 
  ( 
    ( squares :accessor board :initform ( make-blank-chessboard ) )
  )
)


( defclass square () 
  (  
    ( rank :initarg :rank :accessor rank )
	( file :initarg :file :accessor file )
	( occupied-by :accessor occupier :initform nil )
	( northwest-square :accessor nw :initform nil )
	( north-square :accessor n :initform nil )
	( northeast-square :accessor ne :initform nil )
	( east-square :accessor e :initform nil )
	( southwest-square :accessor sw :initform nil )
	( south-square :accessor s :initform nil )
	( southeast-square :accessor se :initform nil )
	( west-square :accessor w :initform nil )
	( number :initarg :number :accessor number :initform nil )
  )
)

(defmethod print-square ((sq square))
  (format t "~A, ~A~%" (file sq) (rank sq)))



( defun make-blank-chessboard ( &aux board num )
  ( setf num 0 )
  ( setf board ( make-array '(8 8) ) )
  ( loop for i from 0 to 7 do
    ( loop for j from 0 to 7 do
      ( setf ( aref board i j ) ( make-instance 'square :rank i :file j :number num) )
      ( incf num )	  
	) 
  )
  board  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions to display the game board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defun display-border ()
  ( format t "  |----------------------------------| ~%" )
)

( defun display-no-square-row () 
  ( format t "  |                                  | ~%" )
)

( defun display-empty-row () 
  ( display-no-square-row)
  ( format t "|  --  --  --  --  --  --  --  --  | ~%" )
)

( defun display-empty-board () 
	( display-border )
	( loop for i downto 1 from 8 do 
	  ( display-empty-row )
	)
	( display-no-square-row )
	( display-border )
)

( defun display-last-line () 
  ( format t "     A   B   C   D    E   F   G   H  ~%~%" )
)

( defmethod display-square ( ( square square ) )
  ( cond 
    ( ( equal nil ( occupier square ) ) 
	  ( format t " -- " )
    )
	( ( eq (type ( occupier square ) ) 'rook )
	  ( format t " ~AR " ( color ( occupier square ) ) ) 
	)
	( ( eq (type ( occupier square ) ) 'king )
	  ( format t " ~AK " ( color ( occupier square ) ) ) 
	)
	( ( eq (type ( occupier square ) ) 'bishop )
	  ( format t " ~AB " ( color ( occupier square ) ) )
	)
	( ( eq (type ( occupier square ) ) 'queen )
	  ( format t " ~AQ " ( color ( occupier square ) ) )
	)
	( ( eq (type ( occupier square ) ) 'pawn )
	  ( format t " ~AP " ( color ( occupier square ) ) )
	)
	( ( eq (type ( occupier square ) ) 'knight )
	  ( format t " ~AN " ( color ( occupier square ) ) )
	)
  )
)

( defmethod display-row ( ( x integer ) ( board chessboard ) )
  (format t "~A | " ( + x 1 ) )
  ( loop for i from 0 to 7 do
    ( let ( ( square ( aref ( board board ) x i ) ) ) 
    ( display-square square ) )
  )
  ( format t " | ~%")
)

( defmethod display-board ( ( board chessboard ) )
  ( display-border )
  ( display-no-square-row )
  ( loop for i downto 0 from 7 do 
	( display-row i board )
	( display-no-square-row )
  )
  ( display-border )
  ( display-last-line )
)

( defun make-gameboard () 
  ( setf *gameboard* ( make-instance 'chessboard ) )

  (setf A1 (aref (board *gameboard*) 0 0)) (setf B1 (aref (board *gameboard*) 0 1)) (setf C1 (aref (board *gameboard*) 0 2)) (setf D1 (aref (board *gameboard*) 0 3)) (setf E1 (aref (board *gameboard*) 0 4)) (setf F1 (aref (board *gameboard*) 0 5)) (setf G1 (aref (board *gameboard*) 0 6)) (setf H1 (aref (board *gameboard*) 0 7))
  (setf A2 (aref (board *gameboard*) 1 0)) (setf B2 (aref (board *gameboard*) 1 1)) (setf C2 (aref (board *gameboard*) 1 2)) (setf D2 (aref (board *gameboard*) 1 3)) (setf E2 (aref (board *gameboard*) 1 4)) (setf F2 (aref (board *gameboard*) 1 5)) (setf G2 (aref (board *gameboard*) 1 6)) (setf H2 (aref (board *gameboard*) 1 7))
  (setf A3 (aref (board *gameboard*) 2 0)) (setf B3 (aref (board *gameboard*) 2 1)) (setf C3 (aref (board *gameboard*) 2 2)) (setf D3 (aref (board *gameboard*) 2 3)) (setf E3 (aref (board *gameboard*) 2 4)) (setf F3 (aref (board *gameboard*) 2 5)) (setf G3 (aref (board *gameboard*) 2 6)) (setf H3 (aref (board *gameboard*) 2 7))
  (setf A4 (aref (board *gameboard*) 3 0)) (setf B4 (aref (board *gameboard*) 3 1)) (setf C4 (aref (board *gameboard*) 3 2)) (setf D4 (aref (board *gameboard*) 3 3)) (setf E4 (aref (board *gameboard*) 3 4)) (setf F4 (aref (board *gameboard*) 3 5)) (setf G4 (aref (board *gameboard*) 3 6)) (setf H4 (aref (board *gameboard*) 3 7))
  (setf A5 (aref (board *gameboard*) 4 0)) (setf B5 (aref (board *gameboard*) 4 1)) (setf C5 (aref (board *gameboard*) 4 2)) (setf D5 (aref (board *gameboard*) 4 3)) (setf E5 (aref (board *gameboard*) 4 4)) (setf F5 (aref (board *gameboard*) 4 5)) (setf G5 (aref (board *gameboard*) 4 6)) (setf H5 (aref (board *gameboard*) 4 7))
  (setf A6 (aref (board *gameboard*) 5 0)) (setf B6 (aref (board *gameboard*) 5 1)) (setf C6 (aref (board *gameboard*) 5 2)) (setf D6 (aref (board *gameboard*) 5 3)) (setf E6 (aref (board *gameboard*) 5 4)) (setf F6 (aref (board *gameboard*) 5 5)) (setf G6 (aref (board *gameboard*) 5 6)) (setf H6 (aref (board *gameboard*) 5 7))
  (setf A7 (aref (board *gameboard*) 6 0)) (setf B7 (aref (board *gameboard*) 6 1)) (setf C7 (aref (board *gameboard*) 6 2)) (setf D7 (aref (board *gameboard*) 6 3)) (setf E7 (aref (board *gameboard*) 6 4)) (setf F7 (aref (board *gameboard*) 6 5)) (setf G7 (aref (board *gameboard*) 6 6)) (setf H7 (aref (board *gameboard*) 6 7))
  (setf A8 (aref (board *gameboard*) 7 0)) (setf B8 (aref (board *gameboard*) 7 1)) (setf C8 (aref (board *gameboard*) 7 2)) (setf D8 (aref (board *gameboard*) 7 3)) (setf E8 (aref (board *gameboard*) 7 4)) (setf F8 (aref (board *gameboard*) 7 5)) (setf G8 (aref (board *gameboard*) 7 6)) (setf H8 (aref (board *gameboard*) 7 7))

  (setf rank1 (list (setf A1 (aref (board *gameboard*) 0 0)) (setf B1 (aref (board *gameboard*) 0 1)) (setf C1 (aref (board *gameboard*) 0 2)) (setf D1 (aref (board *gameboard*) 0 3)) (setf E1 (aref (board *gameboard*) 0 4)) (setf F1 (aref (board *gameboard*) 0 5)) (setf G1 (aref (board *gameboard*) 0 6)) (setf H1 (aref (board *gameboard*) 0 7))))
  (setf rank2 (list (setf A2 (aref (board *gameboard*) 1 0)) (setf B2 (aref (board *gameboard*) 1 1)) (setf C2 (aref (board *gameboard*) 1 2)) (setf D2 (aref (board *gameboard*) 1 3)) (setf E2 (aref (board *gameboard*) 1 4)) (setf F2 (aref (board *gameboard*) 1 5)) (setf G2 (aref (board *gameboard*) 1 6)) (setf H2 (aref (board *gameboard*) 1 7))))
  (setf rank3 (list (setf A3 (aref (board *gameboard*) 2 0)) (setf B3 (aref (board *gameboard*) 2 1)) (setf C3 (aref (board *gameboard*) 2 2)) (setf D3 (aref (board *gameboard*) 2 3)) (setf E3 (aref (board *gameboard*) 2 4)) (setf F3 (aref (board *gameboard*) 2 5)) (setf G3 (aref (board *gameboard*) 2 6)) (setf H3 (aref (board *gameboard*) 2 7))))
  (setf rank4 (list (setf A4 (aref (board *gameboard*) 3 0)) (setf B4 (aref (board *gameboard*) 3 1)) (setf C4 (aref (board *gameboard*) 3 2)) (setf D4 (aref (board *gameboard*) 3 3)) (setf E4 (aref (board *gameboard*) 3 4)) (setf F4 (aref (board *gameboard*) 3 5)) (setf G4 (aref (board *gameboard*) 3 6)) (setf H4 (aref (board *gameboard*) 3 7))))
  (setf rank5 (list (setf A5 (aref (board *gameboard*) 4 0)) (setf B5 (aref (board *gameboard*) 4 1)) (setf C5 (aref (board *gameboard*) 4 2)) (setf D5 (aref (board *gameboard*) 4 3)) (setf E5 (aref (board *gameboard*) 4 4)) (setf F5 (aref (board *gameboard*) 4 5)) (setf G5 (aref (board *gameboard*) 4 6)) (setf H5 (aref (board *gameboard*) 4 7))))
  (setf rank6 (list (setf A6 (aref (board *gameboard*) 5 0)) (setf B6 (aref (board *gameboard*) 5 1)) (setf C6 (aref (board *gameboard*) 5 2)) (setf D6 (aref (board *gameboard*) 5 3)) (setf E6 (aref (board *gameboard*) 5 4)) (setf F6 (aref (board *gameboard*) 5 5)) (setf G6 (aref (board *gameboard*) 5 6)) (setf H6 (aref (board *gameboard*) 5 7))))
  (setf rank7 (list (setf A7 (aref (board *gameboard*) 6 0)) (setf B7 (aref (board *gameboard*) 6 1)) (setf C7 (aref (board *gameboard*) 6 2)) (setf D7 (aref (board *gameboard*) 6 3)) (setf E7 (aref (board *gameboard*) 6 4)) (setf F7 (aref (board *gameboard*) 6 5)) (setf G7 (aref (board *gameboard*) 6 6)) (setf H7 (aref (board *gameboard*) 6 7))))
  (setf rank8 (list (setf A8 (aref (board *gameboard*) 7 0)) (setf B8 (aref (board *gameboard*) 7 1)) (setf C8 (aref (board *gameboard*) 7 2)) (setf D8 (aref (board *gameboard*) 7 3)) (setf E8 (aref (board *gameboard*) 7 4)) (setf F8 (aref (board *gameboard*) 7 5)) (setf G8 (aref (board *gameboard*) 7 6)) (setf H8 (aref (board *gameboard*) 7 7))))

  (setf fileA (list (setf A1 (aref (board *gameboard*) 0 0)) (setf A2 (aref (board *gameboard*) 1 0)) (setf A3 (aref (board *gameboard*) 2 0)) (setf A4 (aref (board *gameboard*) 3 0)) (setf A5 (aref (board *gameboard*) 4 0)) (setf A6 (aref (board *gameboard*) 5 0)) (setf A7 (aref (board *gameboard*) 6 0)) (setf A8 (aref (board *gameboard*) 7 0))))
  (setf fileB (list (setf B1 (aref (board *gameboard*) 0 1)) (setf B2 (aref (board *gameboard*) 1 1)) (setf B3 (aref (board *gameboard*) 2 1)) (setf B4 (aref (board *gameboard*) 3 1)) (setf B5 (aref (board *gameboard*) 4 1)) (setf B6 (aref (board *gameboard*) 5 1)) (setf B7 (aref (board *gameboard*) 6 1)) (setf B8 (aref (board *gameboard*) 7 1))))
  (setf fileC (list (setf D1 (aref (board *gameboard*) 0 2)) (setf D2 (aref (board *gameboard*) 1 2)) (setf D3 (aref (board *gameboard*) 2 2)) (setf D4 (aref (board *gameboard*) 3 2)) (setf D5 (aref (board *gameboard*) 4 2)) (setf D6 (aref (board *gameboard*) 5 2)) (setf D7 (aref (board *gameboard*) 6 2)) (setf D8 (aref (board *gameboard*) 7 2))))
  (setf fileD (list (setf D1 (aref (board *gameboard*) 0 3)) (setf D2 (aref (board *gameboard*) 1 3)) (setf D3 (aref (board *gameboard*) 2 3)) (setf D4 (aref (board *gameboard*) 3 3)) (setf D5 (aref (board *gameboard*) 4 3)) (setf D6 (aref (board *gameboard*) 5 3)) (setf D7 (aref (board *gameboard*) 6 3)) (setf D8 (aref (board *gameboard*) 7 3))))
  (setf fileE (list (setf E1 (aref (board *gameboard*) 0 4)) (setf E2 (aref (board *gameboard*) 1 4)) (setf E3 (aref (board *gameboard*) 2 4)) (setf E4 (aref (board *gameboard*) 3 4)) (setf E5 (aref (board *gameboard*) 4 4)) (setf E6 (aref (board *gameboard*) 5 4)) (setf E7 (aref (board *gameboard*) 6 4)) (setf E8 (aref (board *gameboard*) 7 4))))
  (setf fileF (list (setf F1 (aref (board *gameboard*) 0 5)) (setf F2 (aref (board *gameboard*) 1 5)) (setf F3 (aref (board *gameboard*) 2 5)) (setf F4 (aref (board *gameboard*) 3 5)) (setf F5 (aref (board *gameboard*) 4 5)) (setf F6 (aref (board *gameboard*) 5 5)) (setf F7 (aref (board *gameboard*) 6 5)) (setf F8 (aref (board *gameboard*) 7 5))))
  (setf fileG (list (setf G1 (aref (board *gameboard*) 0 6)) (setf G2 (aref (board *gameboard*) 1 6)) (setf G3 (aref (board *gameboard*) 2 6)) (setf G4 (aref (board *gameboard*) 3 6)) (setf G5 (aref (board *gameboard*) 4 6)) (setf G6 (aref (board *gameboard*) 5 6)) (setf G7 (aref (board *gameboard*) 6 6)) (setf G8 (aref (board *gameboard*) 7 6))))
  (setf fileH (list (setf H1 (aref (board *gameboard*) 0 7)) (setf H2 (aref (board *gameboard*) 1 7)) (setf H3 (aref (board *gameboard*) 2 7)) (setf H4 (aref (board *gameboard*) 3 7)) (setf H5 (aref (board *gameboard*) 4 7)) (setf H6 (aref (board *gameboard*) 5 7)) (setf H7 (aref (board *gameboard*) 6 7)) (setf H8 (aref (board *gameboard*) 7 7))))

  ( setf ( n a1 ) a2 ) ( setf ( ne a1 ) b2 ) ( setf ( e a1 ) b1 )
  ( setf ( w b1 ) a1 ) ( setf ( nw c1 ) c2 ) ( setf ( n b1 ) b2 ) ( setf ( ne b1 ) c2 ) ( setf ( e b1 ) c1 )
  ( setf ( w c1 ) b1 ) ( setf ( nw c1 ) b2 ) ( setf ( n c1 ) c2 ) ( setf ( ne c1 ) d2 ) ( setf ( e c1 ) d1 )
  ( setf ( w d1 ) c1 ) ( setf ( nw d1 ) c2 ) ( setf ( n d1 ) d2 ) ( setf ( ne d1 ) e2 ) ( setf ( e d1 ) e1 )
  ( setf ( w e1 ) d1 ) ( setf ( nw e1 ) d2 ) ( setf ( n e1 ) e2 ) ( setf ( ne e1 ) f2 ) ( setf ( e e1 ) f1 )
  ( setf ( w f1 ) e1 ) ( setf ( nw f1 ) e2 ) ( setf ( n f1 ) f2 ) ( setf ( ne f1 ) g2 ) ( setf ( e f1 ) g1 )
  ( setf ( w g1 ) f1 ) ( setf ( nw g1 ) d2 ) ( setf ( n g1 ) e2 ) ( setf ( ne g1 ) h2 ) ( setf ( e g1 ) h1 ) 
  ( setf ( w h1 ) g1 ) ( setf ( nw h1 ) g2 ) ( setf ( n h1 ) h2 ) 

                                             ( setf ( n a2 ) a3 ) ( setf ( ne a2 ) b3 ) ( setf ( e a2 ) b2 )                       ( setf ( s a2 ) a1 ) ( setf ( se a2 ) b1 )
  ( setf ( w b2 ) a2 ) ( setf ( nw b2 ) a3 ) ( setf ( n b2 ) b3 ) ( setf ( ne b2 ) c3 ) ( setf ( e b2 ) c2 ) ( setf ( sw b2 ) a1 ) ( setf ( s b2 ) b1 ) ( setf ( se b2 ) c1 )
  ( setf ( w c2 ) b2 ) ( setf ( nw c2 ) b3 ) ( setf ( n c2 ) c3 ) ( setf ( ne c2 ) d3 ) ( setf ( e c2 ) d2 ) ( setf ( sw c2 ) b1 ) ( setf ( s c2 ) c1 ) ( setf ( se c2 ) d1 )
  ( setf ( w d2 ) c2 ) ( setf ( nw d2 ) c3 ) ( setf ( n d2 ) d3 ) ( setf ( ne d2 ) e3 ) ( setf ( e d2 ) e2 ) ( setf ( sw d2 ) c1 ) ( setf ( s d2 ) d1 ) ( setf ( se d2 ) e1 )
  ( setf ( w e2 ) d2 ) ( setf ( nw e2 ) d3 ) ( setf ( n e2 ) e3 ) ( setf ( ne e2 ) f3 ) ( setf ( e e2 ) f2 ) ( setf ( sw e2 ) d1 ) ( setf ( s e2 ) e1 ) ( setf ( se e2 ) f1 )
  ( setf ( w f2 ) e2 ) ( setf ( nw f2 ) e3 ) ( setf ( n f2 ) f3 ) ( setf ( ne f2 ) g3 ) ( setf ( e f2 ) g2 ) ( setf ( sw f2 ) e1 ) ( setf ( s f2 ) f1 ) ( setf ( se f2 ) g1 )
  ( setf ( w g2 ) f2 ) ( setf ( nw g2 ) f3 ) ( setf ( n g2 ) g3 ) ( setf ( ne g2 ) h3 ) ( setf ( e g2 ) h2 ) ( setf ( sw g2 ) f1 ) ( setf ( s g2 ) g1 ) ( setf ( se g2 ) h1 )
  ( setf ( w h2 ) g2 ) ( setf ( nw h2 ) g3 ) ( setf ( n h2 ) h3 )                                            ( setf ( sw h2 ) g1 ) ( setf ( s h2 ) h1 )


                                             ( setf ( n a3 ) a4 ) ( setf ( ne a3 ) b4 ) ( setf ( e a3 ) b3 )                       ( setf ( s a3 ) a2 ) ( setf ( se a3 ) b2 )
  ( setf ( w b3 ) a3 ) ( setf ( nw b3 ) a4 ) ( setf ( n b3 ) b4 ) ( setf ( ne b3 ) c4 ) ( setf ( e b3 ) c3 ) ( setf ( sw b3 ) a2 ) ( setf ( s b3 ) b2 ) ( setf ( se b3 ) c2 )
  ( setf ( w c3 ) b3 ) ( setf ( nw c3 ) b4 ) ( setf ( n c3 ) c4 ) ( setf ( ne c3 ) d4 ) ( setf ( e c3 ) d3 ) ( setf ( sw c3 ) b2 ) ( setf ( s c3 ) c2 ) ( setf ( se c3 ) d2 )
  ( setf ( w d3 ) c3 ) ( setf ( nw d3 ) c4 ) ( setf ( n d3 ) d4 ) ( setf ( ne d3 ) e4 ) ( setf ( e d3 ) e3 ) ( setf ( sw d3 ) c2 ) ( setf ( s d3 ) d2 ) ( setf ( se d3 ) e2 )
  ( setf ( w e3 ) d3 ) ( setf ( nw e3 ) d4 ) ( setf ( n e3 ) e4 ) ( setf ( ne e3 ) f4 ) ( setf ( e e3 ) f3 ) ( setf ( sw e3 ) d2 ) ( setf ( s e3 ) e2 ) ( setf ( se e3 ) f2 )
  ( setf ( w f3 ) e3 ) ( setf ( nw f3 ) e4 ) ( setf ( n f3 ) f4 ) ( setf ( ne f3 ) g4 ) ( setf ( e f3 ) g3 ) ( setf ( sw f3 ) e2 ) ( setf ( s f3 ) f2 ) ( setf ( se f3 ) g2 )
  ( setf ( w g3 ) f3 ) ( setf ( nw g3 ) f4 ) ( setf ( n g3 ) g4 ) ( setf ( ne g3 ) h4 ) ( setf ( e g3 ) h3 ) ( setf ( sw g3 ) f2 ) ( setf ( s g3 ) g2 ) ( setf ( se g3 ) h2 )
  ( setf ( w h3 ) g3 ) ( setf ( nw h3 ) g4 ) ( setf ( n h3 ) h4 )                                            ( setf ( sw h3 ) g2 ) ( setf ( s h3 ) h2 )


                                             ( setf ( n a4 ) a5 ) ( setf ( ne a4 ) b5 ) ( setf ( e a4 ) b4 )                       ( setf ( s a4 ) a3 ) ( setf ( se a4 ) b3 )
  ( setf ( w b4 ) a4 ) ( setf ( nw b4 ) a5 ) ( setf ( n b4 ) b5 ) ( setf ( ne b4 ) c5 ) ( setf ( e b4 ) c4 ) ( setf ( sw b4 ) a3 ) ( setf ( s b4 ) b3 ) ( setf ( se b4 ) c3 )
  ( setf ( w c4 ) b4 ) ( setf ( nw c4 ) b5 ) ( setf ( n c4 ) c5 ) ( setf ( ne c4 ) d5 ) ( setf ( e c4 ) d4 ) ( setf ( sw c4 ) b3 ) ( setf ( s c4 ) c3 ) ( setf ( se c4 ) d3 )
  ( setf ( w d4 ) c4 ) ( setf ( nw d4 ) c5 ) ( setf ( n d4 ) d5 ) ( setf ( ne d4 ) e5 ) ( setf ( e d4 ) e4 ) ( setf ( sw d4 ) c3 ) ( setf ( s d4 ) d3 ) ( setf ( se d4 ) e3 )
  ( setf ( w e4 ) d4 ) ( setf ( nw e4 ) d5 ) ( setf ( n e4 ) e5 ) ( setf ( ne e4 ) f5 ) ( setf ( e e4 ) f4 ) ( setf ( sw e4 ) d3 ) ( setf ( s e4 ) e3 ) ( setf ( se e4 ) f3 )
  ( setf ( w f4 ) e4 ) ( setf ( nw f4 ) e5 ) ( setf ( n f4 ) f5 ) ( setf ( ne f4 ) g5 ) ( setf ( e f4 ) g4 ) ( setf ( sw f4 ) e3 ) ( setf ( s f4 ) f3 ) ( setf ( se f4 ) g3 )
  ( setf ( w g4 ) f4 ) ( setf ( nw g4 ) f5 ) ( setf ( n g4 ) g5 ) ( setf ( ne g4 ) h5 ) ( setf ( e g4 ) h4 ) ( setf ( sw g4 ) f3 ) ( setf ( s g4 ) g3 ) ( setf ( se g4 ) h3 )
  ( setf ( w h4 ) g4 ) ( setf ( nw h4 ) g5 ) ( setf ( n h4 ) h5 )                                            ( setf ( sw h4 ) g3 ) ( setf ( s h4 ) h3 )


                                             ( setf ( n a5 ) a6 ) ( setf ( ne a5 ) b6 ) ( setf ( e a5 ) b5 )                       ( setf ( s a5 ) a4 ) ( setf ( se a5 ) b4 )
  ( setf ( w b5 ) a5 ) ( setf ( nw b5 ) a6 ) ( setf ( n b5 ) b6 ) ( setf ( ne b5 ) c6 ) ( setf ( e b5 ) c5 ) ( setf ( sw b5 ) a4 ) ( setf ( s b5 ) b4 ) ( setf ( se b5 ) c4 )
  ( setf ( w c5 ) b5 ) ( setf ( nw c5 ) b6 ) ( setf ( n c5 ) c6 ) ( setf ( ne c5 ) d6 ) ( setf ( e c5 ) d5 ) ( setf ( sw c5 ) b4 ) ( setf ( s c5 ) c4 ) ( setf ( se c5 ) d4 )
  ( setf ( w d5 ) c5 ) ( setf ( nw d5 ) c6 ) ( setf ( n d5 ) d6 ) ( setf ( ne d5 ) e6 ) ( setf ( e d5 ) e5 ) ( setf ( sw d5 ) c4 ) ( setf ( s d5 ) d4 ) ( setf ( se d5 ) e4 )
  ( setf ( w e5 ) d5 ) ( setf ( nw e5 ) d6 ) ( setf ( n e5 ) e6 ) ( setf ( ne e5 ) f6 ) ( setf ( e e5 ) f5 ) ( setf ( sw e5 ) d4 ) ( setf ( s e5 ) e4 ) ( setf ( se e5 ) f4 )
  ( setf ( w f5 ) e5 ) ( setf ( nw f5 ) e6 ) ( setf ( n f5 ) f6 ) ( setf ( ne f5 ) g6 ) ( setf ( e f5 ) g5 ) ( setf ( sw f5 ) e4 ) ( setf ( s f5 ) f4 ) ( setf ( se f5 ) g4 )
  ( setf ( w g5 ) f5 ) ( setf ( nw g5 ) f6 ) ( setf ( n g5 ) g6 ) ( setf ( ne g5 ) h6 ) ( setf ( e g5 ) h5 ) ( setf ( sw g5 ) f4 ) ( setf ( s g5 ) g4 ) ( setf ( se g5 ) h4 )
  ( setf ( w h5 ) g5 ) ( setf ( nw h5 ) g6 ) ( setf ( n h5 ) h6 )                                            ( setf ( sw h5 ) g4 ) ( setf ( s h5 ) h4 )
  

                                             ( setf ( n a6 ) a7 ) ( setf ( ne a6 ) b7 ) ( setf ( e a6 ) b6 )                       ( setf ( s a6 ) a5 ) ( setf ( se a6 ) b5 )
  ( setf ( w b6 ) a6 ) ( setf ( nw b6 ) a7 ) ( setf ( n b6 ) b7 ) ( setf ( ne b6 ) c7 ) ( setf ( e b6 ) c6 ) ( setf ( sw b6 ) a5 ) ( setf ( s b6 ) b5 ) ( setf ( se b6 ) c5 )
  ( setf ( w c6 ) b6 ) ( setf ( nw c6 ) b7 ) ( setf ( n c6 ) c7 ) ( setf ( ne c6 ) d7 ) ( setf ( e c6 ) d6 ) ( setf ( sw c6 ) b5 ) ( setf ( s c6 ) c5 ) ( setf ( se c6 ) d5 )
  ( setf ( w d6 ) c6 ) ( setf ( nw d6 ) c7 ) ( setf ( n d6 ) d7 ) ( setf ( ne d6 ) e7 ) ( setf ( e d6 ) e6 ) ( setf ( sw d6 ) c5 ) ( setf ( s d6 ) d5 ) ( setf ( se d6 ) e5 )
  ( setf ( w e6 ) d6 ) ( setf ( nw e6 ) d7 ) ( setf ( n e6 ) e7 ) ( setf ( ne e6 ) f7 ) ( setf ( e e6 ) f6 ) ( setf ( sw e6 ) d5 ) ( setf ( s e6 ) e5 ) ( setf ( se e6 ) f5 )
  ( setf ( w f6 ) e6 ) ( setf ( nw f6 ) e7 ) ( setf ( n f6 ) f7 ) ( setf ( ne f6 ) g7 ) ( setf ( e f6 ) g6 ) ( setf ( sw f6 ) e5 ) ( setf ( s f6 ) f5 ) ( setf ( se f6 ) g5 )
  ( setf ( w g6 ) f6 ) ( setf ( nw g6 ) f7 ) ( setf ( n g6 ) g7 ) ( setf ( ne g6 ) h7 ) ( setf ( e g6 ) h6 ) ( setf ( sw g6 ) f5 ) ( setf ( s g6 ) g5 ) ( setf ( se g6 ) h5 )
  ( setf ( w h6 ) g6 ) ( setf ( nw h6 ) g7 ) ( setf ( n h6 ) h7 )                                            ( setf ( sw h6 ) g5 ) ( setf ( s h6 ) h5 )
  

                                             ( setf ( n a7 ) a8 ) ( setf ( ne a7 ) b8 ) ( setf ( e a7 ) b7 )                       ( setf ( s a7 ) a6 ) ( setf ( se a7 ) b6 )
  ( setf ( w b7 ) a7 ) ( setf ( nw b7 ) c8 ) ( setf ( n b7 ) b8 ) ( setf ( ne b7 ) c8 ) ( setf ( e b7 ) c7 ) ( setf ( sw b7 ) a6 ) ( setf ( s b7 ) b6 ) ( setf ( se b7 ) c6 )
  ( setf ( w c7 ) b7 ) ( setf ( nw c7 ) b8 ) ( setf ( n c7 ) c8 ) ( setf ( ne c7 ) d8 ) ( setf ( e c7 ) d7 ) ( setf ( sw c7 ) b6 ) ( setf ( s c7 ) c6 ) ( setf ( se c7 ) d6 )
  ( setf ( w d7 ) c7 ) ( setf ( nw d7 ) c8 ) ( setf ( n d7 ) d8 ) ( setf ( ne d7 ) e8 ) ( setf ( e d7 ) e7 ) ( setf ( sw d7 ) c6 ) ( setf ( s d7 ) d6 ) ( setf ( se d7 ) e6 )
  ( setf ( w e7 ) d7 ) ( setf ( nw e7 ) d8 ) ( setf ( n e7 ) e8 ) ( setf ( ne e7 ) f8 ) ( setf ( e e7 ) f7 ) ( setf ( sw e7 ) d6 ) ( setf ( s e7 ) e6 ) ( setf ( se e7 ) f6 )
  ( setf ( w f7 ) e7 ) ( setf ( nw f7 ) e8 ) ( setf ( n f7 ) f8 ) ( setf ( ne f7 ) g8 ) ( setf ( e f7 ) g7 ) ( setf ( sw f7 ) e6 ) ( setf ( s f7 ) f6 ) ( setf ( se f7 ) g6 )
  ( setf ( w g7 ) f7 ) ( setf ( nw g7 ) f8 ) ( setf ( n g7 ) g8 ) ( setf ( ne g7 ) h8 ) ( setf ( e g7 ) h7 ) ( setf ( sw g7 ) f6 ) ( setf ( s g7 ) g6 ) ( setf ( se g7 ) h6 )
  ( setf ( w h7 ) g7 ) ( setf ( nw h7 ) g8 ) ( setf ( n h7 ) h8 )                                            ( setf ( sw h7 ) g6 ) ( setf ( s h7 ) h6 )
  

                       ( setf ( e a8 ) b8 )                       ( setf ( s a8 ) a7 ) ( setf ( se a8 ) b7 )
  ( setf ( w b8 ) a8 ) ( setf ( e b8 ) c8 ) ( setf ( sw b8 ) a7 ) ( setf ( s b8 ) b7 ) ( setf ( se b8 ) c7 )
  ( setf ( w c8 ) b8 ) ( setf ( e c8 ) d8 ) ( setf ( sw c8 ) b7 ) ( setf ( s c8 ) c7 ) ( setf ( se c8 ) d7 )
  ( setf ( w d8 ) c8 ) ( setf ( e d8 ) e8 ) ( setf ( sw d8 ) c7 ) ( setf ( s d8 ) d7 ) ( setf ( se d8 ) e7 )
  ( setf ( w e8 ) d8 ) ( setf ( e e8 ) f8 ) ( setf ( sw e8 ) d7 ) ( setf ( s e8 ) e7 ) ( setf ( se e8 ) f7 )
  ( setf ( w f8 ) e8 ) ( setf ( e f8 ) g8 ) ( setf ( sw f8 ) e7 ) ( setf ( s f8 ) f7 ) ( setf ( se f8 ) g7 )
  ( setf ( w g8 ) f8 ) ( setf ( e g8 ) h8 ) ( setf ( sw g8 ) f7 ) ( setf ( s g8 ) g7 ) ( setf ( se g8 ) h7 )
  ( setf ( w h8 ) g8 )                      ( setf ( sw h8 ) g7 ) ( setf ( s h8 ) h7 )
  
)



(defun get-rank-moves (n)
  (cond
    ((member n rank1) ( remove n rank1))
    ((member n rank2) ( remove n rank2))
    ((member n rank3) ( remove n rank3))
    ((member n rank4) ( remove n rank4))
    ((member n rank5) ( remove n rank5))
    ((member n rank6) ( remove n rank6))
    ((member n rank7) ( remove n rank7))
    ((member n rank8) ( remove n rank8))
    )
  )

(defun get-file-moves (n)
  (cond
    ((member n fileA) ( remove n fileA ) ) 
    ((member n fileB) ( remove n fileB ) )
    ((member n fileC) ( remove n fileC ) )
    ((member n fileD) ( remove n fileD ) )
    ((member n fileE) ( remove n fileE ) )
    ((member n fileF) ( remove n fileF ) )
    ((member n fileG) ( remove n fileG ) )
    ((member n fileH) ( remove n fileH ) )
    )
  )


(defun get-diagonal (n)
  (cond
    ((member n d1) di1)
    ((member n d2) di2)
    ((member n d3) di3)
    ((member n d4) di4)
    ((member n d5) di5)
    ((member n d6) di6)
    ((member n d7) di7)
    ((member n d8) di8)
    ((member n d9) di9)
    ((member n d10) di10)
    ((member n d11) di11)
    ((member n d12) di12)
    ((member n d13) di13)
    )
  )

(defun get-antidiagonal (n)
  (cond
    ((member n ad1) ad1)
    ((member n ad2) ad2)
    ((member n ad3) ad3)
    ((member n ad4) ad4)
    ((member n ad5) ad5)
    ((member n ad6) ad6)
    ((member n ad7) ad7)
    ((member n ad8) ad8)
    ((member n ad9) ad9)
    ((member n ad10) ad10)
    ((member n ad11) ad11)
    ((member n ad12) ad12)
    ((member n ad13) d13)
  )
) 