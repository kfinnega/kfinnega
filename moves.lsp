(load "board.lsp")
(load "pieces.lsp")

;; This file is going to contain the move methods for the pieces

( defun square-in-list-p ( sq lst )
  ( if ( member sq lst )
      t
      nil
  )
)

(defun all-elements-in-list-p (list1 list2)
  ( every ( lambda ( element ) ( square-in-list-p element list2 ) ) list1 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions to do with the rook piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  
( defmethod unoccupied-north-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
    do ( let ( ( next-north ( n sq ) ) )
         ( cond
           ( ( null next-north ) ( return ( reverse unoccupied-squares ) ) )
           ( ( null ( occupier next-north ) )
             ( push next-north unoccupied-squares )
             ( setf sq next-north ) 
		   )
           ( ( not ( eq ( color piece ) ( color ( occupier next-north ) ) ) )
             ( push next-north unoccupied-squares )
             ( return ( reverse unoccupied-squares ) ) )
           ( t ( return ( reverse unoccupied-squares ) ) ) 
	     ) 
	  )
  )
)


( defmethod unoccupied-south-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
     do ( let ( ( next-south ( s sq ) ) )
           ( cond
             ( ( null next-south ) ( return ( reverse unoccupied-squares ) ) )
             ( ( null ( occupier next-south ) )
              ( push next-south unoccupied-squares )
              ( setf sq next-south ) )
             ( ( not ( eq ( color piece ) ( color ( occupier next-south ) ) ) )
               ( push next-south unoccupied-squares )
               ( return ( reverse unoccupied-squares ) ) 
	         )
               ( t ( return ( reverse unoccupied-squares ) ) )
			 ) 
		)
  )
)


( defmethod unoccupied-east-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
     do ( let ( ( next-east ( e sq ) ) )
           ( cond
             ( ( null next-east ) ( return ( reverse unoccupied-squares ) ) )
             ( ( null ( occupier next-east ) )
              ( push next-east unoccupied-squares )
              ( setf sq next-east ) )
             ( ( not ( eq ( color piece ) ( color ( occupier next-east ) ) ) )
               ( push next-east unoccupied-squares )
               ( return ( reverse unoccupied-squares ) ) 
	         )
               ( t ( return ( reverse unoccupied-squares ) ) )
			 ) 
		)
  )
)

( defmethod unoccupied-west-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
     do ( let ( ( next-west ( w sq ) ) )
           ( cond
             ( ( null next-west ) ( return ( reverse unoccupied-squares ) ) )
             ( ( null ( occupier next-west ) )
              ( push next-west unoccupied-squares )
              ( setf sq next-west ) )
             ( ( not ( eq ( color piece ) ( color ( occupier next-west ) ) ) )
               ( push next-west unoccupied-squares )
               ( return ( reverse unoccupied-squares ) ) 
	         )
               ( t ( return ( reverse unoccupied-squares ) ) )
			 ) 
		)
  )
)

( defmethod possible-moves-rook ( ( rook piece ) &aux curr-square )
  ( setf curr-square ( cs rook ) )
  ( append (unoccupied-north-squares curr-square rook)
           ( unoccupied-east-squares curr-square rook ) 
           ( unoccupied-south-squares curr-square rook ) 
           ( unoccupied-west-squares curr-square  rook )
  )
)
 


( defmethod legal-move-rook ( ( rook piece ) ( dest-square square ) )
  ( if 
	( square-in-list-p dest-square ( possible-moves-rook rook ) )
	t
  )  
)

( defmethod pick-square-rook ( ( rook rook ) &aux possible-moves)
  ( setf possible-moves ( possible-moves-rook rook ) )
    (if ( > ( length possible-moves ) 0 )
        ( nth ( random ( length possible-moves ) ) possible-moves )
        nil
    )
)


( defmethod random-move-rook ( ( rook rook ) &aux dest-square )
	( setf dest-square ( pick-square-rook rook ) )
	( move-rook rook dest-square )
)


( defmethod move-rook ( ( rook rook ) ( dest-square square ) &aux curr-square )
  ( setf curr-square ( cs rook ) )
  ( if ( legal-move-rook rook dest-square )
    ( progn
		( setf ( occupier curr-square ) nil )
		( setf ( occupier dest-square ) rook )
		( setf ( cs rook ) dest-square )
	)
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions to do with the bishop piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defmethod unoccupied-se-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
     do ( let ( ( next-se ( se sq ) ) )
           ( cond
             ( ( null next-se ) ( return ( reverse unoccupied-squares ) ) )
             ( ( null ( occupier next-se ) )
              ( push next-se unoccupied-squares )
              ( setf sq next-se ) )
             ( ( not ( eq ( color piece ) ( color ( occupier next-se ) ) ) )
               ( push next-se unoccupied-squares )
               ( return ( reverse unoccupied-squares ) ) 
	         )
               ( t ( return ( reverse unoccupied-squares ) ) )
			 ) 
		)
  )
)

( defmethod unoccupied-sw-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
     do ( let ( ( next-sw ( sw sq ) ) )
           ( cond
             ( ( null next-sw ) ( return ( reverse unoccupied-squares ) ) )
             ( ( null ( occupier next-sw ) )
              ( push next-sw unoccupied-squares )
              ( setf sq next-sw ) )
             ( ( not ( eq ( color piece ) ( color ( occupier next-sw ) ) ) )
               ( push next-sw unoccupied-squares )
               ( return ( reverse unoccupied-squares ) ) 
	         )
               ( t ( return ( reverse unoccupied-squares ) ) )
			 ) 
		)
  )
)

( defmethod unoccupied-ne-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
     do ( let ( ( next-ne ( ne sq ) ) )
           ( cond
             ( ( null next-ne ) ( return ( reverse unoccupied-squares ) ) )
             ( ( null ( occupier next-ne ) )
              ( push next-ne unoccupied-squares )
              ( setf sq next-ne ) )
             ( ( not ( eq ( color piece ) ( color ( occupier next-ne ) ) ) )
               ( push next-ne unoccupied-squares )
               ( return ( reverse unoccupied-squares ) ) 
	         )
               ( t ( return ( reverse unoccupied-squares ) ) )
			 ) 
		)
  )
)

( defmethod unoccupied-nw-squares ( ( sq square ) ( piece piece ) )
  ( loop with unoccupied-squares = nil
     do ( let ( ( next-nw ( nw sq ) ) )
           ( cond
             ( ( null next-nw ) ( return ( reverse unoccupied-squares ) ) )
             ( ( null ( occupier next-nw ) )
              ( push next-nw unoccupied-squares )
              ( setf sq next-nw ) )
             ( ( not ( eq ( color piece ) ( color ( occupier next-nw ) ) ) )
               ( push next-nw unoccupied-squares )
               ( return ( reverse unoccupied-squares ) ) 
	         )
               ( t ( return ( reverse unoccupied-squares ) ) )
			 ) 
		)
  )
)


( defmethod possible-moves-bishop ( ( bishop piece ) &aux curr-square )
  ( setf curr-square ( cs bishop ) )
  ( append ( unoccupied-nw-squares curr-square bishop )
           ( unoccupied-ne-squares curr-square bishop ) 
           ( unoccupied-sw-squares curr-square bishop ) 
           ( unoccupied-se-squares curr-square bishop )
  )
)




( defmethod legal-move-bishop ( ( bishop piece ) ( dest-square square ) )
  ( if 
	( square-in-list-p dest-square ( possible-moves-bishop bishop ) )
	t
  )  
)

( defmethod pick-square-bishop ( ( bishop bishop ) &aux possible-moves)
  ( setf possible-moves ( possible-moves-bishop bishop ) )
    (if ( > ( length possible-moves ) 0 )
        ( nth ( random ( length possible-moves ) ) possible-moves )
        nil
	)
)


( defmethod random-move-bishop ( ( bishop bishop ) &aux dest-square )
	( setf dest-square ( pick-square-bishop bishop ) )
	( move-bishop bishop dest-square )
)

( defmethod move-bishop ( ( bishop bishop ) ( dest-square square ) &aux curr-square )
  ( setf curr-square ( cs bishop ) )
  ( if ( legal-move-bishop bishop dest-square ) 
    ( progn
	  ( setf ( occupier curr-square ) nil )
	  ( setf ( occupier dest-square ) bishop )
	  ( setf ( cs bishop ) dest-square )
	)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions to do with the queen piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defmethod possible-moves-queen ( ( queen queen ) )
  ( append ( possible-moves-bishop queen )
           ( possible-moves-rook queen )
  )
)

( defmethod legal-move-queen ( ( queen queen ) ( dest-square square ) )
  ( or 
	( legal-move-bishop queen dest-square ) 
	( legal-move-rook queen dest-square )
  )
)

( defmethod pick-square-queen ( ( queen queen ) &aux possible-moves)
  ( setf possible-moves ( possible-moves-queen queen ) )
    (if ( > ( length possible-moves ) 0 )
        ( nth ( random ( length possible-moves ) ) possible-moves )
        nil
	)
)


( defmethod random-move-queen ( ( queen queen ) &aux dest-square )
	( setf dest-square ( pick-square-queen queen ) )
	( move-queen queen dest-square )
)


( defmethod move-queen ( ( queen queen ) ( dest-square square ) &aux curr-square )
  ( setf curr-square ( cs queen ) )
  ( if ( legal-move-queen queen dest-square )
    ( progn
	  ( setf ( occupier curr-square ) nil )
	  ( setf ( occupier dest-square ) queen )
	  ( setf ( cs queen ) dest-square )
	)
  )
)  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions to do with the king piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defmethod possible-moves-king ( ( king king ) &aux curr-square valid-squares )
  ( setf curr-square ( cs king ) )
  ( setf valid-squares '() )
  ( progn
    ( dolist ( direction '( n ne e se s sw w nw ) )
      ( let ( ( next-square (funcall direction curr-square)))
        ( when ( and next-square
                   ( or ( null (occupier next-square ) )
                        ( not ( equal ( color ( occupier next-square ) ) ( color king ) ) ) 
					) 
				)
          ( push next-square valid-squares )
		)
	  )
	)
    valid-squares
  )
)


( defmethod pick-square-king ( ( king king ) )
  ( nth ( random ( length ( possible-moves-king king ) ) ) ( possible-moves-king king ) )
)

( defmethod pick-square-king ( ( king king ) &aux possible-moves)
  ( setf possible-moves ( possible-moves-king king ) )
    (if ( > ( length possible-moves ) 0 )
        ( nth ( random ( length possible-moves ) ) possible-moves )
        nil
	)
)



( defmethod legal-move-king ( ( king king ) ( dest-square square ) )
  ( if 
	( square-in-list-p dest-square ( possible-moves-king king ) )
	t
  )  
)

( defmethod move-king ( ( king king ) ( dest-square square ) &aux curr-square )
  ( setf curr-square ( cs king ) )
  ( if ( legal-move-king king dest-square )
    ( progn
	  ( setf ( occupier curr-square ) nil )
	  ( setf ( occupier dest-square ) king )
	  ( setf ( cs king ) dest-square )
	)
  )
)

( defmethod random-move-king ( ( king king ) &aux dest-square )
	( setf dest-square ( pick-square-king king ) )
	( move-king king dest-square )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions to do with the knight piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defmethod get-nne ( ( knight knight ) &aux curr-square n nn nne )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( n curr-square ) ) )
    ( setf n ( n curr-square ) )
    ( setf nn ( n n ) )
    ( when ( not ( null nn ) )
      ( setf nne ( e nn ) )
    )
  nne 
  ) 
)


( defmethod get-nee ( ( knight knight ) &aux curr-square n ne nee )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( n curr-square ) ) )
    ( setf n ( n curr-square ) )
    ( setf ne ( e n ) )
    ( when ( not ( null ne ) )
      ( setf nee ( e ne ) )
    )
  nee 
  ) 
)


( defmethod get-nnw ( ( knight knight ) &aux curr-square n nn nnw )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( n curr-square ) ) )
    ( setf n ( n curr-square ) )
    ( setf nn ( n n ) )
    ( when ( not ( null nn ) )
      ( setf nnw ( w nn ) )
    )
  nnw 
  ) 
)


( defmethod get-nww ( ( knight knight ) &aux curr-square n nw nww )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( n curr-square ) ) )
    ( setf n ( n curr-square ) )
    ( setf nw ( w n ) )
	( when ( not ( null nw ) )
      ( setf nww ( w nw ) )
    )
  nww 
  ) 
)

( defmethod get-sse ( ( knight knight ) &aux curr-square s ss sse )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( s curr-square ) ) )
    ( setf s ( s curr-square ) )
    ( setf ss ( s s ) )
	( when ( not ( null ss ) )
      ( setf sse ( e ss ) )
	)
  sse 
  ) 
)

( defmethod get-see ( ( knight knight ) &aux curr-square s se see )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( s curr-square ) ) )
    ( setf s ( s curr-square ) )
    ( setf se ( e s ) )
	( when see ( not ( null se ) )
      ( setf see ( e se ) )
	)
  see 
  ) 
)

( defmethod get-ssw ( ( knight knight ) &aux curr-square s ss ssw )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( s curr-square ) ) )
    ( setf s ( s curr-square ) )
    ( setf ss ( s s ) )
	( when ( not ( null ss ) )
      ( setf ssw ( w ss ) )
	)
  ssw 
  ) 
)

( defmethod get-sww ( ( knight knight ) &aux curr-square s sw sww )
  ( setf curr-square ( cs knight ) )
  ( when ( not ( null ( s curr-square ) ) )
    ( setf s ( s curr-square ) )
    ( setf sw ( w s ) )
	( when ( not ( null sw ) )
      ( setf sww ( w sw ) )
    )
  sww 
  ) 
)

( defmethod valid-destination-p ( ( knight knight ) ( dest-square square ) )
  ( or ( null ( occupier dest-square ) )
       ( not ( equal ( color ( occupier dest-square ) ) ( color knight ) ) )
  ) 
)


( defmethod possible-moves-knight ( ( knight knight ) &aux curr-square possible-moves nne nee nnw nww sse see sww ssw )
  ( setf curr-square ( cs knight ) )
  ( setf possible-moves '() )

  ( setf nne ( get-nne knight ) ) 
  ( if ( and nne ( valid-destination-p knight nne ) )
      ( push nne possible-moves ) )
	  
  ( setf nnw ( get-nnw knight ) ) 
  ( if ( and nnw ( valid-destination-p knight nnw ) )
      ( push nnw possible-moves ) )
	  
  ( setf nww ( get-nww knight ) ) 
  ( if ( and nww ( valid-destination-p knight nww ) )
      ( push nww possible-moves ) )

  ( setf nee ( get-nee knight ) ) 
  ( if ( and nee ( valid-destination-p knight nee ) )
      ( push nee possible-moves ) )
	  
  ( setf ssw ( get-ssw knight ) ) 
  ( if ( and ssw ( valid-destination-p knight ssw ) )
      ( push ssw possible-moves ) )

  ( setf sse ( get-sse knight ) ) 
  ( if ( and sse ( valid-destination-p knight sse ) )
      ( push sse possible-moves ) )
	  
  ( setf sww ( get-sww knight ) ) 
  ( if ( and sww ( valid-destination-p knight sww ) )
      ( push sww possible-moves ) )

  ( setf see ( get-see knight ) ) 
  ( if ( and see ( valid-destination-p knight see ) )
      ( push see possible-moves ) )
  
  possible-moves
)


( defmethod pick-square-knight ( ( knight knight ) &aux possible-moves)
  ( setf possible-moves ( possible-moves-knight knight ) )
    (if ( > ( length possible-moves ) 0 )
        ( nth ( random ( length possible-moves ) ) possible-moves )
        nil
	)
)


( defmethod random-move-knight ( ( knight knight ) &aux dest-square )
	( setf dest-square ( pick-square-knight knight ) )
	( move-knight knight dest-square )
)




( defmethod legal-move-knight ( ( knight knight ) ( dest-square square ) )
  ( if 
	( square-in-list-p dest-square ( possible-moves-knight knight ) )
	t
  )  
)

( defmethod move-knight ( ( knight knight ) ( dest-square square ) &aux curr-square )
  ( setf curr-square ( cs knight ) )
  ( if ( legal-move-knight knight dest-square )
    ( progn
	  ( setf ( occupier curr-square ) nil )
	  ( setf ( occupier dest-square ) knight )
	  ( setf ( cs knight ) dest-square )
	)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions to do with the pawn piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defmethod possible-moves-wpawn ( ( pawn pawn ) &aux curr-square possible-moves rank )
  ( setf curr-square ( cs pawn ) )
  ( setf rank ( rank curr-square ) )
  ( setf possible-moves '() )
  ( if ( and ( not ( null ( n curr-square ) ) )
             ( null ( occupier ( n curr-square ) ) )
       )
       ( push ( n curr-square ) possible-moves )
  )
  ( if ( and ( = rank 1 )
             ( null ( occupier ( n ( n curr-square ) ) ) )
       )
       ( push ( n ( n curr-square ) ) possible-moves ) 
  )
  ( if ( and ( not ( null ( ne curr-square ) ) )
             ( occupier ( ne curr-square ) )
             ( not ( equal ( color pawn ) ( color ( occupier ( ne curr-square ) ) ) ) )
       )
       ( push ( ne curr-square ) possible-moves )
  )
  ( if ( and ( not ( null ( nw curr-square ) ) )
             ( occupier ( nw curr-square ) )
             ( not ( equal ( color pawn ) ( color ( occupier ( nw curr-square ) ) ) ) )
       )
       ( push ( nw curr-square ) possible-moves )
  )
  possible-moves
)




( defmethod possible-moves-bpawn ( ( pawn pawn ) &aux curr-square possible-moves rank )
  ( setf curr-square ( cs pawn ) )
  ( setf rank ( rank curr-square ) )
  ( setf possible-moves '() )
  ( if ( and ( not ( null ( s curr-square ) ) )
             ( null ( occupier ( s curr-square ) ) )
       )
       ( push ( s curr-square ) possible-moves )
  )
  ( if ( and ( = rank 6 )
             ( null ( occupier ( s ( s curr-square ) ) ) )
       )
       ( push ( s ( s curr-square ) ) possible-moves ) 
  )
  ( if ( and ( not ( null ( se curr-square ) ) )
             ( occupier ( se curr-square ) )
             ( not ( equal ( color pawn ) ( color ( occupier ( se curr-square ) ) ) ) )
       )
       ( push ( se curr-square ) possible-moves )
  )
  ( if ( and ( not ( null ( sw curr-square ) ) )
             ( occupier ( sw curr-square ) )
             ( not ( equal ( color pawn ) ( color ( occupier ( sw curr-square ) ) ) ) )
       )
       ( push ( sw curr-square ) possible-moves )
  )
  possible-moves
)


( defmethod possible-moves-pawn ( ( pawn pawn ) ) 
  ( cond 
    ( ( eq ( color pawn ) 'b ) 
	  ( possible-moves-bpawn pawn )
	) 
	( ( eq ( color pawn ) 'w )
	  ( possible-moves-wpawn pawn )
	)
  )
)

( defmethod pick-square-pawn ( ( pawn pawn ) &aux possible-moves)
  ( setf possible-moves ( possible-moves-pawn pawn ) )
    (if ( > ( length possible-moves ) 0 )
        ( nth ( random ( length possible-moves ) ) possible-moves )
        nil
	)
)



( defmethod random-move-pawn ( ( pawn pawn ) &aux dest-square )
	( setf dest-square ( pick-square-pawn pawn ) )
	( move-pawn pawn dest-square )
)
 
( defmethod legal-move-wpawn ( ( pawn pawn ) ( dest-square square ) )
  ( if 
	( square-in-list-p dest-square ( possible-moves-wpawn pawn ) )
	t
  )
)


( defmethod legal-move-bpawn ( ( pawn pawn ) ( dest-square square ) &aux curr-square )
  ( if 
	( square-in-list-p dest-square ( possible-moves-bpawn pawn ) )
	t
  )
)


( defmethod legal-move-pawn ( ( pawn pawn ) ( dest-square square ) )
  ( cond 
    ( ( eq ( color pawn ) 'b ) 
	  ( legal-move-bpawn pawn dest-square )
	) 
	( ( eq ( color pawn ) 'w )
	  ( legal-move-wpawn pawn dest-square )
	)
  )
)


 
( defmethod move-wpawn ( ( pawn pawn ) ( dest-square square ) &aux curr-square )
  ( setf curr-square ( cs pawn ) )
  ( if (legal-move-pawn pawn dest-square )
    ( progn
	  ( setf ( occupier curr-square ) nil )
	  ( setf ( occupier dest-square ) pawn )
	  ( setf ( cs pawn ) dest-square )
	)
  )
)

( defmethod move-bpawn ( ( pawn pawn ) ( dest-square square ) &aux curr-square )
  ( setf curr-square ( cs pawn ) )
  ( if (legal-move-pawn pawn dest-square )
    ( progn
	  ( setf ( occupier curr-square ) nil )
	  ( setf ( occupier dest-square ) pawn )
	  ( setf ( cs pawn ) dest-square )
	)
  )
)
 
 
( defmethod move-pawn ( ( pawn pawn ) ( dest-square square ) )
  ( cond 
    ( ( eq ( color pawn ) 'b ) 
	  ( move-bpawn pawn dest-square )
	) 
	( ( eq ( color pawn ) 'w )
	  ( move-wpawn pawn dest-square )
	)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( defmethod possible-moves ( ( piece piece ) )
  ( cond 
    ( ( eq ( type piece ) 'pawn ) ( possible-moves-pawn piece ) )
    ( ( eq ( type piece ) 'queen ) ( possible-moves-queen piece ) )
    ( ( eq ( type piece ) 'king ) ( possible-moves-king piece ) )
    ( ( eq ( type piece ) 'rook ) ( possible-moves-rook piece ) )
    ( ( eq ( type piece ) 'knight ) ( possible-moves-knight piece ) )
    ( ( eq ( type piece ) 'bishop ) ( possible-moves-bishop piece ) )
  )
)

( defmethod legal-move ( ( piece piece ) (dest-square square ) )
  ( cond 
    ( ( eq ( type piece ) 'pawn ) ( legal-move-pawn piece dest-square ) )
    ( ( eq ( type piece ) 'queen ) ( legal-move-queen piece dest-square ) )
    ( ( eq ( type piece ) 'king ) ( legal-move-king piece dest-square ) )
    ( ( eq ( type piece ) 'rook ) ( legal-move-rook piece dest-square ) )
    ( ( eq ( type piece ) 'knight ) ( legal-move-knight piece dest-square ) )
    ( ( eq ( type piece ) 'bishop ) ( legal-move-bishop piece dest-square ) )
  )
 )

( defmethod move-piece ( ( piece piece ) ( dest-square square ) &aux color )
  ( setf color ( color piece ) )
  ( if ( not ( null ( occupier dest-square ) ) )
    ( remove ( occupier ( oppo-pieces-of-color color ) ) )
  )
  ( cond 
    ( ( eq ( type piece ) 'pawn ) ( move-pawn piece dest-square ) )
    ( ( eq ( type piece ) 'queen ) ( move-queen piece dest-square ) )
    ( ( eq ( type piece ) 'king ) ( move-king piece dest-square ) )
    ( ( eq ( type piece ) 'rook ) ( move-rook piece dest-square ) )
    ( ( eq ( type piece ) 'knight ) ( move-knight piece dest-square ) )
    ( ( eq ( type piece ) 'bishop ) ( move-bishop piece dest-square ) )
  )
)

( defmethod move-piece ( ( piece piece ) ( dest-square square ) )
  ( if ( legal-move piece dest-square )
      ( progn
        ( setf ( occupier dest-square ) piece )
        ( setf ( occupier ( cs piece ) ) nil )
        ( setf ( cs piece ) dest-square )
        t ) ; Return t to indicate a successful move
      nil ) ; Return nil to indicate an unsuccessful move
)


( defun oppo-pieces-of-color ( color ) 
  ( cond 
    ( ( eq color 'b ) *white-pieces* )
	( ( eq color 'w ) *black-pieces* )
  )
)

( defun oppo-color ( color )
  ( cond 
    ( ( eq color 'w ) 'b )
	( ( eq color 'b ) 'w )
  )
)

( defmethod remove-bpiece ( ( piece piece ) ) 
  ( setf *black-pieces* ( remove  piece *black-pieces*) )
)

( defmethod remove-wpiece ( ( piece piece ) ) 
  ( setf *white-pieces* ( remove  piece *white-pieces*) )
)

( defmethod remove-piece( ( piece piece ) &aux color ) 
  ( setf color ( color piece ) )
  ( cond 
    ( ( eq color 'w ) ( remove-wpiece piece ) )
	( ( eq color 'b ) ( remove-bpiece piece ) )
  )
)


( defmethod move ( ( curr-square square ) ( dest-square square ) &aux color )
  ( setf color ( color ( occupier curr-square ) ) )
  ( if ( occupier dest-square )
       ( remove-piece ( occupier dest-square ) ) 
  )	   
  ( if ( legal-move ( occupier curr-square ) dest-square )
       ( move-piece ( occupier curr-square ) dest-square ) 		
	  ( format t "Invalid Move Chosen")
  )
)


( defun random-move ( move-pairs &aux curr-square dest square selected ) 
  ( setf selected ( nth ( random ( length move-pairs ) ) move-pairs ) )
  ( setf curr-square ( car selected ) )
  ( setf dest-square ( car ( cdr selected ) ) )
  ( move curr-square dest-square )
)

( defmethod get-move-pair-list ( ( piece piece ) &aux curr-square poss-dests )
  ( setf curr-square ( cs piece ) )
  ( setf poss-dests ( possible-moves piece ) )
  ( mapcar ( lambda ( dest ) ( list curr-square dest ) ) poss-dests )
)

( defun get-all-move-pair-list ( pieces )
  ( loop for piece in pieces
        append ( get-move-pair-list piece ) )
)




