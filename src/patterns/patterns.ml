(* Blech. This code is a fucking mess. I should clean it up
   but it works *)

open OUnit;;

type regexAtom = RBlack
		 | RWhite
		 | REmpty
		 | RAny
		 | ROff
		 | ROr of regexAtom*regexAtom
;;

type travelDir = Down
		 | UpRight
		 | UpLeft
		 | DownLeft
		 | DownRight
;;

let fn_of_dir = function
    Down -> Board.down
  | UpRight -> Board.upRight
  | UpLeft -> Board.upLeft
  | DownRight -> Board.downRight
  | DownLeft -> Board.downLeft
;;
let next_dir = function
    UpRight -> UpLeft 
  | UpLeft -> DownLeft
  | DownLeft -> Down 
  | Down -> DownRight 
  | DownRight -> UpLeft 
;;

let rec get_line_rev curr_pos (move_fn, dist) acc=
  if dist < 0 then
    (try
       (List.hd acc, acc)
     with
	 _ -> (curr_pos, []))
  else
    get_line_rev
      (move_fn curr_pos)
      (move_fn, dist-1)
      (curr_pos::acc)
;;

let make_ring start_pos length =
  let ringPath = [UpRight; UpLeft; DownLeft; Down; DownRight] in
  let (_, ret) = List.fold_left 
    (fun (curr_pos, pos_list) dir ->
       let (end_pos, new_rev_list) =
	 match dir with
	     Down -> 
	       ((fn_of_dir dir) curr_pos, [(fn_of_dir dir) curr_pos])
	   | DownRight ->
	       get_line_rev 
		 ((fn_of_dir dir) curr_pos) 
		 ((fn_of_dir dir), length-2) []
	   | _ ->
	       get_line_rev 
		 ((fn_of_dir dir) curr_pos) 
		 ((fn_of_dir dir), length-1) []
       in
	 (end_pos, List.append new_rev_list pos_list))
    (start_pos, [start_pos])
    ringPath
  in
    List.rev ret
;;

(* Takes a starting position and a function that maps stone colors to the
   internal regex format final string will be at least of requested length*)
let regex_of_board start_pos length =
  let rec get_offset ring_dist dist =
    if(ring_dist - dist <= 0) then
      Board.down start_pos
    else
      Board.down (get_offset ring_dist (dist+1))
  in
    
  let rec inner ring_dist acc =
    if (length - (List.length acc) <= 0) then
      acc
    else
      inner (ring_dist + 1) 
	(acc@(make_ring (get_offset ring_dist 1)  ring_dist))
  in
    (* This mess of a function:
       First - creates a list of positions
       Second - maps that list to regex atoms
       Third - converts the list to an array for future savings
    *)
    Array.of_list(
      (List.map 
	 (* This function should probably be replaced by an
	    argument passed to regex_of_board *)
	 (fun x ->
	    (* A mark implies that the stone MUST be as specified *)
	    match Board.get_status x with
		Board.Black ->
		  if (Board.get_mark x > 0) then
		    ROr (RBlack, REmpty)
		  else
		    RBlack
	      | Board.White ->
		  if (Board.get_mark x > 0) then
		    ROr (RWhite, REmpty)
		  else
		    RWhite
	      | Board.Off (_, _) ->
		  ROff
	      | _ (*Ko and Empty*) ->
		  if (Board.get_mark x > 0) then
		    RAny
		  else
		    REmpty
	 )
	 (inner 1 []) 
      )
    )
;;

let rec string_of_regex regex stone_map =
  Array.fold_left 
    (fun acc elem -> acc ^ (stone_map elem))
    ""
    regex
;;

(* It would seem that this operation necessarily eliminates
   one rings worth of data to get the last few elements... hrm... *)
exception NoMoreOffsets;;

let rot_left regex =
  let ring_offsets = [4; 12; 24; 40; 60; 84; 112; 142; 176; 214] in
  let ret_reg = Array.copy regex in
    
  let rotate_ring ring_num =
    let ring_starts_index =
      if(ring_num = 1) then 0
      else (List.nth ring_offsets (ring_num-2)) in
    let ring_ends_index = (List.nth ring_offsets (ring_num-1)) in
      
    let rec build_start pos count inc=
      if(count = 0) then
	()
      else (
	(Array.set 
	   ret_reg    (* effected string *)
	   pos        (* position *)
	   (* replacement char *)
	   (Array.get regex (ring_ends_index - ring_num + inc))
	);
	build_start (pos + 1) (count - 1) (inc+1)
      )
    in
      
    let rec build_end pos =
      if(ring_ends_index = pos) then
	()
      else (
	(Array.set
	   ret_reg    (* effected string *)
	   pos        (* position *)
	   (* replacement char *)
	   (Array.get regex (pos - ring_num)));
	build_end (pos + 1)
      )
    in  
      (build_start 
	 ring_starts_index 
	 ring_num
	 0);
      (build_end 
	 (ring_starts_index + ring_num))
  in
    
  let rec inner offsets ring_num=
    match offsets with
	[] -> raise NoMoreOffsets
      | hd::tl ->
	  if(hd >= Array.length(ret_reg)) then
	    ret_reg
	  else (
	    rotate_ring ring_num;
	    inner tl (ring_num+1)
	  )
  in
    (inner ring_offsets 1)
;;

let reflect regex =
  let build_amts = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21] in
  let ring_offsets = [4; 12; 24; 40; 60; 84; 112; 142; 176; 214] in
  let ret_reg = Array.copy regex in

  let reflect_ring ring_num =
    let ring_starts_index =
      if(ring_num = 1) then 0
      else (List.nth ring_offsets (ring_num-2)) in
    let ring_ends_index = (List.nth ring_offsets (ring_num-1)) in

    let rec build start_pos end_pos count =
      if(count = 0) then
	()
      else (
	(Array.set 
	   ret_reg   (* effected string *)
	   start_pos (* position *)
	   (* replacement char *)
	   (Array.get regex end_pos)
	);
	build (start_pos+1) (end_pos-1) (count-1)
      )
    in
      
    let first_amt = List.nth build_amts (ring_num-1) in
    let second_amt = List.nth build_amts ring_num in

      (* This reflects the 'first part' of the ring *)
      (build
	 ring_starts_index
	 (ring_starts_index - 1 + first_amt)
	 first_amt);

      (* This reflects the 'second part' of the ring *)
      (build
	 (ring_ends_index - second_amt)
	 (ring_ends_index - 1)
	 second_amt)
  in

  let rec inner offsets ring_num =
    match offsets with
	[] -> raise NoMoreOffsets
      | hd::tl ->
	  if(hd >= Array.length ret_reg) then
	    ret_reg
	  else (
	    reflect_ring ring_num;
	    inner tl (ring_num+1)
	  )
  in
    
    (inner ring_offsets 1)
;;

let get_test_suite ()=
  let rec stone_map x = 
    match x with
	 RBlack -> "O"
       | RWhite -> "X"
       | ROff  -> "Z"
       | REmpty -> "."
       | ROr (r1, r2) -> ("(" ^ (stone_map r1) ^ "|" ^ (stone_map r2) ^ ")")
       | RAny -> "." (* This is wrong!!! but my logic needs to be fixed *)
  in
  let simple_regex_string_test center result num display =
    assert_bool display (
      if (string_of_regex (regex_of_board 
	    center
	    num) stone_map  = result) then
	true
      else (
	print_string "\n";
	Board.print_board();
	print_string "\n";
	
	print_string ("EXPECTED: " ^ result ^
			"\nACTUAL: " ^ 
			(string_of_regex (regex_of_board 
					   center
					   num)
			   stone_map
			)
		     );
	false
      ))
  in
  let rec print_pos_list pos_list =
    match pos_list with
	[] -> print_string "\n"
      | hd::tl -> (
	  (match hd with
	       Board.On_Board x -> print_string (" " ^ (string_of_int x))
	     | Board.Off_Board (x, y) -> print_string (" Z(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ")")
	     | _ -> ());
	  print_pos_list tl)
  in

  let size= 5 in
    "Board -> string transformation tests" >:::
      [("ring_dist = 1 - no edge" >:: fun _ ->(
	  Board.boardsize size;
	  Board.clear_board();
	  
	  let center_pos = (Board.pos_of_string "c3") in
	    
	  let expected1 = [Board.pos_of_string "c2";
			   Board.pos_of_string "d3";
			   Board.pos_of_string "c4";
			   Board.pos_of_string "b3";
			   Board.pos_of_string "b2"] in
	  let result1 = (make_ring (Board.down center_pos) 1) in
	    assert_bool "0" (
	      if(expected1 = result1) then
		true
	      else (
		print_string "EXPECTED:";
		print_pos_list expected1;
		print_string "ACTUAL:";
		print_pos_list result1;
		false
	      )
	    );
	    
	    simple_regex_string_test center_pos "....." 5 "1";
	    
	    Board.trymove Board.Black (Board.pos_of_string "c2");
	    simple_regex_string_test center_pos "O...." 5 "2";
	    Board.pop_move();
	    Board.trymove Board.Black (Board.pos_of_string "d3");
	    simple_regex_string_test center_pos ".O..." 5 "3";
	    Board.pop_move();
	    Board.trymove Board.Black (Board.pos_of_string "c4");
	    simple_regex_string_test center_pos "..O.." 5 "4";
	    Board.pop_move();
	    Board.trymove Board.Black (Board.pos_of_string "b3");
	    simple_regex_string_test center_pos "...O." 5 "5";
	    Board.pop_move();
	    Board.trymove Board.Black (Board.pos_of_string "b2");
	    simple_regex_string_test center_pos "....O" 5 "6"
	));
       ("ring_dist = 2+ - no edge" >:: fun _ ->
	  Board.boardsize size;
	  Board.clear_board();
	  
	  let center_pos = (Board.pos_of_string "c3") in
	    
	  let expected1 = [Board.pos_of_string "c1";
			   Board.pos_of_string "d2";
			   Board.pos_of_string "e3";
			   Board.pos_of_string "d4";
			   Board.pos_of_string "c5";
			   Board.pos_of_string "b4";
			   Board.pos_of_string "a3";
			   Board.pos_of_string "a2";
			   Board.pos_of_string "b1"] in
	  let result1 = (make_ring (Board.down (Board.down center_pos)) 2) in
	    assert_bool "0" (
	      if(expected1 = result1) then
		true
	      else (
		print_string "EXPECTED:";
		print_pos_list expected1;
		print_string "ACTUAL:";
		print_pos_list result1;
		false
	      )
	    );
	    simple_regex_string_test center_pos ".............." 14 "1";
	    Board.trymove Board.Black (Board.pos_of_string "c2");
	    simple_regex_string_test center_pos "O............." 14 "2";
	    Board.pop_move();
	    Board.trymove Board.Black (Board.pos_of_string "c1");
	    simple_regex_string_test center_pos ".....O........" 14 "3";
	    Board.trymove Board.Black (Board.pos_of_string "d2");
	    simple_regex_string_test center_pos ".....OO......." 14 "4";
	    Board.trymove Board.White (Board.pos_of_string "c2");
	    simple_regex_string_test center_pos "X....OO......." 14 "5"
       );
       ("ring_dist = 2+ - edge" >:: fun _ ->
	  Board.boardsize size;
	  Board.clear_board();

	  let root_pos = (Board.pos_of_string "a3") in	  

	  let expected1 = [Board.pos_of_string "a2";
			   Board.pos_of_string "b3";
			   Board.pos_of_string "a4";
			   (Board.left (Board.pos_of_string "a3"));
			   (Board.left (Board.pos_of_string "a2"))] in
	  let result1 = (make_ring (Board.down root_pos) 1) in
	    assert_bool "0" (
	      if(expected1 = result1) then
		true
	      else (
		print_string "EXPECTED:";
		print_pos_list expected1;
		print_string "ACTUAL:";
		print_pos_list result1;
		false
	      )
	    );

	  let expected2 = [Board.pos_of_string "a1";
			   Board.pos_of_string "b2";
			   Board.pos_of_string "c3";
			   Board.pos_of_string "b4";
			   Board.pos_of_string "a5";
			   (Board.left (Board.pos_of_string "a4"));
			   (Board.left (Board.left (Board.pos_of_string "a3")));
			   (Board.left (Board.left (Board.pos_of_string "a2")));
			   (Board.left (Board.pos_of_string "a1"))] in

	  let result2 = (make_ring (Board.down (Board.down root_pos)) 2) in
	    assert_bool "1" (
	      if(expected2 = result2) then
		true
	      else (
		print_string "EXPECTED:";
		print_pos_list expected2;
		print_string "ACTUAL:";
		print_pos_list result2;
		false
	      )
	    )
       );
       ("Rotations" >:: fun _ ->
	  Board.boardsize size;
	  Board.clear_board();
	  
          (* After the following moves, the board will look like
	     .....
	     .WBB.
	     .B.W.
	     .WWB.
	     .....
	  *)
	  Board.trymove Board.White (Board.pos_of_string "c2");
	  Board.trymove Board.White (Board.pos_of_string "d3");
	  Board.trymove Board.Black (Board.pos_of_string "c4");
	  Board.trymove Board.Black (Board.pos_of_string "b3");
	  
	  Board.trymove Board.White (Board.pos_of_string "b4");
	  Board.trymove Board.White (Board.pos_of_string "b2");
	  Board.trymove Board.Black (Board.pos_of_string "d2");
	  Board.trymove Board.Black (Board.pos_of_string "d4");
	  
	  let center_pos = (Board.pos_of_string "c3") in
	  let reg1 = regex_of_board center_pos 24 in
	  let reg2 = rot_left reg1 in
	  let reg3 = rot_left reg2 in
	  let reg4 = rot_left reg3 in

	  let result1 = string_of_regex (rot_left reg1) stone_map in
	  let result2 = string_of_regex (rot_left reg2) stone_map in
	  let result3 = string_of_regex (rot_left reg3) stone_map in
	  let result4 = string_of_regex (rot_left reg4) stone_map in

	    print_string ("\n" ^ result1 ^ "\n");
	    assert_bool "1" (result1 = "OXXOX.X.O.O...Z..Z..Z..ZZ.Z");
	    assert_bool "2" (result2 = "OOXXO.X.X.O...Z..Z..Z..ZZ.Z");
	    assert_bool "3" (result3 = "XOOXO.O.X.X...Z..Z..Z..ZZ.Z");
	    assert_bool "4" (result4 = "XXOOX.O.O.X...Z..Z..Z..ZZ.Z");

	    assert_bool "5" ((rot_left reg4) = reg1)
       );
       ("Reflections:" >:: fun _ ->
	  Board.boardsize size;
	  Board.clear_board();
	  
          (* After the following moves, the board will look like
	     .....
	     .WBB.
	     .B.W.
	     .WWB.
	     .....
	  *)
	  Board.trymove Board.White (Board.pos_of_string "c2");
	  Board.trymove Board.White (Board.pos_of_string "d3");
	  Board.trymove Board.Black (Board.pos_of_string "c4");
	  Board.trymove Board.Black (Board.pos_of_string "b3");
	  
	  Board.trymove Board.White (Board.pos_of_string "b4");
	  Board.trymove Board.White (Board.pos_of_string "b2");
	  Board.trymove Board.Black (Board.pos_of_string "d2");
	  Board.trymove Board.Black (Board.pos_of_string "d4");

	  let center_pos = (Board.pos_of_string "c3") in
	  let reg1 = regex_of_board center_pos 24 in
	  let reg2 = reflect reg1 in
	  let result2 = string_of_regex reg2 stone_map in
	    assert_bool "1" (
	      if(result2 = "XOOXO.X.X.O...Z..Z..Z..ZZ.Z") then
		true
	      else (
		print_string ("\nEXPECTED: " ^ "XOOXO.X.X.O...Z..Z..Z..ZZ.Z" ^
			      "\nACTUAL:   " ^ result2 ^ "\n");
		false
	      )
	    )
	  
       )
      ]
;;
