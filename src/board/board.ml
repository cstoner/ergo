open OUnit;;
open Hash;;

exception Bad_Size;;

type int_status = White 
		  | Black 
		  | Ko 
		  | Empty 
		  | Off of int*int;;
type intersection = { mutable status: int_status;
		      mutable pos: int;
		      mutable mark: int};;

type board_pos = On_Board of int 
		 | Off_Board of int*int
		 | Pass;;
type chain = { mutable color: int_status;
	       mutable stones: board_pos list;
	       mutable liberty_count: int;
	       mutable liberties: board_pos list
	     };;

type board_state = {mutable data: intersection array;
		    mutable w_caps: int;
		    mutable b_caps: int;
		    mutable hash: string;
		   };;
let g_max_size = 19;;

(* Holds the actual board data *)
let g_board = ref 
  {data=Array.make (g_max_size * g_max_size) {status=Empty; pos= -1; mark= -1};
   w_caps=0;
   b_caps=0;
   hash=Hash.empty_key};;

(* Special board used to create board diffs for the move stack *)
let g_undo_board = ref
  {data=Array.make(g_max_size * g_max_size) {status=Empty; pos= -1; mark= -1};
   w_caps = 0;
   b_caps = 0;
   hash=Hash.empty_key};;

let g_size = ref 0;;
let g_komi = ref 0.0;;

let g_move_stack = Stack.create();;
(* Fairly arbitrary number of 100 *)
let g_board_cache = Hashtbl.create 100;;

(****************************************************************************)
(* Very handy coersion functions *)
(****************************************************************************)


let string_of_status= function
    Empty
  | Ko -> "."
  | Black -> "b"
  | White -> "w"
  | _ -> ""
;;

let pos_of_int i=
  if(i < (!g_size * !g_size)) && (i >= 0) then
    On_Board i
  else
    Off_Board (-1, -1)
;;

let pos_of_coord (x, y) =
  if(x > -1) && (x < !g_size) &&
    (y > -1) && (y < !g_size) then
      On_Board (!(g_size)*(y)+(x))
  else
    Off_Board (x, y)
;;

let coord_of_pos p =
  match p with
      On_Board x ->
	let x1 = x mod !g_size in
	let y1 = x / !g_size in
	  (x1, y1)
    | Off_Board (x, y) -> (x, y)
    | Pass -> (!g_size, !g_size)
;;

exception Bad_Coord;;
let pos_of_string str=
  let r=Str.regexp_case_fold(" *\\([a-z]\\)\\([0-9]*\\)") in
    if (Str.string_match r str 0)=true then
      let c=Str.matched_group 1 str in
      let r=Str.matched_group 2 str in
      let letter_to_col ch =
	let v = (int_of_char (String.get ch 0 )) - (int_of_char 'a') in
	  if (v = 8) then
	    -1
	  else if (v > 8) then
	    v-1
	  else
	    v
      in
      let r_num = int_of_string r - 1 in
      let c_num = letter_to_col c in
	if (r_num > -1) && (r_num < !g_size) &&
	   (c_num > -1) && (c_num < !g_size) then
	     On_Board ( r_num* !g_size + c_num )
	else
	  Off_Board (c_num, r_num)
    else
      Off_Board (-1, -1)
;;

(****************************************************************************)
(* These are some nice little helper functions for getting around the board *)
(****************************************************************************)
let opponent col=
  match col with
      Black -> White
    | White -> Black
    | x -> x
;;

let up = function
    On_Board x ->
      if x >= (!g_size * !g_size - !g_size) then 
	let (x1, y1) = coord_of_pos (On_Board x) in
	  Off_Board(x1, y1+1)
      else 
	On_Board (x + !g_size)
  | Off_Board (x1, y1) -> 
      pos_of_coord (x1, y1+1);
  | Pass -> Off_Board(!g_size, !g_size)
;;

let down = function
  On_Board x ->
    if x < !g_size then 
      let (x1, y1) = coord_of_pos (On_Board x) in
	Off_Board (x1, y1-1)
    else 
      On_Board (x - !g_size)
  | Off_Board (x1, y1) -> 
      pos_of_coord (x1, y1-1)
  | Pass -> Off_Board (!g_size, !g_size)
;;

let left = function
  On_Board x ->
    if (x mod !g_size = 0) then 
      let (x1, y1) = coord_of_pos (On_Board x) in
	Off_Board (x1-1, y1)
    else 
      On_Board (x - 1)
  | Off_Board (x1, y1) -> 
      pos_of_coord (x1-1, y1)
  | Pass -> Off_Board (!g_size, !g_size)
;;

let right = function
  On_Board x ->
    if( x mod !g_size >= (!g_size -1)) then 
      let (x1, y1) = coord_of_pos (On_Board x) in
	Off_Board (x1+1, y1)
    else 
      On_Board (x+1)
  | Off_Board (x1, y1)->
      pos_of_coord (x1+1, y1)
  | Pass -> Off_Board (!g_size, !g_size)
;;	  

let upRight x = up(right x);;
let upLeft x = up(left x);;
let downRight x = down(right x);;
let downLeft x = down(left x);;
    
let neighbors = function
    On_Board x -> let pos = (On_Board x) in [(up pos); (down pos); (left pos); (right pos)]
  | Off_Board (x, y) -> [(up (pos_of_coord (x, y)));
			 (down (pos_of_coord (x, y)));
			 (left (pos_of_coord (x, y)));
			 (right (pos_of_coord (x, y)))]
  | _  -> [];;

let is_edge pos =
  if List.length (List.filter (function
				   Off_Board _ -> true
				 | _ -> false
			      ) (neighbors pos)
		 ) > 0 then true else false

;;

let is_corner pos =
  if List.length (List.filter (function
				   Off_Board _ -> true
				 | _ -> false
			      ) (neighbors pos)
		 ) > 1 then true else false

;;

(* hash_gen will be used throughout the rest of the module,
 * See hash.ml for the keygen type definition *)
let hash_gen = (Hash.new_keygen 
  (* Turn board positions into their hash mask *)
  (function
       Black -> 1
     | White -> 2
     | Ko -> 3
     | _ -> 0)
  (* This creates an array length 4 that has values indexed by the above
     function. Zero is set by hand for consistency *)
  (Array.init 4 (fun x -> match x with
		     0 -> Nativeint.zero
		   | _ -> (Random.nativeint Nativeint.max_int))))
;;

(* converts a board coordinate into it's intersection *)
let inter_of_pos= function
    On_Board x -> (!g_board.data.(x))
  | Off_Board (x, y) -> {status=(Off (x, y)); mark=0; pos=0}
  | Pass -> {status=(Off (!g_size, !g_size)); mark=0; pos=0}
;;

let boardsize s=
  if s <= g_max_size && s > 0 then (
    g_size :=  s;
    !g_board.data <- (Array.make (s*s) {status=Empty; pos= -1; mark= -1}))
  else raise Bad_Size
;;
let size()= !g_size;;

let set_komi k= g_komi := k;;
let komi()= !g_komi;;


(**
   Board Cache functions.
*)
let print_cache() =
  let board_string b = Array.fold_left 
    (fun acc i -> (acc ^ (string_of_status i.status))) "" b.data in
    
    print_string "Current cache contents:\n";
    Hashtbl.iter (fun key entry ->
		    print_string (key ^ " -> " ^ (board_string entry) ^ "\n")
		 ) g_board_cache;
    print_string "\n"
;;


let hash_of_board() =
  hash_gen.from_board (Array.map (fun x -> x.status) !g_board.data);;
let update_hash old_hash (color, pos) =
  assert (String.length old_hash = Hash.key_byte_size);
  assert (match pos with On_Board x -> x < (size()*size()) | _ -> false);
  match pos with
      On_Board x -> hash_gen.update old_hash color x
    | _ -> Hash.empty_key (* The assert above should prevent this
			     from ever actually happening *)
let save_in_cache() =
  assert (String.length !g_board.hash = Hash.key_byte_size);
  
  Hashtbl.add g_board_cache (!g_board.hash) {data=Array.copy (!g_board.data);
					     w_caps = !g_board.w_caps;
					     b_caps = !g_board.b_caps;
					     hash = !g_board.hash};
;;

let load_from_cache key =
  assert (String.length key = Hash.key_byte_size);
  let new_board = Hashtbl.find g_board_cache key in
    g_board := new_board;
;;

(**
   Undo related functionality
*)

type board_diff = Diff of (board_pos*int_status*int_status) list;;

(* Creates the diffs used for the undo stack as well as updating the undo_board to its new value *)
let rec gen_diff start_pos =
  match start_pos with
      On_Board x ->
	let old_val = !g_undo_board.data.(x).status in
	let new_val = !g_board.data.(x).status in
	if (old_val != new_val) then (
	  !g_undo_board.data.(x).status <- !g_board.data.(x).status;
	  List.append [(start_pos,
			old_val,
			new_val)]
	    (List.fold_left (fun acc elem ->
		match (gen_diff elem) with
		    [] -> acc
		  | x -> List.append x acc
			   ) [] (neighbors start_pos))
	)
	else
	  []
    | _ -> []
;;

let rec apply_diff diff =
  (match diff with
      [] -> print_string "Diff empty!!!\n"
    | _ -> ());

  List.iter (fun elem ->
	       match elem with
		   (On_Board pos, new_val, old_val) -> (
		     !g_undo_board.data.(pos).status <- new_val;
		     !g_board.data.(pos).status <- new_val;
		     match (old_val, new_val) with
			 (Empty, Black) -> (!g_board.w_caps <- !g_board.w_caps -1)
		       | (Empty, White) -> (!g_board.b_caps <- !g_board.b_caps -1)
		       | _ -> ()
		   )
		 | _ -> print_string "Weird shit in board diff\n") diff
;;

let push_move(start_pos) = 
    assert (String.length !g_board.hash = Hash.key_byte_size);
    Stack.push (!g_board.hash,  (gen_diff start_pos)) g_move_stack
;; 

(* This code will definately stop working now 
   I need to write an 'apply_undo val' function that applies the undo and updates the capture counts
*)
let pop_move() =
  match (Stack.pop g_move_stack) with
      (key, board_diff) -> apply_diff board_diff
;;

(* let pop_till key =
  let temp = Stack.pop g_move_stack in
    if String.compare key temp= 0 then
      load_from_cache key;;
*)
(* I generally feel like I've gotten the rest of this file cleaned up a bit up until
   this point. It's a continuing battle, though.
   * I desperately need to write some unit tests for the above functions *)

(****************************************************************************)
(* Functions for handling the mark *)
(* These should really be re-done somehow. Keeping track of the marks suck *)
(****************************************************************************)

let set_mark pos v=
  match pos with
      On_Board x -> (!g_board.data.(x).mark <- v)
    | _ -> ()
;;

let get_mark pos=
  match pos with
      On_Board x -> (!g_board.data.(x).mark)
    | _ -> (* How do I return NOT_A_MARK?*) -1
;;

let clear_mark pos=
  match pos with
      On_Board x -> (!g_board.data.(x).mark <- 0)
    | _ -> ()
;;

let clear_marks posns =
  List.iter (fun x -> clear_mark x) posns
;;


let clear_board() =
  !g_board.w_caps <- 0;
  !g_board.b_caps <- 0;
  !g_board.hash <- Hash.empty_key;

  !g_undo_board.w_caps <- 0;
  !g_undo_board.b_caps <- 0;
  !g_board.hash <- Hash.empty_key;

  (* Clearing the actual board *)
  Array.iteri (fun i _ ->
		 !g_board.data.(i) <- {
		   status = Empty;
		   pos = i;
		   mark = 0;
		 }) !g_board.data;

  (* Clearing the undo board *)
  Array.iteri (fun i _ ->
		 !g_undo_board.data.(i) <- {
		   status = Empty;
		   pos = i;
		   mark = 0;
		 }) !g_undo_board.data;

  Hashtbl.clear g_board_cache;

  Stack.clear g_move_stack;
  (*push_move()*)
;;



(* This is an internal function only
   It takes as an argument a list of (int_status, board_pos) pairs and
   sets the values of board_pos to the requested status 
   I do not deal with undo yet... I'm waiting for the game hashing to do that...
*)

let set_stones pairs=
  List.iter (function
		 (col, On_Board p) -> (!g_board.data.(p).status <- col)
	       | _ -> ()
	    ) pairs
;;

let get_status pos =
  match pos with
      On_Board x -> !g_board.data.(x).status
    | Off_Board (x, y) -> Off (x, y)
    | Pass -> Off(!g_size, !g_size)




let string_of_board ()=
  if !g_size > g_max_size then raise Bad_Size;

  let board_string=
    Array.fold_left (fun root elem ->
		       (root ^ string_of_status elem.status)) "" 
  in

  let final_row=
    let final_row_string = "    ABCDEFGHJKLMNOPQRSTUVWXYZ" in      
      (String.sub final_row_string 0 (4 + !g_size)) ^ 
	"\n\tW_capture: " ^ string_of_int(!g_board.w_caps) ^ 
	"\n\tB_capture: " ^ string_of_int(!g_board.b_caps)
  in
  
  let rec iter row =
    match row with 
	-1 -> ""
      | _ -> string_of_int (succ row) ^ "   " ^
	  (String.sub (board_string !g_board.data) 
	     (row*(!g_size)) 
	     (!g_size)) ^ 
	    "\n" ^ (iter (row - 1))
  in
    iter (!g_size - 1) ^ final_row
;;

exception Bad_Chain;;
let chain_of_pos pos =
  let pos_int= match pos with On_Board x -> x | _ -> -1 in
  let rec inner queue accum=
    match queue with
	[] -> accum
      | (On_Board x)::t -> (
	  if (!g_board.data.(x).status = !g_board.data.(pos_int).status) && (!g_board.data.(x).mark = 0) then
	    (
	      !g_board.data.(x).mark <- 1;
	      inner (List.append t (neighbors(pos_of_int x))) (List.append [(pos_of_int x)] accum)
	    )
	  else 
	    inner t accum
	)
      | _::t -> inner t accum
  in

  let color= !g_board.data.(pos_int).status in
    if color=Empty or color=Ko then raise Bad_Chain;

    !g_board.data.(pos_int).mark <- 1;
    let results = inner (neighbors pos) [pos] in
      clear_marks results;
      
      {stones = results;
       color = (!g_board.data.(pos_int).status);
       liberty_count = 0;
       liberties = []
      }
;;

let libs_count chn =
  let rec unmark queue=
    match queue with
	[] -> ();
      | (On_Board x)::t ->
	  if (!g_board.data.(x).mark=0) then unmark t
	  else (
	    !g_board.data.(x).mark <- 0;
	    match !g_board.data.(x).status with
		Empty -> unmark t
	      | Ko -> unmark t
	      | col -> unmark (List.append (neighbors (On_Board x)) t)
	  )
      | _::t -> unmark t
  in
  let rec inner queue accum=
      match queue with
	  [] -> accum
	| (On_Board x)::t -> (
	    if( !g_board.data.(x).mark = 0) then (
		!g_board.data.(x).mark <- 1;
		match !g_board.data.(x).status with
		    Empty -> inner t (succ accum)
		  | Ko -> inner t (succ accum)
		  | col -> 
		      if(col = chn.color) then 
			inner (List.append (neighbors (pos_of_int x)) t) accum
		      else
			inner t accum
	    )
	    else
	      inner t accum
	  )
	| _::t -> inner t accum
  in
  let results= (inner chn.stones 0) in
    unmark chn.stones;
    results
;;

let print_board ()=
  print_string (string_of_board());;

(****************************************)
(* These define general board functions *)
(****************************************)

(* Bug in this that messes up the board hash... *)
exception Illegal_Move;;
let trymove color pos=
  let opp = opponent color in
  match pos with
      On_Board x -> (
	match (inter_of_pos pos).status with
	    Empty -> (
	      assert(String.length !g_board.hash = Hash.key_byte_size);

	      (* This is apparently a no-no for the board hash... hrm.. *)
	      !g_board.data.(x).status <- color;

	      (* Check for Ko *)
	      (* Kill neighbor opponent groups *)
	      List.iter (function
			     (On_Board neigh) ->
			       begin
				 if(!g_board.data.(neigh).status=opp) then
				   let opp_stones = (chain_of_pos (On_Board neigh)).stones in
				     if libs_count (chain_of_pos (On_Board neigh))=0 then begin
				       set_stones (List.map (fun p -> (Empty, p)) opp_stones);
				       match color with
					   (* White made the move, so he/she gets the caps, etc *)
					   White -> (!g_board.w_caps <- (!g_board.w_caps + (List.length opp_stones)))
					 | Black -> (!g_board.b_caps <- (!g_board.b_caps + (List.length opp_stones)))
					 | _ -> ()
				     end
			       end
			   | _ -> ()) (neighbors (On_Board x));
	      
	      push_move pos;
	      (* Join with friendly neighbor groups - I need a chain table*) 
	      
	      (* Enforce suicide rule *)
	      if libs_count (chain_of_pos (On_Board x))=0 then (
		(* Right now, suicide is totally illegal *)
		pop_move();
		raise Illegal_Move
	      )
	      else (
		assert (String.length !g_board.hash = Hash.key_byte_size);
		!g_board.hash <- (update_hash !g_board.hash (color, pos)))
	    )
	  | _ -> raise Illegal_Move
      ) (* Matches On_Board *)
    | Pass -> assert(String.length !g_board.hash = Hash.key_byte_size); push_move pos
    | _ -> raise Illegal_Move
;;

let get_test_suite ()=
  let size= 5 in
    boardsize size;
    clear_board();

    "Board Tests" >:::
      [(" Opponent Tests" >:: fun _ ->(assert_bool "1" (opponent(Black)=White);
				       assert_bool "2" (opponent(White)=Black)));
       
       (* I should really add more of these *)
       (" PosOf*** Tests" >:: fun _->
	  boardsize size;
	  clear_board();
	  assert_bool "1" (pos_of_string "a1" = On_Board 0);
	  assert_bool "2" (pos_of_string "a2" = On_Board size);
	  assert_bool "3" (pos_of_string "c3" = On_Board (size*2+2));
	  assert_bool "4" (pos_of_string "f1" = Off_Board (5, 0));
	  assert_bool "5" (pos_of_string "a6" = Off_Board (0, 5));
	  assert_bool "6" (pos_of_string "a1" = pos_of_coord (0,0));
	  assert_bool "7" (pos_of_string "a5" = pos_of_coord (0,4));
	  assert_bool "8" (pos_of_string "c3" = pos_of_coord (2,2));
	  assert_bool "9" (pos_of_string "e4" = pos_of_coord (4,3));
	  assert_bool "10" (pos_of_int 1 = (On_Board 1));
	  assert_bool "11" (pos_of_int 25 = Off_Board (-1, -1));
	  assert_bool "12" (pos_of_int 5 = pos_of_string "a2"));
       (" Directional Tests" >:: fun _ ->
	  boardsize size;
	  clear_board();
	  assert_bool " 1" (up(pos_of_string "c3") = pos_of_string "c4");
	  assert_bool " 2" (down(pos_of_string "c3") = pos_of_string "c2");
	  assert_bool " 3" (left(pos_of_string "c3") = pos_of_string "b3");
	  assert_bool " 4" (right(pos_of_string "c3") = pos_of_string "d3");
	  
	  assert_bool " 5" (up(pos_of_string "e5") = Off_Board (4, 5));
	  assert_bool " 6" (down(pos_of_string "a1") = Off_Board(0, -1));
	  assert_bool " 7" (left(pos_of_string "a1") = Off_Board(-1, 0));
	  assert_bool " 8" (right(pos_of_string "e5") = Off_Board(5, 4));
	  
	  assert_bool " 9" (up(pos_of_string "d4") = pos_of_string "d5");
	  assert_bool "10" (down(pos_of_string "b2") = pos_of_string "b1");
	  assert_bool "11" (left(pos_of_string "b2") = pos_of_string "a2");
	  assert_bool "12" (right(pos_of_string "d4") = pos_of_string "e4"));
       
       (" Edge/Corner Tests" >:: fun _ ->
	  boardsize size;
	  clear_board();
	  assert_bool "1" (is_edge (pos_of_coord (0,2)));
	  assert_bool "2" (not (is_edge (pos_of_coord (2,2))));
	  assert_bool "3" (is_edge (pos_of_coord (2,0)));
	  assert_bool "4" (is_corner (pos_of_coord (0,0)));
	  assert_bool "5" (is_corner (pos_of_coord (0,size-1)));
	  assert_bool "6" (is_corner (pos_of_coord (size-1,0)));
	  assert_bool "7" (is_corner (pos_of_coord (size-1,size-1))));

       (" Move Tests" >:::
	  [(" Simple Move" >:: fun _ ->
	      assert_bool "1" (
		try
		  trymove Black (pos_of_string "d3");
		  get_status (pos_of_string "d3") = Black
		with
		    Illegal_Move -> false
	      );
	      assert_bool "2" (
		try
		  trymove Black (pos_of_string "d3");
		  false
		with
		    Illegal_Move -> true
	      );
	   );
	   (" Capture" >:: fun _ ->
	      clear_board();
	      (try
		 trymove Black (pos_of_string "d3");
		 trymove White (pos_of_string "d2");
		 trymove White (pos_of_string "d4");
		 trymove White (pos_of_string "c3");
		 trymove White (pos_of_string "e3");
		 assert_bool "simple capture 1" (get_status (pos_of_string "d3") = Empty);
		 assert_bool "simple capture 2" (!g_board.w_caps = 1)
	       with
		   Illegal_Move -> assert_bool "Improper Illegal_Move 1" false
		 | x -> raise x);
	      
	      (try
		 trymove Black (pos_of_string "a1");
		 trymove Black (pos_of_string "a2");
		 trymove White (pos_of_string "b1");
		 trymove White (pos_of_string "b2");
		 trymove White (pos_of_string "a3");
		 
		 assert_bool "simple capture 3"
		   (get_status (pos_of_string "a1") = Empty);
		 assert_bool "simple_capture 4"
		   (get_status (pos_of_string "a2") = Empty);
		 assert_bool "simple capture 5"
		   (!g_board.w_caps = 3)
	       with
		   Illegal_Move -> assert_bool "Improper Illegal Move 2" false
		 | x -> raise x;
	      )
	   );
	   (" Board Hashing" >:: fun _ ->
	      boardsize size;
	      clear_board();
	      let move_key = ref (hash_of_board()) in
		assert_bool "Key Length" (String.length !move_key = Hash.key_byte_size);

		trymove Black (pos_of_string "c3");
		assert_bool "Hash changes"
		  (String.compare (update_hash !move_key (Black, pos_of_string "c3")) !move_key != 0);
		move_key := (update_hash !move_key (Black, pos_of_string "c3"));
		assert_bool "Hash consistency"
		  (String.compare (hash_of_board()) !move_key = 0);


		trymove White (pos_of_string "b3");
		assert_bool "Hash changes"
		  (String.compare (update_hash !move_key (Black, pos_of_string "b3")) !move_key != 0);
		move_key := (update_hash !move_key (White, pos_of_string "b3"));
		assert_bool "Hash consistency"
		  (String.compare (hash_of_board()) !move_key = 0);

		trymove Black (pos_of_string "d3");
		assert_bool "Hash changes"
		  (String.compare (update_hash !move_key (Black, pos_of_string "d3")) !move_key != 0);
		move_key := (update_hash !move_key (Black, pos_of_string "d3"));
		assert_bool "Hash consistency"
		  (String.compare (hash_of_board()) !move_key = 0);

		trymove White (pos_of_string "e3");
		assert_bool "Hash changes"
		  (String.compare (update_hash !move_key (White, pos_of_string "e3")) !move_key != 0);
		move_key := (update_hash !move_key (White, pos_of_string "e3"));
		assert_bool "Hash consistency"
		  (String.compare (hash_of_board()) !move_key = 0)
	   );
	   (" Board cache" >:: fun _ ->
	      boardsize size;
	      clear_board();
	      save_in_cache();
		let move_key = ref "" in
		let empty_board_key = Hash.empty_key in
		  assert_bool ("Hash size not one: " ^ 
				 (string_of_int (Hashtbl.length g_board_cache)))
		    (Hashtbl.length g_board_cache = 1);
		  assert_bool "Board not empty" (get_status (pos_of_string "c3") = Empty);
		  assert_bool "Global hash not empty" (String.compare Hash.empty_key (!g_board.hash)=0);
		  trymove Black (pos_of_string "c3");
		  
		  move_key := (update_hash empty_board_key (Black, (pos_of_string "c3")));
		  assert_bool "Global hash wrong" (String.compare !move_key (!g_board.hash)=0);
		  save_in_cache();
		  
		  (* Loading from the board cache is currently broken. To fix, a seperate space in memory needs to be
		     allocated somehow to store the board. Probably as a list of position statuses *)
		  load_from_cache empty_board_key;

		  
		  let move_index = match (pos_of_string "c3") with
		      On_Board x -> x
		    | _ -> -1 in
		    assert_bool ("Empty board hash data not empty: " ^ 
				   match !g_board.data.(move_index).status with
				       Black -> "black"
				     | White -> "white"
				     | Ko -> "ko"
				     | _ -> "wtf... this should never happen")
		      (!g_board.data.(move_index).status = Empty);
		    load_from_cache !move_key;
		    assert_bool "Move board hash data" (!g_board.data.(move_index).status = Black)

	   ) 
	  ]
       );
       
       (" Chain Tests" >:: fun _ ->
	  boardsize size;
	  clear_board();

	  (* Chain of 1 *)
	  trymove Black (pos_of_string "c3");
	  assert_bool "1-chn" (List.length ((chain_of_pos (pos_of_string "c3")).stones) = 1);
	  assert_bool "1-lib" (libs_count (chain_of_pos (pos_of_string "c3")) = 4);
	  (* Chain of 2 *)
	  trymove Black (pos_of_string "c4");
	  assert_bool "2-chn" (List.length ((chain_of_pos (pos_of_string "c3")).stones) = 2);
	  assert_bool "2-lib" (libs_count (chain_of_pos (pos_of_string "c3")) = 6);

	  (* Check the corner *)
	  trymove Black (pos_of_string "a1");
	  assert_bool "3" (List.length ((chain_of_pos (pos_of_string "a1")).stones) = 1);
	  
	  (* Capture a black stone *)
	  trymove White (pos_of_string "a2");
	  trymove White (pos_of_string "b1");
	  assert_bool "4" ((inter_of_pos (pos_of_string "a1")).status = Empty)
       );
       
       (clear_board();
	Hash.get_hash_tests())
      ]
;;
