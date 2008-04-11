open Gtp;;

(**********************************************
GTP Hook functions begin here
Maybe these should be moved into another file... later
***********************************************)

let gtp_boardsize str=
  (* Need to make sure that the argument is a valid int... *)
  Board.boardsize (int_of_string str);
  {status=Ok; result=""}
;;

let gtp_komi str=
  (* Need to make sure that the argument is a valid float... *)
  Board.set_komi (float_of_string str);
  {status=Ok; result=""}
;;

let gtp_showboard str=
  {status=Ok; result= "\n" ^ (Board.string_of_board())}
;;

let gtp_undo str=
  try
    Board.pop_move();
    {status=Ok; result=""}
  with
      Stack.Empty -> {status=Error; result="cannot undo"}
;;

let gtp_play str=
  (* I need to split the string up into color and vertex *)
  (* hopefully this matches two different *)
  let r = Str.regexp_case_fold("\\([^ ]*\\) *\\([a-z][0-9]*\\)") in 
    if (Str.string_match r str 0) then
      let col=Str.matched_group 1 str in
      let posn = Str.matched_group 2 str in
	match String.get col 0 with
	    'w' -> (
	      try
		Board.trymove Board.White (Board.pos_of_string posn);
		{status=Ok; result=""}
	      with 
		  Board.Illegal_Move ->  {status=Error; result="illegal move"}
	    )
	  |'b' -> (
	     try
	       Board.trymove Board.Black (Board.pos_of_string posn);
	       {status=Ok; result=""}
	     with 
		 Board.Illegal_Move ->  {status=Error; result="illegal move"};
	    )
	  | _ ->
	      {status=Error; result="not a valid color"}


    else
      {status=Error; result="illegal move"}
;;

let gtp_clear_board str=
  Board.clear_board();
  {status=Ok; result=""}
;;

(* I need to split the input string into filename and (optional) node number *)
(* Then, move through the nodes up to the specified node (or the end) *)

let gtp_loadsgf filename_args=
(*  let tree= Sgf.open_from_file filename_args in *)
    {status=Ok; result=""}
;;

let ergo_run_tests str=
  Test.run_tests str;
  {status=Ok; result=""}
;;

(****************************************************
Argument Parser
*****************************************************)
let parse_cmd_line ()=
  let arglist =
    [("-test", Arg.String (fun str -> (Test.run_tests str; exit 0)), "cmd\t\truns the specified test suite");
     ("-all_tests", Arg.Unit (fun () -> (Test.run_tests "all"; exit 0)), "\t\truns all unit tests")]
  in
  let usage = "Usage: ergo (optional)args \n Available arguments\n" in
    Arg.parse arglist  (* The Actual list of commands *)
      (fun _ -> ())    (* Anonymous function handler (commands without a prefix-) *)
      usage            (* Usage text *)
;;
  
(****************************************************
MAIN
*****************************************************)
let main dummy=
  (* I should be starting a random seed or something here, but whatever *)
  parse_cmd_line();
  print_string "Ergo v0.0.0.1a\n";
  let ftn_hooks = 
    [{name="boardsize"; ftn=gtp_boardsize};
     {name="komi"; ftn=gtp_komi};
     {name="showboard"; ftn=gtp_showboard};
     {name="undo"; ftn=gtp_undo};
     {name="play"; ftn=gtp_play};
     {name="clear_board"; ftn=gtp_clear_board};
     {name="ergo-run-tests"; ftn=ergo_run_tests};
     {name="loadsgf";ftn=gtp_loadsgf}
    ]
  in

    Gtp.gtp_main ftn_hooks
;;

main ();;
