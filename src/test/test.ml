open OUnit;;
open Gtp;;

let rec print_results res=
  let print_result x=
    let str_p y= string_of_path y in
      match x with
	  RSuccess x -> ()
	| RFailure (x, out) -> print_string ((str_p x)^":\t\tFailed\n")
	| RError (x, out) -> print_string ((str_p x)^":\t\t-**Error**-: "^out ^"\n")
  in
    
    match res with
	(* I should add text in here to pretty-print the result *)
	[] -> ()
      | h::t -> (
	  print_result h;
	  print_results t
	)
;;

let suite_of_string suites str=
  let rec srch pairs =
    match pairs with
	[] -> []
      | h::t -> (
	  match h with
	      (suite_name, suite) -> (
		if (compare suite_name str)=0 then
		  suite
		else
		  srch t
	      )
	)
  in
    srch suites
;;


let test_suites_of_string suites str=
  let rec get_all remaining accum =
    match remaining with
	[] -> accum;
      | h::t -> (
	  match h with
	      (_, suite) -> (get_all t (List.append [suite] accum))
	)
  in

  let rec iter n res=
    try
      let r = Str.regexp_case_fold("\\([a-z]*\\)") in
      let next = (Str.search_forward r str n) in
      let matched = Str.matched_string str in
      let suite = suite_of_string suites matched in
	iter next (List.append suite res)
    with
	Not_found -> []
  in

(*  let tests_to_run = (iter 0) in
*)  
    get_all suites []
;;

let all_suites =
  [("board", Board.get_test_suite());
   ("sgf", Sgf.get_test_suite());
   ("patterns", Patterns.get_test_suite())
  ]
;;

let run_tests str_module =
  let rec suites_to_run remaining=
    match (String.compare "all" str_module) with
	0 -> (
	  match remaining with
	      [] -> []
	    | (name, suite)::t -> (List.append [suite] (suites_to_run t))
	)
      | _ -> (
	  match remaining with
	      [] -> []
	    | (name, suite)::t -> (if (String.compare name str_module)=0 then [suite] else suites_to_run t)
	)
  in

    try
      let results= run_test_tt ("TEST" >: TestList( suites_to_run all_suites )) in
	print_string "\n";
	print_results results
    with 
	Failure s -> print_string ("Failure caught during tests: " ^ s ^ "\n")
;;
