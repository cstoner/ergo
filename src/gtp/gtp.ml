type cmd_string= {id:string; 
		  cmd:string; 
		  args:string};;

exception Bad_Input;;

(* Takes a well formed string and converts it into a list containing
   the id (optionally provided), command name, and available arguments 
   if the input doesn't match (somehow...) Bad_Input is raised
*)
let input_to_cmd_string input=
  (* This regexp should match "(id) (command) (args)" *)
  let r=Str.regexp_case_fold(" *\\([0-9]*\\) *\\(\\([^ ]\\)+\\) *\\(.*\\)$") in
    if (Str.string_match r input 0)=true then
      let i = Str.matched_group 1 input in
      let c = Str.matched_group 2 input in
      let a = Str.matched_group 4 input in
	{id=i; cmd=c; args=a}
    else
      raise Bad_Input
;;

exception Cmd_Not_Found;;
type gtp_status = Ok | Error | Quit;;
type cmd_response = {status: gtp_status; result: string};;
type cmd_record = {name: string; ftn: string->cmd_response};;

let print_gtp_result id res=
  match res.status with
      Ok 
    | Quit -> print_string ("="^id^" "^res.result^"\n\n")
    | Error -> print_string ("?"^id^" "^res.result^"\n\n")
;;

let gtp_protocol_version str=
  {status= Ok; result= "2"}
let gtp_name str=
  {status= Ok; result= "JoGo (Ergo?)"};;
let gtp_version str=
  {status=Ok; result= "0.0.0.1a"};;
let gtp_quit str=
  {status=Quit; result= "Goodbye."};; 

(* This function is a little hackish
   It is used as a function of avail_cmds to get a type string -> cmd_response
   This is needed later to avoid a chicken and egg problem with the 
   available_commands list.
*)
let gtp_known_command avail_cmds cmd_str=
  let command_name_matches cmd str= compare cmd.name str=0 in
  let rec inner_loop cmds=
    match cmds with
	[] -> {status=Ok; result="false"}
      | curr::remaining ->
	  if command_name_matches curr cmd_str then
	    {status=Ok; result="true"}
	  else
	    inner_loop remaining
  in
    if compare cmd_str "known_command"=0 then
      {status=Ok; result="true"}
    else if compare cmd_str "list_commands"=0 then
      {status=Ok; result="true"}
    else
      inner_loop avail_cmds
;; 

(* This function gets the same disclaimer as gtp_known_command
   A little hackery is needed to make it work like it should *)
let gtp_list_commands avail_cmds str=
  let rec inner_loop cmds=
    match cmds with
	[] -> ""
      | curr::remains -> curr.name ^ "\n" ^ (inner_loop remains)
  in
    {status=Ok; result="\nknown_command\nlist_commands\n" ^ inner_loop avail_cmds}
;;

(* General purpose command searching function
   cmd_str = the command to search for
   avail_cmds = all of the available commands *NOTE* some exceptions allowed
   on_find = function to execute on found command of type cmd_rec -> 'a

   Raises Cmd_Not_Found exception if the command is not found
   *NOTE* Exceptions to avail_cmds are being made for the known_command
   and list_commands functions as these suffer from the problem of needing
   to know that they them selves exist to execute.
*)

let search_for_cmd cmd_str avail_cmds on_find=
  let rec inner_loop cmds=
    match cmds with
	[] -> raise Cmd_Not_Found
      | curr::remain ->
	  match compare curr.name cmd_str with
	      0 -> on_find curr
	    | _ -> inner_loop remain
  in
    (* Hackish - Only need it for these two commands, though*)
    if compare "known_command" cmd_str=0 then
      on_find {name="known_command"; ftn=(gtp_known_command avail_cmds)}
    else if compare "list_commands" cmd_str=0 then
      on_find {name="list_commands"; ftn=(gtp_list_commands avail_cmds)}
    else
      inner_loop avail_cmds
;;

let rec gtp_loop avail_cmds=
  print_string "gtp> ";
  let parse_command cmd_str=
    search_for_cmd cmd_str.cmd avail_cmds (fun cmd -> cmd)
  in  
  let input=read_line() in
    (*Add error catching code here for bad inputs *)
  let input_cmd = input_to_cmd_string (String.lowercase input) in
    try
      let result = (parse_command input_cmd).ftn input_cmd.args in
	
	print_gtp_result input_cmd.id result;
	
	if result.status=Quit then
	  exit(0)
	else
	  gtp_loop avail_cmds
    with
	Cmd_Not_Found ->
	  print_gtp_result input_cmd.id {status=Error; result="command not found"};
	  gtp_loop avail_cmds
      | Failure _ ->
	  print_gtp_result input_cmd.id {status=Error; result=("bad input")};
	  gtp_loop avail_cmds
;;

let gtp_main hooks=
  let available_commands=
    [{name= "protocol_version" ; ftn= gtp_protocol_version};
     {name= "name"             ; ftn= gtp_name};
     {name= "version"          ; ftn= gtp_version};
     {name= "quit"             ; ftn= gtp_quit}]
  in
  let all_cmds = (List.append hooks available_commands) in

  let rec print_cmds x =
    match x with
	[] -> ();
      | h::t -> print_string (h.name ^ "\n"); (print_cmds t)
  in
    gtp_loop all_cmds
;;

  
let test=
  assert ({id="1"; cmd="version_info"; args="no_args"}= 
      input_to_cmd_string "1 version_info no_args");
  assert ({id=""; cmd="version"; args=""}=
      input_to_cmd_string "version");
  assert ({id="1"; cmd="version"; args=""}= 
      input_to_cmd_string " 1 version");
  assert ({id=""; cmd="version"; args=""}=
      input_to_cmd_string " version ");
;;
