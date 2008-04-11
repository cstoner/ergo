
{ 
  open SgfParser
  open SgfUtil
}

let digit   =['0'-'9']
let ucLetter=['A'-'Z']
let none    =""
let double  = ("1" | "2")
let color   = ("B" | "W")
let sign    = ("+" | "-")
let move = ['a'-'z'] ['a'-'z']
  
rule parse_gametree = parse
  | (" "|"\t"|"\\\n") {parse_gametree lexbuf}
  | ';'          { BEGIN_NODE }
  | '('          { BEGIN_GAME_TREE }
  | ')'          { END_GAME_TREE }
  | '['          { BEGIN_VALUE }
  | ']'          { END_VALUE }
  | digit* as i  { NUMBER (int_of_string i) }
  | digit+ "."
  | digit* "." digit? as f
                 { REAL (float_of_string f) }
  | double as c  { DOUBLE c }
(*  | color as c { COLOR c } *)
  | move as s        {MOVE s}
  | ucLetter+ as s    { PROP_IDENT s }

{

}
    
