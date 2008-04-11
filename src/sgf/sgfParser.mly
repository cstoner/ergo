%token BEGIN_GAME_TREE END_GAME_TREE
%token BEGIN_NODE
%token BEGIN_VALUE END_VALUE
%token COMPOSE
%token NONE
%token <int> NUMBER
%token <float> REAL
%token <string> PROP_IDENT SIMPLE_TEXT TEXT MOVE STONE
%token <char> DOUBLE COLOR

%start game_tree
%type < SgfUtil.sgf_tree > game_tree
%%

game_tree:
     | BEGIN_GAME_TREE sequence variations END_GAME_TREE
	 { (SgfUtil.Tree ( $2, $3 )) }
;
variations:
     | game_tree variations { $1::$2 }
     | { [] }
;
sequence:
     | node sequence { $1::$2 }
     | { [] }
;
node:
     | BEGIN_NODE properties { ( SgfUtil.Node $2 ) }
;
properties:
     | property properties   { $1::$2 }
     | { [] }
;
property:
     | PROP_IDENT p_value p_value_list { ($1, $2::$3) }
;
p_value:
     | BEGIN_VALUE cvalue_type END_VALUE { $2 }
;

p_value_list:
     | p_value p_value_list { $1::$2 }
     | {[ ]}
;
cvalue_type:
     | value_type  { $1 }
/*     | COMPOSE value_type cvalue_type { [$2; $3] } */
;

value_type: 
/*     | NONE        { [] } */
     | NUMBER      { (SgfUtil.Number $1) }
     | REAL        { (SgfUtil.Real $1) }
     | DOUBLE      { (SgfUtil.Double $1) }
     | COLOR       { (SgfUtil.Color $1) }
     | SIMPLE_TEXT { (SgfUtil.SimpleText $1) }
     | TEXT        { (SgfUtil.Text $1) }
     | MOVE        { (SgfUtil.Move $1) }
     | STONE       { (SgfUtil.Stone $1) }
;
