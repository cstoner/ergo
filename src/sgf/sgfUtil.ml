type value = None
	     | Number of int
	     | Real of float
	     | Double of char
	     | SimpleText of string
	     | Text of string
	     | Move of string
	     | Stone of string
	     | Color of char
;;

type property = string*(value list);;

type node = Node of property list;;

type sgf_tree = Tree of node list*(sgf_tree list);;
