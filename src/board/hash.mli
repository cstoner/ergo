type key = string;;
type 'a keygen = {from_board: 'a array -> key;
		  update: key -> 'a -> int -> key};;

val key_byte_size : int;;
val empty_key : string;;
val new_keygen : ('a -> int) -> nativeint array -> 'a keygen;;

(* Unit tests *)
val get_hash_tests : unit -> OUnit.test;;
