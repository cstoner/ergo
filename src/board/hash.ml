(*
 * This module provides zobrist hashing to the best of my ability
 * in ocaml. It is inteded to make a string that will be used as a 
 * Variable bitlength key
 * Currently, only 96 bits are used, but expanding should be easy
 * Any advice on how to make everything a lot cleaner would be much apreciated
 *)
open OUnit;;
open Nativeint;;

type key = string;;
type 'a keygen = {from_board: 'a array -> key;
		  update: key -> 'a -> int -> key};;

(**************************************************************
Setup variables that, as of this moment, are invariant
***************************************************************)
let char_size = 8;; 
let int_byte_size = 4;; 
let int_bit_size = char_size * int_byte_size;;



(*************************************************************
 Setup variables that should behave in a runtime consistant manner
**************************************************************)
let key_bit_size = 96;; (* Number of bits used for the key *)

(* The size of the string needed to hold  a key of key_bit_size *)
let key_byte_size = (key_bit_size / char_size) + 1;;
let empty_key = String.make key_byte_size '\000';;
let int_mask_size = int_byte_size + 1;;

(* The salt value for each piece type. Determined randomly at runtime *)
let index_ftn = ref (function x -> 0);;
let val_table = ref (Array.make 0 zero);;

let char_lxor c1 c2 =
  char_of_int ((int_of_char c1) lxor (int_of_char c2));;


(* Returns a string that contains the bits to be xored over for the given move
 * This is slightly more complicated than it really has to be because ocaml has fairly
 * primitive support for bitwise operators *)

let make_char_mask int_mask pos=
  let bit_offset = (pos mod char_size) in
    
  (* Grabs the 8 bits starting at i and returns that char 
   * negative values of i are padded with zeros until the first bit of the integer*)
  let grab_char_at (i:nativeint) off =
    let char_mask = (sub (shift_left one char_size) one) in (* Should be 255 on every platform I can think of *)
    let rshift_amt = ( int_bit_size -off - char_size) in
      if(rshift_amt >= 0) then
	char_of_int (to_int (logand char_mask (shift_right_logical i rshift_amt)))
      else
	char_of_int (to_int (logand char_mask (shift_left i (-rshift_amt))))
  in

  let returned_string = String.make int_mask_size (char_of_int 0) in
    for i = 0 to (String.length returned_string - 1) do
      returned_string.[i] <- (grab_char_at int_mask (-bit_offset+(i*char_size) ))
    done;
    returned_string
;;

(* Applies the integer mask to the provided string, starting at character n of str and looping if needed *)
let apply_xor_mask str mask n =
  let temp_copy = String.copy str in
  let rec inner index num_xored=
    if num_xored >= int_mask_size then 
      temp_copy
    else (
      assert(index < String.length temp_copy);
      assert(num_xored < int_mask_size);
      temp_copy.[index] <- (char_lxor temp_copy.[index] mask.[num_xored]);
      inner ((index + 1) mod (String.length temp_copy)) (num_xored + 1)
    )
  in
    inner n 0
;;

let get_updated_key prev_key mask pos=
  assert (String.length prev_key = key_byte_size);
  apply_xor_mask prev_key (make_char_mask mask pos) ((pos / char_size) mod key_byte_size)
;;

(* Make a new key from the current board position *)
(* I need to figure out how to link this to board without actually doing so... or at the same time *)

let new_keygen
    (to_index: 'a -> int)
    (mask_vals: nativeint array) =
  {from_board=
      (fun arr -> 
	 let new_key = String.make key_byte_size (char_of_int 0) in
	 let i = ref 0 in
	   Array.iter ( fun current -> (
			  String.blit 
			    (get_updated_key 
			       new_key 
			       (mask_vals.(to_index current))
			       (!i)) 
			    0
			    new_key 
			    0
			    (String.length new_key);
			  i := (!i + 1)
			)) arr;
	   new_key 
      );
   update= (fun old_key ident pos ->
	      assert (String.length old_key = key_byte_size);
	      get_updated_key 
		old_key 
		(mask_vals.(to_index ident))
		pos
	   )
  }
;;

(* I can only test so much in here, because I don't have access to the board library directly.
 * Many of the tests provided by this function are very primitive, and only require hash related
 * functions.
 * Because of this, much of the external testing of this sub_module is tested in the board.ml file
 *)

type test_type = B | W | E | K;;
let get_hash_tests() =
  let test_board = Array.make 81 E in
  let test_keygen = 
    (new_keygen
       (function 
	    B -> 1 
	  | W -> 2 
	  | E -> 0 
	  | K -> 3)
       (Array.init 4 
	  (function
	       0 -> zero
	     | 1 -> one
	     | 2 -> shift_left one 1
	     | 3 -> shift_left one 2
	     | _ -> zero
	  ))) in
    
    (" Basic Hash tests" >:: fun _->
       assert_bool "Basic char_lxor test" ((char_lxor 'a' 'a') = char_of_int 0);
       assert_bool "Mask length" 
	 (String.length (make_char_mask zero 1) = int_byte_size + 1);
       
       (* These probably won't work if I move to a 32 bit machine *)
       (let oldkey = "aaaaaaaaaaaa" in
	let mask = "aaaaa" in
	  assert_bool "Apply mask 1" 
	    (String.compare 
	       (apply_xor_mask oldkey mask 0) 
	       "\000\000\000\000\000aaaaaaa" = 0);

	  assert_bool "Apply mask 2" 
	    (String.compare 
	       (apply_xor_mask oldkey mask 7) 
	       "aaaaaaa\000\000\000\000\000" = 0);

	  assert_bool "Apply mask 3" 
	    (String.compare 
	       (apply_xor_mask oldkey mask 9) 
	       "\000\000aaaaaaa\000\000\000" = 0));
       
       let empty_key = test_keygen.from_board test_board in
	 assert (String.length empty_key = key_byte_size);
	 test_board.(0) <- B;
	 assert_bool "Basic Key consistency 1"
	   (String.compare 
	      (test_keygen.from_board test_board)
	      (test_keygen.update empty_key B 0) = 0);
	 test_board.(0) <- E;
    )
;;
