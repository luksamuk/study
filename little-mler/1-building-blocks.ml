(* This uses OCaml instead of SML *)

#print_depth 20;;

type seasoning =
     Salt
     | Pepper;;

	     
type num =
     Zero
     | One_more_than of num;;
				
(* Zero is a num
   One_more_than(Zero) is a num         *)

One_more_than(Zero);;
One_more_than(
  One_more_than(Zero));;
						       
(* This errors because 0 is not a num
   One_more_than(0)                     *)

One_more_than(
    One_more_than(
	One_more_than(
	    One_more_than(Zero))));;


type 'a open_faced_sandwich =
  Bread of 'a
| Slice of 'a open_faced_sandwich;;

Bread(0);;

Bread(true);;

Bread(
    One_more_than(
	Zero));;

Bread(Bread(0));;


Bread(
    Bread(
	One_more_than(
	    Zero)));;

(* First moral:
* Use type to describe datatypes. When a type
* contains lots of values, the datatype definition
* refers to itself. Use 'a with type to define
* shapes. *)

