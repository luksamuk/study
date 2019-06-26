(* This uses OCaml instead of SML *)

#print_depth 20;;

type shish_kebab =
  Skewer
| Onion of shish_kebab
| Lamb of shish_kebab
| Tomato of shish_kebab;;

Skewer;;
Onion(Skewer);;
Onion(
    Lamb(
        Onion(
            Skewer)));;
Onion(
    Onion(
        Onion(
            Skewer)));;

(* In Standard ML:
fun only_onions(Skewer)
  = true
  | only_onions(Onion(x))
    = only_onions(x)
  | only_onlions(Lamb(x))
    = false
  | only_onions(Tomato(x))
    = false

only_onions : shish_kebab -> bool
 *)

(* In OCaml. The type annotation accompanies the function: *)
let rec (only_onions : shish_kebab -> bool) =
  function
    (Skewer)    -> true
  | (Onion(x))  -> only_onions(x)
  | (Lamb(x))   -> false
  | (Tomato(x)) -> false;;

(* Good practice: make the items in the datatype appear in
 * same order on function definition. *)

only_onions(
    Onion(
        Onion(
            Skewer)));;

let rec (is_vegetarian : shish_kebab -> bool) =
  function
    (Skewer)    -> true
  | (Onion(x))  -> is_vegetarian(x)
  | (Lamb(x))   -> false
  | (Tomato(x)) -> is_vegetarian(x);;


(* Onion now constructs an 'a shish,
 * and not a shish_kebab! *)
type 'a shish =
  Bottom of 'a
| Onion of 'a shish
| Lamb of 'a shish
| Tomato of 'a shish;;

type rod =
  Dagger
| Fork
| Sword;;


type plate =
  Gold_plate
| Silver_plate
| Brass_plate;;



Onion(
    Tomato(
        Bottom(Dagger)));;

Onion(
    Tomato(
        Bottom(Gold_plate)));;


let rec (is_veggie: 'a shish -> bool) =
  function
    (Bottom(x)) -> true
  | (Onion(x))  -> is_veggie(x)
  | (Lamb(x))   -> false
  | (Tomato(x)) -> is_veggie(x);;


(* is_veggie(Onion(Fork));; *)

is_veggie(
    Onion(
        Tomato(
            Bottom(Dagger))));;

is_veggie(
    Onion(
        Tomato(
            Bottom(Gold_plate))));;

is_veggie(
    Onion(
        Tomato(
            Bottom(52))));;


#use "1-building-blocks.ml";;

is_veggie(
    Onion(
        Tomato(
            Bottom(
                One_more_than(Zero)))));;

is_veggie(
    Onion(
        Tomato(
            Bottom(
                false))));;


let rec (what_bottom : 'a shish -> 'a) =
  function
    (Bottom(x)) -> x
  | (Onion(x))  -> what_bottom(x)
  | (Lamb(x))  -> what_bottom(x)
  | (Tomato(x))  -> what_bottom(x);;


what_bottom(
    Bottom(52));;

what_bottom(
    Bottom(Sword));;


what_bottom(
    Tomato(
        Onion(
            Lamb(
                Bottom(52)))));;

(* Second moral:
 * The number and the order of the patterns in the
 * definition of a function should match that of the
 * definition of the consumed datatype. *)

