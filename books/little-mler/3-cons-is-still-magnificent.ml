(* This uses OCaml instead of SML *)

#print_depth 20;;

type pizza =
  Crust
| Cheese of pizza
| Onion of pizza
| Anchovy of pizza
| Sausage of pizza;;

Anchovy(
    Onion(
        Anchovy(
            Anchovy(
                Cheese(
                    Crust)))));;

let rec (remove_anchovy : pizza -> pizza) =
  function
    Crust       -> Crust
  | (Cheese  x) -> Cheese(remove_anchovy x)
  | (Onion   x) -> Onion(remove_anchovy x)
  | (Anchovy x) -> (remove_anchovy x)
  | (Sausage x) -> Sausage(remove_anchovy x);;

remove_anchovy(
    Cheese(
        Anchovy(
            Cheese(
                Crust))));;
