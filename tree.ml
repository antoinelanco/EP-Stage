open Printf

type 'a tree =
  | Null
  | Leaf of 'a
  | Node of 'a tree * 'a * 'a tree


let rec atToS trans : 'a tree -> string = function (*trans : 'a -> string'*)
| Null       -> "Null"
| Leaf a     -> sprintf "Leaf %s" (trans a)
| Node (a,b,c) -> sprintf "Node (%s, %s, %s)"
                      (atToS trans a) (trans b) (atToS trans c)

let () =


let a = Node ((Leaf 5),10,Node(Leaf 4,22,Leaf 30)) in

Printf.printf "%s\n" (atToS string_of_int a)
