let rec run s a t ls dsc1 dsc2 =
  if List.mem_assoc s a
  then true
  else
  if List.mem_assoc s t
  then iter ls (List.assoc s t) t a dsc1 dsc2
  else false

and iter ls t ts a dsc1 dsc2 =
  let dsc2 = if List.length dsc2 = 0 then [("_",[])] else dsc2 in
  if List.length t = 0 || List.length ls = 0
  then false
  else
  if fst (List.hd dsc2) = "_"
  then
    if List.exists ((=) (fst (List.hd t))) (snd (List.hd dsc2))
    then iter ls (List.tl t) ts a dsc1 dsc2 (*OK*)
    else
    if List.hd ls = fst (List.hd t)
    then
      if run (snd (List.hd t)) a ts (List.tl ls)
          (dsc1 @ [List.hd dsc2]) (List.tl dsc2)
      then true
      else iter ls (List.tl t) ts a [] []
    else iter ls (List.tl t) ts a [] [](*add neg*)
  else
  if fst (List.hd dsc2) = fst (List.hd t)
  then
    if run (snd (List.hd t)) a ts (List.tl ls)
        (dsc1 @ [List.hd dsc2]) (List.tl dsc2)
    then true
    else iter ls (List.tl t) ts a [] []
  else iter ls (List.tl t) ts a [] []



let s = 1
let a = [(5, "A")]
let t = [(1,[("a", 2);("a", 3)]);(2,[("b", 4)]);(3,[("b", 4)]);(4,[("c", 5)])]
let ls = ["a";"b";"c"]
let () = Printf.printf "%b\n" (run s a t ls [] [])
