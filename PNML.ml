
let rec stringTermd a =
  if fst a = "_"
  then Printf.sprintf "Neg[%s]" (String.concat ";" (snd a))
  else Printf.sprintf "Pos(%s)" (fst a)


let staticmatch p s =
  if fst s = "_"
  then
    if List.exists ((=) p) (snd s)
    then 0
    else 2
  else
    if fst s = p
    then 1 else 0


let addneg c a = (fst a, c :: snd a)

let decalg (x,y) = (x @ [List.hd y], List.tl y)
let decald (x,y) =
  let a = List.hd (List.rev x) in
  let b = List.rev (List.tl (List.rev x)) in
  (b,a::y)

let notEmpty d = if List.length d = 0 then [("_",[])] else d


let rec run s a t ls dsc1 dsc2 =
  if List.mem_assoc s a
  then (true,(dsc1,dsc2))
  else
  if List.mem_assoc s t
  then iter ls (List.assoc s t) t a dsc1 dsc2
  else (false,decald (dsc1,dsc2))

and aux ls t ts a res =
  if fst res
  then (true,(fst (snd res),snd (snd res)))
  else iter ls (List.tl t) ts a (fst (snd res)) (snd (snd res)) (*BACKTRACK*)



and iter ls t ts a dsc1 dsc2 =
  if List.length t = 0 || List.length ls = 0
  then (false,decald(dsc1,(notEmpty dsc2)))
  else (*match -> switch*)
    match staticmatch (fst (List.hd t)) (List.hd (notEmpty dsc2)) with
    | 1 -> aux ls t ts a (run (snd (List.hd t)) a ts (List.tl ls)
                 (dsc1 @ [List.hd (notEmpty dsc2)])
                 (List.tl (notEmpty dsc2)))

    | 0 -> iter ls (List.tl t) ts a dsc1 (notEmpty dsc2)
    | 2 ->
      if List.hd ls = fst (List.hd t)
      then
         aux ls t ts a (run (snd (List.hd t)) a ts (List.tl ls)
            (dsc1 @ [(fst (List.hd t)),[]])
            (List.tl (notEmpty dsc2)))
      else iter ls (List.tl t) ts a dsc1
      ((addneg (fst (List.hd t)) (List.hd (notEmpty dsc2)))
       :: (List.tl (notEmpty dsc2)))

    | _ -> failwith "only 0,1,2"

let s = 1
let a = [(5, "A")]
let t = [(1,[("a", 2);("a", 3)]);(2,[("d", 4);("b", 6)]);
         (3,[("b", 4)]);(4,[("c", 5)])]
let ls = ["a";"b";"c"]
let (x,y) = run s a t ls [] []
let () = List.iter (fun i -> Printf.printf "%s " (stringTermd i) ) (fst y)
let () = Printf.printf " | "
let () = List.iter (fun i -> Printf.printf "%s " (stringTermd i) ) (snd y)
let () = Printf.printf "\n%b\n" x
