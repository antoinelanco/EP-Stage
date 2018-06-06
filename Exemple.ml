open Types
open Printer
open Printf
open Fun


let nullc = { name = "Null"; arity = 0; span = 3 }
let leafc = { name = "Leaf"; arity = 1; span = 3 }
let nodec = { name = "Node"; arity = 3; span = 3 }


let tt = PCon({name="true"; arity=0; span=2}, [])
let ff = PCon({name="false"; arity=0; span=2},[])

let tup args = PCon({name=""; span=1; arity=(List.length args)} , args)
let pair x y = tup [x; y]

let nil = PCon( {name="nil"; arity=0; span=2}, [])
let cons a b = PCon( {name="cons"; arity=2; span=1} ,[a;b])

let a = PCon(nodec,
             [PCon(leafc,
                   [PVar "9"]); PVar "12"; PCon(nodec,
                                                [PCon(leafc, [PVar "4"]); PVar "7";PCon(nullc,[])])])

let test = [(a,"a");(a,"b")]

let r = main2 (PVar "10") [(PVar "11","A")] (*??*)


let rul =
  [
    (PCon(nodec,[PCon(nodec,[PCon(leafc,[PVar "4"]);PVar "2";PCon(nullc,[])]);PVar "1";PCon(nullc,[])]),"a");
    (PCon(nodec,[PCon(nodec,[PCon(nullc,[]);PVar "2";PCon(leafc,[PVar "5"])]);PVar "1";PCon(nullc,[])]),"b");
    (PCon(nodec,[PCon(nullc,[]);PVar "1";PCon(leafc,[PVar "3"])]),"c")
  ]

let rull =
  [
    (cons nil (PCon( {name="x"; arity=0; span=2}, [])),"111");
    (cons (PCon( {name="x"; arity=0; span=2}, [])) nil,"222")
  ]

let r3 = compile rull


let exemple =
  let () = printf "--------------EXEMPLE--------------\n" in
  let () = printf "%s\n\n" (stringDes "" id r3) in
  let () =
    let res = match r with | Some a -> a | _ -> "none" in
    printf "%s\n" res in


  let () = printf "%s\n" (stringPat a) in
  let () = printf "%s\n" (stringMat test id) in (*id is define in Types. Is a identity function*)
  let () = printf "%s\n" (stringPat tt) in
  let () = printf "%s\n" (stringPat ff) in
  let () = printf "%s\n" (stringPat (pair (PVar "10") (PVar "10"))) in
  let () = printf "%s\n" (stringPat nil) in
  let () = printf "%s\n" (stringPat (cons (PVar "10") (PVar "10"))) in
  ()
