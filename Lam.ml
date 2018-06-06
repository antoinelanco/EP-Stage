open Types
open Printer
open Printf
open Fun



let varc = { name = "Var"; arity = 1; span = 4 }
let lamc = { name = "Lam"; arity = 2; span = 4 }
let appc = { name = "App"; arity = 2; span = 4 }
let letc = { name = "Let"; arity = 3; span = 4 }

let rule =
  [
    (PCon(varc,[PVar "x"]),"111");
    (PCon(lamc,[PVar "x"; PCon(varc,[PVar "y"])]),"222");
    (PCon(lamc,[PVar "x"; PCon(lamc,[PVar "y"; PVar "z"])]),"333");
    (PCon(lamc,[PVar "x"; PCon(appc,[PVar "y"; PVar "z"])]),"444");
    (PCon(appc,[PCon(lamc,[PVar "x"; PVar "y"]); PVar "z"]),"555");
    (PCon(appc,[PCon(appc,[PVar "x"; PVar "y"]); PVar "z"]),"666");
    (PCon(letc,[PVar "x"; PCon(letc,[PVar "y";PVar "z";PVar "v"]); PVar "w"]),
     "777");
    (PCon(lamc,[PVar "x"; PCon(letc,[PVar "y";PVar "z";PVar "v"])]),"888");
    (PCon(letc,[PVar "x"; PVar "y"; PCon(appc,[PVar "z"; PVar "v"])]),"999");
    (PCon(appc,[PCon(appc,[PCon(lamc,[PVar "x";
                                      PCon(lamc,[PVar "y"; PVar "z"])]); PVar "v"]); PVar "w"]),"1010")
  ]

let r = compile rule


let lam =


  (* let r = main2 "a" "a" in *)
  let () = Printf.printf "--------------LAM--------------\n" in
  let l = Let("x",Lam("x",Var "x"), App (Var "y", Var "x")) in
  let () = printf "%s\n" (stringLam l) in
  let () = printf "%s\n" (stringDes "" id r) in


  ()
