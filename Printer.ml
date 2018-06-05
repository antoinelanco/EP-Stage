open Printf
open Types

let rec stringlist funprint = function
  | x :: [] -> funprint x
  | x :: r -> sprintf "%s; %s" (funprint x) (stringlist funprint r)
  | [] -> ""

let rec stringAcc : access -> string = function
| Obj -> "Obj"
| Sel(i,a) -> sprintf "Sel(%d,%s)" i (stringAcc a)

let stringCon a : string = a.name

let rec stringPat : pat -> string = function
  | PVar a -> a
  | PCon (a,b) -> sprintf "%s(%s)" (stringCon a) (stringlist stringPat b)


let stringMat a trans : string =
  sprintf "[%s\n]\n" (List.fold_left (fun ac (e1,e2) ->
            sprintf "%s \n (%s,%s)" ac (stringPat e1) (trans e2) ) "" a)


let rec stringLam : lam -> string = function
  | Var i -> sprintf "%s" i
  | Lam (i,l) -> sprintf "λ%s.%s" i (stringLam l)
  | App (l1,l2) -> sprintf "%s %s" (stringLam l1) (stringLam l2)
  | Let (i,l1,l2) -> sprintf "let %s = %s in %s" i (stringLam l1) (stringLam l2)


let rec stringDes tab trans : 'rhs decision -> string = function
  | Failure -> sprintf "%sFailure" tab
  | Success r -> sprintf "%sSuccess %s" tab (trans r)
  | IfEq (a,c,d1,d2) -> sprintf "%sIfEq(%s == %s)\n%s\n%s" tab (stringAcc a) (stringCon c)
    (stringDes (tab^"  ") trans d1) (stringDes (tab^"  ") trans d2)
