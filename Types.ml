type con = { name : string; arity : int; span : int }
type pat =
  | PVar of string
  | PCon of con * pat list
  | All

type 'rhs mat = (pat * 'rhs) list

type termd =
  | Pos of con * termd list
  | Neg of con list

type context = (con * termd list) list

type matchresult = Yes | No | Maybe

type lam =
  | Var of string
  | Lam of string * lam
  | App of lam * lam
  | Let of string * lam * lam

type access = Obj | Sel of int * access

type 'rhs decision =
  | Failure
  | Success of 'rhs
  | IfEq of access * con * 'rhs decision * 'rhs decision

type color = Red | Blue | Green
