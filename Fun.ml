open Types


let addneg = function
  | (Neg a, c) -> Neg (c :: a)
  | _ -> failwith "Not Neg"


let id = fun i -> i

let staticmatch : con * termd -> matchresult = function
  | (pcon, Pos(scon, _)) -> if pcon = scon then Yes else No
  | (pcon, Neg nonset) ->
    if List.exists ((=) pcon) nonset
    then No
    else if pcon.span = 1 + (List.length nonset)
    then Yes
    else Maybe



let augment = function
| ([],_) -> []
| ((con, args)::rest, dsc) -> (con, dsc :: args) :: rest



let norm = function
| ((con, args) :: rest) -> augment (rest, Pos(con, List.rev args))
| [] -> []


let rec builddsc = function
| ([],dsc,[]) -> dsc
| ((con, args)::rest, dsc, (_, _, dargs) :: work) ->
            builddsc(rest, Pos(con, (List.rev args) @ (dsc :: dargs)), work)
| _ -> failwith "r & l diff arity"


(*-----------------V3--------------------*)


let compile allmrules =

  let rec fail = function
    | (dsc,[]) -> Failure
    | (dsc,(pat1,rhs1) :: rulerest) ->
        matt (pat1,Obj,dsc, [], [],rhs1,rulerest)

  and succeed = function
  | (ctx, [], rhs, rules) -> Success rhs

  | (ctx, work1 :: workr, rhs, rules) ->
    begin
      match work1 with
      | ([],[],[]) -> succeed(norm ctx,workr, rhs, rules)
      | (pat1::patr, obj1::objr, dsc1::dscr) ->
          matt (pat1, obj1, dsc1, ctx,(patr,objr,dscr)::workr, rhs, rules)
      | _ -> failwith "length pat != length obj (imposible)"
    end


  and matt = function
  | (PVar _, obj, dsc, ctx, work, rhs, rules) ->
      succeed(augment (ctx, dsc), work, rhs, rules)
  | (PCon(pcon,pargs),obj, dsc, ctx, work,rhs,rules) ->

    let args num f = List.init num f in

    let getdargs = function
      | (Neg _) -> args pcon.arity (fun _ -> Neg [])
      | (Pos(con, dargs)) -> dargs
    in

    let getoargs () = args pcon.arity (fun i -> Sel(i+1,obj)) in

    let succeed' () =
      succeed((pcon, []) :: ctx,
      (pargs,getoargs (),getdargs dsc)::work,
      rhs, rules)
    in

    let fail' newdsc =
    fail(builddsc(ctx, newdsc, work), rules)
    in
    begin
      match staticmatch(pcon, dsc) with
      | Yes -> succeed' ()
      | No -> fail' dsc
      | Maybe -> IfEq(obj, pcon, succeed' (), fail' (addneg(dsc, pcon)))
    end

  in fail (Neg [], allmrules)


(*-----------------V2--------------------*)


let main2 origobj allmrules =

  let rec fail = function
    | (dsc,[]) -> None
    | (dsc,(pat1,rhs1) :: rulerest) -> matt (pat1,origobj,dsc, [], [],rhs1,rulerest)

  and succeed = function
  | (ctx, [], rhs, rules) -> Some rhs

  | (ctx, work1 :: workr, rhs, rules) ->
    begin
      match work1 with
      | ([],[],[]) -> succeed(norm ctx,workr, rhs, rules)
      | (pat1::patr, obj1::objr, dsc1::dscr) ->
          matt (pat1, obj1, dsc1, ctx,(patr,objr,dscr)::workr, rhs, rules)
      | _ -> failwith "length pat != length obj (imposible)"
    end


  and matt = function
  | (PVar _, obj, dsc, ctx, work, rhs, rules) ->
      succeed(augment (ctx, dsc), work, rhs, rules)
  | (PCon(pcon,pargs),PCon(ocon,oargs), dsc, ctx, work,rhs,rules) ->

    let args num f = List.init num f in

    let getdargs = function
      | (Neg _) -> args pcon.arity (fun _ -> Neg [])
      | (Pos(con, dargs)) -> dargs
    in

    let succeed' () =
      succeed((pcon, []) :: ctx,
      (pargs,oargs,getdargs dsc)::work,
      rhs, rules)
    in

    let fail' newdsc =
    fail(builddsc(ctx, newdsc, work), rules)
    in
    begin
      match staticmatch(pcon, dsc) with
      | Yes -> succeed' ()
      | No -> fail' dsc
      | Maybe ->
        if ocon = pcon
        then succeed' ()
        else fail' (addneg(dsc, pcon))

    end
  | _ -> None

  in fail (Neg [], allmrules)


(*-----------------V1--------------------*)

let main origobj allmrules =

  let rec fail origobj = function
    | [] -> None
    | (pat1,rhs1) :: rulerest -> matt (pat1,origobj,[],rhs1,rulerest)

  and succeed = function
  | ([],rhs, rules) -> Some rhs

  | (work1 :: workr, rhs, rules) ->
    begin
      match work1 with
      | ([],[]) -> succeed(workr, rhs, rules)
      | (pat1::patr, obj1::objr) -> matt (pat1, obj1, (patr,objr)::workr, rhs, rules)
      | _ -> failwith "length pat != length obj (imposible)"
    end


  and matt = function
  | (PVar _, _, work, rhs, rules) -> succeed(work, rhs, rules)
  | (PCon(pcon,pargs),PCon(ocon,oargs),work,rhs,rules) ->

    if pcon = ocon
    then succeed((pargs, oargs) :: work, rhs, rules)
    else fail origobj rules

  | (_, _, _, _, rules) -> fail origobj rules
  in fail origobj allmrules
