(* Semantic checking for the Pyni compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global statement, then check each function 
   We check the following error:
      function have no duplicated formals 
       *)
let check (global_stmts, functions) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Collect function declarations for built-in functions: no bodies *)
  (*
  let built_in_funcs = [
    ("print", {rtyp = String; formals = [(String, "x")]});
    ("upper", {rtyp = String; formals = [(String, "x")]});
    ("lower", {rtyp = String; formals = [(String, "x")]});
  ] in
  *)

  let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [(Int, "x")];
      body = [] } StringMap.empty
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    (*
    and fsig = {rtyp = fd.rtyp, formals = fd.formals}
    *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

      (*
  let built_in_decls = List.fold_left add_func StringMap.empty built_in_funcs 
  in
      *)

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Pyni Parse locals varibale declared in the function body, return a bind list*)
  let rec get_locals body = match body with
    [] -> []
  | hd :: tl -> match hd with 
      BindAssign(t, s, e) -> (t, s) :: (get_locals tl)
    | _ ->  get_locals tl
  in

  (* Pyni Check if duplicated binding exists in global stmts *)
  check_binds "global" (get_locals global_stmts);

  (* Raise an exception if the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if (lvaluet = rvaluet) || (rvaluet = None)
    then lvaluet 
    else raise (Failure err)
  in

  (* Return a variable from our local symbol table *)
  let type_of_identifier s stable =
    try StringMap.find s stable 
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Pyni Return a sementically checked list and the type of its elements 
   * raise an error if element types are inconsistent*) 
  let rec check_list lst symbols = match lst with
    [] -> (None, [])
  | hd :: tl -> 
  let (t1, e1) = check_expr hd symbols
  and (t2, e2) = check_list tl symbols in
  let err = "inconsistent types of " ^ string_of_typ t1 ^ 
            " and " ^ string_of_typ t2 ^ " in the same list"
  in 
  if t2 = None || t1 = t2 then (t1, (t1, e1) :: e2)
  else raise (Failure err)
  
  (* Return a semantically-checked expression, i.e., with a type *)
  and check_expr expr symbols = match expr with 
      Literal l -> (Int, SLiteral l)
    | BoolLit l -> (Bool, SBoolLit l)
    | FLit l -> (Float, SFLit l)
    | StrLit l -> (String, SStrLit l)
    | Id var -> (type_of_identifier var symbols, SId var)
    | ListLit lst -> 
      let (t, slst) = check_list lst symbols in 
      (List(t), SListLit(t, slst))
    | ListAccess(lname, e) ->
      let lt as se1 = type_of_identifier lname symbols in
      let (rt, _) as se = check_expr e symbols in
      if rt <> Int then raise (Failure ("illegal list index of type " ^ string_of_typ rt))
      else (match lt with
          List t -> (t, SListAccess(lt, lname, se))
        | _ -> raise (Failure ("illegal indexing of type " ^ string_of_typ lt)))
    | ListSlice(e1, e2, e3) ->  
      let (lt, _) as se1 = check_expr e1 symbols in
      let (rt1, _) as se2 = check_expr e2 symbols in
      let (rt2, _) as se3 = check_expr e3 symbols in
      if (rt1 <> Int) || (rt2 <> Int) 
      then raise (Failure ("illegal list slice type " ^ string_of_typ rt1 ^ " : " ^ string_of_typ rt2))
      else (match lt with
          List t -> (lt, SListSlice(se1, se2, se3))
        | _ -> raise (Failure ("illegal slicing of type " ^ string_of_typ lt)))
    
    | Len(s) -> 
      let t = type_of_identifier s symbols in
      let err = "illegal len on type " ^ string_of_typ t in
      (match t with
        List _ | String -> (Int, SLen(t, s))
      | _ -> raise (Failure err))
    (* e1 must be list type, e2 must be int type *)
    | ListPop(s) -> 
      let lt = type_of_identifier s symbols in
      let err = "illegal list pop on type " ^ string_of_typ lt in
      (match lt with
        List t -> (t, SListPop(lt, s))
      | _ -> raise (Failure err))
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var symbols
      and (rt, e') = check_expr e symbols in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex 
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))
    | Uniop(e, op)-> 
      let (t, e') = check_expr e symbols in
      let err = "illegal uniary operation " ^ string_of_typ t ^ " " ^
                string_of_op op
      in
      let is_valid = match t, op with
          Int, Incre -> 1
        | Int, Decre -> 1
        | Bool, Not -> 1      
        | _, _ -> 0  
      in 
      if is_valid = 1 
      then (t, SUniop((t, e'), op))
      else raise (Failure err)
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr e1 symbols
      and (t2, e2') = check_expr e2 symbols in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub | Mult | Div when (t1 = Int) || (t1 = Float) -> t1 
          | Add when t1 = String -> String
          | Mod when t1 = Int -> Int
          | Equal | Neq -> Bool
          | Less | Greater | GreaterEq | LessEq 
            when (t1 = Int) || (t1 = Float) -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else raise (Failure err)
    | Call(fname, args) as call ->
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^
                        " arguments in " ^ string_of_expr call))
      else let check_call (ft, _) e =
             let (et, e') = check_expr e symbols in
             let err = "illegal argument found " ^ string_of_typ et ^
                       " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
             in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.rtyp, SCall(fname, args'))
    | Noexpr -> (None, SNoexpr)
  in

  let check_bool_expr e symbols =
    let (t, e') = check_expr e symbols in
    match t with
    | Bool -> (t, e')
    |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
  in

  (* Pyni Add a bind into symbol table and return the new symbol table 
   * raise error if the bind name exists *)
  (*
  let add_to_symbol_table s t stable = 
    let find_stable s m = 
      try StringMap.find s m 
      with 
      | Not_found -> 0 
      | _ -> 1
    in
    let exist = find_stable s stable in
      match exist with
      | 0 -> StringMap.add s t stable
      | _ -> raise (Failure ("variable with the same name declared before"))
  in
  *)

  let rec check_stmt_list ?return_typ:(rtyp=None) stmt_lst symbols = 
    match stmt_lst with 
      [] -> ( [], symbols )
    (*optimization, not necessary (Flatten blocks) *)
    | Block sl :: sl'  -> check_stmt_list (sl @ sl') symbols ~return_typ:rtyp 
    | s :: sl -> 
      let (sst, m') =  check_stmt s symbols ~return_typ:rtyp in
      let (ssl, m'') = check_stmt_list sl m' ~return_typ:rtyp in
      ( sst :: ssl, m'' )
  
  (* Return a semantically-checked statement i.e. containing sexprs *)
  and check_stmt ?return_typ:(rtyp=None) stmt symbols = match stmt with 
    (* A block is correct if each statement is correct and nothing
       follows any Return statement.  Nested blocks are flattened. *)
      Block sl -> let (ssl, m') = check_stmt_list sl symbols in
      ( SBlock (ssl), symbols )
    | Expr e -> ( SExpr (check_expr e symbols), symbols )
    | If(e, st1, st2) ->
      (* Assume if statement block does not variable declaration to symbol table *)
      let (sst1, _ ) = check_stmt st1 symbols 
      and (sst2, _ ) = check_stmt st2 symbols
      in
      ( SIf(check_bool_expr e symbols, sst1, sst2), symbols )
    | While(e, st) ->
      let (sst, _ ) = check_stmt st symbols 
      in
      ( SWhile(check_bool_expr e symbols, sst), symbols )
    | For(s1, e2, e3, st) ->
      let (sst1, m') = check_stmt s1 symbols  
      in
      let (sst2, m'') = check_stmt st m'
      and (t2, e2') = check_expr e2 m' 
      and e3' = check_expr e3 m' in
      if t2 = Bool then  
          ( SFor(sst1, (t2, e2'), e3', sst2), symbols )
      else raise (
          Failure ("Second statement in for condition must be boolean type")
      )
    | BindAssign(lt, id, e) -> 
      let (rt, e') = check_expr e symbols in
      let err = "illegal initialization of " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr e 
      in
      if (lt = rt) || (rt = None) then 
          let m' = StringMap.add id lt symbols
          in
          ( SBindAssign(lt, id, (rt, e')), m' )
      else raise ( Failure err )
    | Return e ->
      let (t, e') = check_expr e symbols in
      if rtyp = None || t = rtyp then ( SReturn (t, e'), symbols )
      else raise (
          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                   string_of_typ rtyp ^ " in " ^ string_of_expr e))
    | ListAppend(s, e) ->
      let lt = type_of_identifier s symbols in
      let (rt, _) as se = check_expr e symbols in
      let err = "illegal list append of type " ^ string_of_typ rt ^ "to " ^ string_of_typ lt in
      (match lt with
        List t when rt = t -> ( SListAppend(s, se), symbols )
      | _ -> raise (Failure err))
    | ListInsert(e1, e2, e3) ->
      let (lt, _) as se1 = check_expr e1 symbols in
      let (rt1, _) as se2 = check_expr e2 symbols in
      let (rt2, _) as se3 = check_expr e3 symbols in
      let err = "illegal insert of type " ^ string_of_typ rt2 ^ " to " ^ string_of_typ lt in
      (match lt with
        List t when (rt2 = t) && (rt1 = Int) -> ( SListInsert(se1, se2, se3), symbols )
      | List t when rt2 = t -> raise (Failure "illegal insert with non int index")
      | _ -> raise (Failure err))
    | ListSort(e) ->
      let (lt, _) as se = check_expr e symbols in
      let err = "illegal sort of non list type" in
      (match lt with
        List _ -> ( SListSort(se), symbols )
      | _ -> raise (Failure err))
    | ListReverse(e) ->
      let (lt, _) as se = check_expr e symbols in
      let err = "illegal reverse of non list type" in
      (match lt with
        List _ -> ( SListReverse(se), symbols )
      | _ -> raise (Failure err))
     
    in 

  let global_locals = get_locals global_stmts in 

  (*
  let global_symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty global_locals
  in
  *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    let func_locals = get_locals func.body in 
    
    check_binds "formal" func.formals;
    check_binds "local" func_locals;

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (global_locals @ func.formals @ func_locals )
    in

    (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = fst (check_stmt_list func.body symbols ~return_typ:func.rtyp)
    }
  in 

  (* Pyni Master function to check the globals stmts *)
  (*
  let rec check_global_stmts lst m = function
      [] -> []
    | hd :: tl -> let (shd, m') = check_stmt hd m in check_global_stmts tl m'
  in 
  *)

  (* let sstmt_lst = check_stmt_list global_stmts global_symbols in *)
  let (sstmt_lst, _) = check_stmt_list global_stmts StringMap.empty in

  (sstmt_lst, List.map check_func functions)
