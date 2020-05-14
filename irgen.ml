(* Code generation: produce IR code from a semantically checked AST 
    using LLVM module 
*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate: Sast.program -> Llvm.module *)
let translate (stmts, functions) = 
    let context = L.global_context () in 

    (* Create the Llvm compilation module into 
       which we will generate code
    *)
    let the_module = L.create_module context "Pyni" in

    (* Get types from the context 
        TODO: dict
    *)
    let i32_t     = L.i32_type context
    (*
    and i8_t      = L.i8_type context
    *)
    and i1_t      = L.i1_type context
    and float_t   = L.double_type context
    and none_t    = L.void_type   context
    and str_t     = L.pointer_type (L.i8_type context)
    and list_t t  = L.struct_type context [|L.pointer_type (L.i32_type context); L.pointer_type t|]
    in

    let rec ltype_of_typ = function 
          A.Int -> i32_t
        | A.Bool -> i1_t
        | A.Float -> float_t
        | A.String -> str_t
        | A.None -> none_t
        | A.List t -> list_t (ltype_of_typ t)
    in 
    let rec type_str t = match t with
          A.Int -> "int"
        | A.Bool -> "bool"
        | A.Float -> "float"
        | A.String -> "str"
        | A.List ty -> type_str ty
        | _ -> raise (Failure "Invalid string map key type")
    in
    (* Built-in function printf *)
    let printf : L.lltype = 
        L.var_arg_function_type i32_t [|str_t|] in
    let printf_func : L.llvalue = 
        L.declare_function "printf" printf the_module in

    let init_list builder list_ptr list_type = 
        let sizePtrPtr = L.build_struct_gep list_ptr 0 "list_size_ptr" builder in 
        let sizePtr = L.build_alloca i32_t "list_size" builder in
        let _ = L.build_store (L.const_int i32_t 0) sizePtr builder in
        ignore(L.build_store sizePtr sizePtrPtr builder);
        let list_array_ptr = L.build_struct_gep list_ptr 1 "list.arry" builder in 
        let p = L.build_array_alloca (ltype_of_typ list_type) (L.const_int i32_t 1028) "p" builder in
        ignore(L.build_store p list_array_ptr builder);
    in
    
    let list_get : L.llvalue StringMap.t = 
    let list_get_ty m typ = 
       let ltype = (ltype_of_typ typ) in 
       let def_name = (type_str typ) in
       let def = L.define_function ("list_get" ^ def_name) (L.function_type ltype [| L.pointer_type (list_t ltype); i32_t |]) the_module in
       let build = L.builder_at_end context (L.entry_block def) in
       let list_ptr = L.build_alloca (L.pointer_type (list_t ltype)) "list_ptr_alloc" build in
       let _ = L.build_store (L.param def 0) list_ptr build in
       let idx_ptr = L.build_alloca i32_t "idx_alloc" build in
       let _ = L.build_store (L.param def 1) idx_ptr build in
       let list_load = L.build_load list_ptr "list_load" build in
       let list_array_ptr = L.build_struct_gep list_load 1 "list_array_ptr" build in
       let list_array_load = L.build_load list_array_ptr "array_load" build in
       let idx = L.build_load idx_ptr "idx_load" build in
       let list_array_element_ptr = L.build_gep list_array_load [| idx |] "list_arry_element_ptr" build in
       let element_val = L.build_load list_array_element_ptr "list_array_element_ptr" build in
       let _ = L.build_ret element_val build in
       StringMap.add def_name def m in
    List.fold_left list_get_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in

    let list_size : L.llvalue StringMap.t = 
    let list_size_ty m typ =
     let ltype = (ltype_of_typ typ) in 
     let def_name = (type_str typ) in
     let def = L.define_function ("list_size" ^ def_name) (L.function_type i32_t [| L.pointer_type (list_t ltype) |]) the_module in
     let build = L.builder_at_end context (L.entry_block def) in
     let list_ptr = L.build_alloca (L.pointer_type (list_t ltype)) "list_ptr_alloc" build in
     ignore(L.build_store (L.param def 0) list_ptr build);
     let list_load = L.build_load list_ptr "list_load" build in
     let list_size_ptr_ptr = L.build_struct_gep list_load 0 "list_size_ptr_ptr" build in 
     let list_size_ptr = L.build_load list_size_ptr_ptr "list_size_ptr" build in
     let list_size = L.build_load list_size_ptr "list_size" build in
     ignore(L.build_ret list_size build);
     StringMap.add def_name def m in 
    List.fold_left list_size_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in

    let list_push : L.llvalue StringMap.t = 
    let list_push_ty m typ =
     let ltype = (ltype_of_typ typ) in 
     let def_name = (type_str typ) in
     let def = L.define_function ("list_push" ^ def_name) (L.function_type none_t [| L.pointer_type (list_t ltype); ltype |]) the_module in
     let build = L.builder_at_end context (L.entry_block def) in
     let list_ptr = L.build_alloca (L.pointer_type (list_t ltype)) "list_ptr_alloc" build in
     ignore(L.build_store (L.param def 0) list_ptr build);
     let valPtr = L.build_alloca ltype "val_alloc" build in
     ignore(L.build_store (L.param def 1) valPtr build);
     let list_load = L.build_load list_ptr "list_load" build in
     let list_array_ptr = L.build_struct_gep list_load 1 "list_array_ptr" build in
     let list_array_load = L.build_load list_array_ptr "list_array_load" build in
     let list_size_ptr_ptr = L.build_struct_gep list_load 0 "list_size_ptr_ptr" build in 
     let list_size_ptr = L.build_load list_size_ptr_ptr "list_size_ptr" build in
     let list_size = L.build_load list_size_ptr "list_size" build in
     let next_index = list_size in
     let next_element_ptr = L.build_gep list_array_load [| next_index |] "list_arry_next_element_ptr" build in
     let next_size = L.build_add list_size (L.const_int i32_t 1) "inc_size" build in
     let _ = L.build_store next_size list_size_ptr build in
     let _ = L.build_store (L.build_load valPtr "val" build) next_element_ptr build in
     let _ = L.build_ret_void build in
     StringMap.add def_name def m in 
    List.fold_left list_push_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in

    let list_pop : L.llvalue StringMap.t = 
    let list_pop_ty m typ =
       let ltype = (ltype_of_typ typ) in 
       let def_name = (type_str typ) in
       let def = L.define_function ("list_pop" ^ def_name) (L.function_type ltype [| L.pointer_type (list_t ltype) |]) the_module in
       let build = L.builder_at_end context (L.entry_block def) in
       let list_ptr = L.build_alloca (L.pointer_type (list_t ltype)) "list_ptr_alloc" build in
       ignore(L.build_store (L.param def 0) list_ptr build);
       let list_load = L.build_load list_ptr "list_load" build in
       let list_array_ptr = L.build_struct_gep list_load 1 "list_array_ptr" build in
       let list_array_load = L.build_load list_array_ptr "list_array_load" build in
       let list_size_ptr_ptr = L.build_struct_gep list_load 0 "list_size_ptr_ptr" build in 
       let list_size_ptr = L.build_load list_size_ptr_ptr "list_size_ptr" build in
       let list_size = L.build_load list_size_ptr "list_size" build in
       let list_sizeMin1 = L.build_sub list_size (L.const_int i32_t 1) "dec_size" build in
       let last_element_ptr = L.build_gep list_array_load [| list_sizeMin1 |] "list_arry_next_element_ptr" build in
       let last_element_val = L.build_load last_element_ptr "list_arry_next_element" build in
       let _ = L.build_store list_sizeMin1 list_size_ptr build in
       let _ = L.build_ret last_element_val build in
    StringMap.add def_name def m in
    List.fold_left list_pop_ty StringMap.empty [ A.Bool; A.Int; A.Float; A.String ] in
    
    (* Define each function (arguments are return type) 
       so we can call it even before we've created its body
    *)

    let function_decls : (L.llvalue * sfunc_def) StringMap.t = 
        let funciton_decl m func_def = 
            let f_name = func_def.sfname
            and formal_types = 
                Array.of_list (List.map (fun (ty,_) -> ltype_of_typ ty) func_def.sformals)
            in let f_type = L.function_type (ltype_of_typ func_def.srtyp) formal_types in 
            StringMap.add f_name (L.define_function f_name f_type the_module, func_def) m
            in List.fold_left funciton_decl StringMap.empty functions 
    in
    
    let rec build_expr builder glo_table loc_table ((_, e): sexpr) = 
        let lookup name = try StringMap.find name loc_table 
                          with Not_found -> 
                            try StringMap.find name glo_table
                            with Not_found -> raise (Failure("variable not declared."))
        in 
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

        match e with 
          SLiteral i -> L.const_int i32_t i 
        | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
        | SFLit f    -> L.const_float float_t f
        | SStrLit s  -> L.build_global_stringptr (s^"\x00") "strptr" builder
        | SListLit (ty, sl) ->
            let ltype = (ltype_of_typ ty) in
            let new_list_ptr = L.build_alloca (list_t ltype) "new_list_ptr" builder in
            let _ = init_list builder new_list_ptr ty in
            let map_func sl = 
              ignore(L.build_call (StringMap.find (type_str ty) list_push) [| new_list_ptr; (build_expr builder glo_table loc_table sl) |] "" builder);
            in
            let _ = List.rev (List.map map_func sl) in
            L.build_load new_list_ptr "new_list" builder
        | SListAccess (ty, id, se)  ->
            L.build_call (StringMap.find (type_str ty) list_get) [| (lookup id); (build_expr builder glo_table loc_table se) |] "list_get" builder
        | SListSlice (se1, se2, se3) -> raise (Failure "not implemented")
        | SLen (ty, id)     -> L.build_call ((StringMap.find (type_str ty)) list_size) [| (lookup id) |] "list_size" builder
        | SListPop (ty, id) -> L.build_call ((StringMap.find (type_str ty)) list_pop) [| (lookup id) |] "list_pop" builder
        | SNoexpr    -> L.const_int i1_t 0
        | SId s      -> L.build_load (lookup s) s builder
        | SAssign (s, se) -> 
            let e' = build_expr builder glo_table loc_table se in 
            ignore(L.build_store e' (lookup s) builder); e'
        | SUniop (se, op) -> 
            let e' = build_expr builder glo_table loc_table se 
            and one = L.const_int i32_t 1 in
            (match op with
                A.Incre -> 
                    let tmp = L.build_add e' one "uniop" builder in 
                    let var_name = match (snd se) with 
                          SId s -> s
                        | _ -> raise (Failure "operand of ++ must be a variable name")
                    in 
                    let old_ptr = lookup var_name in 
                    let _ = L.build_store tmp old_ptr builder in tmp
              | A.Decre -> 
                let tmp = L.build_sub e' one "uniop" builder in 
                let var_name = match (snd se) with 
                      SId s -> s
                    | _ ->  raise (Failure "operand of -- must be a variable name")
                in 
                let old_ptr = lookup var_name in 
                let _ = L.build_store tmp old_ptr builder in tmp
              | A.Not   -> L.build_not e' "uniop" builder          
              | _       -> raise (Failure ("invalid Uniop " ^ A.string_of_op op)))
        | SBinop (se1, op, se2) ->
            let match_fop op = match op with 
                  A.Add      -> L.build_fadd 
                | A.Sub      -> L.build_fsub
                | A.Mult     -> L.build_fmul
                | A.Div      -> L.build_fdiv
                | A.Mod      -> L.build_srem
                | A.Equal    -> L.build_fcmp L.Fcmp.Oeq
                | A.Neq      -> L.build_fcmp L.Fcmp.One
                | A.Less     -> L.build_fcmp L.Fcmp.Olt
                | A.Greater  -> L.build_fcmp L.Fcmp.Ogt
                | A.GreaterEq -> L.build_fcmp L.Fcmp.Oge
                | A.LessEq   -> L.build_fcmp L.Fcmp.Ole
                | _          -> raise (Failure "And & Or operation not allowed on float type")
            in
            (* binop between float *)
            if (fst se1 = A.Float) || (fst se2 = A.Float) then
                let e1' = build_expr builder glo_table loc_table se1 
                and e2' = build_expr builder glo_table loc_table se2 in
                (match_fop op) e1' e2' "float_binop" builder    
            (* binop between others *)              
            else 
                let e1' = build_expr builder glo_table loc_table se1 and e2' = build_expr builder glo_table loc_table se2 
                in
                (match op with 
                      A.Add     -> L.build_add
                    | A.Sub     -> L.build_sub
                    | A.Mult    -> L.build_mul
                    | A.Div     -> L.build_sdiv
                    | A.Mod     -> L.build_srem
                    | A.And     -> L.build_and
                    | A.Or      -> L.build_or
                    | A.Equal   -> L.build_icmp L.Icmp.Eq
                    | A.Neq     -> L.build_icmp L.Icmp.Ne
                    | A.Less    -> L.build_icmp L.Icmp.Slt
                    | A.Greater -> L.build_icmp L.Icmp.Sgt
                    | A.GreaterEq  
                                -> L.build_icmp L.Icmp.Sge                 
                    | A.LessEq  -> L.build_icmp L.Icmp.Sle
                    | _         -> raise (Failure ("invalid Binop " ^ A.string_of_op op))
                ) e1' e2' "normal_binop" builder
        | SCall ("print", [e]) ->
            L.build_call printf_func [| int_format_str ; (build_expr builder glo_table loc_table e)|]
            "printf" builder
        | SCall (f, args) -> 
            let (fdef, func_def) = StringMap.find f function_decls in 
            let llargs = List.rev (List.map (build_expr builder glo_table loc_table) (List.rev args)) in 
            let result = f ^ "_result" in
            L.build_call fdef (Array.of_list llargs) result builder 
    in

    (* LLVM insists each basic block end with exaclty one "terminator"
       instruction that transfer control. This function runs "instr builder"
       if the current block does not already have a terminator. 
    *)
    let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with 
          Some _ -> ()
        | None -> ignore (instr builder) 
    in
    
    (* Build the code for the given statement;
       scope indicate whether build_stmt is in global envrionment or local environment;
       scope = 1, global; scope = 0, local;
       return the builder for the statement's successor, 
       a StringMap of globale variable name and their value, 
       and a StringMap of local variable name and their value
    *)
    let rec build_stmt_list scope glo_table loc_table func_block builder stmt_list = 
        match stmt_list with 
            | [] -> (builder, glo_table, loc_table)
            | hd :: tl -> let (b, g, l) = build_stmt scope glo_table loc_table func_block builder hd in 
                build_stmt_list scope g l func_block b tl

    and build_stmt scope glo_table loc_table func_block builder stmt =
        let lookup name = try StringMap.find name loc_table 
                          with Not_found -> 
                            try StringMap.find name glo_table
                            with Not_found -> raise (Failure("variable not declared."))
        in 
        let add_var m (ty, name) value = 
            L.set_value_name name value;
            let var_addr = L.build_alloca (ltype_of_typ ty) name builder in
                                       ignore(L.build_store value var_addr builder);
                                       StringMap.add name var_addr m
        in
        match stmt with  
          SBlock sl -> build_stmt_list scope glo_table loc_table func_block builder sl
        | SExpr e -> ignore(build_expr builder glo_table loc_table e); (builder, glo_table, loc_table)
        | SReturn e -> ignore(L.build_ret (build_expr builder glo_table loc_table e) builder); (builder, glo_table, loc_table)
        | SFor (s1, se1, se2, for_body) -> 
            build_stmt scope glo_table loc_table func_block builder (SBlock [s1; SWhile (se1, SBlock [for_body; SExpr se2])])
        | SBindAssign (ty, name, sexpr) -> let expr_val = build_expr builder glo_table loc_table sexpr in
                                           if scope == 1 then let glo_table = add_var glo_table (ty, name) expr_val in 
                                           (builder, glo_table, loc_table)
                                           else (let loc_table = add_var loc_table (ty, name) expr_val in
                                           (builder, glo_table, loc_table))
        | SIf (predicate, then_stmt, else_stmt) ->
            let bool_val = build_expr builder glo_table loc_table predicate in 
            let then_bb = L.append_block context "then" func_block in 
            ignore(build_stmt scope  glo_table loc_table func_block (L.builder_at_end context then_bb) then_stmt);
            let else_bb = L.append_block context "else" func_block in 
            ignore(build_stmt scope  glo_table loc_table func_block (L.builder_at_end context else_bb) else_stmt);
            let end_bb = L.append_block context "end" func_block in 
            let build_br_end = L.build_br end_bb in
            add_terminal (L.builder_at_end context then_bb) build_br_end;
            add_terminal (L.builder_at_end context else_bb) build_br_end;
            ignore(L.build_cond_br bool_val then_bb else_bb builder);
            ((L.builder_at_end context end_bb), glo_table, loc_table)
        | SWhile (predicate, body) -> 
            let while_bb = L.append_block context "while" func_block in
            let build_br_while = L.build_br while_bb in 
            ignore (build_br_while builder); 
            let while_builder = L.builder_at_end context while_bb in 
            let bool_val = build_expr while_builder glo_table loc_table predicate in 
            let body_bb = L.append_block context "while_body" func_block in
            let (loc_builder, _, _) = build_stmt scope glo_table loc_table func_block (L.builder_at_end context body_bb) body in
            add_terminal loc_builder build_br_while;
            let end_bb = L.append_block context "while_end" func_block in 
            ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
            ((L.builder_at_end context end_bb), glo_table, loc_table)
        | SListAppend (id, se) -> ignore(L.build_call (StringMap.find (type_str (fst se)) list_push) [| (lookup id); (build_expr builder glo_table loc_table se) |] "" builder); (builder, glo_table, loc_table) 
        | SListInsert (se1, se2, se3) -> raise (Failure "not implemented")
        | SListSort (se) -> raise (Failure "not implemented")
        | SListReverse (se) -> raise (Failure "not implemented")
    in

    (* build the main block of pyni *)
    let build_main main_stmts funcs= 
        (* create the main function *)
        let main : L.lltype = L.function_type i32_t [||] in
        let main_func : L.llvalue = L.define_function "main" main the_module in 
        let main_builder = L.builder_at_end context (L.entry_block main_func) in

        let (main_end_builder, global_table, _) = 
            build_stmt 1 StringMap.empty StringMap.empty main_func main_builder (SBlock main_stmts);
        in
        add_terminal main_end_builder (L.build_ret (L.const_int i32_t 0));

        (* Fill in the body of the given function *)
        let build_function_body func_def=
            let (the_function, _) = StringMap.find func_def.sfname function_decls in
            let f_builder = L.builder_at_end context (L.entry_block the_function) in
            
            (* Build a local symbol table and first fill it with functiob's formals  
             *) 
            let add_formal m (t, n) p = 
                L.set_value_name n p;
                let formal = L.build_alloca (ltype_of_typ t) n f_builder in 
                ignore (L.build_store p formal f_builder);
                StringMap.add n formal m in

            let local_table = List.fold_left2 add_formal StringMap.empty func_def.sformals
                          (Array.to_list (L.params the_function)) in
       
            let (f_end_builder, _, _) = build_stmt 0 global_table local_table the_function f_builder (SBlock func_def.sbody) in 

            (* Add a return if the last block falls off the end *)
            add_terminal f_end_builder (L.build_ret (L.const_int i32_t 0))
        in
        List.iter build_function_body funcs
    in

    build_main stmts functions;
    the_module

