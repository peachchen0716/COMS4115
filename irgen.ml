 (* Code generation: produce IR code from a semantically checked AST 
	using LLVM module 
*)

module L = Llvm
model A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate: Sast.program -> Llvm.module *)
let translate (globals, functionc) = 
	let context = L.global_context () in 

	(* Create the Llvm compilation module into 
	   which we will generate code
	*)
	let the_module = L.create_module context "Pyni" in

	(* Get types from the context 
		TODO: string, None, dict
	*)
	let i32_t     = L.i32_type context
	and i8_t      = L.i8_type context
	and i1_t      = L.i1_type context
	and float_t   = L.float_t context
	and str_t     = L.pointer_type (L.i8_type context)
	and list_t t  = L.struct_type context [|L.pointer_type (L.i32_type context); L.pointer_type t|]
	in

	let rec ltype_of_typ = function 
		  A.Int -> i32_t
		| A.Bool -> i1_t
		| A.Float -> float_t
		| A.String -> str_t
		| A.List t -> list_t (ltype_of_typ t)
	in 

	(* Create a map of global variables after creating each *)
	let global_vars : L.llvalue StringMap.t = 
		let global_var m (ty, name) = 
			let init = L.const_int (ltype_of_typ t) 0 in
			StringMap.add name (L.define_global name init the_module) m in
			List.fold_left global_var StringMap.empty globals 
		in 
	
		(* Built-in function printf *)
		let printf : L.lltype = 
			L.var_arg_function_type i32_t [|L.pointer_type i8_t|] in
		let printf_func : L.llvalue = 
			L.declare_function "printf" printf the_module in 
	
	(* Define each function (arguments are return type) 
	   so we can call it even before we've created its body
	*)
	let function_decls : (L.llvalue * sfunc_def) StringMap.t = 
		let funciton_decl m fdecl = 
			let f_name = fdecl.sfname
			and formal_types = 
				Array.of_list (List.map (fun (ty,_) -> ltype_of_typ t) fdecl.sformals)
			in let f_type = L.fuction_type (ltype_of_typ fedcl.srtyp) formal_types in 
			StringMap.add f_name (L.define_function f_name f_type the_module, fdecl) m
			in List.fold_left function_decl StringMap.empty functions in 
	in

	(* Fill in the body of the given function *)
	let build_function_body fdecl =
		let (the_function, _) = StringMap.find fdec.sfname function_decls in
		let builder = L.builder_at_end context (L.entry_block the_function) in

		let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in 
	(* TODO: *) 

	in

	let lookup n = try StringMap.find n local_vars
		with Not_found -> StringMap.find n global_vars 
	in 

	let rec build_sexpr builder ((_, e): sexpr) = match e with 
		  SLiteral i -> L.const_int i32_t i 
		| SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
		| SFLit f    -> L.const_float float_t f
		| SStrLit s  -> L.build_global_stringptr (s^"\x00") "strptr" builder
		| SListLit l -> (* TODO: list expr *)
		| SId s      -> L.build_load (lookup s) s builder
		| SNoexpr    -> L.const_int i32_t 0
		| SAssign (s, e) -> 
			let e' = build_expr builder e in 
			ignore(L.build_store e' (lookup s) builder); e'
		(* TODO *)
		| SBindAssign (ty, s, se) -> 
		| SUniop (se, op) -> 
			let e' = build_expr builder se 
			and one = L.const_int i32_t 1 in
			match op with 
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
		  	
		| SBinop (se1, op, se2)      ->
			let match_fop op = match op with 
				  A.Add      -> L.build_fadd 
				| A.Sub      -> L.build_fsub
				| A.Mult     -> L.build_fmul
				| A.Div      -> L.build_fdiv
				| A.Equal    -> L.build_fcmp L.Fcmp.Oeq
				| A.Neq      -> L.build_fcmp L.Fcmp. One
				| A.Less     -> L.build_fcmp L.Fcmp.Olt
				| A.Greater  -> L.build_fcmp L.Fcmp.Ogt
				| A.GreateEq -> L.build_fcmp L.Fcmp.Oge
				| A.LessEq   -> L.build_fcmp L.Fcmp.Ole
				| _          -> raise (Failure "And & Or operation not allowed on float type")
			in
			(* binop between float *)
			if (fst se1 = A.Float) || (fst se2 = A.Float) then
				let e1' = build_expr builder se1 
				and e2' = build_expr builder se2 in
				(match_fop op) e1' e2' "float_binop" builder	
			(* binop for others *)				
			else 
				let e1' = build_expr builder se1 and e2' = build_expr builder se2 
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
        			| A.GreateEq     -> L.build_icmp L.Icmp.Sle        			
        			| A.LessEq     -> L.build_icmp L.Icmp.Sge
				) e1' e2' "normal_binop" builder
		| SCall ("printf", [e]) ->
			L.build_call printf_func [|int_format_str ; (build_expr builder e)|]
			"printf" builder
		| SCall (f, args) -> 
			let (fdef, fdecl) = StringMap.find f function_decls in 
			let llargs = List.rev (list.map (build_expr builder) (List.rev args)) in 
			let result = f ^ "_result" in
			L.build_call fdef (Array.of_list llargs) result builder 
		in

	(* TODO *)
	let add_terminal =
	in
	
	(* TODO *)
	let build_stmt = 
		in
	(* Build the code for each statement in the function *)
	let func_builder = build_stmt builder (SBlock fdecl.sbody) in
	(* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
    in

	List.iter build_function_body functions;
	the_module

