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
	and list_t t  = L.struct_type context [|L.pointer_type (L.i32_type context); L.pointer_type t|]
	in

	let rec ltype_of_typ = function 
		  A.Int -> i32_t
		| A.Bool -> i1_t
		| A.Float -> float_t
		| A.List t -> list_t (ltype_of_typ t)
	in 

	(* Create a map of global variables after creating each *)
	let global_Vars : L.llvalue StringMap.t = 
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
	
		(* Dine each function (arguments are return type) 
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
	(* let build_function_body fdecl = 
		let (the_fucntion, _) = StringMap.find fdecl.sfname function_decls in 
	*)

	let rec build_expr builder ((_, e): sexpr) = match e with 
		  SLiteral i  -> L.const_int i32_t i 
		| SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
		| SFLit f -> L.const_float float_t f
		| SListLit l -> (* TODO: list expr *)
		| SUniop (e, op) -> 
		  let e1 = build_expr builder e in
		  



	List.iter build_function_body functions;
	the_module

