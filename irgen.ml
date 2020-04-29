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

	List.iter build_function_body functions;
	the_module