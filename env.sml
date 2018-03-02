signature ENV = 
sig 
  (* type access *)
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv : ty Symbol.table       (* predefined types *)
  val base_venv : enventry Symbol.table (* predefined functions *)
end

structure Env :> ENV =
struct 
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  structure S = Symbol
  val base_tenv: ty Symbol.table = Symbol.empty
  val base_venv: enventry Symbol.table = Symbol.empty
end
              
