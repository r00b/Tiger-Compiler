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
  structure S = Symbol
  structure T = Types
  val base_tenv = S.enter(S.empty, S.symbol "int", Types.INT);
  S.enter(S.empty, S.symbol "string", Types.STRING);
end
              
