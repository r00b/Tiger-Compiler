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
  structure T = Types

  val baseTypes = [
    ("string", T.STRING),
    ("int", T.INT)
  ]

  fun addTy((name, ty), env) = S.enter(env, S.symbol name, ty)

  val base_tenv : ty Symbol.table = foldl addTy S.empty baseTypes
  val base_venv : enventry Symbol.table = S.empty


end
