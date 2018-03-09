signature ENV =
sig
  (* type access *)
  datatype enventry = VarEntry of {ty: Types.ty}
                    | FunEntry of {formals: Types.ty list, result: Types.ty}
  val base_tenv : Types.ty Symbol.table       (* predefined types *)
  val base_venv : enventry Symbol.table (* predefined functions *)
end

structure Env :> ENV =
struct
  datatype enventry = VarEntry of {ty: Types.ty}
                    | FunEntry of {formals: Types.ty list, result: Types.ty}

  structure S = Symbol
  structure T = Types

  val baseTypes = [
    ("string", T.STRING),
    ("int", T.INT)
  ]

  val baseFunctions = [
    ("print", FunEntry {formals=[T.STRING], result=T.UNIT}),
    ("flush", FunEntry {formals=[T.UNIT], result=T.UNIT}),
    ("getchar", FunEntry {formals=[T.UNIT], result=T.STRING}),
    ("ord", FunEntry {formals=[T.STRING], result=T.INT}),
    ("chr", FunEntry {formals=[T.INT], result=T.STRING}),
    ("size",  FunEntry {formals=[T.STRING], result=T.INT}),
    ("substring",  FunEntry {formals=[T.STRING, T.INT, T.INT], result=T.STRING}),
    ("concat",  FunEntry {formals=[T.STRING, T.STRING], result=T.STRING}),
    ("not",  FunEntry {formals=[T.INT], result=T.INT}),
    ("exit", FunEntry {formals=[T.INT], result=T.UNIT})
  ]

  fun addTy((name, ty), env) = S.enter(env, S.symbol name, ty)

  val base_tenv : Types.ty Symbol.table = foldl addTy S.empty baseTypes
  val base_venv : enventry Symbol.table = foldl addTy S.empty baseFunctions



end
