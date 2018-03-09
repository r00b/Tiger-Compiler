signature ENV =
sig
  (* type access *)
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv : ty Symbol.table       (* predefined types *)
  val base_venv : enventry Symbol.table (* predefined functions *)
end

structure Env : ENV =
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

  val base_tenv : ty Symbol.table = foldl addTy S.empty baseTypes
  val base_venv : enventry Symbol.table = foldl addTy S.empty baseFunctions



end
