signature ENV =
sig
  (*
  type access
  *)
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label,
                                   formals: Types.ty list,
                                   result: Types.ty}
  val base_tenv : Types.ty Symbol.table       (* predefined types *)
  val base_venv : enventry Symbol.table (* predefined functions *)
end

structure Env :> ENV =
struct
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label,
                                   formals: Types.ty list,
                                   result: Types.ty}

  structure S = Symbol
  structure T = Types

  val baseTypes = [
    ("string", T.STRING),
    ("int", T.INT),
    ("unit", T.UNIT)
  ]

  val baseFunctions = [
    ("print", [T.STRING], T.UNIT),
    ("flush", [T.UNIT], T.UNIT),
    ("getchar", [T.UNIT], T.STRING),
    ("ord", [T.STRING], T.INT),
    ("chr", [T.INT], T.STRING),
    ("size",  [T.STRING], T.INT),
    ("substring",  [T.STRING, T.INT, T.INT], T.STRING),
    ("concat",  [T.STRING, T.STRING], T.STRING),
    ("not",  [T.INT], T.INT),
    ("exit", [T.INT], T.UNIT)
  ]

  fun addTy((name, ty), env) = S.enter(env, S.symbol name, ty)

  fun addVar((name, formals, returnType), env) =
    let
      val func = FunEntry({level=Translate.newLevel({parent=Translate.outermost,
                                                    name=Temp.namedlabel(name),
                                                    formals=(map (fn _ => false) formals)}),
                          label=Temp.namedlabel(name),
                          formals=formals,
                          result=returnType})
    in
      S.enter(env, S.symbol name, func)
    end

  val base_tenv : Types.ty Symbol.table = foldl addTy S.empty baseTypes
  val base_venv : enventry Symbol.table = foldl addVar S.empty baseFunctions

end
