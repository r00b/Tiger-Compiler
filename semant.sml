structure SEMANT : sig
  (* type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  val transVar: venv * tenv * Absyn.var -> expty
  val transExp: venv * tenv * Absyn.exp -> expty
  val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy:         tenv * Absyn.ty  -> Types.ty *)
  val transProg: Absyn.exp -> unit
end =
struct

fun transProg tree = ()

end
