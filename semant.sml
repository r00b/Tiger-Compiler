structure SEMANT : sig
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  (* val transVar: venv * tenv * Absyn.var -> expty *)
  val transExp: venv * tenv * Absyn.exp -> expty
  (* val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv} *)
  (* val transTy:         tenv * Absyn.ty  -> Types.ty *)
  val transProg: Absyn.exp -> unit
end =
struct

structure A = Absyn
structure E = Env
structure Err = ErrorMsg
structure T = Types

type expty = {exp: Translate.exp, ty: T.ty}
type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table


fun transExp(venv, tenv, A.OpExp{left,oper=A.PlusOp,right,pos}) =
  let val {exp=_, ty=tyleft} = transExp(venv,tenv,left)
      val {exp=_, ty=tyright} = transExp(venv,tenv,right)
   in case tyleft of Types.INT => ()
                   | _ => Err.error pos "integer required";
      case tyright of Types.INT => ()
                   | _ => Err.error pos "integer required";
      print("k");
      {exp=(), ty=Types.INT}
end

fun transProg tree =
  let
    val f = transExp(Env.base_venv, Env.base_tenv, tree)
  in
   print("F");
   ()
   end

end
