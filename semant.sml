signature SEMANT =
sig
  type venv
  type tenv
  type expty
  type exp

  val transProg: exp -> unit
  (* type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  (* val transVar: venv * tenv * Absyn.var -> expty *)
  val transExp: venv * tenv * Absyn.exp -> expty
  val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy:         tenv * Absyn.ty  -> Types.ty *)
end

structure Semant : SEMANT =
struct
  structure A = Absyn
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}
  type exp = A.exp

  fun checkInt ({exp=X, ty=Y}, pos) = case Y of
                                           Types.INT => ()
                                         | _ => print "Typecon mismatch"

  fun transExp(venv, tenv, exp) =
    let fun trexp exp =
      case exp of
          A.OpExp({left=lexp, oper=operation, right=rexp, pos=p}) =>
            (
            case operation of
                A.PlusOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.MinusOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.TimesOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.DivideOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.EqOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.NeqOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.LtOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.LeOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.GtOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
              | A.GeOp => (checkInt(trexp lexp, p); checkInt(trexp rexp, p); {exp=(), ty=Types.INT})
            )
        | A.IntExp(num) => {exp=(), ty=Types.INT}
        | _ => (print "errors. not matching any typs"; {exp=(), ty=Types.UNIT})
    in
      (trexp(exp); ())
    end

  fun transProg exp =
    let val venv = Env.base_venv
        val tenv = Env.base_tenv
    in
      (transExp(venv, tenv, exp) ;())
    end

end
