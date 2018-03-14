signature SEMANT =
sig
  type venv
  type tenv
  type expty
  type exp

  val transProg: exp -> unit
  val transExp: venv * tenv * exp -> expty
  (* type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transVar: venv * tenv * Absyn.var -> expty
  val transTy:         tenv * Absyn.ty  -> Types.ty *)
end

structure Semant : SEMANT =
struct
  structure A = Absyn
  structure T = Types
  structure S = Symbol
  structure E = Env
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}
  type exp = A.exp

  fun tyEq (t1: expty, t2: expty): bool = t1 = t2
  fun tyNeq (t1: expty, t2: expty): bool = t1 <> t2

  fun checkInt ({exp=X, ty=Y}, pos) = if Y = Types.INT then ()
                                      else ErrorMsg.error pos "Expecting INT."

  fun intOper ({left=lexp, oper=operation, right=rexp, pos=p}, f) = case operation of
                A.PlusOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.MinusOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.TimesOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.DivideOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.EqOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.NeqOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.LtOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.LeOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.GtOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})
              | A.GeOp => (checkInt(f lexp, p); checkInt(f rexp, p); {exp=(), ty=Types.INT})

  fun checkIfExp (ty, expectedTy, p) =
    case expectedTy of
         {exp=(), ty=T.UNIT} => if tyEq(ty, {exp=(), ty=T.UNIT}) then {exp=(), ty=T.UNIT}
                   else (ErrorMsg.error p "elseExp returns unit but thenExp does not"; {exp=(), ty=T.UNIT})
       | ty2 => if tyEq(ty, ty2) then ty (* TODO *)
                   else (ErrorMsg.error p "thenExp returns different types from elseExp."; {exp=(), ty=T.UNIT})

  fun transTy (tenv: tenv, ty: A.ty): T.ty =
    case ty of
       A.NameTy(symbol, pos) => (case S.look(tenv, symbol) of 
                                   SOME v => T.NAME(symbol, ref (SOME v))
                                 | NONE => (ErrorMsg.error pos ("Cannot find type\
                                 \: " ^ S.name(symbol)); T.UNIT))
     | A.ArrayTy(symbol, pos) => (case S.look(tenv, symbol) of 
                                   SOME v => T.ARRAY(v, ref ())
                                 | NONE => (ErrorMsg.error pos ("Cannot find type\
                                 \: " ^ S.name(symbol) ^ " in array declaration"); 
                                 T.UNIT))
     | A.RecordTy(symTyPairs) => T.RECORD(tyCheckRecord(symTyPairs, tenv), ref ())
  and tyCheckRecord(symTyPairs, tenv) = 
    let fun helper tenv {name, escape, typ, pos} = 
          case S.look(tenv, typ) of 
             SOME v => (name, v)
           | NONE => (ErrorMsg.error pos ("Cannot find type\
           \: " ^ S.name(typ) ^ " in record field declaration"); (name, T.UNIT))
    in
      map (helper tenv) symTyPairs
    end
                                 




  fun  iterTransTy (tylist, {venv, tenv})= 
    let fun helper ({name, ty, pos}, {venv, tenv}) = {venv=venv, tenv=S.enter(tenv,
    name, transTy(tenv, ty))}
    in
      foldl helper {venv=venv, tenv=tenv} tylist
    end

  fun transExp(venv, tenv, exp) =
    let
      fun trexp exp =
        case exp of
            A.OpExp(x) => intOper (x, trexp) (*TODO fix string/array comparison*)
          | A.IntExp(num) => {exp=(), ty=Types.INT}
          | A.StringExp((s,p)) => {exp=(), ty=Types.STRING}
          | A.IfExp({test=cond, then'=thenExp, else'=elseExp, pos=p}) =>
              (case elseExp of (*TODO return value for IR*)
                    NONE => (checkInt(trexp cond, p);
                    checkIfExp(trexp thenExp, {exp=(), ty=T.UNIT}, p))
                  | SOME v => (checkInt(trexp cond, p);
                    checkIfExp(trexp thenExp, trexp v, p)))
          | A.SeqExp(x) => if List.length x = 0 then {exp=(), ty=T.UNIT}
                           else trexp (case List.last (x) of (v, pos) => v)
          | A.WhileExp({test=exp, body=exp2, pos=p}) =>
              (checkInt(trexp exp, p);
              if tyNeq(trexp exp2, {exp=(), ty=T.UNIT})
              then ErrorMsg.error p "while body produces values"
              else ();
              {exp=(), ty=T.UNIT})
          | A.AssignExp({var=var, exp=exp, pos=pos}) =>
              if tyEq(trvar var, trexp exp)
              then {exp=(), ty = T.UNIT}
              else (ErrorMsg.error pos "assign type mismatch";
                    {exp=(), ty = T.UNIT})
          | A.LetExp{decs, body, pos} =>
              let val {venv=venv', tenv=tenv'} = foldl transDec {venv=venv, tenv=tenv} decs
              in
                transExp (venv', tenv', body)
              end
          | _ => (ErrorMsg.error 0 "Does not match any exp" ; {exp=(), ty=T.UNIT}) (* redundant? *)
        and trvar (A.SimpleVar(varname,pos)) =
          (case Symbol.look (venv, varname) of
                NONE => (ErrorMsg.error pos ("undefined variable " ^ Symbol.name varname);
                {exp=(), ty=T.UNIT})
              | SOME (Env.VarEntry {ty}) => {exp=(), ty=ty}
              | SOME _ => (ErrorMsg.error pos ("got fun instead of var");
                          {exp=(), ty=T.UNIT}))
    in
      trexp exp
    end
  and transDec(A.VarDec{name, escape=ref True, typ=NONE, init, pos}, {venv,
  tenv}) = 
      let val {exp, ty} = transExp (venv, tenv, init)
      in
        {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
      end
    | transDec(A.VarDec{name, escape=ref True, typ=SOME (symbol,p), init, pos},
    {venv, tenv}) = 
      let val {exp, ty} = transExp (venv, tenv, init)
          val isSameTy = case S.look(tenv, symbol) of
                            NONE => (ErrorMsg.error pos ("Cannot find the type:"
                            ^ S.name(symbol)); false)
                          | SOME t => tyEq({exp=(), ty=t}, {exp=(), ty=ty})
      in
        (if isSameTy then {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
        else (ErrorMsg.error pos ("tycon mistach"); {venv=venv, tenv=tenv}))
      end
   | transDec(A.TypeDec(tylist), {venv, tenv}) = iterTransTy(tylist, {venv=venv, tenv=tenv})

  fun transProg exp =
    let val venv = Env.base_venv
        val tenv = Env.base_tenv
    in
      (transExp(venv, tenv, exp) ;())
    end

end
