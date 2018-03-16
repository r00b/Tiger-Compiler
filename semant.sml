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

  fun tyCheckOper (tyLeft: expty, tyRight: expty, oper: A.oper, pos: int) =
    case (#ty tyLeft, #ty tyRight, oper) of
         (T.INT, T.INT, _) => {exp=(), ty=T.INT}
       | (T.STRING, T.STRING, _) => {exp=(), ty=T.INT}
       | (T.ARRAY(_), T.ARRAY(_), A.EqOp) => {exp=(), ty=T.INT}
       | (T.ARRAY(_), T.ARRAY(_), A.NeqOp) => {exp=(), ty=T.INT}
       | (T.ARRAY(_), T.ARRAY(_), otherOp) => (ErrorMsg.error pos
       ("Illegal operator applied to arrays. Only = and <> are allowed."); {exp=(),
       ty=T.UNIT})
       | (T.RECORD(_), T.RECORD(_), A.EqOp) => {exp=(), ty=T.INT}
       | (T.RECORD(_), T.RECORD(_), A.NeqOp) => {exp=(), ty=T.INT}
       | (T.RECORD(_), T.RECORD(_), otherOp) => (ErrorMsg.error pos
       ("Illegal operator applied to records. Only = and <> are allowed."); {exp=(),
       ty=T.UNIT})
       | (_, _, _) => (ErrorMsg.error pos "Types you used are not allowed\
       \for operaotors"; {exp=(), ty=T.UNIT})

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

  fun tyCheckArray (arrSym, tenv: tenv, typeSize: expty, typeInit: expty, pos) =
    let val elementType: T.ty = case S.look(tenv, arrSym) of
                                   SOME t => t
                                 | NONE => (ErrorMsg.error pos
                                            ("Cannot find type:" ^ S.name(arrSym));
                                            T.UNIT)
        val arrType = case S.look(tenv, arrSym) of
                         SOME v => {exp=(), ty=v}
                       | NONE => {exp=(), ty=T.UNIT}
    in
      (checkInt(typeSize, pos); tyEq({exp=(), ty=elementType},  typeInit);
      arrType)
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
            A.OpExp{left, oper, right, pos} => tyCheckOper(trexp left,
                                                           trexp right,
                                                           oper,
                                                           pos)
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
          | A.ArrayExp{typ, size, init, pos} => tyCheckArray(typ, tenv, trexp(size), trexp(init), pos)
          | _ => (ErrorMsg.error 0 "Does not match any exp" ; {exp=(), ty=T.UNIT}) (* redundant? *)
        and trvar (A.SimpleVar(varname,pos)) =
          (case Symbol.look (venv, varname) of
                NONE => (ErrorMsg.error pos ("undefined variable " ^ Symbol.name varname);
                {exp=(), ty=T.UNIT})
              | SOME (Env.VarEntry {ty}) => {exp=(), ty=ty}
              | SOME _ => (ErrorMsg.error pos ("got fun instead of var");
                          {exp=(), ty=T.UNIT}))
          | trvar (A.FieldVar(var,fieldname, pos)) =
            let val {exp,ty} = trvar var in
              case ty of
                T.RECORD(fieldlist,_) =>
                  (case List.find (fn x => #1x = fieldname) fieldlist of
                    NONE =>
                    (ErrorMsg.error pos ("field " ^ S.name fieldname ^ " not found in record");
                    {exp=(), ty=T.NIL})
                  | SOME(field) =>
                    {exp=(), ty=(#2field)})
              | ty =>
                (ErrorMsg.error pos ("expected record type");  (* TODO write fun to convert type to str *)
                {exp=(), ty=T.NIL})
            end
    in
      trexp exp
    end
  and transDec(A.VarDec{name, escape=ref True, typ=NONE, init, pos}, {venv,
  tenv}) = (case init of
              A.NilExp => (ErrorMsg.error pos "NIL is not allowed\
            \ without specifying types in variable declarations";
            {venv=venv, tenv=tenv})
            | otherExp => let val {exp, ty} = transExp (venv, tenv, otherExp)
                          in {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
                          end)
    | transDec(A.VarDec{name, escape=ref True, typ=SOME (symbol,p), init, pos},
    {venv, tenv}) =
      let val {exp, ty} = transExp (venv, tenv, init)
          val isSameTy = case S.look(tenv, symbol) of
                            NONE => (ErrorMsg.error pos "var of undeclared type set"; false)
                          | SOME found_ty => tyEq({exp=(), ty=found_ty}, {exp=(), ty=ty})
      in
        case isSameTy of
           true => {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
         | false => (ErrorMsg.error pos ("tycon mismach");
                     {venv=venv, tenv=tenv})
      end
   | transDec(A.TypeDec(tylist), {venv, tenv}) = iterTransTy(tylist, {venv=venv, tenv=tenv})

  fun transProg exp =
    let val venv = Env.base_venv
        val tenv = Env.base_tenv
    in
      (transExp(venv, tenv, exp) ;())
    end

end
