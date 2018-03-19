structure A = Absyn
structure T = Types
structure S = Symbol
structure E = Env

signature SEMANT =
sig
  type venv
  type tenv
  type expty
  type exp

  val transProg: exp -> unit
  val transExp: venv * tenv * exp -> expty
  (*val tyCheckTypeDec: tenv * ({name: Absyn.symbol, ty: Absyn.ty, pos:
  * Absyn.pos} list) -> {venv: venv, tenv: tenv}*)
  (* type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transVar: venv * tenv * Absyn.var -> expty
  val transTy:         tenv * Absyn.ty  -> Types.ty *)
end

structure Semant : SEMANT =
struct
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}
  type exp = A.exp

  fun tyEq (t1: T.ty, t2: T.ty): bool =
    (* test cases TODO*)
    case (t1, t2) of
        (T.RECORD(r1), T.RECORD(r2)) => (#2 r1) = (#2 r2)
       | (T.RECORD(r1), _) => false
       | (_, T.RECORD(r1)) => false
       | (T.STRING, T.STRING) => true
       | (T.INT, T.INT) => true
       | (T.ARRAY(s, r1), T.ARRAY(s2, r2)) => r1 = r2
       | (T.NIL, T.NIL) => false (* TODO *)
       | (T.NAME(n1), T.NAME(n2)) => n1 = n2
       | (_, _) => ( false)

  fun tyNeq (t1: T.ty, t2: T.ty): bool = not (tyEq(t1, t2))

  fun checkInt ({exp=X, ty=Y}, pos) = case Y of
                                      Types.INT => ()
                                     |_ => ErrorMsg.error pos "Expecting INT."

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
         {exp=(), ty=T.UNIT} => if tyEq(#ty ty, T.UNIT) then {exp=(), ty=T.UNIT}
                   else (ErrorMsg.error p "elseExp returns unit but thenExp does not"; {exp=(), ty=T.UNIT})
       | ty2 => if tyEq(#ty ty, #ty ty2) then ty (* TODO *)
                   else (ErrorMsg.error p "thenExp returns different types from elseExp."; {exp=(), ty=T.UNIT})

  fun recordTyGenerator (tyList: A.field list, tenv) : T.ty =
    let
      fun lookUpTy {name, escape, typ, pos} = case S.look(tenv, typ) of
                       SOME v => (name, v)
                     | NONE => (ErrorMsg.error 0 ("Cannot find: " ^ S.name(typ));
                                (name, T.UNIT))
    in
      T.RECORD((fn () => map lookUpTy tyList, ref ()))
    end

  fun tyCheckArrayExp (arrSym, tenv: tenv, typeSize: expty, typeInit: expty, pos) =
    let val elementType: T.ty = case S.look(tenv, arrSym) of
                                   SOME t => t
                                 | NONE => (ErrorMsg.error pos
                                            ("Cannot find type:" ^ S.name(arrSym));
                                            T.UNIT)
        val arrType = case S.look(tenv, arrSym) of
                         SOME v => {exp=(), ty=v}
                       | NONE => {exp=(), ty=T.UNIT}
    in
      (checkInt(typeSize, pos); tyEq(elementType,  #ty typeInit);
      arrType)
    end

  fun tyCheckRecordExp(fields, typ, pos, tenv) =
    let fun checkFields(x::xs: (S.symbol*T.ty*int) list, y::ys: (S.symbol*T.ty) list, allCorrect: bool) =
            (case (S.name(#1 x) = S.name(#1 y), tyEq(#2 x, #2 y)) of
              (true, true) => checkFields(xs, ys, allCorrect)
              | _ => (
                  (print (S.name(#1 x) ^ " : "
                  ^ T.tyToString(#2 x) ^ "\n" ^ S.name(#1 y) ^ " : " ^
                  T.tyToString(#2 y) ^ "\n"));
                  checkFields(xs, ys, false)
              )
            )
          | checkFields([], [], allCorrect) = allCorrect
          | checkFields(_, _, _) = false (* TODO print some useful information*)
  in
    (case S.look(tenv, typ) of
        SOME v =>(case v of
                    T.RECORD (r, _) => (case checkFields (fields,  r(), true) of
                       true => {exp=(), ty=v}
                     | false =>( ErrorMsg.error pos "Fail to create a record\
                     \ becasuse of type mismatch"; {exp=(), ty=T.UNIT}))
                  | _ => ( ErrorMsg.error pos "Fail to create a record\
                     \ becasuse of type mismatch"; {exp=(), ty=T.UNIT}))
      | NONE => (ErrorMsg.error pos ("Cannot locate type:" ^ S.name typ);
               {exp=(), ty=T.UNIT})
    )
  end

  fun tyCheckOneField (allNames: tenv*A.symbol list) (symTy: A.symbol * A.pos): bool =
    let
      fun eqItem (item1: A.symbol) (item2: A.symbol) = item1 = item2
      val (tenv, newTypes) = allNames
      val (sym, pos) = symTy
      val foundInNewTypes = case List.find (eqItem sym) newTypes of
                               NONE => false
                             | SOME v => true
      val foundInTENV = case S.look(tenv, sym) of
                             SOME v => true
                           | NONE => false
    in
      foundInNewTypes orelse foundInTENV
    end

  fun tyCheckRecordTy(fields, allNames) =
    let
      fun helper allNames ({name, escape, typ, pos}, allCorrect:bool): bool =
        let
          val typeFound = tyCheckOneField allNames (typ, pos)
        in
          allCorrect andalso typeFound
        end
    in
      foldl (helper allNames) true fields
    end

  fun tyCheckTypeDec(tenv, tylist: {name: A.symbol, ty: A.ty, pos: A.pos} list) =
    let
      val newTypes = map (fn r => #name r) tylist
      val allNames = (tenv, newTypes)
      fun isLegal allNames {name, ty, pos} =
        let
        in
          (case ty of
                A.NameTy(nameTy) => tyCheckOneField allNames nameTy
              | A.RecordTy(recordTy) => tyCheckRecordTy(recordTy, allNames)
              | A.ArrayTy(arrTy) => tyCheckOneField allNames arrTy
          )
        end
      val legalTypes = List.filter (isLegal allNames) tylist
    in
      case tylist = legalTypes of
           true => tylist
         | false => tyCheckTypeDec(tenv, legalTypes)
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
              if tyNeq(#ty (trexp exp2), T.UNIT)
              then ErrorMsg.error p "while body produces values"
              else ();
              {exp=(), ty=T.UNIT})
          | A.AssignExp({var=var, exp=exp, pos=pos}) =>
              if tyEq(#ty (trvar var), #ty (trexp exp))
              then {exp=(), ty = T.UNIT}
              else (ErrorMsg.error pos "assign type mismatch";
                    {exp=(), ty = T.UNIT})
          | A.LetExp{decs, body, pos} =>
              let val {venv=venv', tenv=tenv'} = foldl transDec {venv=venv, tenv=tenv} decs
              in
                transExp (venv', tenv', body)
              end
          | A.ArrayExp{typ, size, init, pos} =>
              tyCheckArrayExp(typ, tenv, trexp(size), trexp(init), pos)
          | A.RecordExp{fields, typ, pos} => tyCheckRecordExp(
                           map (fn (sym, exp, pos) =>
                                   (sym, #ty (transExp(venv, tenv, exp)), pos)
                               ) fields,
                           typ,
                           pos,
                           tenv)
          | A.VarExp var => trvar var
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
                            NONE => (ErrorMsg.error pos ("Cannot find the type:"
                            ^ S.name(symbol)); false)
                          | SOME t => tyEq(t, ty)
      in
        (if isSameTy then {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
        else (ErrorMsg.error pos ("tycon mistach"); {venv=venv, tenv=tenv}))
      end
   | transDec(A.TypeDec(tylist), {venv, tenv}) = (tyCheckTypeDec(tenv, tylist);
   {venv=venv, tenv=tenv})

  fun transProg exp =
    let val venv = Env.base_venv
        val tenv = Env.base_tenv
    in
      (transExp(venv, tenv, exp) ;())
    end

end
