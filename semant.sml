signature SEMANT =
sig
  type venv
  type tenv
  type expty
  type exp

  val transProg: exp -> unit
  val transExp: venv * tenv * exp -> expty


  (* val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transVar: venv * tenv * Absyn.var -> expty
  val transTy:         tenv * Absyn.ty  -> Types.ty *)
end

structure Semant : SEMANT =
struct

  structure A = Absyn
  structure T = Types
  structure S = Symbol
  structure E = Env
  structure ERR = ErrorMsg

  type venv = E.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}
  type exp = A.exp

  fun typeToString(ty:T.ty) =
    case ty of
        T.NIL => "nil"
      | T.UNIT => "unit"
      | T.INT => "int"
      | T.STRING => "string"
      | T.ARRAY(t,_) => "array of " ^ typeToString(t)
      | T.NAME(symbol,_) => "name of " ^ S.name(symbol)
      | T.RECORD(_,_) => "record"

  fun checkInt (ty:T.ty, pos) =
    if ty = T.INT then ()
    else ERR.error pos ("type mismatch: replace " ^ typeToString(ty) ^ " with int")

  fun checkTyEq (t1: T.ty, t2: T.ty, pos: int) =
    if t1 = t2 then ()
    else case (t1,t2) of
              (T.RECORD(_,_),T.NIL) => ()
            | (T.NIL,T.RECORD(_,_)) => ()
            | (_,_) => (ERR.error pos ("type mismatch: replace " ^ typeToString(t2) ^ " with " ^ typeToString(t1)); ())










  fun tyCheckRecordTy(symTyPairs, tenv) =
    let fun helper tenv {name, escape, typ, pos} =
          case S.look(tenv, typ) of
             SOME v => (name, v)
           | NONE => (ERR.error pos ("Cannot find type\
           \: " ^ S.name(typ) ^ " in record field declaration"); (name, T.UNIT))
    in
      map (helper tenv) symTyPairs
    end

  fun transTy (tenv: tenv, ty: A.ty): T.ty =
    case ty of
       A.NameTy(symbol, pos) => (case S.look(tenv, symbol) of
                                   SOME v => T.NAME(symbol, ref (SOME v))
                                 | NONE => (ERR.error pos ("Cannot find type\
                                 \: " ^ S.name(symbol)); T.UNIT))
     | A.ArrayTy(symbol, pos) => (case S.look(tenv, symbol) of
                                   SOME v => T.ARRAY(v, ref ())
                                 | NONE => (ERR.error pos ("Cannot find type\
                                 \: " ^ S.name(symbol) ^ " in array declaration");
                                 T.UNIT))
     | A.RecordTy(symTyPairs) => T.RECORD(tyCheckRecord(symTyPairs, tenv), ref ())
  and tyCheckRecord(symTyPairs, tenv) =
    let fun helper tenv {name, escape, typ, pos} =
          case S.look(tenv, typ) of
             SOME v => (name, v)
           | NONE => (ERR.error pos ("Cannot find type\
           \: " ^ S.name(typ) ^ " in record field declaration"); (name, T.UNIT))
    in
      map (helper tenv) symTyPairs
    end

  fun tyCheckArrayExp (arrSym, tenv: tenv, typeSize: expty, typeInit: expty, pos) =
    let val elementType: T.ty = case S.look(tenv, arrSym) of
                                   SOME t => t
                                 | NONE => (ERR.error pos
                                            ("Cannot find type:" ^ S.name(arrSym));
                                            T.UNIT)
        val arrType = case S.look(tenv, arrSym) of
                         SOME v => {exp=(), ty=v}
                       | NONE => {exp=(), ty=T.UNIT}
    in
      (checkInt((#ty typeSize), pos);
      checkTyEq(typeInit,(#ty elementType),pos);


      (* tyEq({exp=(), ty=elementType},typeInit); *)
      arrType)
    end




  fun  iterTransTy (tylist, {venv, tenv})=
    let fun helper ({name, ty, pos}, {venv, tenv}) = {venv=venv, tenv=S.enter(tenv,
    name, transTy(tenv, ty))}
    in
      foldl helper {venv=venv, tenv=tenv} tylist
    end














    fun checkRecord(fieldTypes, recordTyp, pos, tenv) =
      let
        fun checkFields(foundField::foundFields:(S.symbol*T.ty) list, recordField::recordFields:(S.symbol*T.ty*int) list, t) =
                (checkTyEq((#2 foundField),(#2 recordField), pos);
                if S.name(#1 foundField) = S.name(#1 recordField)
                then (checkFields(foundFields,recordFields,t))
                else (ERR.error (#3 recordField) ("error: expected field name of " ^ S.name(#1 foundField) ^ " but got " ^ S.name(#1 recordField)); {exp=(),ty=T.NIL}))
         | checkFields([], [], t) = {exp=(),ty=t}
         | checkFields(_,_,_) = {exp=(),ty=T.NIL}
      in
        (case S.look(tenv, recordTyp) of
            SOME(t) => (case t of
                          T.RECORD(foundFieldTypes,_) =>
                            let
                              val numFoundFields = length(foundFieldTypes)
                              val numRecordFields = length(fieldTypes)
                            in
                              if numFoundFields <> numRecordFields
                              then (ERR.error pos ("error: " ^ Int.toString(numRecordFields) ^ " fields needed but only " ^ Int.toString(numFoundFields) ^ " provided"); {exp=(), ty=T.NIL})
                              else checkFields(foundFieldTypes,fieldTypes,t)

                              (* checkFields(foundFieldTypes,fieldTypes) then {exp=(), ty=t}
                              else (ERR.error pos ("bad field types"); {exp=(), ty=T.NIL}); {exp=(), ty=T.NIL}) *)
                            end
                        | t => (ERR.error pos ("type mismatch: replace " ^ typeToString(t) ^ " with " ^ S.name(recordTyp)); {exp=(), ty=T.NIL}))
          | NONE => (ERR.error pos ("error: record of type " ^ S.name(recordTyp) ^ " not found"); {exp=(), ty=T.NIL}))
      end

    fun checkOp (expLeft:expty, expRight:expty, oper: A.oper, pos: int) =
      let
        val tyLeft = (#ty expLeft)
        val tyRight = (#ty expRight)

        fun checkArithOp() =
          (checkInt(tyLeft,pos); checkInt(tyRight,pos))

        fun checkCompOp() =
          case tyLeft of
            T.INT => checkTyEq(T.INT, tyRight, pos)
          | T.STRING => checkTyEq(T.STRING, tyRight, pos)
          | _ => (ERR.error pos ("illegal: cannot check comparison with " ^ typeToString(tyLeft)); ())

        fun checkEqOp() =
          case tyLeft of
            T.INT => checkTyEq(T.INT, tyRight, pos)
          | T.STRING => checkTyEq(T.STRING, tyRight, pos)
          | T.ARRAY(ty,uniq) => checkTyEq(T.ARRAY(ty,uniq), tyRight, pos)
          | T.RECORD(fieldList,uniq) => checkTyEq(T.RECORD(fieldList,uniq), tyRight, pos)
          | _ => (ERR.error pos ("illegal: cannot check equality with " ^ typeToString(tyLeft)); ())

      in
        case oper of
          A.PlusOp => (checkArithOp(); {exp=(), ty=T.INT})
        | A.MinusOp => (checkArithOp(); {exp=(), ty=T.INT})
        | A.TimesOp => (checkArithOp(); {exp=(), ty=T.INT})
        | A.DivideOp => (checkArithOp(); {exp=(), ty=T.INT})
        | A.LtOp => (checkCompOp(); {exp=(), ty=T.INT})
        | A.LeOp => (checkCompOp(); {exp=(), ty=T.INT})
        | A.GtOp => (checkCompOp(); {exp=(), ty=T.INT})
        | A.GeOp => (checkCompOp(); {exp=(), ty=T.INT})
        | A.EqOp => (checkEqOp(); {exp=(), ty=T.INT})
        | A.NeqOp => (checkEqOp(); {exp=(), ty=T.INT})
      end

  fun transExp(venv, tenv, exp) =
    let
      fun trexp exp =
        case exp of
            A.VarExp(var) => trvar(var)
          | A.NilExp => {exp=(), ty=T.NIL}
          | A.IntExp(num) => {exp=(), ty=T.INT}
          | A.StringExp((str,pos)) => {exp=(), ty=Types.STRING}
          | A.CallExp({func,args,pos}) =>
              (case S.look(venv,func) of
                NONE => (ERR.error pos ("error: function " ^ S.name(func) ^ " not defined"); {exp=(), ty=T.NIL})
              | SOME(E.VarEntry({ty})) => (ERR.error pos ("type mismatch: replace var of type " ^ typeToString(ty) ^ " with function call"); {exp=(), ty=T.NIL})
              | SOME(E.FunEntry({formals,result})) =>
                    let
                      val numFormals = length(formals)
                      val numArgs = length(args)
                      fun checkTyEqList(l1:T.ty list, l2:T.ty list) =
                        if List.null(l1) then {exp=(),ty=result}
                        else (checkTyEq((hd l1), (hd l2), pos);
                              checkTyEqList((tl l1), (tl l2)))
                    in
                      if numFormals <> numArgs then (ERR.error pos ("error: " ^ Int.toString(numFormals) ^ " args needed but only " ^ Int.toString(numArgs) ^ " provided"); {exp=(), ty=T.NIL})
                      else checkTyEqList(map #ty (map trexp args), formals)
                    end)
          | A.OpExp{left,oper,right,pos} => checkOp(trexp(left),trexp(right),oper,pos)

          | A.RecordExp{fields, typ, pos} =>
              let
                (* compute a list of the record's fields and their types *)
                val fieldTys = map (fn (fieldName, fieldExp, pos) => (fieldName, (#ty (trexp(fieldExp))), pos)) fields
              in
                checkRecord(fieldTys, typ, pos, tenv)
              end
          | A.SeqExp(exps) => (map (fn (exp,_) => (trexp exp)) exps;
                              if List.null(exps) then {exp=(), ty=T.UNIT}
                              else trexp (case List.last(exps) of (expr, pos) => expr))
          | A.AssignExp({var,exp,pos}) =>
                              let
                                val varType = trvar(var)
                                val expType = trexp(exp)
                              in
                                (checkTyEq((#ty varType),(#ty expType),pos);
                                {exp=(),ty=T.UNIT})
                              end
          | A.IfExp({test,then',else',pos}) =>
              let
                val {exp=testExp,ty=testTy} = trexp(test)
                val {exp=thenExp,ty=thenTy} = trexp(then')
              in
                case else' of
                  NONE => (checkInt(testTy,pos);
                          checkTyEq(thenTy,T.UNIT,pos);
                          {exp=(),ty=T.UNIT})
                | SOME (e) =>
                    let
                      val {exp=elseExp,ty=elseTy} = trexp(e)
                    in
                      (checkInt(testTy,pos);
                      checkTyEq(thenTy,elseTy,pos);
                      {exp=(),ty=thenTy})
                    end
              end
          | A.WhileExp({test,body=body,pos}) =>
              let
                val {exp=testExp,ty=testTy} = trexp(test)
                val {exp=bodyExp,ty=bodyTy} = trexp(body)
              in
                (checkInt(testTy, pos);
                checkTyEq(bodyTy,T.UNIT,pos);
                {exp=(),ty=T.UNIT})
              end
          | A.BreakExp(_) => {exp=(),ty=T.UNIT}

          (*
          | A.LetExp{decs, body, pos} =>
              let val {venv=venv', tenv=tenv'} = foldl transDec {venv=venv, tenv=tenv} decs
              in
                transExp (venv', tenv', body)
              end
          | A.ArrayExp{typ, size, init, pos} =>
              tyCheckArrayExp(typ, tenv, trexp(size), trexp(init), pos) *)

          | _ => (ERR.error 0 "no exp match found" ; {exp=(), ty=T.UNIT}) (* will be redundant once all A cases covered *)

        and trvar (A.SimpleVar(varname,pos)) =
              (case Symbol.look(venv, varname) of
                    NONE => (ERR.error pos ("error: undefined variable " ^ Symbol.name varname);
                    {exp=(), ty=T.UNIT})
                  | SOME (Env.VarEntry {ty}) => {exp=(), ty=ty}
                  | SOME _ => (ERR.error pos ("error: found function but expected var");
                              {exp=(), ty=T.UNIT}))
          | trvar (A.FieldVar(var,fieldname,pos)) = (* var is the record *)
              let
                val {exp,ty} = trvar(var)
              in
                case ty of
                  T.RECORD(fieldlist,_) =>
                    (case List.find (fn field => (#1 field) = fieldname) fieldlist of
                      NONE => (ERR.error pos ("error: field " ^ S.name(fieldname) ^ " not found");
                              {exp=(), ty=T.NIL})
                    | SOME(field) => {exp=(), ty=(#2 field)})
                | ty => (ERR.error pos ("error: expected record but got " ^ typeToString(ty));
                        {exp=(), ty=T.NIL})
              end
          | trvar (A.SubscriptVar(var,indexExp,pos)) = (* var is the array, exp is the index *)
              let
                val {exp,ty} = trvar(var)
              in
                case ty of
                  T.ARRAY(t,_) =>
                    let
                      val {exp=subExp,ty=subTy} = trexp(indexExp)
                    in
                      if subTy = T.INT
                      then {exp=(),ty=t}
                      else (ERR.error pos ("error: array can only be indexed with int, but found " ^ typeToString(subTy)); {exp=(),ty=T.NIL})
                    end
                | t => (ERR.error pos ("type mismatch: replace " ^ typeToString(t) ^ " with array"); {exp=(),ty=T.NIL})
              end
    in
      trexp exp
    end

  (* and transDec (A.VarDec{name, escape=ref True, typ=NONE, init, pos}, {venv, tenv}) = (case init of
              A.NilExp => (ERR.error pos "NIL is not allowed\
            \ without specifying types in variable declarations";
            {venv=venv, tenv=tenv})
            | otherExp => let val {exp, ty} = transExp (venv, tenv, otherExp)
                          in {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
                          end)
    | transDec(A.VarDec{name, escape=ref True, typ=SOME (symbol,p), init, pos},
    {venv, tenv}) =
      let val {exp, ty} = transExp (venv, tenv, init)
          val isSameTy = case S.look(tenv, symbol) of
                            NONE => (ERR.error pos "var of undeclared type set"; false)
                          | SOME found_ty => tyEq({exp=(), ty=found_ty}, {exp=(), ty=ty})
      in
        case isSameTy of
           true => {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
         | false => (ERR.error pos ("tycon mismach");
                     {venv=venv, tenv=tenv})
      end
   | transDec(A.TypeDec(tylist), {venv, tenv}) = iterTransTy(tylist, {venv=venv, tenv=tenv}) *)


  (* translate an entire program *)
  fun transProg exp =
    let val venv = Env.base_venv
        val tenv = Env.base_tenv
    in
      (transExp(venv, tenv, exp) ;())
    end

end
