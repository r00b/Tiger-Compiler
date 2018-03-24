structure A = Absyn
structure T = Types
structure S = Symbol
structure E = Env
structure ERR = ErrorMsg

signature SEMANT =
sig
  type venv
  type tenv
  type expty
  type exp

  val transProg: exp -> unit
  val transExp: venv * tenv * exp -> expty
end

structure Semant : SEMANT =
struct
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}
  type exp = A.exp

  val err_rep = {exp=(),ty=T.BOTTOM}
  val error = ErrorMsg.error

  fun tyEq (t1:T.ty, t2:T.ty, pos:int): bool =
    case (t1, t2) of
         (T.RECORD(u1), T.RECORD(u2)) => (#2 u1) = (#2 u2)
       | (T.RECORD(u1), _) => false
       | (_, T.RECORD(u1)) => false
       | (T.STRING, T.STRING) => true
       | (T.INT, T.INT) => true
       | (T.UNIT, T.UNIT) => true
       | (T.ARRAY(t1, u1), T.ARRAY(t2, u2)) => u1 = u2
       | (T.NIL, T.NIL) => false (* TODO *)
       | (T.NAME(n1), T.NAME(n2)) => n1 = n2
       | (T.BOTTOM, _) => true
       | (_, T.BOTTOM) => true
       | (_, _) => false

  fun isSubtype(t1: T.ty, t2: T.ty) =
    (* Whether t1 is a subtype of t2 *)
    case (t1, t2) of
         (T.NIL, T.RECORD(_)) => true
       | (_, _) => false

  fun tyEqOrIsSubtype(t1: T.ty, t2: T.ty, pos:int) =
    (* true if t1 is a subtype of t2 or t1 is of same type with t2 *)
    isSubtype(t1, t2) orelse tyEq(t1, t2, pos)

  fun tyNeq (t1: T.ty, t2: T.ty, pos:int): bool = not (tyEq(t1, t2, pos))

  fun isInt (ty:T.ty, pos) = tyEq(ty,T.INT,pos)

  fun recordTyGenerator (tyList: A.field list, tenvRef: tenv ref) : T.ty =
    let
      fun lookUpTy {name, escape, typ, pos} = case S.look(!tenvRef, typ) of
                       SOME v => (name, v)
                     | NONE => (ERR.error 0 ("Cannot find: " ^ S.name(typ));
                                (name, T.BOTTOM))
    in
      T.RECORD((fn () => map lookUpTy tyList, ref ()))
    end

  fun tyCheckArrayExp (arrSym, tenv: tenv, typeSize: expty, typeInit: expty, pos) =
    let val elementType: T.ty = case S.look(tenv, arrSym) of
                                   SOME (T.ARRAY(t)) => #1 t
                                 | SOME otherType => (ERR.error pos
                                            ("TypeError: expecting array type:"
                                            ^ S.name(arrSym) ^ " and get " ^
                                            T.toString(otherType));
                                            T.BOTTOM)
                                 | NONE => (ERR.error pos
                                            ("Cannot find type:" ^ S.name(arrSym));
                                            T.BOTTOM)
        val arrType = S.look(tenv, arrSym)
        val isIndexInt = tyEq(#ty typeSize, T.INT, pos)
        val defaultValueRightType = tyEq(elementType,  #ty typeInit, pos)
    in
      (case (isIndexInt, defaultValueRightType, arrType) of
             (true, true, SOME v) => {exp=(), ty=v}
          | (false, _, _) => (ERR.error pos "IndexTypeError";
                              {exp=(), ty=T.BOTTOM})
          | (_, false, _) => (ERR.error pos "DefaultValueTypeError";
                              {exp=(), ty=T.BOTTOM})
          | (_, _, NONE) => (ERR.error pos "Cannot find type of the array";
                              {exp=(), ty=T.BOTTOM}))
    end

  fun tyCheckRecordExp(fields, typ, pos, tenv) =
    let fun checkFields(x::xs: (S.symbol*T.ty*int) list, y::ys: (S.symbol*T.ty) list, allCorrect: bool) =
            (case (S.name(#1 x) = S.name(#1 y), tyEqOrIsSubtype(#2 x, #2 y, pos)) of
              (true, true) => checkFields(xs, ys, allCorrect)
              | _ => (
                  (print (S.name(#1 x) ^ " : "
                  ^ T.toString(#2 x) ^ "\n" ^ S.name(#1 y) ^ " : " ^
                  T.toString(#2 y) ^ "\n"));
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
                     | false =>( ERR.error pos "Fail to create a record\
                     \ becasuse of type mismatch"; {exp=(), ty=T.BOTTOM}))
                  | _ => ( ERR.error pos "Fail to create a record\
                     \ becasuse of type mismatch"; {exp=(), ty=T.BOTTOM}))
      | NONE => (ERR.error pos ("Cannot locate type:" ^ S.name typ);
               {exp=(), ty=T.BOTTOM})
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

  fun filterAndPrint f [] = []
    | filterAndPrint f (x::xs:{name: A.symbol, ty: A.ty, pos: A.pos} list) =
      if f(x) then x::(filterAndPrint f xs)
      else (
      ERR.error (#pos x) ("TypeDecError in for type:" ^ S.name (#name x));
      filterAndPrint f xs
      )

  fun tyCheckTypeDec(tenvRef, tylist: {name: A.symbol, ty: A.ty, pos: A.pos} list) =
    let
      val newTypes = map (fn r => #name r) tylist
      val allNames = (!tenvRef, newTypes)
      fun isLegal allNames {name, ty, pos} =
        let
        in
          (case ty of
                A.NameTy(nameTy) => tyCheckOneField allNames nameTy
              | A.RecordTy(recordTy) => tyCheckRecordTy(recordTy, allNames)
              | A.ArrayTy(arrTy) =>tyCheckOneField allNames arrTy
          )
        end
      val legalTypes = filterAndPrint (isLegal allNames) tylist
    in
      case tylist = legalTypes of
           true => tylist (* stop updating; return the value *)
         | false => tyCheckTypeDec(tenvRef, legalTypes)
    end

  fun updateTenv(tenvRef: tenv ref, legalTylist):tenv =
    (* This function add legalTylist to tenv
    *
    * After passing tylist to tyCheckTypeDec, we gain legalTylist where
    * we are ready to add these legal types to tenv.
    * *)
    let fun helper tenvRef ({name, ty, pos}) =
      (case ty of
          A.NameTy(nameTy) => (tenvRef := S.enter(!tenvRef,
                                      name,
                                      T.NAME((#1 nameTy), ref (S.look(!tenvRef, (#1
                                      nameTy))))))
        | A.ArrayTy(arrTy) => (tenvRef := S.enter(!tenvRef,
                                      name,
                                      T.ARRAY(valOf(S.look(!tenvRef, #1 arrTy)),
                                      (ref ()))))
        | A.RecordTy(recordTy) => (tenvRef := S.enter(!tenvRef,
                                          name,
                                          recordTyGenerator(recordTy, tenvRef))))
    in
      (map (helper tenvRef) legalTylist; !tenvRef)
    end


  fun checkOp (expLeft:expty, expRight:expty, oper: A.oper, pos: int) =
        let
          val tyLeft = (#ty expLeft)
          val tyRight = (#ty expRight)

          fun checkArithOp() =
            if not (isInt(tyLeft,pos))
            then (error pos ("type mismatch: cannot perform arithmetic with " ^ T.toString(tyLeft));
                  err_rep)
            else if not (isInt(tyRight,pos))
            then (error pos ("type mismatch: cannot perform arithmetic with " ^ T.toString(tyRight));
                  err_rep)
            else {exp=(), ty=T.INT}

          fun checkCompOp() =
            case tyLeft of
              T.INT => if tyEq(T.INT, tyRight, pos)
                       then {exp=(), ty=T.INT}
                       else (error pos ("type mismatch: cannot compare int with " ^ T.toString(tyRight));
                             err_rep)
            | T.STRING => if tyEq(T.STRING, tyRight, pos)
                          then {exp=(), ty=T.INT}
                          else (error pos ("type mismatch: cannot compare string with " ^ T.toString(tyRight));
                                err_rep)
            | _ => (error pos ("type mismatch: cannot check comparison with " ^ T.toString(tyLeft));
                    err_rep)

          fun checkEqOp() =
            case tyLeft of
              T.INT => if tyEq(T.INT, tyRight, pos)
                       then {exp=(), ty=T.INT}
                       else (error pos ("type mismatch: cannot compare int with " ^ T.toString(tyRight));
                             err_rep)
            | T.STRING => if tyEq(T.STRING, tyRight, pos)
                          then {exp=(), ty=T.INT}
                          else (error pos ("type mismatch: cannot compare string with " ^ T.toString(tyRight));
                                err_rep)
            | T.ARRAY(ty,u) => if tyEq(T.ARRAY(ty,u), tyRight, pos)
                               then {exp=(), ty=T.INT}
                               else ((case tyRight of
                                         T.ARRAY(_,_) => error pos ("error: cannot compare arrays of different types")
                                       | t => error pos ("error: cannot compare array with " ^ T.toString(t)));
                                     err_rep)
            | T.RECORD(fields,u) => if tyEq(T.RECORD(fields,u), tyRight, pos)
                                    then {exp=(), ty=T.INT}
                                    else ((case tyRight of
                                              T.RECORD(_,_) => error pos ("error: cannot compare two different record types")
                                            | t => error pos ("error: cannot compare record with " ^ T.toString(t)));
                                          err_rep)
            | _ => (error pos ("error: cannot check equality with " ^ T.toString(tyLeft));
                    err_rep)
        in
          case oper of
            A.PlusOp => (checkArithOp())
          | A.MinusOp => (checkArithOp())
          | A.TimesOp => (checkArithOp())
          | A.DivideOp => (checkArithOp())
          | A.LtOp => (checkCompOp())
          | A.LeOp => (checkCompOp())
          | A.GtOp => (checkCompOp())
          | A.GeOp => (checkCompOp())
          | A.EqOp => (checkEqOp())
          | A.NeqOp => (checkEqOp())
        end

    fun getHeader tenv {name=nameFun, params, result, body, pos}: S.symbol *
      E.enventry *bool =
      let
        fun foldHelper ({name=nameVar, escape, typ, pos}, ans): T.ty list =
          case ans of
               [T.BOTTOM] => [T.BOTTOM]
             | tyList => (case S.look(tenv, typ) of
                             NONE => (ERR.error pos ("Cannot find " ^ S.name(typ) ^
                             " in the header of " ^ S.name(nameFun)); [T.BOTTOM])
                           | SOME v => v::tyList)
        val formals = foldl foldHelper [] params
        val returnType = case result of
                            NONE => T.UNIT
                          | SOME (sym, pos) =>
                             (case S.look(tenv, sym) of
                                  SOME t => t
                                | NONE => (ERR.error pos ("Cannot find the return type " ^ S.name(sym)); T.BOTTOM))
        val badHeader = case (formals, returnType) of
                             (_, T.BOTTOM) => true
                           | ([T.BOTTOM], _) => true
                           | _ => false
      in
        (nameFun, E.FunEntry{formals=formals, result=returnType}, badHeader)
      end

    fun addHeaders(headerList, venv): venv * bool =
      let fun helper(header, (venv, broken)) =
            case (header, broken) of
                 ((_, _, true), _) => (venv, true)
               |(_, true) => (venv, true)
               | ((name, funEntry, false), false) =>
                   (S.enter(venv, name, funEntry), false)
      in
        foldl helper (venv, false) headerList
      end

  fun transExp(venv, tenv: tenv, exp) =
    let
      fun trexp exp =
        case exp of
            A.VarExp(var) => trvar(var)
          | A.NilExp => {exp=(), ty=T.NIL}
          | A.IntExp(num) => {exp=(), ty=Types.INT}
          | A.StringExp((str,pos)) => {exp=(), ty=Types.STRING}
          | A.CallExp({func,args,pos}) =>
              (case S.look(venv,func) of
                NONE => (ERR.error pos ("error: function " ^ S.name(func) ^ " not defined");
                        {exp=(), ty=T.BOTTOM})
              | SOME(E.VarEntry({ty})) => (ERR.error pos ("type mismatch: replace var " ^
                                          S.name(func) ^ " with function call");
                                          {exp=(), ty=T.BOTTOM})
              | SOME(E.FunEntry({formals,result})) =>
                    let
                      val numFormals = length(formals)
                      val numArgs = length(args)
                      fun tyEqList(l1:T.ty list, l2:T.ty list) =
                        if List.null(l1)
                        then {exp=(),ty=result}
                        else (case tyEq((hd l1), (hd l2), pos) of
                               false => (ERR.error pos ("Type mismatch " ^
                                         T.toString(hd l1) ^ " and " ^
                                         T.toString(hd l2));
                                         {exp=(), ty=T.BOTTOM})
                             | true => tyEqList((tl l1), (tl l2)))
                    in
                      if numFormals <> numArgs
                      then (ERR.error pos ("error: " ^ Int.toString(numFormals)
                           ^ " args needed but only " ^ Int.toString(numArgs) ^
                           " provided");
                           {exp=(), ty=T.BOTTOM})
                      else tyEqList(map #ty (map trexp args), formals)
                    end)
          | A.OpExp({left,oper,right,pos}) => checkOp(trexp(left),
                                                      trexp(right),
                                                      oper,pos)

          | A.RecordExp{fields,typ,pos} => tyCheckRecordExp(
                           map (fn (sym, exp, pos) =>
                                   (sym, #ty (transExp(venv, tenv, exp)), pos)
                               ) fields,
                           typ,
                           pos,
                           tenv)
          | A.IfExp({test,then',else',pos}) =>
              let
                val {exp=testExp,ty=testTy} = trexp(test)
                val {exp=thenExp,ty=thenTy} = trexp(then')
              in
                case else' of
                  NONE => if not (isInt(testTy,pos))
                          then (error pos ("type mismatch: test expression must be int, not " ^ T.toString(testTy));
                               err_rep)
                          else if tyEq(thenTy,T.UNIT,pos)
                          then {exp=(),ty=T.UNIT}
                          else (error pos ("error: then expression must return unit, not " ^ T.toString(thenTy));
                                err_rep)
                | SOME (e) =>
                    let
                      val {exp=elseExp,ty=elseTy} = trexp(e)
                    in
                      if not (isInt(testTy,pos))
                      then (error pos ("type mismatch: test expression must be int, not " ^ T.toString(testTy));
                           err_rep)
                      else if tyEq(thenTy,elseTy,pos)
                      then {exp=(),ty=T.UNIT}
                      else (error pos ("type mismatch: " ^ T.toString(thenTy) ^ " and " ^ T.toString(elseTy));
                            err_rep)
                    end
              end
          | A.SeqExp(exps) => (map (fn (exp,_) => (trexp exp)) (rev (tl (rev exps)));
                              if List.null(exps) then {exp=(), ty=T.UNIT}
                              else trexp (case List.last(exps) of (expr, pos) => expr))

          | A.AssignExp({var,exp,pos}) =>
              let
                val varType = #ty (trvar(var))
                val expType = #ty (trexp(exp))
              in
                if tyEqOrIsSubtype(expType, varType, pos)
                then {exp=(),ty=T.UNIT}
                else (error pos ("type mismatch: cannot assign " ^ T.toString(expType) ^ " to var of " ^ T.toString(varType));
                      err_rep)
              end

          | A.WhileExp({test,body,pos}) =>
              let
                 val {exp=testExp,ty=testTy} = trexp(test)
                 val {exp=bodyExp,ty=bodyTy} = trexp(body)
              in
                if not (isInt(testTy,pos))
                then (error pos ("type mismatch: test expression must be int, not " ^ T.toString(testTy));
                      err_rep)
                else if tyEq(bodyTy,T.UNIT,pos)
                then {exp=(),ty=T.UNIT}
                else (error pos ("error: while body must eval to unit, not " ^ T.toString(bodyTy));
                      err_rep)
              end
          | A.ForExp({var,escape,lo,hi,body,pos}) =>
            let
              val limit = S.symbol("limit")
              val loTy = #ty (trexp(lo))
              val hiTy = #ty (trexp(hi))
              val bodyTy = #ty (trexp(body))
              val loopVar = A.SimpleVar(var,pos)
              val limitVar = A.SimpleVar(limit,pos)
              val loopDecs = [
                A.VarDec({name=var,escape=escape,typ=NONE,init=lo,pos=pos}),
                A.VarDec({name=limit,escape=ref false,typ=NONE,init=hi,pos=pos})
              ]
              val loop = A.WhileExp({
                test=A.OpExp({
                  left=A.VarExp(loopVar),
                  oper=A.LeOp,
                  right=A.VarExp(limitVar),
                  pos=pos
                  }),
                body=A.SeqExp([
                  (body,pos),
                  (A.AssignExp({
                    var=loopVar,
                    exp=A.OpExp({
                      left=A.VarExp(loopVar),
                      oper=A.PlusOp,
                      right=A.IntExp(1),
                      pos=pos
                    }),
                    pos=pos
                  }),pos)
                ]),
                pos=pos
              })
            in
              (* TODO don't let body reassign loop var *)
              if not (isInt(loTy,pos))
              then (error pos ("error: loop var type must be int, not " ^ T.toString(loTy));
                    err_rep)
              else if not (isInt(hiTy,pos))
              then (error pos ("error: limit var type must be int, not " ^ T.toString(hiTy));
                    err_rep)
              else if not (tyEq(bodyTy,T.UNIT,pos))
              then (error pos ("error: body must eval to unit, not " ^ T.toString(bodyTy));
                    err_rep)
              else trexp(A.LetExp{decs=loopDecs,body=body,pos=pos})
            end
          | A.BreakExp(_) => {exp=(),ty=T.UNIT}
          | A.LetExp({decs,body,pos}) =>
              let val {venv=venv', tenv=tenv'} = foldl transDec {venv=venv, tenv=tenv} decs
              in
                transExp (venv', tenv', body)
              end
          | A.ArrayExp{typ, size, init, pos} =>
              tyCheckArrayExp(typ, tenv, trexp(size), trexp(init), pos)
        and trvar (A.SimpleVar(varname,pos)) =
          (case Symbol.look (venv, varname) of
                NONE => (ERR.error pos ("undefined variable " ^ Symbol.name varname);
                {exp=(), ty=T.BOTTOM})
              | SOME (Env.VarEntry {ty}) => {exp=(), ty=ty}
              | SOME _ => (ERR.error pos ("got fun instead of var");
                          {exp=(), ty=T.BOTTOM}))
          | trvar (A.SubscriptVar(var,indexExp,pos)) = (* var is the array, exp is the index *)
              let
                val {exp,ty} = trvar(var)
              in
                case ty of
                  T.ARRAY(t,_) =>(
                    let
                      val {exp=subExp,ty=subTy} = trexp(indexExp)
                    in
                      if tyEq(subTy, T.INT, pos)
                      then {exp=(),ty=t}
                      else (ERR.error pos
                            ("error: array can only be indexed with int, but found " ^
                             T.toString(subTy));
                             {exp=(),ty=T.BOTTOM})
                    end)
                | otherTy => (ERR.error pos ("type mismatch: replace " ^
                T.toString(otherTy) ^ " with array"); {exp=(),ty=T.BOTTOM})
              end
          | trvar (A.FieldVar(var,fieldname,pos)) = (* var is the record *)
              let
                val {exp,ty} = trvar(var)
              in
                case ty of
                  T.RECORD(fieldlist,_) =>
                    (case List.find (fn field => (#1 field) = fieldname) (fieldlist()) of
                      NONE => (ERR.error pos ("error: field " ^ S.name(fieldname) ^ " not found");
                              {exp=(), ty=T.BOTTOM})
                    | SOME(field) => {exp=(), ty=(#2 field)})
                | ty => (ERR.error pos ("error: expected record but got " ^ T.toString(ty));
                        {exp=(), ty=T.BOTTOM})
              end
    in
      trexp exp
    end

  and transDec(A.VarDec{name, escape=ref True, typ=NONE, init, pos}, {venv,
  tenv}) = (case init of
              A.NilExp => (ERR.error pos "NIL is not allowed\
            \ without specifying types in variable declarations";
            {venv=venv, tenv=tenv})
            | otherExp => let val {exp, ty} = transExp (venv, tenv, otherExp)
                          in {venv=S.enter(venv, name, E.VarEntry{ty=ty}), tenv=tenv}
                          end)
    | transDec(A.VarDec{name, escape=ref True, typ=SOME (symbol,p), init, pos},
    {venv, tenv}) =
      let val {exp, ty=tyInit} = transExp (venv, tenv, init)
          val isSameTy = case S.look(tenv, symbol) of
                            NONE => (ERR.error pos ("Cannot find the type:"
                            ^ S.name(symbol)); false)
                          | SOME t => tyEqOrIsSubtype(tyInit, t, pos)
      in
        (if isSameTy then {venv=S.enter(venv, name, E.VarEntry{ty=tyInit}), tenv=tenv}
        else (ERR.error pos ("tycon mistach"); {venv=venv, tenv=tenv}))
      end
   | transDec(A.TypeDec(tylist), {venv, tenv}) =
         {venv=venv, tenv=updateTenv(ref tenv, tyCheckTypeDec(ref tenv, tylist))}
   | transDec(A.FunctionDec(fundecList), {venv, tenv}) =
       checkFunctionDec(fundecList, {venv=venv, tenv=tenv})
  and checkFunctionDec(fundecList, {venv, tenv}) =
    let
      val headerList = map (getHeader tenv) fundecList
      val (venv', badHeader) = addHeaders(headerList, venv)
    in
      case badHeader of
           true => (ERR.error (#pos (hd fundecList)) "Something wrong with headers of functions";
           {venv=venv, tenv=tenv})
         | false => (case checkEachFundec(fundecList, {venv=venv', tenv=tenv}, headerList) of
                        true => {venv=venv', tenv=tenv}
                      | false => (ERR.error (#pos (hd fundecList)) "Failing\
                      \ fundec at the second pass: type checking return type\
                      \ of each fundec";{venv=venv, tenv=tenv} ))
    end
   and checkEachFundec(fundecList: A.fundec list, {venv, tenv},  headerList: (Symbol.symbol * Env.enventry * bool) list): bool =
    let
      fun checkFundec {venv: venv, tenv: tenv} ((fundec, header), false) = false
        | checkFundec {venv, tenv: tenv} ((fundec, header), true) =
            let
              fun addVar(fieldList): venv =
                 let fun helper ({name, escape, typ, pos}, table) =
                   S.enter(table, name, E.VarEntry{ty=valOf(S.look(tenv, typ))})
                 in
                   foldl helper venv fieldList
                 end
              val {name, params, result, body, pos} = fundec
              val (nameFun, E.FunEntry{formals, result=expectedType}, badHeader) = header
              val venvNew = addVar params
              val t =  transExp(venvNew, tenv, body)
            in
               case tyEq(#ty t, expectedType, pos) of
                    true => true
                  | false => (ERR.error pos ("Get " ^ T.toString(#ty t) ^ " rather than " ^ T.toString(expectedType)); false)
            end
    in
      foldl (checkFundec {venv=venv, tenv=tenv}) true (ListPair.zip(fundecList, headerList))
    end

  fun transProg exp =
    let val venv = Env.base_venv
        val tenv = Env.base_tenv
    in
      (transExp(venv, tenv, exp) ;())
    end

end
