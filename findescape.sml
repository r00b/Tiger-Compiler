structure FindEscape: sig
  val findEscape: Absyn.exp -> unit end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun traverseVar(env:escEnv, d:depth, s:A.var): unit =
    let
      fun checkVarDepth(SOME(foundDepth,esc)) = if d > foundDepth
                                        then esc := true
                                        else esc := false
        | checkVarDepth(NONE) = () (* handled elsewhere? *)
    in
      case s of
        A.SimpleVar(symbol,pos) => checkVarDepth(S.look(env,symbol))
      | A.FieldVar(var,symbol,pos) => traverseVar(env,d,var)
      | A.SubscriptVar(var,exp,pos) => traverseVar(env,d,var)
    end

  and traverseExp(env:escEnv, d:depth, s:A.exp): unit =
    case s of
      A.VarExp(var) => traverseVar(env,d,var)
    | A.NilExp => ()
    | A.IntExp(_) => ()
    | A.StringExp(_,_) => ()
    | A.CallExp({func,args,pos}) => (map (fn(arg) => traverseExp(env,d,arg)) args; ())
    | A.OpExp({left,oper,right,pos}) => (traverseExp(env,d,left);
                                         traverseExp(env,d,right))
    | A.RecordExp({fields,typ,pos}) => (map (fn(field) => traverseExp(env,d,#2 field)) fields; ())
    | A.IfExp({test,then',else',pos}) => (traverseExp(env,d,test);
                                          traverseExp(env,d,then');
                                          case else' of
                                            SOME(e) => traverseExp(env,d,e)
                                          | NONE => ())
    | A.SeqExp(seqs) => (map (fn(seq) => traverseExp(env,d,#1 seq)) seqs; ())
    | A.AssignExp({var,exp,pos}) => (traverseVar(env,d,var);
                                     traverseExp(env,d,exp))
    | A.WhileExp({test,body,pos}) => (traverseExp(env,d,test);
                                      traverseExp(env,d,body))
    | A.ForExp({var,escape,lo,hi,body,pos}) => let
                                                  val forEnv = S.enter(env,var,(d,escape))
                                               in
                                                  (traverseExp(env,d,lo);
                                                   traverseExp(env,d,hi);
                                                   traverseExp(forEnv,d,body))
                                               end
    | A.BreakExp(_) => ()
    | A.LetExp({decs,body,pos}) => let
                                      val decEnv = traverseDecs(env,d,decs)
                                   in
                                      traverseExp(decEnv,d,body)
                                   end
    | A.ArrayExp({typ,size,init,pos}) => (traverseExp(env,d,size);
                                          traverseExp(env,d,init))

  and traverseDecs(env, d, decs:A.dec list): escEnv =
      let
        fun traverseDec(env,d,A.FunctionDec(decs)) =
             let
                fun checkDec({name,params,result,body,pos},funEnv) =
                  let
                    fun checkParam({name,escape,typ,pos},bodyEnv) =
                      (escape := false;
                       (* add each param to table *)
                       S.enter(bodyEnv,name,(d+1,escape)))
                    (* compute new env with params *)
                    val bodyEnv = foldl checkParam funEnv params
                  in
                    (* now go through body and see what escapes *)
                    (traverseExp(bodyEnv,d+1,body);
                    funEnv)
                  end
              in
                 (* go through each fundec and compute new env from scoped vars *)
                 foldl checkDec env decs
              end

          | traverseDec(env,d,A.VarDec({name,escape,typ,init,pos})) =
             (escape := false;
              traverseExp(env,d,init);
              S.enter(env,name,(d,escape)))

          | traverseDec(env,d,A.TypeDec(decs)) = env (* no variable access in type decs *)
      in
        (* go through each dec and compute new env *)
        foldl (fn(dec,env) => traverseDec(env,d,dec)) env decs
      end

  fun findEscape(prog:A.exp): unit = traverseExp(S.empty,0,prog)

end
