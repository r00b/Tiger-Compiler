
structure Main:
sig
  val compile: string -> unit
end =
struct
  fun compile filename =
    let
      val absyn = Parse.parse(filename)
      val outs : TextIO.outstream ref = ref (TextIO.openOut "tree.txt")
    in
      (FindEscape.findEscape(absyn);
      PrintAbsyn.print(!outs,absyn);
      Semant.transProg (absyn))
    end
end
