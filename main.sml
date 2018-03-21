
structure Main:
sig
  val compile: string -> unit
end =
struct
  fun compile filename =
    let
      val absyn = Parse.parse(filename)
    in
      (FindEscape.findEscape(absyn);
      Semant.transProg (Parse.parse filename))
    end
end
