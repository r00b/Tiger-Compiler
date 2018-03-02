
structure Main:
sig
  val compile: string -> unit
end =
struct
  fun compile filename =
    SEMANT.transProg (Parse.parse filename)
end
