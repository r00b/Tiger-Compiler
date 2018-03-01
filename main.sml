
structure Main:
sig
  val compile: string -> unit
end =
struct
  fun compile filename =
    Semant.transProg (Parse.parse filename)
end
