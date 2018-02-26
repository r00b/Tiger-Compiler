structure Main : sig val typeCheck : string -> unit  end =
struct
  fun typeCheck filename = Semant.transProg (Parse.parse filename)
end
