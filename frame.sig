signature FRAME =
sig
  type frame
  type access
  val newFrame : {name: Temp.label,
                  formals: bool list} -> FRAME
  val name : frame -> Temp.label
  val formals: frame -> access List
  val allocLocal : frame -> bool -> access

end
