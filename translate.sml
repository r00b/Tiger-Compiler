signature TRANSLATE =
sig
  type level
  type access (* Not the same as Frame.access *)

  val outermost : level
  val newLevel : {parent: level, name: Temp.label
                  formals: bool list} -> level

  val formals: level -> access list
  val allocLocal: level -> bool -> access

structure Translate : TRANSLATE =
struct
  type exp = unit
end
