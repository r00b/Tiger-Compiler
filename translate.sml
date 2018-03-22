structure Frame : FRAME = MipsFrame
signature TRANSLATE =
sig
  type level
  type access (* Not the same as Frame.access *)
  type exp

  val outermost : level
  val newLevel : {parent: level, name: Temp.label,
                  formals: bool list} -> level

  val formals: level -> access list
  val allocLocal: level -> bool -> access
end

structure Translate : TRANSLATE =
struct
  type level = int
  type access = level * Frame.access
  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
end
