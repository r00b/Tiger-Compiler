structure Frame: FRAME = MipsFrame

signature TRANSLATE =
sig
  type exp

  type level
  type access (* not the same as Frame.access *)
  val outermost : level
  val newLevel : {parent: level, name: Temp.label,
                  formals: bool list} -> level

  (* val formals: level -> access list *)
  (* val allocLocal: level -> bool -> access *)

end

structure Translate : TRANSLATE =
struct

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

  datatype level = Out
                 | Lev of {frame: Frame.frame, parent: level} * unit ref

  val outermost = Out

  type access = level * Frame.access



  fun newLevel({parent,name,formals}): level =
      Lev({frame=Frame.newFrame({name=name,formals=true::formals}), (* cons true for static link *)
           parent=parent}, ref ())




end
