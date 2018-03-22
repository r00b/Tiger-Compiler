structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int
                  | InReg of Temp.temp

  type frame = {name: Temp.label,
                formals: bool list,
                accesses: access list,
                numLocals: int}

  type frame = {name: Temp.label, accessTypes: string list, formals: bool list}
  fun formals(f: frame) access list =

  fun newFrame {name: Temp.label, formals: bool list} =
 (* Because this “shift of view” depends on the calling conventions of the
 * target machine, it must be handled by the Frame module, starting with
 * newFrame. For each formal parameter, newFrame must calculate two things:
 *    1. How the parameter will be seen from inside the function
 *       (in a register, or in a frame location)
 *    2. What instructions must be produced to implement the “view shift.”*)

  fun allocLocal (f:frame) (b: bool): access = access

en
