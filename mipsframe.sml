structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int
                  | InReg of Temp.temp

  type frame = {name: Temp.label,
                formals: bool list,
                accesses: access list,
                numLocals: int ref}

  fun formals(f: frame): access list = #accesses f

  fun allocLocal (f:frame) false: access = InReg(Temp.newtemp())
    | allocLocal (f:frame) true: access = 
    let 
      fun calOffset (numLocals: int ref) = (numLocals := !numLocals + 1; (0 - 4 * !numLocals))
    in
      InFrame(calOffset (#numLocals f))
    end

  fun newFrame {name: Temp.label, formals: bool list} =
    let
      val emptyFrame = {name=name, formals=formals, accesses=[], numLocals=ref 0}
      val accesses = map (allocLocal emptyFrame) formals
    in
      {name=name, formals=formals, accesses=accesses, numLocals=(#numLocals emptyFrame)}
    end

  fun name (f:frame): Temp.label = #name f

  val wordSize = 4

end
