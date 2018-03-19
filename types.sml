structure Types =
struct

  type unique = unit ref

  datatype ty =
            RECORD of (unit -> (Symbol.symbol * ty) list) * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of Symbol.symbol * ty option ref
          | UNIT
          | BOTTOM of string

  fun typeToString ty =
    case ty of
      RECORD(_) =>"record"
    | NIL =>  "record: nil"
    | INT => "int"
    | STRING => "string"
    | ARRAY(t,_) => "array of " ^ typeToString(t)
    | NAME(symbol,_) => "name of " ^ Symbol.name(symbol)
    | UNIT => "unit"
    | BOTTOM str => str
end
