structure Types =
struct

  type unique = unit ref

  datatype ty =
            RECORD of (Symbol.symbol * ty) list * unique
          | RECORDF of (unit -> (Symbol.symbol * ty) list) * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of Symbol.symbol * ty option ref
          | UNIT

  fun tyToString ty =
    case ty of
      RECORD(_) =>"record"
    | RECORDF(_) =>"record"
    | NIL =>  "record: nil"
    | INT => "int"
    | STRING => "string"
    | ARRAY(_) =>  "array"
    | NAME(symbol, r) => Symbol.name symbol
    | UNIT => "unit"
end
