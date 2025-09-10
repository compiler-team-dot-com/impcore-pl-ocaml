{
  open Parser

  exception Error of string
}

rule read = parse
  | eof { EOF }