resource ResEpo = open Prelude in {

  param
    -- Parameters for nouns
    Number = Sg | Pl ;
    Case = Nom | Acc ;
    
  -- Define linearization categories here to allow easier overloading
  oper
    Noun = { s : Number => Case => Str } ;
    Verb = SS ;
    Adjective = SS ;
}
