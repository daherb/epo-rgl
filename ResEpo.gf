resource ResEpo = open
  Prelude
  , Predef -- for error
  , ParamX -- for Tense, Number
  in {

  param
    -- Parameters for nouns
    Case = Nom | Acc ;
    
  -- Define linearization categories here to allow easier overloading
  oper
    Noun = { s : Number => Case => Str } ;
    Verb = { s : Tense => Str } ;

    mkVerb : Str -> Verb = \v ->
      case v of {
	skrib + "i" =>
	  { s = table { Pres => skrib + "as" ;
			Past => skrib + "is" ;
			Fut  => skrib + "os" ;
			Cond => skrib + "us"
	      } ;
	  } ;
	_ => error ( "Not a verb: " + v)
      } ;

    mkNoun : Str -> Noun = \n ->
      { s = table {
	  Sg => table { Nom => n ; Acc => n + "n" } ;
	  Pl =>  table { Nom => n + "j" ; Acc => n + "jn" }
	  }
      } ;
      
    -- Function to put an prefix before a noun
    prefixNoun : Str -> Noun -> Noun = \p,n ->
      { s = \\num,cas => p + n.s ! num ! cas } ;

    -- Function to put an infix into a noun
    infixNoun : Noun -> Str -> Noun = \n,i ->
      case n.s ! Sg ! Nom of {
	vir + "o" => mkNoun ( vir + i + "o" ) ;
	varm + "a" => mkNoun (varm + i + "a" ) ;
	_ => error ( "Neither noun or adjective: " + n.s ! Sg ! Nom )
      } ;

  oper
    
    Determiner = { s : Str ;  n : Number } ;
    
    CommonNoun = {
      s : Number => Case => Str ;   -- the noun
      adj : Number => Case => Str ; -- list of adjective modifiers
      isAPre : Bool ;               -- marker if adjectives are before or after the noun
      adv : Str                     -- the adverb
      } ;
    NounPhrase = {
      s : Case => Str ;
      adj : Case => Str ;
      isAPre : Bool ;  -- see common noun
      adv : Str ;
      det : Str } ;
    
}
