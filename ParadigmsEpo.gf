--1 Latin Lexical Paradigms
--
-- This is an API for the user of the resource grammar 
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
-- 
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$.

resource ParadigmsEpo = open ResEpo, CatEpo, Prelude, Predef, ParamX in {

  oper
    mkN : (vorto : Str) -> N = \v -> lin N (mkNoun v) ;
    mkN2 : N -> N2 = \v -> lin N2 v ;
    mkN3 : N -> N3 = \v -> lin N3 v ;
    -- create a female noun from a male one by adding the infix "in"
    femaleN : N -> N = \n -> lin N (infixNoun n "in" ) ;

	 
    mkV : (skribi : Str) -> V = \v -> lin V (mkVerb v) ;
    mkVV : V -> VV = \v -> lin VV v ;
    mkVQ : V -> VQ = \v -> lin VQ v ;
    mkVS : V -> VS = \v -> lin VS v ;
    mkVA : V -> VA = \v -> lin VA v ;
    mkV2 : V -> V2 = \v -> lin V2 v ;
    mkV2Q : V -> V2Q = \v -> lin V2Q v ;
    mkV2S : V -> V2S = \v -> lin V2S v ;
    mkV2A : V -> V2A = \v -> lin V2A v ;
    mkV3 : V -> V3 = \v -> lin V3 v ;
    
    mkA : (bona : Str) -> A = \v -> lin A (mkNoun v) ;
    mkA2 : A -> A2 = \v -> lin A2 v ;

    augmentA : A -> A = \a -> lin A (infixNoun a "eg") ;

    mkOrd : (dekstra : Str) -> Ord = \v -> lin Ord (mkNoun v) ;
}
	      
