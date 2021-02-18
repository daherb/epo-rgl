--1 Numerals

-- This grammar defines numerals from 1 to 999999. 
-- The implementations are adapted from the
-- [numerals library http://www.cs.chalmers.se/~aarne/GF/examples/numerals/] 
-- which defines numerals for 88 languages.
-- The resource grammar implementations add to this inflection (if needed)
-- and ordinal numbers.
--
-- *Note* 1. Number 1 as defined 
-- in the category $Numeral$ here should not be used in the formation of
-- noun phrases, and should therefore be removed. Instead, one should use
-- [Structural Structural.html]$.one_Quant$. This makes the grammar simpler
-- because we can assume that numbers form plural noun phrases.
--
-- *Note* 2. The implementations introduce spaces between
-- parts of a numeral, which is often incorrect - more work on
-- (un)lexing is needed to solve this problem.

concrete NumeralEpo of Numeral = CatEpo [Numeral,Digits] **
  open
    ParamX -- for Number
  , ResEpo -- for NumeralType
  , Prelude in {

  param
    NumType = NCard | NOrd ;
  lincat 
    Digit = { s : Str ; n : Number } ;      -- 2..9
    Sub10 = { s : NumType => Str ; n : Number ; isSub10 : Bool } ;       -- 1..9
    Sub100 = { s : NumType => Str ; n : Number ; isSub10 : Bool } ;      -- 1..99
    Sub1000 = { s : NumType => Str ; n : Number ; isSub10 : Bool } ;     -- 1..999
    Sub1000000 = { s : NumType => Str ; n : Number } ;  -- 1..999999

  lin
    -- num : Sub1000000 -> Numeral ; -- 123456 [coercion to top category]
    num n = { s = n.s ! NCard ; ord = mkNoun (n.s ! NOrd ++ BIND ++ "a") } ; -- putA t ; n = n.n } ;
    
    -- n2, n3, n4, n5, n6, n7, n8, n9 : Digit ;
    n2 = { s = "du" ; n = Pl } ;
    n3 = { s = "tri" ; n = Pl } ;
    n4 = { s = "kvar" ; n = Pl } ;
    n5 = { s = "kvin" ; n = Pl } ;
    n6 = { s = "ses" ; n = Pl } ;
    n7 = { s = "sep" ; n = Pl } ;
    n8 = { s = "ok" ; n = Pl } ;
    n9 = { s = "naŭ" ; n = Pl } ;
    
    -- pot01 : Sub10 ;                               -- 1
    pot01 = { s = \\_ => "unu" ; n = Sg ; isSub10 = True } ;
    -- pot0 : Digit -> Sub10 ;                       -- d * 1
    pot0 d = { s = \\t => d.s ; n = d.n ; isSub10 = False } ;
    -- pot110 : Sub100 ;                             -- 10
    pot110 = { s = \\t => "dek" ; n = Pl ; isSub10 = False } ;
    -- pot111 : Sub100 ;                             -- 11
    pot111 = { s = table { NCard => "dek unu" ; NOrd => "dek-unu" } ; n = Pl ; isSub10 = False } ;
    -- pot1to19 : Digit -> Sub100 ;                  -- 10 + d
    pot1to19 d = { s = table { NCard => "dek" ++ d.s ; NOrd => "dek-" ++ BIND ++ d.s } ; n = Pl ; isSub10 = False} ;
    -- pot0as1 : Sub10 -> Sub100 ;                   -- coercion of 1..9
    pot0as1 n = n ;
    -- pot1 : Digit -> Sub100 ;                      -- d * 10
    pot1 d = { s = \\t => putPlural d.n (d.s ++ BIND) ++ "dek"; n = Pl ; isSub10 = False } ;
    -- pot1plus : Digit -> Sub10 -> Sub100 ;         -- d * 10 + n
    pot1plus d n = { s = table { NCard => putPlural d.n (d.s ++ BIND) ++ "dek" ++ n.s ! NCard;
				 NOrd => putPlural d.n (d.s ++ BIND) ++ "dek-" ++ BIND ++ n.s ! NOrd} ;
		     n = Pl ; isSub10 = False } ;
    -- pot1as2 : Sub100 -> Sub1000 ;                 -- coercion of 1..99
    pot1as2 n = n ;
    -- pot2 : Sub10 -> Sub1000 ;                     -- m * 100
    pot2 m = { s = \\t => putPlural m.n (m.s ! NCard ++ BIND) ++ "cent" ; n = Pl ; isSub10 = False } ;
    -- pot2plus : Sub10 -> Sub100 -> Sub1000 ;       -- m * 100 + n
    pot2plus m n = { s = table { NCard => putPlural m.n (m.s ! NCard ++ BIND) ++ "cent" ++ n.s ! NCard ;
				 NOrd => putPlural m.n (m.s ! NOrd ++ BIND) ++ "cent-" ++ BIND ++ n.s ! NOrd} ;
		     n = Pl ; isSub10 = False } ;
    -- pot2as3 : Sub1000 -> Sub1000000 ;             -- coercion of 1..999
    pot2as3 n = n ;
    -- pot3 : Sub1000 -> Sub1000000 ;                -- m * 1000
    pot3 m = { s = \\t => putPlural m.n (m.s ! t ++ putBind m.isSub10) ++ "mil" ; n = Pl ; isSub10 = False } ;
    -- pot3plus : Sub1000 -> Sub1000 -> Sub1000000 ; -- m * 1000 + n
    pot3plus m n = { s = table { NCard => putPlural m.n (m.s ! NCard ++ case m.isSub10 of { True => "" ; False => BIND }) ++ "mil" ++ n.s ! NCard ;
				 NOrd => putPlural m.n (m.s ! NOrd ++ BIND) ++ "mil-" ++ BIND ++ n.s ! NOrd  } ;
		     n = Pl } ;
    
-- Numerals as sequences of digits have a separate, simpler grammar

  lincat
    Dig = SS ;  -- single digit 0..9

  lin
    -- IDig  : Dig -> Digits ;       -- 8
    IDig d =  { s = d.s ; ord = mkNoun (d.s ++ BIND ++ "-a") } ;
    -- IIDig : Dig -> Digits -> Digits ; -- 876
    IIDig d ds =  { s = d.s ++ BIND ++ ds.s ; ord = { s = \\n,c => d.s ++ BIND ++ ds.ord.s ! n ! c } } ;
    
    -- D_0, D_1, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9 : Dig ;
    D_0 = ss "0" ;
    D_1 = ss "1" ;
    D_2 = ss "2" ;
    D_3 = ss "3" ;
    D_4 = ss "4" ;
    D_5 = ss "5" ;
    D_6 = ss "6" ;
    D_7 = ss "7" ;
    D_8 = ss "8" ;
    D_9 = ss "9" ;

  oper
    -- only put a string if number is plural
    putPlural : Number -> Str -> Str =
      \n,s ->
      case n of {
	Sg => "" ;
	Pl => s
      } ;

    -- -- add the a suffix if it is an ordinal
    -- putA : NumType -> Str =
    --   \n ->
    --   case n of {
    -- 	NCard => "" ;
    -- 	NOrd =>  BIND ++ "a"
    --   } ;

    putBind : Bool -> Str =
      \b ->
      case b of { False => "" ; True => BIND } ;
}
