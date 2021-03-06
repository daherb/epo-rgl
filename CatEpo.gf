--1 Cat: the Category System

-- The category system is central to the library in the sense
-- that the other modules ($Adjective$, $Adverb$, $Noun$, $Verb$ etc)
-- communicate through it. This means that a e.g. a function using
-- $NP$s in $Verb$ need not know how $NP$s are constructed in $Noun$:
-- it is enough that both $Verb$ and $Noun$ use the same type $NP$,
-- which is given here in $Cat$.
-- 
-- Some categories are inherited from [``Common`` Common.html].
-- The reason they are defined there is that they have the same
-- implementation in all languages in the resource (typically,
-- just a string). These categories are
-- $AdA, AdN, AdV, Adv, Ant, CAdv, IAdv, PConj, Phr$, 
-- $Pol, SC, Tense, Text, Utt, Voc, Interj$.
--
-- Moreover, the list categories $ListAdv, ListAP, ListNP, ListS$
-- are defined on $Conjunction$ and only used locally there.


concrete CatEpo of Cat = CommonX ** open ParamX, ResEpo, Prelude in {

  -- Most categories are defined in ResEpo and renamed here
  
  lincat

--2 Sentences and clauses

-- Constructed in [Sentence Sentence.html], and also in
-- [Idiom Idiom.html].

    S = SS ;        -- declarative sentence                e.g. "she lived here"
    QS = SS ;       -- question                            e.g. "where did she live"
    RS = SS ;       -- relative                            e.g. "in which she lived"
    Cl = SS ;       -- declarative clause, with all tenses e.g. "she looks at this"
    ClSlash = SS ;  -- clause missing NP (S/NP in GPSG)    e.g. "she looks at"
    SSlash = SS ;   -- sentence missing NP                 e.g. "she has looked at"
    Imp = SS ;      -- imperative                          e.g. "look at this"

--2 Questions and interrogatives

-- Constructed in [Question Question.html].

    QCl = SS ;      -- question clause, with all tenses    e.g. "why does she walk"
    IP = SS ;       -- interrogative pronoun               e.g. "who"
    IComp = SS ;    -- interrogative complement of copula  e.g. "where"
    IDet = SS ;     -- interrogative determiner            e.g. "how many"
    IQuant = SS ;   -- interrogative quantifier            e.g. "which"

--2 Relative clauses and pronouns

-- Constructed in [Relative Relative.html].

    RCl = SS ;      -- relative clause, with all tenses    e.g. "in which she lives"
    RP = SS ;       -- relative pronoun                    e.g. "in which"

--2 Verb phrases

-- Constructed in [Verb Verb.html].

    VP = SS ;       -- verb phrase                       e.g. "is very warm"
    Comp = SS ;     -- complement of copula, such as AP  e.g. "very warm"
    VPSlash = SS ;  -- verb phrase missing complement    e.g. "give to John"

--2 Adjectival phrases

-- Constructed in [Adjective Adjective.html].

    AP = SS ;       -- adjectival phrase                   e.g. "very warm"

--2 Nouns and noun phrases

-- Constructed in [Noun Noun.html]. 
-- Many atomic noun phrases e.g. "everybody"
-- are constructed in [Structural Structural.html].
-- The determiner structure is
-- ``` Predet (QuantSg | QuantPl Num) Ord
-- as defined in [Noun Noun.html].

    CN = CommonNoun ;       -- common noun (without determiner)    e.g. "red house"
    NP = NounPhrase ;       -- noun phrase (subject or object)     e.g. "the red house"
    Pron = NounPhrase ;     -- personal pronoun                    e.g. "she"
    Det = Determiner ;      -- determiner phrase                   e.g. "those seven"
    Predet = SS ;   -- predeterminer (prefixed Quant)      e.g. "all"
    Quant = SS ;    -- quantifier ('nucleus' of Det)       e.g. "this/these"
    Num = { s : Str ; n : Number } ;      -- number determining element          e.g. "seven"
    Card = SS ;     -- cardinal number                     e.g. "seven"
    ACard = SS ;    -- adjective like cardinal             e.g. "few", "many"
    Ord = Noun ;       -- ordinal number (used in Det)        e.g. "seventh"
    DAP = SS ;      -- determiner with adjective           e.g. "three small"

--2 Numerals

-- Constructed in [Numeral Numeral.html].

    Numeral = { s : Str ; ord : Noun } ;  -- cardinal or ordinal in words       e.g. "five/fifth"
    Digits = { s : Str ; ord : Noun } ;   -- cardinal or ordinal in digits      e.g. "1,000/1,000th"

--2 Structural words

-- Constructed in [Structural Structural.html].

    Conj = SS ;     -- conjunction                         e.g. "and"
---b    DConj = SS ; -- distributed conjunction             e.g. "both - and"
    Subj = SS ;     -- subjunction                         e.g. "if"
    Prep = SS ;     -- preposition, or just case           e.g. "in"

--2 Words of open classes

-- These are constructed in [Lexicon Lexicon.html] and in 
-- additional lexicon modules.

    V = Verb ;        -- one-place verb                      e.g. "sleep" 
    V2 = Verb ;       -- two-place verb                      e.g. "love"
    V3 = Verb ;       -- three-place verb                    e.g. "show"
    VV = Verb ;       -- verb-phrase-complement verb         e.g. "want"
    VS = Verb ;       -- sentence-complement verb            e.g. "claim"
    VQ = Verb ;       -- question-complement verb            e.g. "wonder"
    VA = Verb ;       -- adjective-complement verb           e.g. "look"
    V2V = Verb ;      -- verb with NP and V complement       e.g. "cause"
    V2S = Verb ;      -- verb with NP and S complement       e.g. "tell"
    V2Q = Verb ;      -- verb with NP and Q complement       e.g. "ask"
    V2A = Verb ;      -- verb with NP and AP complement      e.g. "paint"

    A = Noun ;        -- one-place adjective                 e.g. "warm"
    A2 = Noun ;       -- two-place adjective                 e.g. "divisible"

    N = Noun ;        -- common noun                         e.g. "house"
    N2 = Noun ;       -- relational noun                     e.g. "son"
    N3 = Noun ;       -- three-place relational noun         e.g. "connection"
    PN = Noun ;       -- proper name                         e.g. "Paris"

-- DEPRECATED: QuantSg, QuantPl
---    QuantSg ;-- quantifier ('nucleus' of sing. Det) e.g. "every"
---    QuantPl ;-- quantifier ('nucleus' of plur. Det) e.g. "many"

}
