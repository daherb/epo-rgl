--1 Grammar: the Main Module of the Resource Grammar

-- This grammar is a collection of the different grammar modules,
-- To test the resource, import [``Lang`` Lang.html], which also contains
-- a lexicon.

concrete GrammarEpo of Grammar = 
  NounEpo
  , VerbEpo
  , AdjectiveEpo
  , AdverbEpo
  , NumeralEpo
  , SentenceEpo 
  , QuestionEpo
  , RelativeEpo
  , ConjunctionEpo
  , PhraseEpo
  , TextEpo
  , StructuralEpo
  , IdiomEpo
  , TenseEpo
--  , TransferEpo 
  ;

