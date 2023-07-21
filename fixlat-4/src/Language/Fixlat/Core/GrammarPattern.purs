module Language.Fixlat.Core.GrammarPattern where

import Language.Fixlat.Core.Grammar
import Prelude

import Control.Bug (bug)
import Hole (hole)

-- matchTerm {application} (ApplicationTerm name args lat) = application name args lat
-- matchTerm {zero} (ConstructorTerm  [] lat) = zero lat
-- matchTerm {suc} (ConstructorTerm (SucConstructor) [n] lat) = suc n lat
-- matchTerm {infinity} (ConstructorTerm (InfinityConstructor) [] lat) = infinity lat
-- matchTerm {tuple} (ConstructorTerm (TupleConstructor) [x, y] lat) = tuple x y lat
-- matchTerm {string} (ConstructorTerm (StringConstructor s) [] lat) = string s lat
-- matchTerm {zeta} (ConstructorTerm (ZetaConstructor) [] lat) = zeta lat
-- matchTerm {bool} (ConstructorTerm (BoolConstructor b) [] lat) = bool b lat
-- matchTerm {set} (ConstructorTerm (SetConstructor) xs lat) = set xs lat
-- matchTerm {domain} (ConstructorTerm (DomainConstructor) [] lat) = domain lat
-- matchTerm {quant} (QuantTerm x lat) = quant x lat
-- matchTerm {bound} (BoundTerm name lat) = bound name lat
-- matchTerm _ term = bug $ "Invalid LatticeTerm: " <> show term

-- matchConcreteTerms 