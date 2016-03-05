module Tin where

import Control.Concurrent
import Control.Monad
import System.IO
import Control.Monad.Reader
import Data.List

import Text.PrettyPrint.HughesPJ

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type Name = String
type Value = Name
type Op = String
type Var = String

data Exp = EBinOP Exp Op Exp 
         | EInt Int
         | EBool Bool
         | EString String
         | EVar Var
         | EUnit
         | ERead
         | EPrint
         
data Stmt = SExp Exp
          | SReceive Name [Var]
          | SSend Name [Exp]
          | SWhile Exp Stmt
          | SIf Exp Stmt Stmt
          | SSeq Stmt Stmt
          | SSkip -- we don't parse this one it's just for internal use
