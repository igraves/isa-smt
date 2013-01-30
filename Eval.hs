{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell, FlexibleInstances, NoMonomorphismRestriction #-}
module Eval where
import QQLang
import qualified X86 as X86
import qualified Language.Haskell.TH as TH


eval [x86|add r/m8<o1>, imm8<o2>|] = o2

