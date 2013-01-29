{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell, FlexibleInstances, NoMonomorphismRestriction #-}
module Eval where
import QQLang
import X86
import qualified Language.Haskell.TH as TH


eval [x86|add r/m8<o1>, r/m8<o2>|] = o1

