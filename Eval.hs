{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell, FlexibleInstances, NoMonomorphismRestriction #-}
module Eval where
import QQLang
import qualified X86 as X86
import qualified Language.Haskell.TH as TH
import Control.Monad.Trans.State
import Data.SBV

--Rename imports
mapRegLit = X86.mapRegLit
updateReg = X86.updateReg


--constants
{-
initial = X86.System {
                           X86.srax = sInt64 "foo"
                         , X86.srbx = sInt64 "foo"
                         , X86.srcx = sInt64 "foo"
                         , X86.srdx = sInt64 "foo"
                         , X86.srbp = sInt64 "foo"
                         , X86.srsi = sInt64 "foo"
                         , X86.srdi = sInt64 "foo"
                         , X86.srsp = sInt64 "foo"
                         , X86.sr8  = sInt64 "foo"
                         , X86.sr9  = sInt64 "foo"
                         , X86.sr10 = sInt64 "foo"
                         , X86.sr11 = sInt64 "foo"
                         , X86.sr12 = sInt64 "foo"
                         , X86.sr13 = sInt64 "foo"
                         , X86.sr14 = sInt64 "foo"
                         , X86.sr15 = sInt64 "foo"
                     }
-}

--Monadic boilerplate
type Postcon = StateT X86.System IO
getreg r = do
              s <- get
              return $ mapRegLit r s
putreg r v = do
              s <- get
              let s' = updateReg r v s
              put s'
--Operand resolution 
restwo :: X86.Operand -> X86.Operand -> Postcon (SInt64, SInt64) 
restwo (X86.RegMem (X86.R r) s _) (X86.I i _) = do
                              sr <- getreg r
                              return (sr .&. (rmask s), (fromIntegral i) .&. (rmask s))
--restwo (R r s _) (R r' s' _) = 

rmask X86.B8L = 0xFF
rmask X86.B8H = 0x00FF
rmask X86.B8  = error "B8 cannot be used in this context."
rmask X86.B16 = 0xFFFF
rmask X86.B32 = 0xFFFFFFFF
rmask X86.B64 = 0xFFFFFFFFFFFFFFFF
prjreg (X86.RegMem (X86.R r) _ _) = r

--The add instruction
add :: X86.Operand -> X86.Operand -> Postcon ()
add o1 o2 = do
             (o1',o2') <- restwo o1 o2
             let o1'' = o1' + o2'
             putreg (prjreg o1) o1''
             return ()

--The add instruction specifications
step [x86|add r/m8<o1>, r8<o2>|] = add o1 o2
step [x86|add r/m16/32<o1>, r16/32<o2>|] = add o1 o2
step [x86|add r8<o1>, r/m8<o2>|] = add o1 o2 
step [x86|add r16/32<o1>, r/m16/32<o2>|] = add o1 o2 
step [x86|add al<o1>, imm8<o2>|] = add o1 o2 
step [x86|add ax<o1>, imm16<o2>|] = add o1 o2 
step [x86|add eax<o1>, imm32<o2>|] = add o1 o2 
step [x86|add r/m8<o1>, imm8<o2>|] = add o1 o2 
step [x86|add r/m16/32<o1>, imm16/32<o2>|] = add o1 o2 
step [x86|add r/m8<o1>, imm8<o2>|] = add o1 o2 
step [x86|add r/m16/32<o1>, imm8<o2>|] = add o1 o2 


evalins :: [X86.Ins] -> Postcon ()
evalins xs = do
               mapM step xs
               return ()


test1 = [x86|add eax, eax|] : [x86|add eax, eax|] : []

run = do
         r <- execStateT (evalins test1)
         return ()


