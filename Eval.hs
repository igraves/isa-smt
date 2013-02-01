{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell, FlexibleInstances, NoMonomorphismRestriction #-}
module Eval where
import QQLang
import qualified X86 as X86
import qualified Language.Haskell.TH as TH
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.SBV

--Rename imports
mapRegLit = X86.mapRegLit
updateReg = X86.updateReg


--constants

initial :: Symbolic X86.System
initial = do
            rax <- sInt64 "rax"
            rbx <- sInt64 "rbx"
            rcx <- sInt64 "rcx"
            rdx <- sInt64 "rdx"
            rbp <- sInt64 "rbp"
            rsi <- sInt64 "rsi"
            rdi <- sInt64 "rdi"
            rsp <- sInt64 "rsp"
            r8  <- sInt64 "r8"
            r9  <- sInt64 "r9"
            r10 <- sInt64 "r10"
            r11 <- sInt64 "r11"
            r12 <- sInt64 "r12"
            r13 <- sInt64 "r13"
            r14 <- sInt64 "r14"
            r15 <- sInt64 "r15"
            return $ X86.System {
                           X86.srax = rax
                         , X86.srbx = rbx 
                         , X86.srcx = rcx 
                         , X86.srdx = rdx 
                         , X86.srbp = rbp 
                         , X86.srsi = rsi 
                         , X86.srdi = rdi 
                         , X86.srsp = rsp 
                         , X86.sr8  = r8
                         , X86.sr9  = r9 
                         , X86.sr10 = r10 
                         , X86.sr11 = r11 
                         , X86.sr12 = r12 
                         , X86.sr13 = r13 
                         , X86.sr14 = r14 
                         , X86.sr15 = r15 
                     }


--Monadic boilerplate
type Postcon = StateT X86.System Symbolic 
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
restwo (X86.RegMem (X86.R r) s _) (X86.RegMem (X86.R r') s' _) =  do
                                  sr <- getreg r
                                  sr' <- getreg r'
                                  return (sr .&. (rmask s), sr' .&. (rmask s'))

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

mov :: X86.Operand -> X86.Operand -> Postcon ()
mov o1 o2 = do
             (o1', o2') <- restwo o1 o2
             let o1'' = o2'
             putreg (prjreg o1) o1''
             


--The add instruction specifications
step [x86|add r/m8<o1>, r8<o2>|]           = add o1 o2
step [x86|add r/m16/32<o1>, r16/32<o2>|]   = add o1 o2
step [x86|add r8<o1>, r/m8<o2>|]           = add o1 o2 
step [x86|add r16/32<o1>, r/m16/32<o2>|]   = add o1 o2 
step [x86|add al<o1>, imm8<o2>|]           = add o1 o2 
step [x86|add ax<o1>, imm16<o2>|]          = add o1 o2 
step [x86|add eax<o1>, imm32<o2>|]         = add o1 o2 
step [x86|add r/m8<o1>, imm8<o2>|]         = add o1 o2 
step [x86|add r/m16/32<o1>, imm16/32<o2>|] = add o1 o2 
step [x86|add r/m8<o1>, imm8<o2>|]         = add o1 o2 
step [x86|add r/m16/32<o1>, imm8<o2>|]     = add o1 o2 
--The mov instruction specifications
step [x86|mov r/m8<o1>, r8<o2>|]                 = mov o1 o2
step [x86|mov r/m16/32/64<o1>, r16/32/64<o2>|]   = mov o1 o2
step [x86|mov r8<o1>, r/m8<o2>|]           = mov o1 o2
step [x86|mov r16/32/64<o1>, r/m16/32/64<o2>|] = mov o1 o2
step [x86|mov r8<o1>, imm8<o2>|]           = mov o1 o2
step [x86|mov r16/32/64<o1>, imm16/32/64<o2>|] = mov o1 o2
step [x86|mov r/m8<o1>, imm8<o2>|]          = mov o1 o2
step [x86|mov r/m16/32/64<o1>, imm16/32<o2>|] = mov o1 o2
step x = error $ "Step can't resolve instruction: " ++ (show x)


evalins :: [X86.Ins] -> Postcon ()
evalins xs = do
               mapM step xs
               return ()


test1 = [x86|mov eax, ebx|] : []
test2 = [x86|add eax, 0|] : [x86|mov eax, ebx|] : [] --[x86|add eax, eax|] : []


--runS :: Symbolic ()
runS = do
             i <- initial
             r <- execStateT (evalins test1) i
             s <- execStateT (evalins test2) i
             return $ (X86.srax r) .== (X86.srax s)

             {-
             return $ [
                          (X86.srax r) .== (X86.srax s)
                        , (X86.srbx r) .== (X86.srbx s)
                        , (X86.srcx r) .== (X86.srcx s)
                        , (X86.srdx r) .== (X86.srdx s)
                        , (X86.srbp r) .== (X86.srbp s)
                        , (X86.srsi r) .== (X86.srsi s)
                        , (X86.srdi r) .== (X86.srdi s)
                        , (X86.srsp r) .== (X86.srsp s)]-}


