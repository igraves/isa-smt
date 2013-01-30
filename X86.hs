{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell, FlexibleInstances #-}
module X86 where
import Data.SBV
import Data.Data
import Data.Typeable
import Data.Char
import qualified Language.Haskell.TH.Syntax as THS
import qualified Language.Haskell.TH as TH

data System = System {
                          srax :: SInt64
                        , srbx :: SInt64
                        , srcx :: SInt64
                        , srdx :: SInt64
                        , srbp :: SInt64
                        , srsi :: SInt64
                        , srdi :: SInt64
                        , srsp :: SInt64
                        , sr8  :: SInt64
                        , sr9  :: SInt64
                        , sr10 :: SInt64
                        , sr11 :: SInt64
                        , sr12 :: SInt64
                        , sr13 :: SInt64
                        , sr14 :: SInt64
                        , sr15 :: SInt64
                    }

mapRegLit :: Register -> (System -> Symbolic SInt64)
mapRegLit RAX = srax
mapRegLit RBX = srbx
mapRegLit RCX = srcx
mapRegLit RDX = srdx
mapRegLit RBP = srbp
mapRegLit RSI = srsi
mapRegLit RDI = srdi
mapRegLit RSP = srsp
mapRegLit R8  = sr8
mapRegLit R9  = sr9
mapRegLit R10 = sr10
mapRegLit R11 = sr11
mapRegLit R12 = sr12
mapRegLit R13 = sr13
mapRegLit R14 = sr14
mapRegLit R15 = sr15


updateReg :: Register -> Symbolic SInt64 -> System -> System
updateReg RAX v s = s {srax=v}
updateReg RBX v s = s {srbx=v}
updateReg RCX v s = s {srcx=v}
updateReg RDX v s = s {srdx=v}
updateReg RBP v s = s {srbp=v}
updateReg RSI v s = s {srsi=v}
updateReg RDI v s = s {srdi=v}
updateReg RSP v s = s {srsp=v}
updateReg R8  v s = s {sr8=v}
updateReg R9  v s = s {sr9=v}
updateReg R10 v s = s {sr10=v}
updateReg R11 v s = s {sr11=v}
updateReg R12 v s = s {sr12=v}
updateReg R13 v s = s {sr13=v}
updateReg R14 v s = s {sr14=v}
updateReg R15 v s = s {sr15=v}


data Size = B8L | B8H | B8 | B16 | B32 | B64 deriving (Data, Typeable, Show, Eq)

instance THS.Lift Size where
        lift B8L = [|B8L|]
        lift B8H = [|B8H|]
        lift B8  = [|B8|]
        lift B16 = [|B16|]
        lift B32 = [|B32|]
        lift B64 = [|B64|]

data Register = RAX
                | RBX
                | RCX
                | RDX
                | RBP
                | RSI
                | RDI
                | RSP
                | R8
                | R9
                | R10
                | R11
                | R12
                | R13
                | R14
                | R15 deriving (Data, Typeable, Show, Eq)

instance THS.Lift Register where
          lift RAX = [|RAX|]
          lift RBX = [|RBX|]
          lift RCX = [|RCX|]
          lift RDX = [|RDX|]
          lift RBP = [|RBP|]
          lift RSI = [|RSI|]
          lift RDI = [|RDI|]
          lift RSP = [|RSP|]
          lift R8  = [|R8 |]
          lift R9  = [|R9 |]
          lift R10 = [|R10|]
          lift R11 = [|R11|]
          lift R12 = [|R12|]
          lift R13 = [|R13|]
          lift R14 = [|R14|]
          lift R15 = [|R15|]



data Opcode =   ADD 
              | XOR 
              | MOV deriving (Data, Typeable, Show, Eq)

instance THS.Lift Opcode where
          lift ADD = [|ADD|]
          lift XOR = [|XOR|]
          lift MOV = [|MOV|]


liftp ADD = [p|ADD|]
liftp XOR = [p|XOR|]
liftp MOV = [p|MOV|]

instance THS.Lift OpSpec where
          lift (RegLit r s m _) = TH.appsE $
                                    [
                                      TH.conE $ TH.mkName "X86.RegLit",
                                      [|r|],
                                      [|s|],
                                      [|m|]
                                    ]
          lift (Reg m _) = TH.appsE $
                              [
                                TH.conE $ TH.mkName "X86.Reg",
                                [|m|]
                              ]
          lift (RM m _)  = TH.appsE $
                              [
                                TH.conE $ TH.mkName "X86.RM",
                                [|m|]
                              ]
          lift (Imm m _) = TH.appsE $
                              [
                                TH.conE $ TH.mkName "X86.Imm",
                                [|m|]
                              ]

instance THS.Lift (Operands OpSpec) where
          lift None = TH.conE $ TH.mkName "X86.None"
          lift (OneOp x) = TH.appsE $ 
                              [
                                TH.conE $ TH.mkName "X86.OneOp",
                                [|x|]
                              ]
          lift (TwoOp x y) = TH.appsE $
                              [
                                TH.conE $ TH.mkName "X86.TwoOp",
                                [|x|],
                                [|y|]
                              ]
          lift (ThreeOp x y z) = TH.appsE $
                              [
                                TH.conE $ TH.mkName "X86.ThreeOp",
                                [|x|],
                                [|y|],
                                [|z|]
                              ]
                        
opMap x = case (map toLower x) of
                "add" -> Just ADD
                "xor" -> Just XOR
                "mov" -> Just MOV
                _     -> Nothing

type Is8  = Bool
type Is16 = Bool
type Is32 = Bool
type Is64 = Bool

type Mask = (Is8,Is16,Is32,Is64)
type Label = String

data OpSpec =    RegLit Register Size Mask (Maybe Label)
               | Reg Mask (Maybe Label)
               | RM Mask (Maybe Label)
               | Imm Mask (Maybe Label) deriving (Data, Typeable, Show, Eq)

data RMDesc =  M Int
              | R Register deriving (Data, Typeable, Show, Eq)

data Operand =  I Integer Mask
               | RegMem RMDesc Size Mask deriving (Data, Typeable, Show, Eq)
                

data Operands a = None
                | OneOp a
                | TwoOp a a
                | ThreeOp a a a deriving (Typeable, Data, Show, Eq)


data InsSpec = InsSpec Opcode (Operands OpSpec) deriving (Typeable, Data, Show, Eq)
data Ins = Ins Opcode (Operands Operand) deriving (Data, Typeable, Show, Eq)



{-
regWrite :: Register -> System -> (SInt64 -> System)
regWrite RAX sys = \i -> sys {srax=i}
regWrite RBX sys = \i -> sys {srbx=i}
regWrite RCX sys = \i -> sys {srcx=i}
regWrite RDX sys = \i -> sys {srdx=i}
regWrite RBP sys = \i -> sys {srbp=i}
regWrite RSI sys = \i -> sys {srsi=i}
regWrite RDI sys = \i -> sys {srdi=i}
regWrite RSP sys = \i -> sys {srsp=i}
regWrite R8  sys = \i -> sys {sr8=i}
regWrite R9  sys = \i -> sys {sr9=i}
regWrite R10 sys = \i -> sys {sr10=i}
regWrite R11 sys = \i -> sys {sr11=i}
regWrite R12 sys = \i -> sys {sr12=i}
regWrite R13 sys = \i -> sys {sr13=i}
regWrite R14 sys = \i -> sys {sr14=i}
regWrite R15 sys = \i -> sys {sr15=i}

maskOp (RegLit r B8L) swrd  = swrd .&. 0xFF
maskOp (RegLit r B8H) swrd  = swrd .&. 0x00FF
maskOp (RegLit r B16) swrd  = swrd .&. 0xFFFF
maskOp (RegLit r B32) swrd  = swrd .&. 0xFFFFFFFF
maskOp (RegLit r B64) swrd  = swrd 

writeRegLit reg@(RegLit r s) swrd sys = let swrd' = maskOp reg swrd
                                            update = regWrite r sys
                                         in update swrd'

-}
regMap x = case (map toLower x) of
                    "al" ->  al
                    "ah" ->  ah
                    "bl" ->  bl
                    "bh" ->  bh
                    "cl" ->  cl
                    "ch" ->  ch
                    "dl" ->  dl
                    "dh" ->  dh
                    "ax" ->  ax
                    "bx" ->  bx
                    "cx" ->  cx
                    "dx" ->  dx
                    "bp" ->  bp
                    "si" ->  si
                    "di" ->  di
                    "sp" ->  sp
                    "eax" ->  eax
                    "ebx" ->  ebx
                    "ecx" ->  ecx
                    "edx" ->  edx
                    "ebp" ->  ebp
                    "esi" ->  esi
                    "edi" ->  edi
                    "esp" ->  esp
                    "rax" ->  rax
                    "rbx" ->  rbx
                    "rcx" ->  rcx
                    "rdx" ->  rdx
                    "rbp" ->  rbp
                    "rsi" ->  rsi
                    "rdi" ->  rdi
                    "rsp" ->  rsp
                    "r8"  ->  r8
                    "r9"  ->  r9
                    "r10" ->  r10
                    "r11" ->  r11
                    "r12" ->  r12
                    "r13" ->  r13
                    "r14" ->  r14
                    "r15" ->  r15

bit8 = (True,False,False,False)
bit16 = (False,True,False,False)
bit32 = (False,False,True,False)
bit64 = (False,False,False,True)

al = (RegLit RAX B8L) bit8
ah = (RegLit RAX B8H) bit8
bl = (RegLit RBX B8L) bit8
bh = (RegLit RBX B8H) bit8
cl = (RegLit RCX B8L) bit8
ch = (RegLit RCX B8H) bit8
dl = (RegLit RDX B8L) bit8
dh = (RegLit RDX B8H) bit8

ax = (RegLit RAX B16) bit16
bx = (RegLit RBX B16) bit16
cx = (RegLit RCX B16) bit16
dx = (RegLit RDX B16) bit16
bp = (RegLit RBP B16) bit16
si = (RegLit RSI B16) bit16
di = (RegLit RDI B16) bit16
sp = (RegLit RSP B16) bit16

eax = (RegLit RAX B32) bit32
ebx = (RegLit RBX B32) bit32
ecx = (RegLit RCX B32) bit32
edx = (RegLit RDX B32) bit32
ebp = (RegLit RBP B32) bit32
esi = (RegLit RSI B32) bit32
edi = (RegLit RDI B32) bit32
esp = (RegLit RSP B32) bit32

rax = (RegLit RAX B64) bit64
rbx = (RegLit RBX B64) bit64
rcx = (RegLit RCX B64) bit64
rdx = (RegLit RDX B64) bit64
rbp = (RegLit RBP B64) bit64
rsi = (RegLit RSI B64) bit64
rdi = (RegLit RDI B64) bit64
rsp = (RegLit RSP B64) bit64
r8  = (RegLit R8  B64) bit64
r9  = (RegLit R9  B64) bit64
r10 = (RegLit R10 B64) bit64
r11 = (RegLit R11 B64) bit64
r12 = (RegLit R12 B64) bit64
r13 = (RegLit R13 B64) bit64
r14 = (RegLit R14 B64) bit64
r15 = (RegLit R15 B64) bit64

