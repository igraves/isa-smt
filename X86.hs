module X86 where
import Data.SBV
import Data.Char

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

mapRegLit :: Register -> (System -> SInt64)
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

data Size = B8L | B8H | B8 | B16 | B32 | B64 deriving (Show, Eq)
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
                | R15 deriving (Show, Eq)

data Opcode =   ADD 
              | XOR 
              | MOV 

opMap x = case (map toLower x) of
                "add" -> Just ADD
                "xor" -> Just XOR
                "mov" -> Just MOV
                _     -> Nothing

data OpSpec =   RegLit Register Size 
               | Reg Size
               | RM Size 
               | Imm Size deriving (Show, Eq)

data RMDesc =  M Int
              | R Register deriving (Show, Eq)

data Operand =  I Int Size
               | RegMem RMDesc Size deriving (Show, Eq)
                

data Operands = None
                | OneOp Operand
                | TwoOp Operand Operand
                | ThreeOp Operand Operand Operand deriving (Show, Eq)

data Instruction = Ins Opcode Operands


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



al = (RegLit RAX B8L)
ah = (RegLit RAX B8H)
bl = (RegLit RBX B8L)
bh = (RegLit RBX B8H)
cl = (RegLit RCX B8L)
ch = (RegLit RCX B8H)
dl = (RegLit RDX B8L)
dh = (RegLit RDX B8H)

ax = (RegLit RAX B16)
bx = (RegLit RBX B16)
cx = (RegLit RCX B16)
dx = (RegLit RDX B16)
bp = (RegLit RBP B16)
si = (RegLit RSI B16)
di = (RegLit RDI B16)
sp = (RegLit RSP B16)

eax = (RegLit RAX B32)
ebx = (RegLit RBX B32)
ecx = (RegLit RCX B32)
edx = (RegLit RDX B32)
ebp = (RegLit RBP B32)
esi = (RegLit RSI B32)
edi = (RegLit RDI B32)
esp = (RegLit RSP B32)

rax = (RegLit RAX B64)
rbx = (RegLit RBX B64)
rcx = (RegLit RCX B64)
rdx = (RegLit RDX B64)
rbp = (RegLit RBP B64)
rsi = (RegLit RSI B64)
rdi = (RegLit RDI B64)
rsp = (RegLit RSP B64)
r8  = (RegLit R8 B64)
r9  = (RegLit R9 B64)
r10 = (RegLit R10 B64)
r11 = (RegLit R11 B64)
r12 = (RegLit R12 B64)
r13 = (RegLit R13 B64)
r14 = (RegLit R14 B64)
r15 = (RegLit R15 B64)
