{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell, FlexibleInstances #-}
module QQLang (x86) where
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import qualified X86 as X86

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Debug.Trace



{-
 - The purpose of this is to fashion a DSL for describing X86 instructions in a way
 - that is more conducive to literacy.  In other words, we want to be able to pattern
 - match on *actual written x86 assembly* in addition to assembly descriptors from the
 - opcode documentation.  Here are some examples:
 -
 - and eax, eax
 - and r/m8, imm8
 - add r/m16/32, r16/32
 -
 - eval [ins|add eax, eax|] = ...
 -}

lexer = P.makeTokenParser emptyDef 
identifier = P.identifier lexer
symbol = P.symbol lexer
natural = P.natural lexer
angles = P.angles lexer


insspec = do
            opc <- popcode
            ops <- operand' `sepBy` (symbol ",")
            let ops'  = case (length ops) of
                                  0 -> X86.None
                                  1 -> X86.OneOp $ head ops
                                  2 -> X86.TwoOp (head ops) (head $ tail ops)
                                  3 -> X86.ThreeOp (head ops) (head $ tail ops) (head $ tail $ tail ops) 
                                  _ -> error "Instruction specified with more than three operands"


            return $ X86.InsSpec opc ops'

operand' = do
              op <- operand
              lbl <- opspeclabel
              return $ op lbl
operand = do 
            try (specr8) <|> try (do reglit) <|> try (regmemspec) <|> try regspec  <|> immspec

specr8 = do
            symbol "r8"
            return $ X86.Reg (True, False, False, False)

popcode = do
            ident <- identifier
            case (X86.opMap ident) of
                    (Just x) -> return x
                    Nothing  -> fail "Invalid opcode"

regmemspec = do
               symbol "r/m"
               ns <- (try natural) `sepBy1` (symbol "/")
               return $ X86.RM (8 `elem` ns, 16 `elem` ns, 32 `elem` ns, 64 `elem` ns)
       
regspec = do
               symbol "r"
               ns <- (try natural) `sepBy1` (symbol "/")
               return $ X86.Reg (8 `elem` ns, 16 `elem` ns, 32 `elem` ns, 64 `elem` ns)


immspec = do
            symbol "imm"
            ns <- (try natural) `sepBy1` (symbol "/")
            return $ X86.Imm (8 `elem` ns, 16 `elem` ns, 32 `elem` ns, 64 `elem` ns)

opspeclabel = do
                try (do
                       pid <- angles identifier
                       return $ Just pid) <|> return Nothing
                
                
instruction = do
                oc <- popcode
                ops <- op `sepBy` (symbol ",")
                let ops'  = case (length ops) of
                                      0 -> X86.None
                                      1 -> X86.OneOp $ head ops
                                      2 -> X86.TwoOp (head ops) (head $ tail ops)
                                      3 -> X86.ThreeOp (head ops) (head $ tail ops) (head $ tail $ tail ops) 
                                      _ -> error "Instruction specified with more than three operands"
                return $ X86.Ins oc ops'

op = do
        try (do r <- reglit
                return $ rl2op $ r Nothing)
        <|> do n <- natural
               return $ X86.I n (n < 2^8, n < 2^16, n < 2^32, n < 2^64) 
    where
      rl2op (X86.RegLit reg sz m _) = X86.RegMem (X86.R reg) sz m
               

reglit = do            
                    try(do r <- symbol "al" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "ah" ;return $ X86.regMap r)  <|>
                      try(do r <- symbol "bl" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "bh" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "cl" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "ch" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "dl" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "dh" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "ax" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "bx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "cx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "dx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "bp" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "si" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "di" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "sp" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "eax" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "ebx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "ecx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "edx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "ebp" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "esi" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "edi" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "esp" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rax" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rbx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rcx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rdx" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rbp" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rsi" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rdi" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "rsp" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r8"  ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r9"  ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r10" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r11" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r12" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r13" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r14" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "r15" ;return $ X86.regMap r)


--See: http://www.haskell.org/haskellwiki/Quasiquotation, right above #3
parsePat :: Monad m => (String, Int, Int) -> String -> m X86.InsSpec
parsePat (file, line, col) s =
  case runParser p () "" s of
    Left  err -> fail $ show err
    Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- insspec 
            eof
            return e

parseExpr :: Monad m => (String, Int, Int) -> String -> m X86.Ins
parseExpr (file, line, col) s =
  case runParser p () "" s of
    Left  err -> fail $ show err
    Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- instruction
            eof
            return e

toPatQ x = [p| x |] 
toExpQ x = [| x |]

--Compiling Spec Patterns to a corresponding haskell pattern on Regular Instructions

boolCon True  = TH.wildP 
boolCon False = toPatQ False

bit8  = TH.tupP [TH.wildP, [p|False|], [p|False|], [p|False|]]
bit16 = TH.tupP [[p|False|], TH.wildP, [p|False|], [p|False|]]
bit32 = TH.tupP [[p|False|], [p|False|], TH.wildP, [p|False|]]
bit64 = TH.tupP [[p|False|], [p|False|], [p|False|], TH.wildP]

maskx (a,b,c,d) = TH.tupP [flip a, flip b, flip c, flip d]
    where
      flip False = [p|False|]
      flip True  = TH.wildP

cose (X86.RegLit X86.RAX X86.B8L m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RAX|]], [|X86.B8L|], [|m|]]
cose (X86.RegLit X86.RAX X86.B8H m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RAX|]], [|X86.B8H|], [|m|]]
cose (X86.RegLit X86.RBX X86.B8L m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBX|]], [|X86.B8L|], [|m|]]
cose (X86.RegLit X86.RBX X86.B8H m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBX|]], [|X86.B8H|], [|m|]]
cose (X86.RegLit X86.RCX X86.B8L m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RCX|]], [|X86.B8L|], [|m|]]
cose (X86.RegLit X86.RCX X86.B8H m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RCX|]], [|X86.B8H|], [|m|]]
cose (X86.RegLit X86.RDX X86.B8L m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDX|]], [|X86.B8L|], [|m|]]
cose (X86.RegLit X86.RDX X86.B8H m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDX|]], [|X86.B8H|], [|m|]]
cose (X86.RegLit X86.RAX X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RAX|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RBX X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBX|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RCX X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RCX|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RDX X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDX|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RBP X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBP|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RSI X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RSI|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RDI X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDI|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RSP X86.B16 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RSP|]], [|X86.B16|], [|m|]]
cose (X86.RegLit X86.RAX X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RAX|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RBX X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBX|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RCX X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RCX|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RDX X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDX|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RBP X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBP|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RSI X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RSI|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RDI X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDI|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RSP X86.B32 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RSP|]], [|X86.B32|], [|m|]]
cose (X86.RegLit X86.RAX X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RAX|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.RBX X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBX|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.RCX X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RCX|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.RDX X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDX|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.RBP X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RBP|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.RSI X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RSI|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.RDI X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RDI|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.RSP X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.RSP|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R8  X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R8 |]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R9  X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R9 |]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R10 X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R10|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R11 X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R11|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R12 X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R12|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R13 X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R13|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R14 X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R14|]], [|X86.B64|], [|m|]]
cose (X86.RegLit X86.R15 X86.B64 m _) = TH.appsE [TH.conE (TH.mkName "X86.RegMem"), TH.appsE [TH.conE (TH.mkName "X86.R"), [|X86.R15|]], [|X86.B64|], [|m|]]


--cosp: RegLits
cosp (X86.RegLit X86.RAX X86.B8L _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RAX|]]), [p|X86.B8L|], bit8]
cosp (X86.RegLit X86.RAX X86.B8H _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RAX|]]), [p|X86.B8H|], bit8]
cosp (X86.RegLit X86.RBX X86.B8L _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBX|]]), [p|X86.B8L|], bit8]
cosp (X86.RegLit X86.RBX X86.B8H _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBX|]]), [p|X86.B8H|], bit8]
cosp (X86.RegLit X86.RCX X86.B8L _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RCX|]]), [p|X86.B8L|], bit8]
cosp (X86.RegLit X86.RCX X86.B8H _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RCX|]]), [p|X86.B8H|], bit8]
cosp (X86.RegLit X86.RDX X86.B8L _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDX|]]), [p|X86.B8L|], bit8]
cosp (X86.RegLit X86.RDX X86.B8H _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDX|]]), [p|X86.B8H|], bit8]
cosp (X86.RegLit X86.RAX X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RAX|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RBX X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBX|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RCX X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RCX|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RDX X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDX|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RBP X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBP|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RSI X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RSI|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RDI X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDI|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RSP X86.B16 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RSP|]]), [p|X86.B16|], bit16]
cosp (X86.RegLit X86.RAX X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RAX|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RBX X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBX|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RCX X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RCX|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RDX X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDX|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RBP X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBP|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RSI X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RSI|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RDI X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDI|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RSP X86.B32 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RSP|]]), [p|X86.B32|], bit32]
cosp (X86.RegLit X86.RAX X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RAX|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.RBX X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBX|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.RCX X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RCX|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.RDX X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDX|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.RBP X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RBP|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.RSI X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RSI|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.RDI X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RDI|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.RSP X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.RSP|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R8  X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R8 |]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R9  X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R9 |]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R10 X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R10|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R11 X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R11|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R12 X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R12|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R13 X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R13|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R14 X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R14|]]), [p|X86.B64|], bit64]
cosp (X86.RegLit X86.R15 X86.B64 _ _) = TH.conP (TH.mkName "X86.RegMem") [(TH.conP (TH.mkName "X86.R") [[p|X86.R15|]]), [p|X86.B64|], bit64]
--cosp Registers
cosp (X86.Reg m _) = TH.conP (TH.mkName "X86.RegMem")
                           [
                             TH.conP (TH.mkName "X86.R") [(TH.wildP)],
                             TH.varP (TH.mkName "s"),
                             maskx m
                           ]
--cosp Registers/Memory
cosp (X86.RM m _) = TH.conP (TH.mkName "X86.RegMem")
                          [
                            TH.wildP, --(TH.mkName "rm"),
                            TH.wildP, --(TH.mkName "s"),
                            maskx m 
                          ]

cosp (X86.Imm m _) = TH.conP (TH.mkName "X86.I")
                             [
                              TH.wildP, --(TH.mkName "addr"),
                              maskimm m
                             ]
    where 
      maskimm (a,b,c,d) = TH.tupP [mflip a, mflip b, mflip c, mflip d]
      mflip True = [p|True|]
      mflip False = TH.wildP 


cosps X86.None = TH.conP (TH.mkName "X86.None") []
cosps (X86.OneOp x) = TH.conP (TH.mkName "X86.OneOp")
                              [
                                lblcosp x
                              ]
cosps (X86.TwoOp x y) = TH.conP (TH.mkName "X86.TwoOp")
                                [
                                  lblcosp x,
                                  lblcosp y
                                ]
cosps (X86.ThreeOp x y z) = TH.conP (TH.mkName "X86.ThreeOp")
                                  [
                                    lblcosp x,
                                    lblcosp y,
                                    lblcosp z
                                  ]

lblcosp operand = case (proptlbl operand) of
                            Nothing -> cosp operand
                            (Just l) -> TH.asP (TH.mkName l) $ cosp operand

proptlbl :: X86.OpSpec -> Maybe String
proptlbl (X86.RegLit _ _ _ l) = l
proptlbl (X86.Reg m l) = l
proptlbl (X86.RM _ l) = l
proptlbl (X86.Imm _ l) = l

compSpecPat :: X86.InsSpec -> Maybe (TH.Q TH.Pat)
compSpecPat (X86.InsSpec oc ops) = Just $ do 
                                              TH.conP (TH.mkName "X86.Ins") $
                                                      [
                                                        (X86.liftp oc),
                                                        cosps ops
                                                      ] 

quoteSpecPat :: String -> TH.PatQ
quoteSpecPat s = do 
                loc <- TH.location
                let pos = (TH.loc_filename loc,
                           fst (TH.loc_start loc),
                           snd (TH.loc_start loc))
                pat <- parsePat pos s
                dataToPatQ (const Nothing `extQ` compSpecPat) pat


quoteInsExpr :: String -> TH.ExpQ
quoteInsExpr s = do 
                loc <- TH.location
                let pos = (TH.loc_filename loc,
                           fst (TH.loc_start loc),
                           snd (TH.loc_start loc))
                pat <- parseExpr pos s
                dataToExpQ (const Nothing) pat
                
                

x86 = QuasiQuoter { quotePat = quoteSpecPat
                       , quoteExp = quoteInsExpr
                       , quoteType = error "Quote Type not defined."
                       , quoteDec  = error "Quote Dec not defined."
                     }
