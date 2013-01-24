--ghci QQLang.hs -XDeriveDataTypeable -XQuasiQuotes
module QQLang where
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import qualified X86 as X86

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote



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



insspec = do
            opc <- popcode
            ops <- many operand
            let ops'  = case (length ops) of
                                  0 -> X86.None
                                  1 -> X86.OneOp $ head ops
                                  2 -> X86.TwoOp (head ops) (head $ tail ops)
                                  3 -> X86.ThreeOp (head ops) (head $ tail ops) (head $ tail $ tail ops) 
                                  _ -> error "Instruction specified with more than three operands"


            return $ X86.InsSpec opc ops'
            
operand = do 
            try (do reglit) <|> try (regmemspec) <|> try regspec  <|> immspec

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
            n <- natural
            case (n) of
                 8  -> return $ X86.Imm X86.B8 (True, False, False, False)
                 16 -> return $ X86.Imm X86.B16 (False, True, False, False)
                 32 -> return $ X86.Imm X86.B32 (False, False, True, False)
                 _  -> fail "Invalid immediate width"

bit8  = (True,False,False,False)
bit16 = (False,True,False,False)
bit32 = (False,False,True,False)
bit64 = (False,False,False,True)

reglit = do            
                    try(do r <- symbol "al" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "ah" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "bl" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "bh" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "cl" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "ch" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "dl" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "dh" ;return $ X86.regMap r bit8) <|>
                      try(do r <- symbol "ax" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "bx" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "cx" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "dx" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "bp" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "si" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "di" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "sp" ;return $ X86.regMap r bit16) <|>
                      try(do r <- symbol "eax" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "ebx" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "ecx" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "edx" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "ebp" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "esi" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "edi" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "esp" ;return $ X86.regMap r bit32) <|>
                      try(do r <- symbol "rax" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "rbx" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "rcx" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "rdx" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "rbp" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "rsi" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "rdi" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "rsp" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r8"  ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r9"  ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r10" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r11" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r12" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r13" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r14" ;return $ X86.regMap r bit64) <|>
                      try(do r <- symbol "r15" ;return $ X86.regMap r bit64)


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

compSpecPat :: X86.InsSpec -> Maybe (TH.Q TH.Pat)
compSpecPat (X86.InsSpec x@X86.ADD X86.None) = Just $ TH.conP 
                                                            (TH.mkName "ADD") 
                                                            [TH.varP (TH.mkName "None")]

compSpecExpr :: X86.InsSpec -> Maybe (TH.Q TH.Exp)
compSpecExpr (X86.InsSpec X86.ADD X86.None) = Just $ TH.appE (TH.appE (TH.conE (TH.mkName "X86.Ins"))
                                                             (TH.conE (TH.mkName "X86.ADD")))
                                                             (TH.conE (TH.mkName "X86.None"))


quoteSpecPat :: String -> TH.PatQ
quoteSpecPat s = do 
                loc <- TH.location
                let pos = (TH.loc_filename loc,
                           fst (TH.loc_start loc),
                           snd (TH.loc_start loc))
                pat <- parsePat pos s
                dataToPatQ (const Nothing `extQ` compSpecPat) pat


quoteSpecExpr :: String -> TH.ExpQ
quoteSpecExpr s = do 
                loc <- TH.location
                let pos = (TH.loc_filename loc,
                           fst (TH.loc_start loc),
                           snd (TH.loc_start loc))
                pat <- parsePat pos s
                dataToExpQ (const Nothing `extQ` compSpecExpr) pat
                
                

cquote = QuasiQuoter { quotePat = quoteSpecPat
                       , quoteExp = quoteSpecExpr}
