module QQLang where
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import qualified X86 as X86
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

popcode = do
            ident <- identifier
            case (X86.opMap ident) of
                    (Just x) -> return x
                    Nothing  -> fail "Invalid opcode"
                

immspec = do
            symbol "imm"
            n <- natural
            case (n) of
                 8  -> return $ X86.Imm X86.B8
                 16 -> return $ X86.Imm X86.B16
                 32 -> return $ X86.Imm X86.B32
                 _  -> fail "Invalid immediate width"


regLit = do            
                    try(do r <- symbol "al" ;return $ X86.regMap r) <|>
                      try(do r <- symbol "ah" ;return $ X86.regMap r) <|>
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


