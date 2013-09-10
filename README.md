A Toy x86 Code Comparison Tool in Haskell
=========================================

This is a toy/proof of concept tool written to compare sequences of x86 instructions for equality and potentially other things.  This work is an offshoot of the efforts of investigating ROP Gadget provenance with the Center for High Assurance Computing at the University of Missouri.  Emphasis was given to enabling easy specification of instructions similar to the way they appear in x86 instruction specifications like those at [http://ref.x86asm.net/](http://ref.x86asm.net/).  The tool makes use of the [Haskell SBV library](http://hackage.haskell.org/package/sbv) by Levent Erkok for demonstrating code comparison.

The tool requires that the Yices SMT solver be installed on your machine and present in the PATH to run.  If you wish to use another SMT solver supported by SBV, changing the import declarations accordingly should work.  

This tool is a proof of concept and should be treated as such if put to any real use.  
