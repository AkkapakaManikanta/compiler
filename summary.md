#lab0 Preparation:
1. This is basically preparation lab where we need to create the repository for submitting all the labs.
2. During this we have first created a `README.md file`, a `CHANGELOG.md` file, `tc.sml` file and a `Makefile`.
3. CHANGELOG.md is for updating the labs finished and tc.sml and a make file was created for testing how to write a makefile and all these are for ensuring if the files are getting submitted on gitlab.

#lab1 Reverse-polish-compiler:
1. For this lab, a directory named `reverse-polish` is created and the files given by the sir where created inside the directory. 
2. The reverse-polish directory contains the compilation code for expression language.
3. The code works for the expressions involving integers and operators like plus, minus and multiplication.
4. In, this lab, the exploration of the files was done which gave the idea about the lexing and parsing phases.
5. This mainly uses a stack machine to push the intergers and operator.
6. After exploration of the all the files, the codes are extended to work for division and parenthesis, so now it works for division and parenthesis also.
7. The command `make` can be used to run the code in reverse-polish directory to create the executables.

#lab2 MIPS-AST ():

1. The abstract syntax tree for mips assembly program is done.
2. The directory target contains `mips.sml` file which as has the ast of mips and generation of mips code or printing of mips code.
3. The file has `MIPS structure` which has reg datatype, ('l, 't) inst datatype, ('l, 't) stmt datatype and prInst, prStmt, prtlst, prtreg functions. These functions are used to print the mips statements or instructions.
4. There are 64 registers available in `MIPS structure` including 32 floating point registers and a total of 139 instructions are available.
5. The MIPS structure can be used to print all the instructions required for mips assembly programs.
6. This is usually called as *pretty printing*.
7. There would be changes need to be done in this mips.sml for further labs. This mips.sml file in target directory is not distured for that purposes and those changes were made in the mips.sml in src folder which would be used for writing the compiler code.


#lab3 Language with Variables:
**the source language is tiger and the target language is MIPS**
1. In the directory src, there the following files:
	a. tigerAst.sml
	b. tigerExpr.grm
	c. tigerExpr.lex
	d. temp.sml
	e. mips.sml
	f. translate.sml
	g. ir.sml
	h. ec.mlb
	i. ec.sml
	j. test.tiger
	k. Makefile
2. The command `make` can be used to run the code/compiler.
3. Running the code generates few intermediate files, an executable file `ec` and `out.mips` file. This `ec` executable takes a testfile `test.tiger` as input and produces the `out.mips` file. All this was done using the `make` command.
4. The `test.tiger` is the input file which contains the `tiger` code and `out.mips` is the output file which contains the `mips` instruction of the input tiger code.
5. The code supports for the tiger instructions which has variable assignment and print statements.
6. Variables can be assigned with integer, mathematical expressions containing plus, minus, multiplication, division and parenthesis and other variables which are assigned.
7. The print statement can print intergers, the values of variables and instructions.
```	E.g., input tiger code:

	      x := 1
	      y := 1 + 2 * (x - 0)
	      print y / x
	      
	      the mips output code would be:
	      
	      		.data
			.text
			.globl main
	      main: 
			li $zero, 1
			li $v1, 1
			li $a2, 2
			addi $t0, $zero, 0
			li $t1, 0
			sub $a3, $t0, $t1
			mul $a1, $a2, $a3
			add $at, $v1, $a1
			addi $t3, $at, 0
			addi $t4, $zero, 0
			div $t2, $t3, $t4
			li $v0, 1
			addi $a0, $t2, 0
			syscall
```
8. `tigerAst.sml` would have `TigerAst` structure which has the abstract syntax tree of the tiger code.
9. The file `mips.sml` is added here which was created during lab2 and changes were made in it to serve the purpose where a `temp` type could be supported by it using string conversion.
10. the file `temp.sml` has Temp structure which has a function which can generate a temp each time when called.

**parsing**:
1. The parsing and lexing would be done by the `tigerExpr.grm` and `tigerExpr.lex`files.
2. The tigerExpr.grm would have the grammer and the tigerExpr.lex file will involve in making token.
3. The input tiger may also contain the empty lines and comments, these can be properly handled by these files.
4. We use `ML-lex` and `ML-yacc` for the parsing purpose.

**Code generation**:
1. The `ir.sml` has the structure IR which can capture the intermediate representation.
2. The code generation is done in the `IR structure` and `Translate structure` in ir.sml and translate.sml.
3. The register allocation follows the rule where it allocates all the registers one by one and if the registers are over then an exception is raised.
4. The temp should be converted to reg while before pretty printing to use MIPS.
5. The changes in mips were done to satisfy this purpose where `mapI` and `mapS` were introduced.
6. The tiger statements should be mapped with the mips statements for the code generation.
7. The printzero and printone variable are used to store the integers 0 and 1 which are the standard values for the regester v0 and a0.
8. The register v0 has to be 1 before printing. So registers v0 and a0 are given standard values and are not used in register allocation to avoid any discrepencies.

#lab4 For loop:
1. For loop is added to the tiger input and now the compiler can compiler for loop too.
2. For this we have done changes in the Translate Structure in translate.sml to support for loop.
3. An stmt Label is added in the mips file to print the label in the mips code.
4. A type label is declared in Temp structure in temp.sml and a function which produces a new temp each time when used just like temp.
5. The syntax for capturing the for loop in the input file for parsing is given in the .grm and .lex files in the project.
6. The instructions may or may not be separated by semi colon.
```
	E.g., input tiger code:
	      y := 8;
	      #comment
	      for x = 0 to 5 
	      do     #comment
	      print x + y   
              done;     #comment

	      x := 8;
              #comment
              
              the output mips code:
              		.data
			.text
			.globl main
	      main: 
			li $zero, 8
			li $at, 0
	      0: 
			addi $a1, $at, 0
			addi $a2, $zero, 0
			add $v1, $a1, $a2
			li $v0, 1
			addi $a0, $v1, 0
			syscall
			addi $at, $at, 1
			ble $at, $a1, 0
			li $a3, 8

```
7. The codes can be runned using `make` command.

#lab5 tree ir:
1. Added tree signature

#lab6 Basic Blocks:
1. The main structure and functor used for basic block is done.
2. A basic block is a maximal block in a control flow graph.
3. This lab's focus is to find a basic block where the control flow graph is given as a list of instructions (labeled would be better).
4. Given an instruction we need to know if it is an ordinary instruction or a control changing instruction like a jump.
5. For each instruction, we need to know if it has a label on it so that it can be a target of other jump.
6. The functor BasicBlocks is done and the rest is pending.
#lab7 Graphs for compilers:
1. the signature is written for graph.

*TODO*: 
#lab5 IR Tree:
1. We have to write another intermediate representation called the Tree IR. The code generation should be done by first converting the source high level language (tiger) to Tree IR followed by Tree IR to MIPS.
*Semantic mismatch*
Typically there is significant difference
between the high level language and the machine language. The IR’s
    semantics is some where in between such that both  High-level to IR
    as well as IR to machine are easier. There can even be multiple layers
    of IR if the semantics of the high level language is much higher than
    the machine to which it is compiling to.
*Portability*
Targetting multiple processors becomes easier as we
    now have to give a translation from the IR to the new machine at
    hand. This would be significantly easier than directly converting
    from the high level language because IR is closer to the machine.
2. IRs also offer possibilities for machine independent optimisation.
3. Represent the Tree IR as an AST in the Standard ML. Tree IR canonisation. Chapter 8 in the book (Section 8.1). One of
    the need for canonisation is to make it easy to generate MIPS. The idea
    is to convert any Tree IR statement into a list of statements such that
   There is no ESEQ, SEQ statements Calls should not occur any where except when it occurs as the to  statement MOVE (CALL (f, args), TEMP t)
   
#lab6 Basic Blocks:
1. The Instmips should be done and canonisation should also be done

#lab7 Graphs for compilers:
1. To  represent a graph using an SML structure.



