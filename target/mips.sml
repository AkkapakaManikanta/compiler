structure MIPS = struct

 (* The registers of the mips machine *)
 datatype reg =  zero
 	       | at
 	       | v0
 	       | v1
 	       | a0
 	       | a1
 	       | a2
 	       | a3
 	       | t0
 	       | t1
 	       | t2
 	       | t3
 	       | t4
 	       | t5
 	       | t6
 	       | t7
 	       | s0
 	       | s1
 	       | s2
 	       | s3
 	       | s4
 	       | s5
 	       | s6
 	       | s7
 	       | t8
 	       | t9
 	       | k0
 	       | k1
 	       | gp
 	       | sp
 	       | fp
 	       | ra
 	       | f0
 	       | f1
 	       | f2
 	       | f3
 	       | f4
 	       | f5
 	       | f6
 	       | f7
 	       | f8
 	       | f9
 	       | f10
 	       | f11
 	       | f12
 	       | f13
 	       | f14
 	       | f15
 	       | f16
 	       | f17
 	       | f18
 	       | f19
 	       | f20
 	       | f21
 	       | f22
 	       | f23
 	       | f24
 	       | f25
 	       | f26
 	       | f27
 	       | f28
 	       | f29
 	       | f30
 	       | f31 
 	       


 (* The instruction *)
 datatype  ('l,'t) inst = add of 't * 't * 't (* add r1 r2 r3   r1 <- r2 + r3  *)
 			  | addi of 't * 't * int
 			  | abs of 't * 't
 			  | addu of 't * 't * 't
 			  | addui of 't * 't * int
 			  | AND of 't * 't * 't
 			  | andi of 't * 't * int
 			  | DIV of 't * 't
 			  | DIVU of 't * 't
 			  | Div of 't * 't * 't
 			  | Divu of 't * 't * 't
 			  | mul of 't * 't * 't
 			  | mulo of 't * 't * 't
 			  | mulou of 't * 't * 't
 			  | mult of 't * 't
 			  | multu of 't * 't
 			  | neg of 't * 't
 			  | negu of 't * 't
 			  | nor of 't * 't * 't
 			  | not of 't * 't
 			  | or of 't * 't * 't
 			  | ori of 't * 't * int
 			  | rem of 't * 't * 't
 			  | remu of 't * 't * 't
 			  | rol of 't * 't * 't
 			  | ror of 't * 't * 't
 			  | sll of 't * 't * 't
 			  | sllv of 't * 't * 't
 			  | sra of 't * 't * 't
 			  | srav of 't * 't * 't
 			  | srl of 't * 't * 't
 			  | srlv of 't * 't * 't
 			  | sub of 't * 't * 't
 			  | subu of 't * 't * 't
 			  | xor of 't * 't * 't
 			  | xori of 't * 't * int
 			  | li of 't * int
 			  | lui of 't * int
 			  | seq of 't * 't * 't
 			  | sge of 't * 't * 't
 			  | sgeu of 't * 't * 't
 			  | sgt of 't * 't * 't
 			  | sgtu of 't * 't * 't
 			  | sle of 't * 't * 't
 			  | sleu of 't * 't * 't
 			  | slt of 't * 't * 't
 			  | slti of 't * 't * int
 			  | sltu of 't * 't * 't
 			  | sltiu of 't * 't * int
 			  | sne of 't * 't * 't
 			  | B of 'l
 			  | bczt of 'l
 			  | bczf of 'l
 			  | beq of 't * 't * 'l
 			  | beqz of 't * 'l
 			  | bge of 't * 't * 'l
 			  | bgeu of 't * 't * 'l
 			  | bgez of 't * 'l
 			  | bgezal of 't * 'l
 			  | bgt of 't * 't * 'l
 			  | bgtu of 't * 't * 'l
 			  | bgtz of 't * 'l
 			  | ble of 't * 't * 'l
 			  | bleu of 't * 't * 'l
 			  | blez of 't * 'l
 			  | Bgezal of 't * 'l
 			  | bltzal of 't * 'l
 			  | blt of 't * 't * 'l
 			  | bltu of 't * 't * 'l
 			  | bltz of 't * 'l
 			  | bne of 't * 't * 'l
 			  | bnez of 't * 'l
 			  | j of 'l
 			  | jal of 'l
 			  | jalr of 't
 			  | jr of 't
 			  | la of 't * 'l
 			  | lb of 't * 'l
 			  | lbu of 't * 'l
 			  | ld of 't * 'l
 			  | lh of 't * 'l
 			  | lhu of 't * 'l
 			  | lw of 't * 'l
 			  | lwcz of 't * 'l
 			  | lwl of 't * 'l
 			  | lwr of 't * 'l
 			  | ulh of 't * 'l
 			  | ulhu of 't * 'l
 			  | ulw of 't * 'l
 			  | sb of 't * 'l
 			  | sd of 't * 'l
 			  | sh of 't * 'l
 			  | sw of 't * 'l
 			  | swcz of 't * 'l
 			  | swl of 't * 'l
 			  | swr of 't * 'l
 			  | ush of 't * 'l
 			  | usw of 't * 'l
 			  | move of 't * 't
 			  | mfhi of 't
 			  | mflo of 't
 			  | mthi of 't
 			  | mtlo of 't
 			  | mfcz of 't * 't
 			  | mfc1_d of 't * 't
 			  | mtcz of 't * 't
 			  | rfe
 			  | syscall
 			  | break of int
 			  | nop
 			  | abs_d of 't * 't
 			  | abs_s of 't * 't
 			  | add_d of 't * 't * 't
 			  | add_s of 't * 't * 't
 			  | c_eq_d of 't * 't
 			  | c_eq_s of 't * 't
 			  | c_le_d of 't * 't
 			  | c_le_s of 't * 't
 			  | c_lt_d of 't * 't
 			  | c_lt_s of 't * 't
 			  | cvt_d_s of 't * 't
 			  | cvt_d_w of 't * 't
 			  | cvt_s_d of 't * 't
 			  | cvt_s_w of 't * 't
 			  | cvt_w_d of 't * 't
 	 		  | cvt_w_s of 't * 't
 			  | div_d of 't * 't * 't
 			  | div_s of 't * 't * 't
 			  | l_d of 't * 'l
 			  | l_s of 't * 'l
 			  | mov_d of 't * 't
 			  | mov_s of 't * 't
 			  | mul_d of 't * 't * 't
 			  | mul_s of 't * 't * 't
 			  | neg_d of 't * 't
 			  | neg_s of 't * 't
 			  | s_d of 't * 't
 			  | s_s of 't * 't
 			  | sub_d of 't * 't * 't
 			  | sub_s of 't * 't * 't    
 	
 			  
 (* The instructions and assembler directives *)
 datatype ('l,'t) stmt = Inst of ('l,'t) inst
			 | ascii of string
			 | asciiz of string
			 | data of int
			 | text of int
			 | word of int list
			 | byte of int list
			 | float of int list
			 | double of int list
			 | extern of int * int
			 | globl of string
			 | half of int list
			 | kdata of int
			 | ktext of int
			 | space of int
			 | align of int

 (* Print the instructions when the labels are strings and
    registers are actual MIPS registers
 *)
 fun  prtreg zero = "$zero"
    | prtreg at   = "$at"
    | prtreg v0   = "$v0"
    | prtreg v1   = "$v1"
    | prtreg a0   = "$a0"
    | prtreg a1   = "$a1"
    | prtreg a2   = "$a2"
    | prtreg a3   = "$a3"
    | prtreg t0   = "$t0"
    | prtreg t1   = "$t1"
    | prtreg t2   = "$t2"
    | prtreg t3   = "$t3"
    | prtreg t4   = "$t4"
    | prtreg t5   = "$t5"
    | prtreg t6   = "$t6"
    | prtreg t7   = "$t7"
    | prtreg s0   = "$s0"
    | prtreg s1   = "$s1"
    | prtreg s2   = "$s2"
    | prtreg s3   = "$s3"
    | prtreg s4   = "$s4"
    | prtreg s5   = "$s5"
    | prtreg s6   = "$s6"
    | prtreg s7   = "$s7"
    | prtreg t8   = "$t8"
    | prtreg t9   = "$t9"
    | prtreg k0   = "$k0"
    | prtreg k1   = "$k1"
    | prtreg gp   = "$gp"
    | prtreg sp   = "$sp"
    | prtreg fp   = "$fp"
    | prtreg ra   = "$ra"
    | prtreg f0   = "$f0"
    | prtreg f1   = "$f1"
    | prtreg f2   = "$f2" 
    | prtreg f3   = "$f3" 
    | prtreg f4   = "$f4"
    | prtreg f5   = "$f5"
    | prtreg f6   = "$f6"
    | prtreg f7   = "$f7"
    | prtreg f8   = "$f8"
    | prtreg f9   = "$f9"
    | prtreg f10  = "$f10"
    | prtreg f11  = "$f11"
    | prtreg f12  = "$f12"
    | prtreg f13  = "$f13"
    | prtreg f14  = "$f14"
    | prtreg f15  = "$f15"
    | prtreg f16  = "$f16"
    | prtreg f17  = "$f17"
    | prtreg f18  = "$f18"
    | prtreg f19  = "$f19"
    | prtreg f20  = "$f20"
    | prtreg f21  = "$f21"
    | prtreg f22  = "$f22"
    | prtreg f23  = "$f23"
    | prtreg f24  = "$f24"
    | prtreg f25  = "$f25"
    | prtreg f26  = "$f26"
    | prtreg f27  = "$f27"
    | prtreg f28  = "$f28"
    | prtreg f29  = "$f29"
    | prtreg f30  = "$f30"
    | prtreg f31  = "$f31"
  
    
 fun prInst (abs(a, b) : (string, reg) inst) = "abs " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (add(a,b,c)) = "add " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (addi(a,b,c)) = "addi " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ Int.toString c
   | prInst (addu(a,b,c)) = "addu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (addui(a,b,c)) = "addui " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ Int.toString c
   | prInst (AND(a,b,c)) = "and " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (andi(a,b,c)) = "andi " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ Int.toString c
   | prInst (DIV(a,b)) = "div " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (DIVU(a,b)) = "divu " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (Div(a,b,c)) = "div " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (Divu(a,b,c)) = "divu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (mul(a,b,c)) = "mul " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (mulo(a,b,c)) = "mulo " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (mulou(a,b,c)) = "mulou " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (mult(a,b)) = "mult " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (multu(a,b)) = "multu " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (neg(a,b)) = "neg " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (negu(a,b)) = "negu " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (nor(a,b,c)) = "nor " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (not(a,b)) = "not " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (or(a,b,c)) = "or " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (ori(a,b,c)) = "ori " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ Int.toString c
   | prInst (rem(a,b,c)) = "rem " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (remu(a,b,c)) = "remu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (rol(a,b,c)) = "rol " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (ror(a,b,c)) = "ror " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sll(a,b,c)) = "sll " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sllv(a,b,c)) = "sllv " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sra(a,b,c)) = "sra " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (srav(a,b,c)) = "srav " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (srl(a,b,c)) = "srl " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (srlv(a,b,c)) = "srlv " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sub(a,b,c)) = "sub " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (subu(a,b,c)) = "subu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (xor(a,b,c)) = "xor " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (xori(a,b,c)) = "xori " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ Int.toString c
   | prInst (li(a,b)) = "li " ^ prtreg a ^ ", " ^ Int.toString b 
   | prInst (lui(a,b)) = "lui " ^ prtreg a ^ ", " ^ Int.toString b 
   | prInst (seq(a,b,c)) = "seq " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sge(a,b,c)) = "sge " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sgeu(a,b,c)) = "sgeu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sgt(a,b,c)) = "sgt " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sgtu(a,b,c)) = "sgtu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sle(a,b,c)) = "sle " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sleu(a,b,c)) = "sleu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (slt(a,b,c)) = "slt " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (slti(a,b,c)) = "slti " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ Int.toString c
   | prInst (sltu(a,b,c)) = "sltu " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sltiu(a,b,c)) = "sltiu " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ Int.toString c
   | prInst (sne(a,b,c)) = "sne " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (B(a)) = "b " ^ a 
   | prInst (bczt(a)) = "bczt " ^ a 
   | prInst (bczf(a)) = "bczf " ^ a 
   | prInst (beq(a,b,c)) = "beq " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (beqz(a,b)) = "beqz " ^ prtreg a ^ ", " ^ b 
   | prInst (bge(a,b,c)) = "bge " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bgeu(a,b,c)) = "bgeu " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bgez(a,b)) = "bgez " ^ prtreg a ^ ", " ^ b 
   | prInst (bgezal(a,b)) = "bgezal " ^ prtreg a ^ ", " ^ b 
   | prInst (bgt(a,b,c)) = "bgt " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bgtu(a,b,c)) = "bgtu " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bgtz(a,b)) = "bgtz" ^ prtreg a ^ ", " ^ b 
   | prInst (ble(a,b,c)) = "ble " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bleu(a,b,c)) = "bleu " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (blez(a,b)) = "blez" ^ prtreg a ^ ", " ^ b 
   | prInst (Bgezal(a,b)) = "bgezal " ^ prtreg a ^ ", " ^ b 
   | prInst (bltzal(a,b)) = "bltzal " ^ prtreg a ^ ", " ^ b 
   | prInst (blt(a,b,c)) = "blt " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bltu(a,b,c)) = "bltu " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bltz(a,b)) = "bltz " ^ prtreg a ^ ", " ^ b 
   | prInst (bne(a,b,c)) = "bne " ^ prtreg a ^ ", " ^ prtreg b ^ ", " ^ c
   | prInst (bnez(a,b)) = "bnez " ^ prtreg a ^ ", " ^ b 
   | prInst (j(a)) = "j " ^ a 
   | prInst (jal(a)) = "jal " ^ a 
   | prInst (jalr(a)) = "jalr " ^ prtreg a
   | prInst (jr(a)) = "jr " ^ prtreg a
   | prInst (la(a,b)) = "la " ^ prtreg a ^ ", " ^ b 
   | prInst (lb(a,b)) = "lb " ^ prtreg a ^ ", " ^ b 
   | prInst (lbu(a,b)) = "lbu " ^ prtreg a ^ ", " ^ b 
   | prInst (ld(a,b)) = "ld " ^ prtreg a ^ ", " ^ b 
   | prInst (lh(a,b)) = "lh " ^ prtreg a ^ ", " ^ b 
   | prInst (lhu(a,b)) = "lhu " ^ prtreg a ^ ", " ^ b 
   | prInst (lw(a,b)) = "lw " ^ prtreg a ^ ", " ^ b 
   | prInst (lwcz(a,b)) = "lwcz " ^ prtreg a ^ ", " ^ b 
   | prInst (lwl(a,b)) = "lwl " ^ prtreg a ^ ", " ^ b 
   | prInst (lwr(a,b)) = "lwr " ^ prtreg a ^ ", " ^ b 
   | prInst (ulh(a,b)) = "ulh " ^ prtreg a ^ ", " ^ b 
   | prInst (ulhu(a,b)) = "ulhu " ^ prtreg a ^ ", " ^ b 
   | prInst (ulw(a,b)) = "ulw " ^ prtreg a ^ ", " ^ b 
   | prInst (sb(a,b)) = "sb " ^ prtreg a ^ ", " ^ b 
   | prInst (sd(a,b)) = "sd " ^ prtreg a ^ ", " ^ b 
   | prInst (sh(a,b)) = "sh " ^ prtreg a ^ ", " ^ b 
   | prInst (sw(a,b)) = "sw " ^ prtreg a ^ ", " ^ b 
   | prInst (swcz(a,b)) = "swcz " ^ prtreg a ^ ", " ^ b 
   | prInst (swl(a,b)) = "swl  " ^ prtreg a ^ ", " ^ b 
   | prInst (swr(a,b)) = "swr " ^ prtreg a ^ ", " ^ b 
   | prInst (ush(a,b)) = "ush " ^ prtreg a ^ ", " ^ b 
   | prInst (usw(a,b)) = "usw " ^ prtreg a ^ ", " ^ b 
   | prInst (move(a,b)) = "move " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (mfhi(a)) = "mfhi " ^ prtreg a
   | prInst (mflo(a)) = "mflo " ^ prtreg a
   | prInst (mthi(a)) = "mthi " ^ prtreg a
   | prInst (mtlo(a)) = "mtlo " ^ prtreg a
   | prInst (mfcz(a,b)) = "mfcz " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (mfc1_d(a,b)) = "mfc1.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (mtcz(a,b)) = "mtcz " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (rfe) = "rfe" 
   | prInst (syscall) = "syscall" 
   | prInst (break(a)) = "break " ^ Int.toString a
   | prInst (nop) = "nop"
   | prInst (abs_d(a,b)) = "abs.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (abs_s(a,b)) = "abs.s" ^ prtreg a ^ ", " ^ prtreg b
   | prInst (add_d(a,b,c)) = "add.d " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (add_s(a,b,c)) = "add.s " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (c_eq_d(a,b)) = "c.eq.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (c_eq_s(a,b)) = "c.eq.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (c_le_d(a,b)) = "c.le.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (c_le_s(a,b)) = "c.le.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (c_lt_d(a,b)) = "c.lt.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (c_lt_s(a,b)) = "c.lt.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (cvt_d_s(a,b)) = "cvt.d.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (cvt_d_w(a,b)) = "cvt.d.w " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (cvt_s_d(a,b)) = "cvt.s.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (cvt_s_w(a,b)) = "cvt.s.w " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (cvt_w_d(a,b)) = "cvt.w.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (cvt_w_s(a,b)) = "cvt.w.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (div_d(a,b,c)) = "div.d " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (div_s(a,b,c)) = "div.s " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (l_d(a,b)) = "l.d " ^ prtreg a ^ ", " ^ b 
   | prInst (l_s(a,b)) = "l.s " ^ prtreg a ^ ", " ^ b 
   | prInst (mov_d(a,b)) = "mov.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (mov_s(a,b)) = "mov.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (mul_d(a,b,c)) = "mul.d " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (mul_s(a,b,c)) = "mul.s " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (neg_d(a,b)) = "neg.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (neg_s(a,b)) = "neg.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (s_d(a,b)) = "s.d " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (s_s(a,b)) = "s.s " ^ prtreg a ^ ", " ^ prtreg b
   | prInst (sub_d(a,b,c)) = "sub.d " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
   | prInst (sub_s(a,b,c)) = "sub.s " ^ prtreg a ^ ", " ^ prtreg b ^ ", "  ^ prtreg c
 
 fun prtlst (x :: xs) = Int.toString x ^ ", " ^ prtlst xs
   | prtlst     _     =                ""
   
 fun prStmt (Inst a)     = prInst a ^ "\n" 
   | prStmt (ascii a)    = ".ascii " ^ a
   | prStmt (asciiz a)   = ".asciiz " ^ a
   | prStmt (data a)     = ".data " ^ Int.toString a
   | prStmt (text a)     = ".text " ^ Int.toString a
   | prStmt (word a)     = ".word " ^ prtlst a
   | prStmt (byte a)     = ".byte " ^ prtlst a
   | prStmt (float a)    = ".float " ^ prtlst a 
   | prStmt (double a)   = ".double " ^ prtlst a
   | prStmt (extern(a,b)) = ".extern " ^ Int.toString a ^ " " ^ Int.toString b
   | prStmt (globl a)    = ".globl " ^ a
   | prStmt (half a)     = ".half " ^ prtlst a
   | prStmt (kdata a)    = ".kdada " ^ Int.toString a
   | prStmt (ktext a)    = ".ktext " ^ Int.toString a
   | prStmt (space a)    = ".space " ^ Int.toString a
   | prStmt (align a)    = ".align " ^  Int.toString a
   
 (* actual code that SPIM can understand is (string, reg) inst *)
 
 end
