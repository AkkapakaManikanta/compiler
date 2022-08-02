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
 			  | DIV of 't * 't           (*div2*)
 			  | DIVU of 't * 't          (*divu2*)
 			  | Div of 't * 't * 't      (*div3*)
 			  | Divu of 't * 't * 't      (*divu3*)
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
			 | ptext 
			 | pdata
			 | Label of string

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
  
    
 fun mapI toprint (abs(a, b)) = "abs " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (add(a,b,c)) = "add " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (addi(a,b,c)) = "addi " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ Int.toString c
   | mapI toprint (addu(a,b,c)) = "addu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (addui(a,b,c)) = "addui " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ Int.toString c
   | mapI toprint (AND(a,b,c)) = "and " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (andi(a,b,c)) = "andi " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ Int.toString c
   | mapI toprint (DIV(a,b)) = "div " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (DIVU(a,b)) = "divu " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (Div(a,b,c)) = "div " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (Divu(a,b,c)) = "divu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (mul(a,b,c)) = "mul " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (mulo(a,b,c)) = "mulo " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (mulou(a,b,c)) = "mulou " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (mult(a,b)) = "mult " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (multu(a,b)) = "multu " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (neg(a,b)) = "neg " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (negu(a,b)) = "negu " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (nor(a,b,c)) = "nor " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (not(a,b)) = "not " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (or(a,b,c)) = "or " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (ori(a,b,c)) = "ori " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ Int.toString c
   | mapI toprint (rem(a,b,c)) = "rem " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (remu(a,b,c)) = "remu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (rol(a,b,c)) = "rol " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (ror(a,b,c)) = "ror " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sll(a,b,c)) = "sll " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sllv(a,b,c)) = "sllv " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sra(a,b,c)) = "sra " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (srav(a,b,c)) = "srav " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (srl(a,b,c)) = "srl " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (srlv(a,b,c)) = "srlv " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sub(a,b,c)) = "sub " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (subu(a,b,c)) = "subu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (xor(a,b,c)) = "xor " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (xori(a,b,c)) = "xori " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ Int.toString c
   | mapI toprint (li(a,b)) = "li " ^ toprint a ^ ", " ^ Int.toString b 
   | mapI toprint (lui(a,b)) = "lui " ^ toprint a ^ ", " ^ Int.toString b 
   | mapI toprint (seq(a,b,c)) = "seq " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sge(a,b,c)) = "sge " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sgeu(a,b,c)) = "sgeu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sgt(a,b,c)) = "sgt " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sgtu(a,b,c)) = "sgtu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sle(a,b,c)) = "sle " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sleu(a,b,c)) = "sleu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (slt(a,b,c)) = "slt " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (slti(a,b,c)) = "slti " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ Int.toString c
   | mapI toprint (sltu(a,b,c)) = "sltu " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sltiu(a,b,c)) = "sltiu " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ Int.toString c
   | mapI toprint (sne(a,b,c)) = "sne " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (B(a)) = "b " ^ a 
   | mapI toprint (bczt(a)) = "bczt " ^ a 
   | mapI toprint (bczf(a)) = "bczf " ^ a 
   | mapI toprint (beq(a,b,c)) = "beq " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (beqz(a,b)) = "beqz " ^ toprint a ^ ", " ^ b 
   | mapI toprint (bge(a,b,c)) = "bge " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bgeu(a,b,c)) = "bgeu " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bgez(a,b)) = "bgez " ^ toprint a ^ ", " ^ b 
   | mapI toprint (bgezal(a,b)) = "bgezal " ^ toprint a ^ ", " ^ b 
   | mapI toprint (bgt(a,b,c)) = "bgt " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bgtu(a,b,c)) = "bgtu " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bgtz(a,b)) = "bgtz" ^ toprint a ^ ", " ^ b 
   | mapI toprint (ble(a,b,c)) = "ble " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bleu(a,b,c)) = "bleu " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (blez(a,b)) = "blez" ^ toprint a ^ ", " ^ b 
   | mapI toprint (Bgezal(a,b)) = "bgezal " ^ toprint a ^ ", " ^ b 
   | mapI toprint (bltzal(a,b)) = "bltzal " ^ toprint a ^ ", " ^ b 
   | mapI toprint (blt(a,b,c)) = "blt " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bltu(a,b,c)) = "bltu " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bltz(a,b)) = "bltz " ^ toprint a ^ ", " ^ b 
   | mapI toprint (bne(a,b,c)) = "bne " ^ toprint a ^ ", " ^ toprint b ^ ", " ^ c
   | mapI toprint (bnez(a,b)) = "bnez " ^ toprint a ^ ", " ^ b 
   | mapI toprint (j(a)) = "j " ^ a 
   | mapI toprint (jal(a)) = "jal " ^ a 
   | mapI toprint (jalr(a)) = "jalr " ^ toprint a
   | mapI toprint (jr(a)) = "jr " ^ toprint a
   | mapI toprint (la(a,b)) = "la " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lb(a,b)) = "lb " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lbu(a,b)) = "lbu " ^ toprint a ^ ", " ^ b 
   | mapI toprint (ld(a,b)) = "ld " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lh(a,b)) = "lh " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lhu(a,b)) = "lhu " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lw(a,b)) = "lw " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lwcz(a,b)) = "lwcz " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lwl(a,b)) = "lwl " ^ toprint a ^ ", " ^ b 
   | mapI toprint (lwr(a,b)) = "lwr " ^ toprint a ^ ", " ^ b 
   | mapI toprint (ulh(a,b)) = "ulh " ^ toprint a ^ ", " ^ b 
   | mapI toprint (ulhu(a,b)) = "ulhu " ^ toprint a ^ ", " ^ b 
   | mapI toprint (ulw(a,b)) = "ulw " ^ toprint a ^ ", " ^ b 
   | mapI toprint (sb(a,b)) = "sb " ^ toprint a ^ ", " ^ b 
   | mapI toprint (sd(a,b)) = "sd " ^ toprint a ^ ", " ^ b 
   | mapI toprint (sh(a,b)) = "sh " ^ toprint a ^ ", " ^ b 
   | mapI toprint (sw(a,b)) = "sw " ^ toprint a ^ ", " ^ b 
   | mapI toprint (swcz(a,b)) = "swcz " ^ toprint a ^ ", " ^ b 
   | mapI toprint (swl(a,b)) = "swl  " ^ toprint a ^ ", " ^ b 
   | mapI toprint (swr(a,b)) = "swr " ^ toprint a ^ ", " ^ b 
   | mapI toprint (ush(a,b)) = "ush " ^ toprint a ^ ", " ^ b 
   | mapI toprint (usw(a,b)) = "usw " ^ toprint a ^ ", " ^ b 
   | mapI toprint (move(a,b)) = "move " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (mfhi(a)) = "mfhi " ^ toprint a
   | mapI toprint (mflo(a)) = "mflo " ^ toprint a
   | mapI toprint (mthi(a)) = "mthi " ^ toprint a
   | mapI toprint (mtlo(a)) = "mtlo " ^ toprint a
   | mapI toprint (mfcz(a,b)) = "mfcz " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (mfc1_d(a,b)) = "mfc1.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (mtcz(a,b)) = "mtcz " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (rfe) = "rfe" 
   | mapI toprint (syscall) = "syscall" 
   | mapI toprint (break(a)) = "break " ^ Int.toString a
   | mapI toprint (nop) = "nop"
   | mapI toprint (abs_d(a,b)) = "abs.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (abs_s(a,b)) = "abs.s" ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (add_d(a,b,c)) = "add.d " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (add_s(a,b,c)) = "add.s " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (c_eq_d(a,b)) = "c.eq.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (c_eq_s(a,b)) = "c.eq.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (c_le_d(a,b)) = "c.le.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (c_le_s(a,b)) = "c.le.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (c_lt_d(a,b)) = "c.lt.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (c_lt_s(a,b)) = "c.lt.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (cvt_d_s(a,b)) = "cvt.d.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (cvt_d_w(a,b)) = "cvt.d.w " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (cvt_s_d(a,b)) = "cvt.s.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (cvt_s_w(a,b)) = "cvt.s.w " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (cvt_w_d(a,b)) = "cvt.w.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (cvt_w_s(a,b)) = "cvt.w.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (div_d(a,b,c)) = "div.d " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (div_s(a,b,c)) = "div.s " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (l_d(a,b)) = "l.d " ^ toprint a ^ ", " ^ b 
   | mapI toprint (l_s(a,b)) = "l.s " ^ toprint a ^ ", " ^ b 
   | mapI toprint (mov_d(a,b)) = "mov.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (mov_s(a,b)) = "mov.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (mul_d(a,b,c)) = "mul.d " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (mul_s(a,b,c)) = "mul.s " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (neg_d(a,b)) = "neg.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (neg_s(a,b)) = "neg.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (s_d(a,b)) = "s.d " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (s_s(a,b)) = "s.s " ^ toprint a ^ ", " ^ toprint b
   | mapI toprint (sub_d(a,b,c)) = "sub.d " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
   | mapI toprint (sub_s(a,b,c)) = "sub.s " ^ toprint a ^ ", " ^ toprint b ^ ", "  ^ toprint c
 	
 	
 fun prInst a = mapI prtreg a

 fun prtlst (x :: xs) = Int.toString x ^ ", " ^ prtlst xs
   | prtlst     _     =                ""
   
 fun mapS print (Inst a)     = "\t"^ mapI print a ^ "\n" 
   | mapS print (ascii a)    = "\t.ascii " ^ a ^ "\n"
   | mapS print (asciiz a)   = "\t.asciiz " ^ a ^ "\n"
   | mapS print (data a)     = "\t.data " ^ Int.toString a ^ "\n"
   | mapS print (text a)     = "\t.text " ^ Int.toString a ^ "\n"
   | mapS print (word a)     = "\t.word " ^ prtlst a ^ "\n"
   | mapS print (byte a)     = "\t.byte " ^ prtlst a ^ "\n"
   | mapS print (float a)    = "\t.float " ^ prtlst a  ^ "\n"
   | mapS print (double a)   = "\t.double " ^ prtlst a ^ "\n"
   | mapS print (extern(a,b)) = "\t.extern " ^ Int.toString a ^ " " ^ Int.toString b ^ "\n"
   | mapS print (globl a)    = "\t.globl " ^ a ^ "\n"
   | mapS print (half a)     = "\t.half " ^ prtlst a ^ "\n"
   | mapS print (kdata a)    = "\t.kdata " ^ Int.toString a ^ "\n"
   | mapS print (ktext a)    = "\t.ktext " ^ Int.toString a ^ "\n"
   | mapS print (space a)    = "\t.space " ^ Int.toString a ^ "\n"
   | mapS print (align a)    = "\t.align " ^  Int.toString a  ^ "\n"
   | mapS print (ptext )     = "\t.text" ^ "\n"
   | mapS print (pdata )     = "\t.data" ^ "\n"
   | mapS print (Label a)    = a ^ ": \n"
  
 fun prStmt a = mapS prtreg a

 fun prProg (x :: xs) = prStmt x ^ prProg xs
   | prProg _ = "\n"
 (* actual code that SPIM can understand is (string, reg) inst *)
 
end



