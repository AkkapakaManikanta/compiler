structure Translate = struct
    exception VariableNotFound
    
    type Env = Temp.temp AtomMap.map
      

	

    fun assign_reg (envt : Env) (tem : Temp.temp) (TigerAst.Const x)      = [MIPS.Inst(MIPS.li (tem, x))]
      | assign_reg (envt : Env) (tem : Temp.temp) (TigerAst.Var x)        = let
                                                                  			val g = Atom.atom x
                                                                 	 		val f = AtomMap.find (envt, g)   
                                                               		in
                                                                     		case f of 
                                                                      			NONE   => raise VariableNotFound
                                                                     			| SOME y => [MIPS.Inst(MIPS.addi (tem, y, 0))]
                                                              	 	end
                                                                
      | assign_reg (envt : Env) (tem : Temp.temp) (TigerAst.Op (x,y,z))  = let
                                                                  		 	val p1 = Temp.newtemp ()
                                                                 		 	val p2 = Temp.newtemp ()
                                                                 		 	val q1 = assign_reg envt p1 x
                                                                   		 	val q2 = assign_reg envt p2 z
                                                                	      in
                                                                      		case y of
                                                                        	  		TigerAst.Plus  => q1 @ q2 @ [MIPS.Inst(MIPS.add  (tem,p1, p2))]
                                                                          		      | TigerAst.Minus => q1 @ q2 @ [MIPS.Inst(MIPS.sub  (tem,p1, p2))]
                                                          			  	      | TigerAst.Mul   => q1 @ q2 @ [MIPS.Inst(MIPS.mul  (tem,p1, p2))]
                                                          				      | TigerAst.Div   => q1 @ q2 @ [MIPS.Inst(MIPS.Div  (tem,p1, p2))]
                                                               	      end  

    fun trans (envt : Env) (TigerAst.Assign (x,y))                       = let
                                                 					val a = Atom.atom x
                                                 					val b = AtomMap.find (envt, a)   
                                           				      in
                                                    					case b of 
                                                        					NONE   => let
                                                   	               					val newtem = Temp.newtemp ()
                                                                      					val new_envt = AtomMap.insert (envt, a, newtem)
                                                               				          in
                                                                    						(assign_reg envt newtem y, new_envt)
                                                               					  end
                                                      					| SOME z =>   (assign_reg envt z y, envt)
                                                   			     end
                                                   
      | trans (envt : Env) (TigerAst.Print x)      			   = let 
                                                    					val newtem = Temp.newtemp ()
                                                   		             in 
                                                             			((assign_reg envt newtem x) @ [MIPS.Inst(MIPS.li (Temp.printzero, 1))] @ [MIPS.Inst(MIPS.addi (Temp.printone, newtem,0))] @ [MIPS.Inst(MIPS.syscall)], envt)
                                                   			     end
                                                   			     
      | trans (envt : Env) (TigerAst.For (u,v,w,z))			   = let
                                                    					val newtem = Temp.newtemp ()
                                                    					val new_envt = AtomMap.insert (envt, (Atom.atom u), newtem)
                                                    					val e = transproghelp new_envt z
                                                    					val newlabl = Temp.newlabel ()
                                                   		     	     in
                                                    					([MIPS.Inst(MIPS.li (newtem, v))] @ [MIPS.Label (Temp.labelToString newlabl)] @ e @ [MIPS.Inst(MIPS.addi (newtem, newtem, 1))] @ [MIPS.Inst(MIPS.ble (newtem, Temp.inttotemp w, Temp.labelToString newlabl))], envt)
                                                   			     end

    and transproghelp (envr : Env) (x :: xs) 				   = let
                                                    					val (inp, new_envr) = trans envr x
                                               			     in
                                                    					inp @ (transproghelp new_envr xs)
                                                			     end
      | transproghelp (envr : Env) _         				   = []
    
    fun transprog (envn : Env) x                                        = [MIPS.pdata] @ [MIPS.ptext] @ [MIPS.globl "main"] @ [MIPS.Label "main"] @ (transproghelp envn x)

end



