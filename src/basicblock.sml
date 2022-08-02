signature INST = sig
	type ('a, 'b) t
	val isJumpLike : ('a, 'b) t -> bool 
	val isTarget : ('a, 'b) t -> bool
end

functor BasicBlocks (I : INST) 
	= struct 
		structure Inst = I
		
		fun f1 []     = ([], [])
		  | f1 (x::xs) =  (x,xs) 
  
		fun flip xs = foldl (fn (x,y) => x::y) [] xs
		
		type ('a, 'b) block = ('a, 'b) I.t list
		fun basicblocks arrs arrs2 = let
			val (x, arrs2) = f1(arrs2) 
			val arrs2 = 
					if (I.isJumpLike arrs) then ([]::(x @ [arrs])::arrs2)
					else if (I.isTarget arrs)then
									if null x then((x @ [arrs])::arrs2)
									else ([arrs]::x::arrs2)
					else
					((x @ [arrs])::arrs2)
			in
				arrs2
			end
	fun basicblock []      arr = arr
  	  | basicblock (x::xs) arr = (basicblock xs (basicblocks x arr))

	fun basicBlocks (ins) = flip (basicblock ins [[]])

 end
