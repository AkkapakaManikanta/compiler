signature TEMP =
  sig
     type temp
     val newtemp    : unit -> temp
     val tempToString : temp -> string
     val printzero : temp
     val printone : temp
     val inttotemp : int -> temp
     type label
     val newlabel   : unit -> label
     val labelToString : label -> string
  end

structure Temp :> TEMP = struct

   type temp  = int (* 2ʷ many variables on a w-sized machine *)
		      (* you can use IntInf.int if you want unbounded *)

   val nextTemp       = ref 2 (* Keep track of how many temps have been allocated *)
   fun newtemp  _     = ((nextTemp := (!nextTemp + 1)); (!nextTemp-1))
   fun tempToString t = Int.toString(t) 
   
   type label = int
   val nextLabel = ref 0
   fun newlabel _ = ((nextLabel := (!nextLabel + 1)); (!nextLabel-1))
   fun labelToString l = Int.toString(l)
   
   val printzero = 0
   val printone = 1
   fun inttotemp x = x
end
