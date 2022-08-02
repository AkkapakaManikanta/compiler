signature GRAPH = sig

type node
type 'a graph
val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node


val addEdge : (node * node) -> unit


val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit
val all     : 'a graph -> node list
end
