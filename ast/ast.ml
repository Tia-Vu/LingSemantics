
module LexiconMap = Map.Make (struct
  type t = string
  let compare = compare
end)

type typ = 
  | Entity
  | TruthVal
  | Function of typ * typ

type lexicon = typ LexiconMap.t

type terminal = string

type tree_node = 
  | Leaf of terminal * typ option
  | Node of nonterminal * typ option
  
and nonterminal = 
  {
    symbol : string;
    children : tree_node list;
  }

type tree = tree_node
