module Unbalanced_set (Element : Std.Ordered) :
  Binary_search_tree_intf.Solutions
  with type elem = Element.t

module I : Binary_search_tree_intf.Solutions with type elem = int
module S : Binary_search_tree_intf.Solutions with type elem = string
