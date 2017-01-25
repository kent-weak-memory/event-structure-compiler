val mem : 'a list -> 'a -> bool
val not_mem : 'a list -> 'a -> bool
val make_proper : 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val intersection : 'a list -> 'a list -> 'a list
val subset : 'a list -> 'a list -> bool
val disjoint : 'a list -> 'a list -> bool
val cross : 'a list -> 'b list -> ('a * 'b) list
