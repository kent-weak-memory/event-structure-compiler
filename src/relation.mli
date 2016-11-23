(*
 * Event Structure Compiler
 * Copyright (c) 2016 Simon Cooksey
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *)

type 'a edge = 'a * 'a
type 'a relation = ('a edge) list

val edge_in_relation : 'a edge -> 'a relation -> bool
val relation_in_relation : 'a relation -> 'a relation -> bool
val is_reflexive : 'a relation -> 'a list -> bool
val is_symmetric : 'a relation -> bool
val is_antisymmetric : 'a relation -> bool
