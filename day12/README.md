

# jigsaw

enter smalltalk directory

bash start.sh -- will start a dedicated advent of code day 12 session automatically


```lisp
FUN> (boards ex)
(("###" "##." "##.") ("###" "##." ".##") (".##" "###" "##.")
 ("##." "###" "##.") ("###" "#.." "###") ("###" ".#." "###"))
FUN> (boards in)
(("##." ".##" "###") ("..#" "###" "###") ("#.#" "###" "#.#")
 ("###" "..#" "###") ("#.." "##." "###") ("##." ".##" "..#"))
FUN> 

in example 

###
##.
##.

this shape can be rotated and flipped 

lets start with an original shape S1

123
456
789

rotate clockwise = S2
741
852
963

rotate clockwise (2nd time) = S3
987
654
321

rotate clockwise (3rd time) = S4
369
258
147

rotate clockwise (4th time) = return original
123
456
789

flip horz starting original S1 = S5
321
654
987

flip horz S2 = S6
147
258
369

flip horz S3 = S7 
789
456
123

flip horz S4 = S8
963
852
741

flip horz S5 = S1 original
123
456
789

8 states 

can this shape fit into grid at x y ?



```

```text
presents are 3 x 3 in nature on 2d grid 
#
.
```

