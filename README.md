# COMP301 Fall22 Project 4

#### Description
Koc University Programming Language Concepts class project.

Expanding EREF language to support vector and stack data structures, with operations:
- newvector(length, value) initializes a vector of size 'length' with the value 'value'.
- update-vector(vec, index, value) updates the value of the vector 'vec' at index 'index' by value 'value'.
- read-vector(vec, index) returns the element of the vector 'vec' at index 'index'.
-  length-vector(vec) returns the length of the vector 'vec'.
- swap-vector(vec, index, index) swaps the values of the indexes in the vector 'vec'.
- copy-vector(vec) initializes an new vector with the same values of the given vector 'vec'.
- vec-mult(vec1, vec2) vector multiplication.
- newstack(L) returns an empty stack with max-size 'L'.
- push(s, val) adds the element 'val' to the stack 's'. If the stack is full it throws a stack overflow error.
- pop(s) removes the last element of the stack 's' and returns its value.
- stack-size(s) returns the number of elements in the 's'.
- peek(s) returns the value of the last element in the stack 's' without removal.
- empty-stack?(s) returns true if there is no element inside the stack 's' and false otherwise.
- print-stack(s) prints the elements in the stack 's'.


#### Files Edited
- data-structures.scm
- interp.scm
- lang.scm

Code added can be found under the "; ###### ENTER YOUR CODE HERE " blocks.

