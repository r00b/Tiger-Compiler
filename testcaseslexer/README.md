##  Lexer

### Comments

#### one layer comment
lexer\_comment1.tig : unmatched comment  
lexer\_comment2.tig : matched comment  
lexer\_comment3.tig : matched comment  
lexer\_comment4.tig : matched comment  
lexer\_comment5.tig : unmatched comment  
lexer\_comment6.tig : matched comment  

#### two layer comment
lexer\_comment7.tig : matched comment  

### String

lexer\_string1.tig : matched quotes
lexer\_string2.tig : unmatched quotes
lexer\_string3.tig : matched quotes
lexer\_string4.tig : unmatched quotes

TODO

escape sequence checking

#### \ddd
lexer\_slashddd1.tig: match all 32 - 126 characters.
lexer\_slashddd2.tig: match \10 to \n, the new line character.
lexer\_slashddd3.tig: match \123 and then 4.
lexer\_slashddd4.tig: show error for \20 since it is not within the range of printable characters
  nor is it among {\n, \t, \f}.
