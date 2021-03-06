# Xenon EBNF Grammar Rules
 1. **statements**
	  : NEWLINE* statement (NEWLINE+ statement)* NEWLINE*

 2. **statement**
	  : KEYWORD:RETURN expr?
	  : KEYWORD:CONTINUE
	  : KEYWORD:BREAK
	  : expr

 3. **expr**
	  : KEYWORD:VAR IDENTIFIER EQ expr
	  : comp-expr ((KEYWORD:AND||KEYWORD:OR) comp-expr)*

  4. **comp-expr**
	  : NOT comp-expr
	  : arith-expr ((EE||LT||GT||LTE||GTE) arith-expr)*

  5. **arith-expr**
	   : term ((PLUS||MINUS) term)*

  6. **term**
	  : factor ((MUL||DIV) factor)*

  7. **factor**
	  : (PLUS||MINUS) factor
	  : power

  8. **power**
	  : call (POW factor)*

  9. **call**
	   : atom (LPAREN (expr (COMMA expr)*)? RPAREN)?

   10. **atom**
	    : INT||FLOAT||STRING||IDENTIFIER
	    : LPAREN expr RPAREN
	    : list-expr
	    : if-expr
	    : for-expr
	    : while-expr
	    : func-def

11. **list-expr**
	 : LSQUARE (expr (COMMA expr)*)? RSQUARE

12.  **if-expr**
	 : KEYWORD:IF expr KEYWORD:THEN
	 (statement if-expr-b|if-expr-c?) || (NEWLINE statements KEYWORD:END|if-expr-b|if-expr-c)

13. **if-expr-b**
	: KEYWORD:ELIF expr KEYWORD:THEN
	 (statement if-expr-b||if-expr-c?) || (NEWLINE statements KEYWORD:END||if-expr-b||if-expr-c)

14. **if-expr-c**
	 : KEYWORD:ELSE
	 statement | (NEWLINE statements KEYWORD:END)

15.  **for-expr**
	 : KEYWORD:FOR IDENTIFIER EQ expr KEYWORD:TO expr
	 (KEYWORD:STEP expr)? KEYWORD:THEN
	 statement || (NEWLINE statements KEYWORD:END)

16. **while-expr**
	 : KEYWORD:WHILE expr KEYWORD:THEN
	 statement || (NEWLINE statements KEYWORD:END)

17. **func-def**
	 : KEYWORD:FUN IDENTIFIER?
	 LPAREN (IDENTIFIER (COMMA IDENTIFIER)*)? RPAREN
	 (ARROW expr) || (NEWLINE statements KEYWORD:END)

18. **STRING**
	: 
	 QUOTE (
		ASCII - [ ", \, Isolated CR ] |
		QUOTE_ESCAPE |
		ASCII_ESCAPE | 
		NEWLINE
	)* QUOTE

19. **INT**
	:
	[0 - 9]+ 	{Decimal}
	0x[0 - F]+	{Hexadecimal}
	0o[0 - 7]+	{Octal}
	0b[0 - 1]+	{Binary}
	
20. **FLOAT**
	:
	[0 - F]+ (e | E | p | P) [0 - F]+
	Conditional, if opening integer is hex, P must be used to separate
				if opening integer i decimal, E must be used to separate
	
21. **IDENTIFIER**
	:
	(
		ASCII - [ ", \, Isolated CR]
	)*