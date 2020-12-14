
# Xenon Specification Documentation

## Reserved Keywords

There are a specific list of keywords within Xenon which will change as the language gets updated over time.
```
 KEYWORDS: Use Cases
 
    VAR:  	   variable declaration
    
    AND:  	   logical AND operator
    
    OR:   	   logical OR operator
    
    NOT:  	   logical NOT operator
    
    IF:  	   reserved for IF statement
    
    ELIF:  	   reserved for IF statement
    
    ELSE:  	   reserved for IF statement
    
    FOR:  	   reserved for FOR loop
    
    TO:  	   reserved for FOR loop
    
    STEP:  	   inc/dec for FOR loop
    
    WHILE:     reserved for WHILE loop
    
    FUN:  	   function declaration
    
    THEN:  	   used after loop/statement use
    
    END:  	   ending of loop
    
    RETURN:    python return
    
    CONTINUE:  python return
    
    BREAK:     python return
    


```

## Tokens

There are a specific list of tokens used within Xenon which will change as the language gets updated over time.
```
 Tokens:
TT_INT  =  'INT' : interger value ( 123 )

TT_FLOAT  =  'FLOAT' : float value ( 12.3 )

TT_STRING  =  'STRING' : string value ( "HI MOM" )

TT_QUOTE = 'QUOTE' : opening/closing double quotes ( " " )

TT_CHARQUOTES = 'CHARQUOTES' : opening/closing single quotes ( ' ' )

TT_IDENTIFIER  =  'IDENTIFIER' : variable name ( x )

TT_KEYWORD  =  'KEYWORD' : above keyword ( VAR )

TT_PLUS  =  'PLUS' : addition ( + )

TT_MINUS  =  'MINUS' : subtraction/list removal ( - / -1)

TT_MUL  =  'MUL' : multiplcation/concatination ( * / [1]*[2] => [1,2] )

TT_DIV  =  'DIV' : division/index retrival ( / / list/0 == list[0] )

TT_POW  =  'POW' : exponent ( 5^2 )

TT_EQ  =  'EQ' : equals ( = )

TT_LPAREN  =  'LPAREN' : left parentheses ( '(' )

TT_RPAREN  =  'RPAREN' : right parentheses ( ')' )

TT_LSQUARE  =  'LSQUARE' : left bracket ( '[' )

TT_RSQUARE  =  'RSQUARE' : right bracket ( ']' )

TT_EE  =  'EE' : boolean expression ( == )

TT_NE  =  'NE' : logical not equals ( != )

TT_LT  =  'LT' : less than ( < )

TT_GT  =  'GT' : greater than ( > )

TT_LTE  =  'LTE' : less than equal to ( <= )

TT_GTE  =  'GTE' : greater than equal to ( >= )

TT_COMMA  =  'COMMA' : parameter seperator ( ',' )

TT_ARROW  =  'ARROW' : function declaration ( FUN(a,b) -> a + b  )

TT_NEWLINE  =  'NEWLINE' : multiline support ( /n )

TT_EOF  =  'EOF' : end of file ( yea lol )


```

## Symbol Table

Functions that are built into the language ( this will be changing )

```
CommonX = SymbolTable()

CommonX.set("NULL", Number.null)

CommonX.set("FALSE", Number.false)

CommonX.set("TRUE", Number.true)

CommonX.set("MATH_PI", Number.math_PI)

CommonX.set("FAC", Number.factorial)

CommonX.set("PRINT", Common.print)

CommonX.set("PRINT_RET", Common.print_ret)

CommonX.set("INPUT", Common.input)

CommonX.set("INPUT_INT", Common.input_int)

CommonX.set("CLEAR", Common.clear)

CommonX.set("CLS", Common.clear)

CommonX.set("IS_NUM", Common.is_number)

CommonX.set("IS_STR", Common.is_string)

CommonX.set("IS_FIN", Common.is_finite)

CommonX.set("FAC", Common.factorial)

CommonX.set("CEIL", Common.ceil)

CommonX.set("FABS", Common.fabs)

CommonX.set("FLOOR", Common.floor)

CommonX.set("SQRT", Common.sqrt)

CommonX.set("POTATOSOUP", Common.potatosoup)

CommonX.set("IS_LIST", Common.is_list)

CommonX.set("IS_FUN", Common.is_function)

CommonX.set("APPEND", Common.append)

CommonX.set("POP", Common.pop)

CommonX.set("EXTEND", Common.extend)

CommonX.set("LEN", Common.len)

CommonX.set("RUN", Common.run)

```


## Xenon Error Handling
We have implemented four custom error handling classes
```
class  IllegalChar(Error):

	   super().__init__(starting_position, ending_position, 'Illegal Character', details)


class  ExpectedChar(Error):

	   super().__init__(starting_position, ending_position, 'Expected Character', details)


class  InvalidSyntaxError(Error):

	   super().__init__(starting_position, ending_position, 'Invalid Syntax', details)


class  RTError(Error):

	   super().__init__(starting_position, ending_position, 'Runtime Error', details)
```
