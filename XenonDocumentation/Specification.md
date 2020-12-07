
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

Functions that are built into the language ( this will be chaning )

```
global_symbol_table =  SymbolTable()

global_symbol_table.set("NULL", Number.null)

global_symbol_table.set("FALSE", Number.false)

global_symbol_table.set("TRUE", Number.true)

global_symbol_table.set("MATH_PI", Number.math_PI)

global_symbol_table.set("PRINT", BuiltInFunction.print)

global_symbol_table.set("PRINT_RET", BuiltInFunction.print_ret)

global_symbol_table.set("INPUT", BuiltInFunction.input)

global_symbol_table.set("INPUT_INT", BuiltInFunction.input_int)

global_symbol_table.set("CLEAR", BuiltInFunction.clear)

global_symbol_table.set("CLS", BuiltInFunction.clear)

global_symbol_table.set("IS_NUM", BuiltInFunction.is_number)

global_symbol_table.set("IS_STR", BuiltInFunction.is_string)

global_symbol_table.set("IS_LIST", BuiltInFunction.is_list)

global_symbol_table.set("IS_FUN", BuiltInFunction.is_function)

global_symbol_table.set("APPEND", BuiltInFunction.append)

global_symbol_table.set("POP", BuiltInFunction.pop)

global_symbol_table.set("EXTEND", BuiltInFunction.extend)

global_symbol_table.set("LEN", BuiltInFunction.len)

global_symbol_table.set("RUN", BuiltInFunction.run)
```


## Xenon Error Handling
We have implemented four custom error handling classes
```
class  IllegalCharError(Error):

	   super().__init__(pos_start, pos_end, 'Illegal Character', details)


class  ExpectedCharError(Error):

	   super().__init__(pos_start, pos_end, 'Expected Character', details)


class  InvalidSyntaxError(Error):

	   super().__init__(pos_start, pos_end, 'Invalid Syntax', details)


class  RTError(Error):

	   super().__init__(pos_start, pos_end, 'Runtime Error', details)
```
