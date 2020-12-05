https://ruslanspivak.com/lsbasi-part1/

This language was built with retorspect to the above article
Step 1: create a simple parser to generatre token with a given key such as Int:1
		Slight error handling on incorrect syntax (Illegal Character)
		Generate list of tokens like ints floats blah blah
		input: 1 + 2
		output: [INT:1, PLUS, INT:2]

Step 2: Basic math operations with pemdas recognigtion to correctly so math can be
		done correctly.
	 	Beef up error handling ofc.
		input: 2 * 3 + 1
		output: ((INT:2, MUL, INT:3), PLUS, INT:1)
		Support uniary operations

Step 3: Interpreter
		Lexed -> parse -> interpreted, taking in the tokens now to actualy evaluate
		implemetaion of checking tokens to add simple mathmatiecal logic to the token being used (+,-,/)
		uniary operations such as -4 which is a negative numbers along with --4 which would be a postive 4
		need to also add support for /0 because atm python takes over and crashes
		during runtime keep track of any errors or any results
		all inline so far nee to break into a text file soon

Step 4: Variables
		Format: VAR a = 10 just like js
		VAR-keyoword
		var name  - indetifier
		= - equals
		need symbol table as well
		input: VAR x = 5
		output: 5
		input: x
		output: 5

Step 5: Logical Operators
		Additon of ! as well as equals, less, greater, equals toos
		neeed new format to be looking like this (factor) == (factor)
		addition of NOTS and booleans that instead of returrning true or false go by binary values
		input: 5 == 5
		output: 1
		input: 5 == 3
		output: 0
		additiaonl support can be TRUE,FALSE equalling their bianry values

Step 6: IF statement
		