<img
src="images/Xenon_Logo.jpg"
raw=true
alt="XENON"
style="margin-right: 10px;"
/>

This code was written in learning from the articles from this source. They were loosely followed however thhe main source came from here
https://ruslanspivak.com/lsbasi-part1/
https://ruslanspivak.com/lsbasi-part2/
https://ruslanspivak.com/lsbasi-part3/
https://ruslanspivak.com/lsbasi-part4/
https://ruslanspivak.com/lsbasi-part5/
https://ruslanspivak.com/lsbasi-part6/
https://ruslanspivak.com/lsbasi-part7/
https://ruslanspivak.com/lsbasi-part8/
https://ruslanspivak.com/lsbasi-part9/
https://ruslanspivak.com/lsbasi-part10/
https://ruslanspivak.com/lsbasi-part11/
https://ruslanspivak.com/lsbasi-part12/
https://ruslanspivak.com/lsbasi-part13/
https://ruslanspivak.com/lsbasi-part14/
https://ruslanspivak.com/lsbasi-part15/
https://ruslanspivak.com/lsbasi-part16/
https://ruslanspivak.com/lsbasi-part17/
https://ruslanspivak.com/lsbasi-part18/
https://ruslanspivak.com/lsbasi-part19/

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
		add new rule to grammar need to check for an IF <condition> THEN ELIF/ELSE
		also scenario where we have multiple elseifs which oculd cause trouble
		make function to determine if a resilut is true or not based on the return value 1/0 rem. 0 = FALSE
		will javascripty names and variables later but easier to keep in py for the time being
		input: IF 5==5 THEN 123
		output: 132
		input: IF 5 == 4 THEN 132
		output:
		input: IF 5 == 4 THEN 132 ELSE 111
		output: 111

Step 6: gimmie the loop, gimmie the loop the for loop lol
		js for loop = for( i = 0; i < FACTOR; i++) CODE
		basic for loop syntax FOR <var name here> = <starting value> TO <ending value> THEN <expr>
		VAR result = 1
		FOR i = 1 TO 11 THEN result * i
		also include while as its copy/pasta
		input: FOR i = 1 TO 6 THEN VAR result = result * i
		output: error result not defined
		input: VAR result = 1
		repeat FOR i = 1 TO 6 THEN VAR result = result * i
		output:
		input: result
		output: 120
		also allows step so i++/+--
		but format is FOR i = 5 TO 0 STEP -1 THEN VAR result = result * i
		--
		input: WHILE TRUE THEN 123
		output: will run inf...\

Step 7: fn()'s
		basic function functioanlity lol no pun intended
		assigning fucntion to a var name
		anon fns just nameless ones
		argument excpetion on a fucntion
		input: FUN add(a, b) -> a + b
		output: <function add>
		so now that funciton has been defined we can now use
		so calling
		input: add(2,5)
		output: 7

Step 8: strings
		a couple of things to account for here
		"TEXT"
		"TEXT WITH /"QUOTES/""
		"TEXT WITH \\BACKSLASH\\"
		"TEXT WITH \n /nnewlines"

Step 9: Lists
		brakcets will be used for the lists [] seperated by commas
		[1,2,3,4,5]
		also to add an item to a list
		[1,2,3] + 4 = [1,2,3,4]
		to concat two lists
		[1,2,3] * [3,4,5] = [1,2,3,3,4,5,]
		[1,2,3] - -1 = [1,2] - removes last element
		[1,2,3] - 1 = [1,3] removes index element
		[1,2,3] / 0 = 1 just like js listname[0]
		[1,2,3] / -1 = 3 just like js listname[listname.length - 1]
		input: [1,2,3,4]
		output: [1,2,3,4]
		can now be used with for and while loop seeing as they return a list

Step 9: Built in Stoof
		print
		print_ret
		input
		input_int
		clear
		is_number
		is_string
		is_list
		is_function
		append
		pop
		extend
		VAR age = INPUT_INT() - now allows user to type in a certain age
		PRINT("HELLOW WORLD!")

Step 9: Multi-Line
		Need to reorder if statement to allow multiple expressions and end with "END"
		update lexer with the new line and the end line aslo add to keywords
		statement rule
		return a new list of resluts based on semi colon so ex:
		input: 1 + 2; 3 + 4; 5 + 6;
		output: [3, 7, 11]
		empty expression fix on just space enter
		checks for new lines

Step 10: Return; Continue; Break;