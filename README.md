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


Step 3: