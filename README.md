# DBasic
WELCOME TO DBASIC DOCUMENTATION

\-------------------------------------------------------------------------\
\Introduction\
I have been wanting to create my own programming language for a really
long time (like months); and there it is! I wrote it because I needed a
multi-purpose, compilable and FREE programming language, that could be
installed from a simple ZIP file. Since none of the languages I tried to
use met all of the requierements, I had to write one by myself. It might
not be the most powerful, fastest, compact language you've ever seen, but
I hope you'll enjoy coding with it :).

I personally like to use .dbs as the file extension of my DBasic source code
files, but you do you.

Make sure to read at least the first section, which is about using the
compiler.

\-------------------------------------------------------------------------\
\Using the compiler\
The first thing to do in order to use DBasic is to add the path to the
compiler folder in your %PATH%. Then, you can run it via the command line.
To compile a file, give the filename in argument to the compiler:

DBasic.vbs code.dbs

The assembly output of the compilation will be written in the output.asm
file in the compiler folder if you want to take a look at it. It is then
assembled by feeding it to FAsm.exe, and you can then find you executable
in the working directory, named with the same file name, with the file
extension replaced by .exe.

To see the command line output of the compiler, you need to launch it with
CScript:

CScript.exe DBasic.vbs code.dbs

There is no available IDE yet.

\-------------------------------------------------------------------------\
\Structure of a program\
A DBasic program always contains contain at least a MAIN block:

MAIN
	...
ENDMAIN

It may also contain a GLOBAL block, and an INIT block, in the following
order:

\contains global variable declarations\
GLOBAL
	...
ENDGLOBAL

\there really is no difference between the MAIN and the INIT block, I just
put it there because I thought it would allow for cleaner code in some
cases\
INIT
	...
ENDINIT

MAIN
	...
ENDMAIN

(Notice that the language is case-insensitive, that comments are enclosed
by backslashes, and that '...' is a valid statement that just does nothing).

\-------------------------------------------------------------------------\
\Variables declarations\
Declaring a variable is done using the keyword DIM:

DIM x

You may also declare multiple variables at once:

DIM a,b,c,d

To declare global variables, declare them in the GLOBAL block of a program:

GLOBAL
	DIM x
	DIM y \you can write as many DIM statements as you want\
ENDGLOBAL

You can also declare local variables by declaring them at the top of the
main block, or of a procedure:

MAIN
	DIM a,b,c
	DIM x \notice that again, you can use multiple DIM statements\
ENDMAIN

SUB foo()
	DIM i,j
	DIM y
ENDSUB

Finally, variables can have the type INT or STR. They are declared with the
type INT by default, but you can specify it:

\x and y will contain a number, while z will contain a string\
DIM x, INT y, STR z

Variables are all 4-bytes wide. Lastly, NEVER use an uninitialized variable,
because it will give unexpected results; never even assume that INTs are
set to 0, because they're not.

\-------------------------------------------------------------------------\
\Assignements\
If you've written even a single line of code in you life, you know that you
can you the '=' operator to assign a value to a variable. I also implemented
use the '+=', '-=' and '*=' operators because they're pretty useful, and
the generated code is more efficient that the code for 'x = x + y'. I'll
still give an example, just in case:

x = 2 + 4 * (5-6)
x *= 5
x -= 3

\-------------------------------------------------------------------------\
\Mathematics\
The available operators for mathematics expressions in DBasic are +,-,*,/
and % for modulo. You can also use parentheses. Divisions are euclidians,
because I have no clue on how floating points work on the binary level,
and I have no desire to dive into that. That also means floating points
number do not exist in DBasic. Like, who even needs floating points ?
Anyways, there's no power operator either, feel free to code a Pow()
function.

Boolean operations are a little bit more varied, don't worry. The available
operators are OR (|), XOR (~), AND (&), NOT (!), <, <=, >, >=, and = for
equalities. Just keep in mind that you can not mix types in boolean
relations, so don't write things like 'IF 2 = "2"', it won't even compile.

Finally, you can directly increment or decrement a variable by one, using
'x++' or 'x--'. The code generated for it is far more efficient than the
code for 'x = x + 1'.

\-------------------------------------------------------------------------\
\Strings\
Strings can be stored in variables of type STR. They are enclosed in single
or double quotes:

"that's a string" or 'that is a "string" as well'

you can also write multi-line strings, and the line break will appear in
them:

"that is the first line of the string
and this is the second"

You can compare them using '=', and concatenate them using '&'

\-------------------------------------------------------------------------\
\Control Structures\
There are 5 control structures in DBasic, which should cover all your needs.
First, the IF:

IF string = "hello" THEN \The THEN is optional\
	...
ELSE \The ELSE part is optional as well\
	...
ENDIF

Note that there's no ELSEIF, I hope to include it in the next release.
Next, the SELECT:

SELECT x \imagine that x is an INT\
	CASE 1
		MsgBox("Good morning,"")
	CASE 2
		MsgBox("Good evening!","")
	CASE ELSE
		MsgBox("Hello!")
ENDSELECT

The CASE ELSE is optional, and must always be placed in last position. The
same syntax applies for strings.
Now, on to the WHILE loop:

WHILE x > 5
	...
ENDWHILE

You can use the BREAK keyword to get out of it at anytime.
Next, the simplest one, the DO. It's just an infinite loop:

DO
	...
LOOP

You can also use BREAK to get out of it.
And finally, the REPEAT. It repeats until the given condition is true.

REPEAT
	x++
UNTIL x = 3

I also implemented labels, because who knows, it might be useful to someone.
You can define a label and then jump to it like this:

here:
...
GOTO here

At last, I have no idea where to talk about the END() statement, so I'll
just put it here. It immediately ends the program when executed. It is
followed by an empty pair of parentheses, because some people (like me) are
used to a syntax like 'END IF' instead of 'ENDIF'. Since the compiler
doesn't mind multiple statements on the same line, it will look for a
boolean expression after the IF, because if thinks that 'END IF' is an END,
followed by the IF control construct, leading to cryptic error messages.
The parentheses are there to avoid that.

\-------------------------------------------------------------------------\
\About semicolons\
Some programmers are used to semicolons, and some others are not. To make
DBasic suitable for everyone, semicolons are optional. You can put them at
the end of any statements, except DIM statements.

\-------------------------------------------------------------------------\
\Procedures\
You can declare a procedure like this:
SUB foo(x, STR y) : INT \SUB stands for 'subroutine', I took it from VBS\
	...
	RETURN(1)
ENDSUB

Procedure declarations are located after the MAIN block of the program.
The colon followed by a type name indicate the return type. The RETURN
statement halts the execution of the procedure and returns control to the
caller.

Notice that you have to write the expected type of the arguments, but that
no type checking is done while calling it. Specifying the type is important
nevertheless because it tells the body of the function how to treat them,
and giving the function a wrong type of argument may result in undefined
behavior.

To call a procedure, simply write:

foo(1,"hey") or CALL foo(1,"hey") \the CALL keyword from VBS looked cool\

or:

x = 1 + foo()

A procedure doesn't have to include a RETURN statement. A procedure
returns 0 by default, if no RETURN statement is executed.

\-------------------------------------------------------------------------\
\Inline Assembly\
DBasic includes inline assembly to allow for optimization, and for easy
access to non-implemented features.

The '$' symbol indicates that the rest of the line is in assembly. The code
must be written in Flat Assembly, and in order to use label, procedure or
variable names, you shall prefix them with "V_" and write them in
capital letters:

SUB infinite_loop():
	here:
	...
	$JMP V_HERE
ENDSUB

If you want to manipulate strings, be aware that they are stored in heap
buffers in the default process heap, and that they are prefixed by a DWORD
containing their length, and that they are null-terminated. Here is an
example, showing how the string 'hello' would be stored:

+-+-+-+-+---+---+---+---+---+-+
|0|0|0|5|104|101|108|108|111|0|
+-+-+-+-+---+---+---+---+---+-+

To use the heap, you don't have to call GetProcessHeap repeatedly; it is
called once at the top of every program, and the default heap handle is
stored in the DWORD hHeap variable. That means you can access it by writing
something like this:

MOV eax, DWORD [hHeap]

Finally, NEVER use inline asm to add in your code a label named in the form
'L0' (where 0 can be any number), or RETFUNCTIONNAME, because it's the
forms that are used for labels written by the compiler. It would create
duplicate labels, and throw an assembler error.

\-------------------------------------------------------------------------\
\Library writing\
Libraries are an essential part of the language. They are the ones that
provide for every needed functionality. That's why you may want to create
your own. Libraries are stored in the LIBS folder of the compiler, and are
named NAME.LIB by convention, but you may name them however you want. They
are coded in Flat Assembly, and will be included in the executable at
compile time. Here is the template for writing a function:

;$SUB TESTFUNCTION(ARG1, ARG2, STR ARG3) : INT
V_TESTFUNCTION:
	PUSH ebp
	MOV ebp, esp
	
	MOV eax, 1
	
	MOV esp, ebp
	POP ebp
	RET

This is a function that takes three INTs as arguments, and does nothing but
return 1.
To write a function, the first step is to write its header in a comment
above the code itself. You need to prefix it with $, write it all in capital
letters, specify the number and the type of the arguments, and the return
type of the function, in order for it to work correctly with the rest of
you code. Then, you need to write a label in the for V_FUNCTIONNAME, all
in capital letters. And finally, open a stack frame and write the code of
your function.

While writing a library, you need to follow the same rules about label names
and string handling as with inline assembly.

\-------------------------------------------------------------------------\
\Arrays\
Don't use arrays yet, I have not finished implementing them.
