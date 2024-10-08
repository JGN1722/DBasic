# WELCOME TO DBASIC DOCUMENTATION

## Introduction
I have been wanting to create my own programming language for a really
long time (like months); and there it is! I wrote it because I needed a
multi-purpose, compilable and FREE programming language, that could be
installed from a simple ZIP file. Since none of the languages I tried to
use met all of the requierements, I had to write one by myself. It might
not be the most powerful, fastest, most compact language you've ever seen,
but I hope you'll enjoy coding with it :).

I personally like to use `.dbs` as the file extension of my DBasic source code
files, but you do you.

Make sure to read at least the first section, which is about using the
compiler.

## Using the compiler
The first thing to do in order to use DBasic is to add the path to the
compiler folder in your `%PATH%`. Then, you can run it via the command line.
To compile a file, give the filename in argument to the compiler:
`DBasic.vbs code.dbs`

The assembly output of the compilation will be written in the `output.asm`
file in the compiler folder if you want to take a look at it. It is then
assembled by feeding it to `FAsm.exe`, and you can then find you executable
in the working directory, named with the same file name, with the file
extension replaced by `.exe`.

To see the command line output of the compiler, you need to run it with
the CScript VBS interpreter, available by default on windows:
`CScript.exe DBasic.vbs code.dbs`

To import libraries in your code, you need to write their name on a new line
in the `LIBS.INI` file. The libraries need to be located in the `LIBS` subfolder
of the compiler.
When you import a library, know that the WHOLE library will be in the
executable, and not only the functions you use, so do not import useless
libraries if you care at least a little bit about code size
I won't document the libraries that come with the compiler. Look inside the
files, and look for the lines beginning with a `$`. They contain the name
of the functions and their arguments, which should be self explanatory.
If you still have a doubt, you may look them up on the internet, the names
I used are pretty common.

There is no available IDE yet.

## Troubleshooting
The compiler might occasionally not work. If you notice that your executable
is not appearing after compilation, try running it again with cscript via
cmd:
`CScript.exe DBasic.vbs code.dbs`

You should see the compiler output on screen. If the output is 'compilation
successful' and the executable is still not appearing, try reinstalling the
compiler.

## Structure of a program
A DBasic program always contains contain at least a MAIN block:
```
MAIN
	...
ENDMAIN
```

The blocks constituting a program must follow this order:
First, an optional METADATA block:
```
METADATA
	...
ENDMETADATA
```

It may also contain a GLOBAL block, and an INIT block, in the following
order:
```
\contains global variable declarations\
GLOBAL
	...
ENDGLOBAL
```

After the GLOBAL block, you can place as many ENUMERATIONs as you want:
```
ENUMERATION
	constant_1 \ = 0\
	constant_2 \ = 1\
	constant_3 \ = 2\
	...
ENDENUMERATION
```

What this block does is automatically assigning values to constants, which
may be useful when you need to declare a lot of constants and you don't want
to make the GLOBAL block unreadable.

After that, you may place an INIT block, then there is the mandatory MAIN
block:
```
\there really is no difference between the MAIN and the INIT block, I just
put it there because I thought it would allow for cleaner code in some
cases\
INIT
	\also note that the INIT block can't have local variables\
	...
ENDINIT

MAIN
	...
ENDMAIN
```

Notice that the language is case-insensitive, that comments are enclosed
by backslashes, and that `...` is a valid statement that just does nothing.

## Executable metadata
The first block of a program is the one specifying the metadatas of the
executable. It is basically a list of statements redacted like this:
```
MetaData
	\In this example code, the strings are enclosed in single quotes,
	but double quotes are valid as well\
	icon: 'icon.ico'
	FileDescription: 'Test program'
	LegalCopyright: 'No rights reserved'
	FileVersion: '1.0'
	ProductVersion: '1.0'
	OriginalFileName: 'Test.dbs'
EndMetaData
```

You may place these statements in any order that you like, and omit as many
of them as you want. All the available statements are showed in this example.

Note that the icon file path must be relative to the compiler, and not
to the compiled file.

## Variables declarations
Declaring a variable is done using the keyword DIM:
`DIM x`

You may also declare multiple variables at once:
`DIM a,b,c,d`

To declare global variables, declare them in the GLOBAL block of a program:
```
GLOBAL
	DIM x
	DIM y \you can write as many DIM statements as you want\
ENDGLOBAL
```

In the GLOBAL block, you can also declare constants. The format for a
constant declaration is:
`CONST x = 1, y = 3, random_thing = 123456789, ...`

The CONST statement works just like DIM, except you have to initialize the
values, and the values must be numbers.

Constants are somewhat like variables, except that they cannot be modified
dynamically. They're also more efficient on the level of generated code, so
if you have a variable you often use and whose value doesn't change, you
should rather make it a constant.

You can also declare local variables by declaring them at the top of the
main block, or of a procedure:
```
MAIN
	DIM a,b,c
	DIM x \notice that again, you can use multiple DIM statements\
ENDMAIN

SUB foo()
	DIM i,j
	DIM y
ENDSUB
```

Finally, variables can have the type INT or STR. They are declared with the
type INT by default, but you can specify it:
```
\x and y will contain a number, while z will contain a string\
DIM x, INT y, STR z
```

Variables are all 4-bytes wide. Lastly, NEVER use an uninitialized variable,
because it will give unexpected results; never even assume that INTs are
set to 0 on declaration, because they're not.

## Assignements
As in most programming language, the DBasic assignement operator is `=`. I
also implemented the `+=`, `-=` and `*=` operators because they're pretty
useful, and the generated code is more efficient that the code for
`x = x + y`. I'll still give an example, just in case:
```
x = 2 + 4 * (5-6)
x *= 5
x -= 3
```

## Mathematics
The available operators for mathematics expressions in DBasic are `+`,`-`,`*`,`/`
and `%` for modulo. You can also use parentheses. Divisions are euclidians,
because I have no clue about how floating points work on the binary level,
and I have no desire to dive into that. That also means floating points
number do not exist in DBasic. Like, who even needs floating points ?
Anyways, there's no power operator either, feel free to code a Pow()
function.

Boolean operations are a little bit more varied, don't worry. The available
operators are OR (`|`), XOR (`~`), AND (`&`), NOT (`!`), `<`, `<=`, `>`, `>=`, and `=` for
equalities. Just keep in mind that you can not mix types in boolean
relations, so don't write things like `IF 2 = "2"`, it won't even compile.

The bit-shift operations are also available, and the operators for these
are `<<` and `>>`. For the bitwise logical operations, you can simply use the
designated boolean operators.

Windows Api constants are also available for direct use, by prepending a ##:
`Var1 = ##WM_COMMAND`

Finally, you can directly increment or decrement a variable by one, using
`x++` or `x--`. The code generated for it is far more efficient than the
code for `x = x + 1`.

## Strings
Strings can be stored in variables of type STR. They are enclosed in single
or double quotes:\
`"that's a string"` or `'that is a "string" as well'`

You can also write multi-line strings, and the line break will appear in
them:
```
"that is the first line of the string
and this is the second"
```

You can compare them using `=`, and concatenate them using `&`.

You can also use the `&=` operator to concatenate a string to another with
better performances.

## Arrays
Arrays are declared in DIM statements, just like variables. They are
followed by the number of cells they contain enclosed in square brackets:\
```DIM INT arr[42]```

They can contain either strings or integers. DBasic does not support
multi-dimensional arrays yet.

If you wish to pass an array as an argument to a procedure, you need to
use the keyword ARRAY:
```
CALL FOO(ARRAY bar[])

...

SUB FOO(argument_array[])
	...
ENDSUB
```

Arrays are always passed by reference to procedures. Passing them by value
was overly complex to implement, and it slowed the code a lot compared to
the limited extension of possibilities brought by the value passing

## Control Structures
There are 5 control structures in DBasic, which should cover all your needs.

### IF:
```
IF string = "hello" THEN \The THEN is optional\
	...
ELSEIF string = "hey" \Optional, you can put as many of them as you want\
	...
ELSE \The ELSE part is optional as well\
	...
ENDIF
```

### SELECT:
```
SELECT x \imagine that x is an INT\
	CASE 1
		MsgBox("Good morning,"")
	CASE 2
		MsgBox("Good evening!","")
	CASE ELSE
		MsgBox("Hello!")
ENDSELECT
```

The `CASE ELSE` is optional, and must always be placed in last position. The
same syntax applies for strings.

### WHILE:
```
WHILE x > 5
	...
ENDWHILE
```

You can use the `BREAK` keyword to get out of it at anytime.

### DO
It's the simplest one. It's just an infinite loop:
```
DO
	...
LOOP
```

You can also use `BREAK` to get out of it.

### REPEAT
It repeats until the given condition is true.
```
REPEAT
	x++
UNTIL x = 3
```

### GOTO

I also implemented labels and a GOTO instruction, because who knows, it
might be useful to someone. You can define a label and then jump to it like
this:
```
here:
...
GOTO here \Or GOTO(here)\
```

### END
At last, I have no idea where to talk about the END statement, so I'll
just put it here. It immediately ends the program when executed. It is
used like this:
```
END(exit_code)
or
END()
```

The empty parentheses mean a 0 exit code. The exit code is the number
that is returned when another program calls your executable.

## About semicolons
Some programmers are used to semicolons, and some others are not. To make
DBasic suitable for everyone, semicolons are optional. You can put them at
the end of any statements.
```
\Procedures\
You can declare a procedure like this:
SUB foo(x, STR y) : INT \SUB stands for 'subroutine', I took it from VBS\
	...
	RETURN(1)
ENDSUB
```

If the argument list is empty, you must still write parentheses after the
function name. Parentheses are a mandatory part of any function name, meaning
they're also necessary when calling the said function.

Procedure declarations are located after the `MAIN` block of the program.
The colon followed by a type name indicate the return type. The `RETURN`
statement halts the execution of the procedure and returns control to the
caller.

Notice that you have to write the expected type of the arguments, but that
no type checking is done while calling it. Specifying the type is important
nevertheless because it tells the body of the function how to treat them,
and giving the function a wrong type of argument may result in undefined
behavior.

To call a procedure, simply write:
```
foo(1,"hey")
or
CALL foo(1,"hey") \the CALL keyword from VBS looked cool\
```

or, you can directly use the return value of procedures in expressions:\
```x = 1 + foo()```

A procedure doesn't have to include a `RETURN` statement. A procedure
returns 0 by default, if no `RETURN` statement is executed.

Another feature of DBasic is that when you want to give the value 0 to an
argument to ignore it, you can simply write nothing:
```
CALL foo(1,)

SUB foo(x,y)
	...
ENDSUB
```

If you want your function to follow an alternative calling convention, you
can write its name between parentheses right after the argument list:\
```SUB FOO() (STDCALL) : INT```

Right now, the only alternative calling convention available is stdcall.
Note that specifying a calling convention here won't affect the way the
rest of your program calls the procedure, which means that you should only
use this functionality if you are comfortable with the concept. It was intended
to allow programmers to write window callback procedures, which must follow
the stdcall convention.

## Classes
DBasic supports classes. Classes must be declared after ENUMERATIONs, and
are redacted like this:
```
Class class1
	Property prop1
	Property INT prop2
	Property STR prop3
	
	Method Method1(STR msg)
		MsgBox(msg, self.prop3, msgbox_information)
	EndMethod
	
	Method Method2()
		...
	EndMethod
EndClass
```

You can then create instances:
```
Global
	Dim CLASS1 var1, ...
	...
EndGlobal
```

Or
```
Sub foo()
	Dim CLASS1 var1, ...
	...
EndSub
```

The operator `.` is used to access properties and methods:
```
var1.prop3 = "hey!"
var1.Method1("Hey there! :)")
```

## Inline Assembly
DBasic includes inline assembly to allow for optimization and for easy
access to non-implemented features.

The `$` symbol indicates that the rest of the line is in assembly. The code
must be written in Flat Assembly, and in order to use label, procedure or
variable names, you shall prefix them with `V_` and write them in
capital letters:
```
SUB infinite_loop()
	here:
	...
	$JMP V_HERE
ENDSUB
```

If you want to manipulate strings, be aware that they are stored in heap
buffers in the default process heap, and that they are prefixed by a DWORD
containing their length, and that they are null-terminated. Here is an
example, showing how the string 'hello' would be stored:

|0|0|0|5|104|101|108|108|111|0|
|-|-|-|-|---|---|---|---|---|-|

The same goes for arrays: they have an invisible first element containing
their length

To use the heap, you don't have to call `GetProcessHeap` repeatedly; it is
called once at the top of every program, and the default heap handle is
stored in the DWORD `hHeap` variable. That means you can access it by writing
something like this:
```
MOV eax, DWORD [hHeap]
```

Finally, NEVER use inline asm to add in your code a label named in the form
`L0` (where 0 can be any number), or `RETFUNCTIONNAME`, because it's the
forms that are used for labels written by the compiler. It would create
duplicate labels, and throw an assembler error.

## Library writing
Libraries are an essential part of the language. They are the ones that
provide for every needed functionality. That's why you may want to create
your own. Libraries are stored in the `LIBS` folder of the compiler, and are
named `NAME.LIB` by convention, but you may name them however you want. They
are coded in Flat Assembly, and will be included in the executable at
compile time. Here is the template for writing a function:
```
;$SUB TESTFUNCTION(ARG1, ARG2, ARG3) : INT
V_TESTFUNCTION:
	PUSH ebp
	MOV ebp, esp
	
	MOV eax, 1
	
	MOV esp, ebp
	POP ebp
	RET
```

This is a function that takes three arguments, and does nothing but return 1.
It's important that you do not give a type to arguments.
To write a function, the first step is to write its header in a comment
above the code itself. You need to prefix it with `$`, write it all in capital
letters, specify the arguments, and the return type of the function, in
order for it to work correctly with the rest of you code. Then, you need to
write a label in the for `V_FUNCTIONNAME`, all in capital letters. And finally,
open a stack frame and write the code of your function.

To access the arguments, consider that they are pushed in reverse order.
Then, the code is `DWORD [ebp + ' & ArgNumber * 4 + 4 & ']`

You can also declare constants specific to the library, that will be included
in the code if the program is compiled with your library. The syntax is simple:
```
;$CONST SOME_CONSTANT = 1234567890
```

While writing a library, you need to follow the same rules about label names
and string handling as with inline assembly.
