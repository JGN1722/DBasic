# DBasic

Welcome to my biggest project so far, the DBasic programming language !  
DBasic is a BASIC-like, multi-purpose, powerful amateur programming language, 
that empowers programmers to quickly bring their ideas to life.  
This compiler targets x86, 32 or 64bit Windows machines.

## Features
+ Ease of use: DBasic is a high-level language that allows programmers to code efficiently without getting bogged-down in machine-specific complications
+ Object-oriented: Classes allow for clean, reusable, concise code
+ User-friendly: With a BASIC-like, case insensitive syntax, DBasic is very intuitive and easy to read and write
+ Customizable librairies: DBasic libs are plain text-assembly, making them easy to modify and extend, empowering programmers to create their perfect environment
+ Permissive syntax: DBasic syntax is designed to accomodate everyone, and thus includes a lot of optional syntax bits
+ Swift compilation across all windows versions: The compiler is easy to install and engineered for speed, produces tight code, and is written in VBScript to allow it to run even on the oldest windows versions
+ Easy access to unimplemented features: DBasic implements inline assembly to allow programmers to directly write in machine code with FAsm syntax to unlock the full power of their machine within DBasic
+ Hand-optimizable: DBasic shows the compiled assembly as plain text in output.asm, allowing the programmer to modify it directly, and reassemble it. Inline assembly can also be used to optimize bottlenecks and critical functions

## Getting started
To get started with DBasic, follow these simple steps:
1. Download this repository
2. Explore the documentation and example files
3. Compile your code simply by specifiying the librairies to use in LIBS.INI, and running the compiler via the command line
4. You can now find your executable in the same folder as the source !

## Example
Here's a Hello world in DBasic:
```
Main
    MsgBox("Hello, world!","Hello from DBasic",msgbox_information)
EndMain
```
And here's how to open a basic window:
```
Main
	Dim WindowID
	Dim PanelID
	WindowID = OpenWindow(100,100,600,300,"Template program",@WindowCallback)
	
	Repeat Until HandleWindowEvents() = 0
EndMain

Sub WindowCallback(lparam,wparam,wmsg,hwnd) (stdcall)
	Select wmsg
		Case #WM_DESTROY
			PostQuitMessage(0)
		Case Else
			return(DefaultMessageHandling(lparam,wparam,wmsg,hwnd))
	EndSelect
EndSub
```

Happy coding, and thank you for choosing DBasic :)
