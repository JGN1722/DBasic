'//DBasic Compiler
Option Explicit
Dim Shell:Set Shell = CreateObject("WScript.Shell")
Dim Fso:Set Fso = CreateObject("Scripting.FileSystemObject")
Dim ST :Set ST  = CreateObject("Scripting.Dictionary")
Dim STP:Set STP = CreateObject("Scripting.Dictionary")

Dim FormalParamST:Set FormalParamST = CreateObject("Scripting.Dictionary")
Dim NumParams

Dim ErrLn

'---------------------------------------------------------------------
'{Definition Of Keywords And Token Types}
Dim KWList
KWList = Array(	"IF","THEN","ELSE","ENDIF","WHILE","LOOP","DIM",_
		"GLOBAL","ENDGLOBAL","MAIN","ENDMAIN","DO","LOOP",_
		"REPEAT","UNTIL","BREAK","SELECT","CASE","ENDSELECT",_
		"SUB","ENDSUB","RETURN","END")
Dim KWCode:KWCode=Array("x","i","t","l","e","w","e","d","g","e","m",_
			"e","p","e","r","e","b","s","c","e","u","e",_
			"f","E")

'---------------------------------------------------------------------
'{Variable Declarations}
Dim CodeOutput
Dim DataOutput

CodeOutput =	""

DataOutput =	""

Dim Look	'Lookahead Character
Dim Token	'Encoded Token
Dim Value	'Unencoded Token

Dim StreamPos	'Current Position in stream
Dim Text	'Input Stream

Dim LCount	'Generated Labels Count

Sub Init
	Dim FileName
	If WScript.Arguments.Count = 0 Then
		Abort("No File Name Provided")
	End If
	FileName = WScript.Arguments.Item(0)
	If Not Fso.GetFile(FileName).Size = 0 then
		Text = Fso.OpenTextFile(FileName).ReadAll
	Else
		WScript.Quit
	End If
	StreamPos = 1
	LCount = 0
	ErrLn = 1
	
	GetChar
	Next1
End Sub

'---------------------------------------------------------------------
MainCode
WriteOutput
Assemble

Sub MainCode
	Init
	Prog
End Sub

Sub dbg
	msgbox "token: "&token&vbcrlf&"value: "&value
End Sub

'---------------------------------------------------------------------
'{Write Output To File}
Sub WriteOutput
	Dim File:Set File = 	Fso.OpenTextFile(_
				left(wscript.scriptfullname,len(_
				wscript.scriptfullname)-len(_
				wscript.scriptname)) & "output.asm",2)
	If DataOutput <> "" Then
		DataOutput = "section '.data' data readable writeable" & VbCrLf & DataOutput
	End If
	File.Write 	CodeOutput & VbCrLf &_
			DataOutput
	File.Close
End Sub

'---------------------------------------------------------------------
Sub Assemble
	Dim CommandLine,OutFileName
	If Mid(WScript.Arguments.Item(0),2,2) =":\" Then
		CommandLine = Fso.GetParentFolderName(WScript.ScriptFullName) & "\fasm\FAsm.exe " & Fso.GetParentFolderName(WScript.ScriptFullName) & "\output.asm " & WScript.Arguments.Item(0) & ".exe"
	Else
		CommandLine = Fso.GetParentFolderName(WScript.ScriptFullName) & "\fasm\FAsm.exe " & Fso.GetParentFolderName(WScript.ScriptFullName) & "\output.asm " & Shell.CurrentDirectory & "\" & WScript.Arguments.Item(0) & ".exe"
	End If
	MsgBox ExecStdOut(CommandLine),VbInformation, "Assembler Output"
End Sub

Function execStdOut(cmd)
	Dim Output,i
	Dim aRet: Set aRet = Shell.exec(cmd)
	Do While aRet.Status = 0
		WScript.Sleep 100
	Loop
	execStdOut = aRet.StdOut.ReadAll() & VbCrLf & aRet.StdErr.ReadAll()
End Function 

'---------------------------------------------------------------------
'{Read New Character From Input Stream And Eliminate Comments}
Sub GetChar
	Look = Mid(Text,StreamPos,1)
	StreamPos = StreamPos + 1
End Sub

'---------------------------------------------------------------------
Sub Warning(s)
	MsgBox "Warning: " & s & ".",VbExclamation,"Warning (Ln: " & ErrLn & ")"
End Sub

'---------------------------------------------------------------------
'{Report An Error}
Sub Error(s)
	MsgBox "Error: " & s & ".",VbCritical,"Error (Ln: " & ErrLn & ")"
End Sub

'---------------------------------------------------------------------
'{Report Error And Halt}
Sub Abort(s)
	Error(s)
	WScript.Quit
End Sub

'---------------------------------------------------------------------
'{Report What Was Expected}
Sub Expected(s)
	Abort(s & " Expected (Instead Of " & Value & ")")
End Sub

'---------------------------------------------------------------------
Sub Undefined(n)
	Abort("Undefined Identifier (" & n & ")")
End Sub

'---------------------------------------------------------------------
Sub Duplicate(n)
	Abort("Duplicate Identifier (" & n & ")")
End Sub

'---------------------------------------------------------------------
'{Check To Make Sure The Current Token Is An Identifier}
Sub CheckIdent
	If Token <> "x" Then Expected("Identifier")
End Sub

'---------------------------------------------------------------------
'{Recognize An Alpha Character}
Function IsAlpha(c)
	If c = "" Then
		IsAlpha = False
		Exit Function
	End If
	If InStr("ABCDEFGHIJKLMNOPQRSTUVWXYZ_",UCase(c)) Then
		IsAlpha = True
	Else
		IsAlpha = False
	End If
End Function

'---------------------------------------------------------------------
'{Recognize A Decimal Digit}
Function IsDigit(c)
	If c = "" Then
		IsDigit = False
		Exit Function
	End If
	If InStr("0123456789",c) Then
		IsDigit = True
	Else
		IsDigit = False
	End If
End Function

'---------------------------------------------------------------------
'{Recognize An Alphanumeric}
Function IsAlNum(c)
	IsAlNum = IsAlpha(c) Or IsDigit(c)
End Function

'---------------------------------------------------------------------
'{Recognize An Addop}
Function IsAddop(c)
	If c = "+" Or c = "-" Then
		IsAddop = True
	Else
		IsAddop = False
	End If
End Function

'---------------------------------------------------------------------
'{Recognize An Addop}
Function IsMulop(c)
	If c = "*" Or c = "/" Then
		IsMulop = True
	Else
		IsMulop = False
	End If
End Function

'---------------------------------------------------------------------
'{Recognize An Orop}
Function IsOrop(c)
	If c = "|" Or c = "~" Then
		IsOrop = True
	Else
		IsOrop = False
	End If
End Function

'---------------------------------------------------------------------
'{Recognize A Relop}
Function IsRelop(c)
	If c = "=" Or c = "#" Or c = "<" Or c = ">" Then
		IsRelop = True
	Else
		IsRelop = False
	End If
End Function

'---------------------------------------------------------------------
'{Recognize White Space}
Function IsWhite(c)
	If c = " " Or c = Chr(9) Or c = Chr(13) Or c = Chr(10) Then
		IsWhite = True
	Else
		IsWhite = False
	End If
End Function

'---------------------------------------------------------------------
'{Skip White Space}
Sub SkipWhite
	Do While IsWhite(Look)
		If Look = Chr(10) Then : ErrLn = ErrLn + 1 : End If
		GetChar
	Loop
End Sub

'---------------------------------------------------------------------
Sub SkipComments
	Do
		GetChar
		If Look = "*" Then
			GetChar
			If Look = "/" Then
				GetChar
				Exit Do
			End If
		End If
	Loop
End Sub

'---------------------------------------------------------------------
'{Table Lookup}
Function Lookup(table,s,n)
	Dim i:i=n
	Dim found:found = false
	Do While (i >= 0) And Not Found
		If s = table(i) Then
			Found = True
		Else
			i = i - 1
		End If
	Loop
	Lookup = i
End Function

'---------------------------------------------------------------------
'{Look For Symbol In Table}
Function InTable(n)
	InTable = ST.Exists(n)
End Function

Function InTableP(n)
	InTableP = STP.Exists(n)
End Function

'---------------------------------------------------------------------
'{Check If Identifier Is In The ST}
Sub CheckTable(n)
	If Not InTable(n) And Not InTableP(n) And Not IsParam(n) Then Undefined(n)
End Sub

'---------------------------------------------------------------------
Sub CheckDup(n)
	If InTable(n) Then
		Duplicate(n)
	End If
End Sub

'---------------------------------------------------------------------
'{Add New Entry To The Symbol Table}
Sub AddEntry(n,t)
	If InTable(n) Then Abort("Duplicate Identifier (" & n & ")")
	ST.Add n,t
End Sub

'---------------------------------------------------------------------
'{Add New Entry To The Symbol Table}
Sub AddEntryP(n,t)
	If InTableP(n) Then Abort("Duplicate Identifier (" & n & ")")
	STP.Add n,t
End Sub

'---------------------------------------------------------------------
'{Get An Identifier}
Sub GetName
	SkipWhite
	If Not IsAlpha(Look) Then Expected("Identifier")
	Token = "x"
	Value = ""
	Do
		Value = Value & UCase(Look)
		GetChar
	Loop Until Not IsAlnum(Look)
End Sub

'---------------------------------------------------------------------
'{Get A Number}
Sub GetNum
	SkipWhite
	Token = "#"
	Value = ""
	If Not IsDigit(Look) Then Expected("Integer")
	Do
		Value = Value & Look
		GetChar
	Loop Until Not IsDigit(Look)
End Sub

'---------------------------------------------------------------------
'{Get An Operator}
Sub GetOp
	SkipWhite
	Token = Look
	Value = Look
	GetChar
End Sub

'---------------------------------------------------------------------
'{Get The Next Input Token}
Sub Next1
	SkipWhite
	If IsAlpha(Look) Then
		GetName
	ElseIf IsDigit(Look) Then
		GetNum
	Else
		GetOp
	End If
End Sub

'---------------------------------------------------------------------
'{Get An Identifier And Scan It For Keywords}
Sub Scan
	If Token = "x" Then
		Token = KWCode(Lookup(KWList,Value,UBound(KWList))+1)
	End If
End Sub

'---------------------------------------------------------------------
'{Match A Specific Input String}
Sub MatchString(x)
	If Value <> x Then Expected(x)
	Next1
End Sub

'---------------------------------------------------------------------
'{OutPut A String With CrLf}
Sub EmitLn(s)
	CodeOutput = CodeOutput & s & VbCrLf
End Sub

'---------------------------------------------------------------------
'{Output A String With Crlf To Data Section}
Sub EmitLnD(s)
	DataOutput = DataOutput & s & VbCrLf
End Sub

'---------------------------------------------------------------------
'{Generate A Unique Label}
Function NewLabel
	NewLabel = "L" & LCount
	LCount = LCount + 1
End Function

'---------------------------------------------------------------------
'{Post A Label To Ouput}
Sub PostLabel(l)
	EmitLn(l & ":")
End Sub

'---------------------------------------------------------------------
'{Parse And Translate A Program}
Sub Prog
	Header
	TopDecls
	Main
End Sub

'---------------------------------------------------------------------
'{Write Header Info}
Sub Header
	EmitLn(	"format PE GUI 4.0" & VbCrLf &_
		"entry start" & VbCrLf &_
		"include 'win32a.inc'" & VbCrLf &_
		VbCrLf &_
		"section '.text' code readable executable" & VbCrLf)
End Sub

'---------------------------------------------------------------------
'{Write The Prolog}
Sub Prolog
	PostLabel("start")
End Sub

'---------------------------------------------------------------------
'{Write The Epilog}
Sub Epilog
	EmitLn("invoke ExitProcess,0" & VbCrLf)
End Sub

'---------------------------------------------------------------------
Sub Footer
	EmitLn(	"section '.idata' import data readable writeable" & VbCrLf &_
		"library kernel32,'KERNEL32.DLL',\" & VbCrLf &_
		"	 user32,'USER32.DLL'" & VbCrLf &_
		"include 'api\kernel32.inc'" & VbCrLf &_
		"include 'api\user32.inc'")
End Sub
'---------------------------------------------------------------------
'{Parse And Translate A Main Program}
Sub Main
	MatchString("MAIN")
	Prolog
	Block "",""
	MatchString("ENDMAIN")
	Epilog
	ProcDecl
	Footer
End Sub

'---------------------------------------------------------------------
'{Allocate Storage For A Static Variable}
Sub Allocate(n, Val)
	EmitLnD(n & " dd " & Val)
End Sub

'---------------------------------------------------------------------
'{Allocate Storage For A Variable}
Sub Alloc
	Next1
	Dim n:n = Value
	If Token <> "x" Then Expected("Variable Name")
	CheckDup(n)
	AddEntry n, "v"
	Next1
	If Token = "=" Then
		Next1
		If Token = "-" Then
			GetNum
			Allocate n, -Value
		Else
			If Token <> "#" Then Expected("Integer")
			Allocate n, Value
		End If
		Next1
	Else
		Allocate n, "0"
	End If
End Sub

'---------------------------------------------------------------------
'{Process A Data Declaration}
Sub Decl
	GetName
	Alloc
	Do While Look = ","
		Match(",")
		GetName
		Alloc
	Loop
End Sub

'---------------------------------------------------------------------
'{Parse And Translate Global Declarations}
Sub TopDecls
	Scan
	If Token = "g" Then
		MatchString("GLOBAL")
		Scan
		Do While Token = "d"
			Alloc
			Do While Token = ","
				Alloc
			Loop
			Scan
		Loop
		MatchString("ENDGLOBAL")
	End If
End Sub

'---------------------------------------------------------------------
Sub ProcDecl
	Scan
	Do
		Select Case Token
			Case "u" : DoSub : Scan
			'Case "f" : 'soon
			'Case "E" : 'soon
			'Case Else: Expected("Sub Or Function")
			Case Else: Exit Do
		End Select
	Loop
End Sub

'---------------------------------------------------------------------
'{Parse And Tanslate An Assignement Statement}
Sub Assignement(n)
	CheckTable(n)
	MatchString("=")
	BoolExpression
	If IsParam(n) Then
		StoreParam(ParamNumber(n))
	Else
		Store(n)
	End If
End Sub

'---------------------------------------------------------------------
'{Parse And Translate A Block Of Statement}
Sub Block(L,Ret)
	Dim n
	Scan
	Do While Token <> "l" And Token <> "e" And Token <> "c"
		Select Case Token
			Case "i" :DoIf L,Ret
			Case "s" :DoSelect L,Ret
			Case "w" :DoWhile Ret
			Case "p" :DoLoop Ret
			Case "r" :DoRepeat Ret
			Case "b" :DoBreak L
			Case "$" :InlineAsm
			Case "f" :DoReturn(Ret)
			Case "E" :DoEnd
			Case Else:
				n = Value
				Next1
				If Token = "=" Then
					Assignement(n)
				ElseIf Token = "(" Then
					CallProc(n)
				Else
					Expected("Identifier")
				End If
		End Select
		Scan
	Loop
End Sub

'---------------------------------------------------------------------
Sub CallProc(n)
	dim nbytes
	nbytes = ParamList
	JmpToProc(n)
	CleanStack(nbytes)
End Sub

'---------------------------------------------------------------------
Function ParamList
	Dim n:n = 0
	MatchString("(")
	If Token <> ")" Then
		Param
		n = n + 4
		Do While Token = ","
			MatchString(",")
			Param
			n = n + 4
		Loop
	End If
	MatchString(")")
	ParamList = n
End Function

'---------------------------------------------------------------------
Sub Param
	If Token = "," Or Token = ")" Then
		Clear
		Push
	Else
		Expression
		Push
	End If
End Sub

'---------------------------------------------------------------------
Sub InlineAsm
	Dim Line
	Do Until Look = Chr(13)
		Line = Line & Look
		GetChar
	Loop
	EmitLn(Line)
	Next1
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Math Expressions}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Parse And Translate A Math Factor}
Sub Factor
	Dim n
	If Token = "(" Then
		Next1
		BoolExpression
		MatchString(")")
	Else
		If Token = "x" Then
			n = Value
			Next1
			If Token = "(" Then
				CallProc(n)
			Else
				If IsParam(n) Then
					LoadParam(ParamNumber(n))
				Else
					LoadVar(n)
				End If
			End If
		ElseIf Token = "#" Then
			LoadConst(Value)
			Next1
		Else
			Expected("Math Factor")
		End If
	End If
End Sub

'{Parse And Translate A Negative Factor}
Sub NegFactor
	Match("-")
	If IsDigit(Look) Then
		LoadConst("-" & GetNum)
	Else
		Factor
		Negate
	End If
End Sub

'{Recognize And Translate A Multiply}
Sub Multiply
	Next1
	Factor
	PopMul 
End Sub

'{Recognize And Translate A Divide}
Sub Divide
	Next1
	Factor
	PopDiv
End Sub

'{Parse And Translate A Math Term}
Sub Term
	Factor
	Do While IsMulop(Token)
		Push
		Select Case Token
			Case "*":Multiply
			Case "/":Divide
		End Select
	Loop
End Sub

'{Recognize And Translate An Add}
Sub Add
	Next1
	Term
	PopAdd
End Sub

'{Recognize And Translate A Subtract}
Sub Subtract
	Next1
	Term
	PopSub
End Sub

'{Parse And Translate An Expression}
Sub Expression
	If IsAddOp(Token) Then
		Clear
	Else
		Term
	End If
	Do While IsAddop(Token)
		Push
		Select Case Token
			Case "+":Add
			Case "-":Subtract
		End Select
	Loop
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Boolean Expressions}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub CompareExpression
	Expression
	PopCompare
End Sub

Sub NextExpression
	Next1
	CompareExpression
End Sub

'{Recognize And Translate A Relational "Equals"}
Sub Equal
	NextExpression
	SetEqual
End Sub

Sub LessOrEqual
	NextExpression
	SetLessOrEqual
End Sub

'{Recognize And Translate A Relational "Not Equals"}
Sub NotEqual
	NextExpression
	SetNEqual
End Sub

'{Recognize And Translate A Relational "Less Than"}
Sub Less
	Next1
	Select Case Token
		Case "=" :LessOrEqual
		Case ">" :NotEqual
		Case Else
			CompareExpression
			SetLess
	End Select
End Sub

'{Recognize And Translate A Relational "Greater Than"}
Sub Greater
	Next1
	If Token = "=" Then
		NextExpression
		SetGreaterOrEqual
	Else
		CompareExpression
		SetGreater
	End If
End Sub

'{Parse And Translate A Relation}
Sub Relation
	Expression
	If IsRelop(Token) Then
		Push
		Select Case Token
			Case "=":Equal
			Case "#":NotEqual
			Case "<":Less
			Case ">":Greater
		End Select
	End If
End Sub

'{Parse And Translate A Boolean Facto With A Leading NOT}
Sub NotFactor
	If Look = "!" Then
		Next1
		Relation
		NotIt
	Else
		Relation
	End If
End Sub

'{Parse And Translate A Boolean Term}
Sub BoolTerm
	NotFactor
	Do While Token = "&"
		Push
		Next1
		NotFactor
		PopAnd
	Loop
End Sub

'{Recognize And Translate A Boolean OR}
Sub BoolOr
	Next1
	BoolTerm
	PopOr
End Sub

'{Recognize And Translate A Boolean XOR}
Sub BoolXor
	Next1
	BoolTerm
	PopXor
End Sub

'{Parse And Translate A Boolean Expression}
Sub BoolExpression
	BoolTerm
	Do While IsOrop(Token)
		Push
		Select Case Token
			Case "|":BoolOr
			Case "~":BoolXor
		End Select
	Loop
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Control Structures}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub DoIf(L,Ret)
	Dim L1, L2
	MatchString("IF")
	BoolExpression
	MatchString("THEN")
	L1 = NewLabel
	L2 = L1
	BranchFalse(L1)
	Block L,Ret
	If Token = "l" Then
		MatchString("ELSE")
		L2 = NewLabel
		Branch(L2)
		PostLabel(L1)
		Block L,Ret
	End If
	MatchString("ENDIF")
	PostLabel(L2)
End Sub

Sub DoSelect(L,Ret)
	Dim Name, L1
	MatchString("SELECT")
	L1 = NewLabel
	Name = Value
	LoadVar(Name)
	Next1
	Scan
	'MsgBox "token: "&token&vbcrlf&"value: "&value
	Do While Token = "c"
		MatchString("CASE")
		Scan
		If Token = "l" Then
			MatchString("ELSE")
			Block L,Ret
			Branch(L1)
			Exit Do
		Else
			If Token = "-" Then
				GetNum
				Compare(-Value)
			Else
				If Token = "#" Then
					Compare(Value)
				ElseIf Token = "x" Then
					CompareVar(Value)
				Else
					Expected("Integer Or Identifier")
				End If
			End If
			BranchIfFalse("@f")
			Next1
			Block L,Ret
			Branch(L1)
			PostLabel("@@")
			Scan
		End If
	Loop
	MatchString("ENDSELECT")
	PostLabel(L1)
End Sub

Sub DoWhile(Ret)
	Dim L1, L2
	MatchString("WHILE")
	L1 = NewLabel
	L2 = NewLabel
	PostLabel(L1)
	BoolExpression
	BranchFalse(L2)
	Block L2,Ret
	MatchString("LOOP")
	Branch(L1)
	PostLabel(L2)
End Sub

Sub DoLoop(Ret)
	Dim L1,L2
	MatchString("DO")
	L1 = NewLabel
	L2 = NewLabel
	PostLabel(L1)
	Block L2,Ret
	MatchString("LOOP")
	Branch(L1)
	PostLabel(L2)
End Sub

Sub DoRepeat(Ret)
	Dim L1,L2
	MatchString("REPEAT")
	L1 = NewLabel
	L2 = NewLabel
	PostLabel(L1)
	Block L2,Ret
	MatchString("UNTIL")
	BoolExpression
	BranchFalse(L1)
	PostLabel(L2)
End Sub

Sub DoBreak(L)
	MatchString("BREAK")
	If L <> "" Then
		Branch(L)
	Else
		Warning("No Loop To Break From")
	End If
End Sub

Sub DoEnd
	EndProgram
	Next1
End Sub


'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Procedure Handling}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub DoSub
	Dim Name,k,L1
	MatchString("SUB")
	Name = Value
	L1 = NewLabel
	Next1
	MatchString("(")
	FormalList
	MatchString(")")
	If InTableP(Name) Then Duplicate(Name)
	AddEntryP Name,"p"
	PostLabel(Name)
	k = LocDecls
	LocAlloc(k)
	ProcHeader
	Block "",L1
	LocFree k,L1
	Return
	MatchString("ENDSUB")
	ClearParams
End Sub

Sub FormalList
	If Token <> ")" Then
		FormalParam
		Do While Token = ","
			MatchString(",")
			FormalParam
		Loop
	End If
End Sub

Sub FormalParam
	AddParam(Value)
	Next1
End Sub

'---------------------------------------------------------------------
Function LocDecls
	Dim n:n = 0
	Scan
	If Token = "d" Then
		MatchString("DIM")
		LocDecl
		n = n + 1
		Do While Token = ","
			MatchString(",")
			LocDecl
			n = n + 1
		Loop
	End If
	LocDecls = n
End Function

Sub LocDecl
	AddParam(Value)
	Next1
End Sub

Sub ClearParams
	FormalParamST.RemoveAll
End Sub

Function ParamNumber(n)
	ParamNumber = FormalParamST.Item(n)
End Function

Function IsParam(n)
	IsParam = FormalParamST.Exists(n)
End Function

Sub AddParam(n)
	If IsParam(n) Then Duplicate(n)
	NumParams = NumParams + 1
	FormalParamST.Add n, NumParams
End Sub

Sub DoReturn(Ret)
	If Ret = "" Then Abort("RETURN Outside Of A Procedure")
	MatchString("RETURN")
	BoolExpression
	Branch(Ret)
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Math Code Generation Routines}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Clear The Primary Register}
Sub Clear
	EmitLn("MOV eax, 0")
End Sub

'{Negate The Primary Register}
Sub Negate
	EmitLn("NEG eax")
End Sub

'{Load Constant Value To Primary Register}
Sub LoadConst(n)
	EmitLn("MOV eax, " & n)
End Sub

'{Load Variable To Primary Register}
Sub LoadVar(n)
	If Not InTable(n) Then Undefined(n)
	EmitLn("MOV eax, [" & n & "]")
End Sub

'{Push Primary Register Onto Stack}
Sub Push
	EmitLn("PUSHD eax")
End Sub

'{Add Top Of The Stack To Primary}
Sub PopAdd
	EmitLn("ADD eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

'{Subtract Primary From Top Of The Stack}
Sub PopSub
	EmitLn("SUB eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
	EmitLn("NEG eax")
End Sub

'{Multiply Top Of Stack By Primary}
Sub PopMul
	EmitLn("IMUL DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

'{Divide Top Of Stack By Primary}
Sub PopDiv
	EmitLn("MOV ebx, DWORD [esp]")
	EmitLn("ADD esp, 4")
	EmitLn("XCHG eax, ebx")
	EmitLn("XOR edx, edx")
	EmitLn("IDIV ebx")
End Sub

'{Store Primary To Variable}
Sub Store(n)
	If Not InTable(n) Then Undefined(n)
	EmitLn("MOV [" & n & "], eax")
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Boolean Code Generation Routines}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Complement The Primary Register}
Sub NotIt
	EmitLn("NOT eax")
End Sub

'{AND Top Of The Stack With Primary Register}
Sub PopAnd
	EmitLn("AND eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

'{OR Top Of The Stack With Primary Register}
Sub PopOr
	EmitLn("OR eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

'{XOR Top Of The Stack With Primary Register}
Sub PopXor
	EmitLn("XOR eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

'{Compare Top Of The Stack With Primary Register}
Sub PopCompare
	EmitLn("MOV ebx, DWORD [esp]")
	EmitLn("ADD esp, 4")
	EmitLn("CMP ebx, eax")
End Sub

Sub Compare(n)
	EmitLn("CMP eax, " & n)
End Sub

Sub CompareVar(n)
	If Not InTable(n) Then Undefined(n)
	EmitLn("CMP eax, [" & n & "]")
End Sub

'{Set eax If Compare Was =}
Sub SetEqual
	EmitLn("MOV eax, 0")
	EmitLn("SETE al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

'{Set eax If Compare Was !=}
Sub SetNEqual
	EmitLn("MOV eax, 0")
	EmitLn("SETNE al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

'{Set eax If Compare Was >}
Sub SetGreater
	EmitLn("MOV eax, 0")
	EmitLn("SETA al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

'{Set eax If Compare Was <}
Sub SetLess
	EmitLn("MOV eax, 0")
	EmitLn("SETB al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

Sub SetLessOrEqual
	EmitLn("MOV eax, 0")
	EmitLn("SETBE al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

Sub SetGreaterOrEqual
	EmitLn("MOV eax, 0")
	EmitLn("SETAE al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Control Structures Code Generation Routines}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Branch Unconditional}
Sub Branch(l)
	EmitLn("JMP " & l)
End Sub

'{Branch If False}
Sub BranchFalse(l)
	EmitLn("TEST eax, eax")
	EmitLn("JZ " & l)
End Sub

'{Branch If Last Compare Was False}
Sub BranchIfFalse(l)
	EmitLn("JNE " & l)
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'{Proc Handling Code Generation Routines}
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub JmpToProc(n)
	EmitLn("CALL " & n)
End Sub

Sub ProcHeader
	EmitLn("PUSH ebp")
	EmitLn("MOV ebp, esp")
End Sub

Sub Return
	EmitLn("RET")
End Sub

Sub LoadParam(n)
	Dim Offset:Offset = ((NumParams + 2) * 4) - (n * 4)
	EmitLn("MOV eax, DWORD [ebp + " & Offset & "]")
End Sub

Sub StoreParam(n)
	Dim Offset:Offset = ((NumParams + 2) * 4) - (n * 4)
	EmitLn("MOV DWORD [ebp + " & Offset & "], eax")
End Sub

Sub CleanStack(n)
	If n > 0 Then
		EmitLn("ADD esp, " & n)
	End If
End Sub

Sub LocAlloc(k)
	If k > 0 Then
		EmitLn("MOV ebx, DWORD [esp]")
		EmitLn("MOV DWORD [esp], 0")
		EmitLn("SUB esp, " & (k - 1) * 4)
		EmitLn("PUSHD ebx")
	End If
End Sub

Sub LocFree(k,L)
	EmitLn("MOV eax, 0")
	PostLabel(L)
	EmitLn("MOV esp, ebp")
	EmitLn("POP ebp")
	If k > 0 Then
		EmitLn("MOV ebx, DWORD [esp]")
		EmitLn("ADD esp, " & k * 4 + 4)
		EmitLn("PUSHD ebx")
	End If
End Sub

Sub EndProgram
	EmitLn("invoke ExitProcess,0")
End Sub