'//DBasic Compiler
Option Explicit
Dim Shell:Set Shell = CreateObject("WScript.Shell")
Dim Fso:Set Fso = CreateObject("Scripting.FileSystemObject")
Dim ST :Set ST  = CreateObject("Scripting.Dictionary")
'<LabelName> :: Array(<p|v|a>, <returntype|type|type>, <argnumber|str_initialized|initial_len>)

Dim FormalParamST:Set FormalParamST = CreateObject("Scripting.Dictionary")
'<Name> :: Array(<ParamNumber>, <type>, <v|a>, <str_initialized|initial_len>)
Dim StringTable:Set StringTable = CreateObject("Scripting.Dictionary")
Dim NumParams

Dim ClassST:Set ClassST = CreateObject("Scripting.Dictionary")
'<Name> :: <Dict>
'-> Dict: <Name> :: <method|property>, <...|property_count>, ....

Dim CharLib,CounterLib,ValueLib,TextLib

Dim ErrLn
Dim DebugMode
Dim PassNumber

'---------------------------------------------------------------------
'Definition Of Keywords And Token Types
Dim KWList
KWList = Array(	"IF","THEN","ELSEIF","ELSE","ENDIF","WHILE","LOOP","DIM",_
		"GLOBAL","ENDGLOBAL","MAIN","ENDMAIN","DO","LOOP",_
		"REPEAT","UNTIL","BREAK","SELECT","CASE","ENDSELECT",_
		"SUB","ENDSUB","RETURN","END","GOTO","INT","STR",_
		"OR","XOR","AND","NOT","CALL","INIT","ENDINIT","METADATA",_
		"ENDMETADATA","ENUMERATION","ENDENUMERATION","CLASS","ENDCLASS",_
		"PROPERTY","METHOD","ENDMETHOD","SET")
Dim EndKeywords:EndKeywords = "|ENDINIT|ENDMAIN|" &_
		"LOOP|UNTIL|ENDIF|ENDSELECT|ENDSUB|CASE|" &_
		"ELSE|ELSEIF|ENDMETHOD|"

'---------------------------------------------------------------------
'Constants Declarations
Dim Constants:Set Constants = CreateObject("Scripting.Dictionary")
Constants.Add "TRUE", Array("INT",-1)
Constants.Add "FALSE", Array("INT",0)
Constants.Add "MEANING_OF_LIFE", Array("INT",42)

'---------------------------------------------------------------------
'Variable Declarations
Dim CodeOutput,DataOutput,RessourceOutput

Dim Look, Token, Value

Dim StreamPos	'Current Position in stream
Dim Text		'Input Stream

Dim LCount		'Generated Labels Count

'---------------------------------------------------------------------
'Main Code
ForceConsole
Initialize
Prog
WriteOutput
Assemble

Sub ForceConsole
	Dim l,i,strArg
	If InStr(LCase(WScript.FullName), "cscript.exe") = 0 Then
		l = "CScript.exe " & Chr(34) & WScript.ScriptFullName & Chr(34)
		For each strArg in WScript.Arguments
			l = l & " " & strArg
		Next
		Shell.Run l
		WScript.Quit
	End If
End Sub

Sub Initialize
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
	If WScript.Arguments.Count > 1 Then
		If WScript.Arguments.Item(1) = "-d" Then
			DebugMode = 1
		End If
	End If
	StreamPos = 1
	LCount = 0
	ErrLn = 1
	
	WScript.Echo "Compiling..."
	
	GetChar
	Next1
End Sub

Sub dbg
	msgbox "token: " & token & vbcrlf & "value: " & value
End Sub

'---------------------------------------------------------------------
'Assembly And Output
Sub WriteOutput
	Dim File:Set File = 	Fso.OpenTextFile(_
				left(wscript.scriptfullname,len(_
				wscript.scriptfullname)-len(_
				wscript.scriptname)) & "output.asm",2)
	If DataOutput <> "" Then
		DataOutput = "section '.data' data readable writeable" & VbCrLf & DataOutput
	End If
	File.Write 	CodeOutput & VbCrLf &_
			DataOutput & VbCrLf &_
			RessourceOutput
	File.Close
End Sub

Sub Assemble
	Dim CommandLine,OutFileName,CurrentPath,StdErrResult
	CurrentPath = Fso.GetParentFolderName(WScript.ScriptFullName)
	If Mid(WScript.Arguments.Item(0),2,1) = ":" Then
		CommandLine = CurrentPath & "\fasm\FAsm.exe " & CurrentPath & "\output.asm " & Left(WScript.Arguments.Item(0),InStr(WScript.Arguments.Item(0),".") - 1) & ".exe"
	Else
		CommandLine = CurrentPath & "\fasm\FAsm.exe " & CurrentPath & "\output.asm " & Shell.CurrentDirectory & "\" & Left(WScript.Arguments.Item(0),InStr(WScript.Arguments.Item(0),".") - 1) & ".exe"
	End If
	StdErrResult = ExecStdOut(CommandLine)
	If StdErrResult <> "" Then
		WScript.Echo StdErrResult
	Else
		WScript.Echo "Compilation successful"
	End If
End Sub

Function execStdOut(cmd)
	Dim Output,i
	Dim aRet: Set aRet = Shell.exec(cmd)
	Do While aRet.Status = 0
		WScript.Sleep 100
	Loop
	execStdOut = aRet.StdErr.ReadAll()
End Function 

'---------------------------------------------------------------------
'Cradle Subs
Sub GetChar
	Look = Mid(Text,StreamPos,1)
	StreamPos = StreamPos + 1
End Sub

Sub Warning(s)
	WScript.Echo "(Ln: " & ErrLn & ") Warning: " & s & "."
End Sub

Sub Abort(s)
	WScript.Echo "(Ln: " & ErrLn & ")Error: " & s & "."
	WScript.Quit
End Sub

Sub Expected(s)
	Abort("'" & s & "' expected (instead of '" & Value & "')")
End Sub

Sub Undefined(n)
	Abort("Undefined identifier ('" & n & "')")
End Sub

Sub Duplicate(n)
	Abort("Duplicate identifier ('" & n & "')")
End Sub

Function IsAlpha(c)
	If c = "" Then
		IsAlpha = False
		Exit Function
	End If
	If InStr("ABCDEFGHIJKLMNOPQRSTUVWXYZ_?§",UCase(c)) Then
		IsAlpha = True
	Else
		IsAlpha = False
	End If
End Function

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

Function IsAlNum(c)
	IsAlNum = IsAlpha(c) Or IsDigit(c)
End Function

Function IsAddop(c)
	If c = "+" Or c = "-" Then
		IsAddop = True
	Else
		IsAddop = False
	End If
End Function

Function IsMulop(c)
	If c = "*" Or c = "/" Or c = "%" Then
		IsMulop = True
	Else
		IsMulop = False
	End If
End Function

Function IsOrop(c)
	If c = "|" Or c = "~" Or c = "o" Or c = "X" Then
		IsOrop = True
	Else
		IsOrop = False
	End If
End Function

Function IsRelop(c)
	If c = "=" Or c = "<" Or c = ">" Or c = "!" Then
		IsRelop = True
	Else
		IsRelop = False
	End If
End Function

Function IsWhite(c)
	If c = " " Or c = Chr(9) Or c = Chr(13) Or c = Chr(10) Then
		IsWhite = True
	Else
		IsWhite = False
	End If
End Function

Function IsKeyword(n)
	Dim i
	IsKeyWord = False
	Do While i < UBound(KWList) + 1
		If n = KWList(i) Then
			IsKeyword = True
		End If
		i = i + 1
	Loop
End Function

Sub SkipWhite
	If PassNumber = 2 Then
		Do While IsWhite(Look)
			If Look = Chr(10) Then : ErrLn = ErrLn + 1 : End If
			GetChar
		Loop
	Else
		Do While IsWhite(Look) Or Look = Chr(34) Or Look = "'"
			If Look = Chr(34) Or Look = "'" Then SkipString(Look)
			If Look = Chr(10) Then : ErrLn = ErrLn + 1 : End If
			GetChar
		Loop
	End If
End Sub

Sub SkipString(terminator_type)
	Do
		GetChar
		If Look = terminator_type Then
			Exit Do
		End If
		If Look = "" Then
			Abort("Unmatched '" & Chr(34) & "'")
			Exit Do
		End If
	Loop
End Sub

Function InTable(n)
	InTable = ST.Exists(n)
End Function

Sub CheckTable(n)
	If Not InTable(n) And Not IsParam(n) Then Undefined(n)
End Sub

Sub CheckDup(n)
	If InTable(n) Or Constants.Exists(n) Then
		Duplicate(n)
	End If
End Sub

Sub AddEntry(n,t)
	If InTable(n) Then Abort("Duplicate Identifier (" & n & ")")
	ST.Add n,t
End Sub

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

Sub GetNum
	SkipWhite
	Token = "0"
	Value = ""
	If Not IsDigit(Look) Then Expected("Integer")
	Do
		Value = Value & Look
		GetChar
	Loop Until Not IsDigit(Look)
End Sub

Sub GetOp
	SkipWhite
	Token = Look
	Value = Look
	GetChar
End Sub

Sub GetComment
	Token = "\"
	Value = ""
	GetChar
	Do While Look <> "\"
		If Look = Chr(10) Then ErrLn = ErrLn + 1
		Value = Value & Look
		GetChar
	Loop
	GetChar
End Sub

Sub Next1
	SkipWhite
	If IsAlpha(Look) Then
		GetName
	ElseIf IsDigit(Look) Then
		GetNum
	ElseIf Look = "\" Then
		GetComment
		Next1
	Else
		GetOp
	End If
End Sub

Function GetIdentType(n)
	If IsParam(n) Then
		GetIdentType = FormalParamST.Item(n)(2)
	ElseIf InTable(n) Then
		GetIdentType = ST.Item(n)(0)
	End If
End Function

Function GetDataType(n)
	If IsParam(n) Then
		GetDataType = FormalParamST.Item(n)(1)
	ElseIf InTable(n) Then
		GetDataType = ST.Item(n)(1)
	End If
End Function

Function GetAdditionalInfo(n)
	If IsParam(n) Then
		GetAdditionalInfo = FormalParamST.Item(n)(3)
	ElseIf InTable(n) Then
		GetAdditionalInfo = ST.Item(n)(2)
	End If
End Function

Function SetAdditionalInfo(n,v)
	If IsParam(n) Then
		FormalParamST.Item(n)(3) = v
	ElseIf InTable(n) Then
		ST.Item(n)(2) = v
	End If
End Function

Sub MatchString(x)
	If Value <> x Then Expected(x)
	Next1
End Sub

Sub EmitLn(s)
	CodeOutput = CodeOutput & s & VbCrLf
End Sub

Sub EmitLnD(s)
	DataOutput = DataOutput & s & VbCrLf
End Sub

Sub EmitLnR(s)
	RessourceOutput = RessourceOutput & s & VbCrLf
End Sub

Function NewLabel
	NewLabel = "L" & LCount
	LCount = LCount + 1
End Function

Sub PostLabel(l)
	EmitLn(l & ":")
End Sub

'---------------------------------------------------------------------
'Preparsing subs
Sub PreParseProcedures
	If Value = "METADATA" Then SkipBlock("METADATA")
	If Value = "GLOBAL" Then SkipBlock("GLOBAL")
	Do While Value = "ENUMERATION"
		SkipBlock("ENUMERATION")
	Loop
	Do While Value = "CLASS"
		SkipBlock("CLASS")
	Loop
	If Value = "INIT" Then SkipBlock("INIT")
	SkipBlock("MAIN")
	RegisterProcedures
End Sub

Sub SkipBlock(Name)
	Dim SkippedSymbol, EndKeyword
	EndKeyword = "END" & Name
	Do While Value <> EndKeyword
		Next1
		If Token = "'" Or Token = Chr(34) Then
			SkippedSymbol = Token
			Do While Look <> SkippedSymbol
				GetChar
			Loop
			Next1
		End If
		If Token = "" Then
			Abort("Missing keyword: " & EndKeyword)
		End If
	Loop
	Next1
End Sub

Sub RegisterProcedures
	Dim n, arr(2)
	arr(0) = "procedure"
	Do While Value = "SUB"
		arr(2) = 0
		MatchString("SUB")
		n = Value
		If InTable(n) Then Duplicate(n)
		Next1
		MatchString("(")
		Do While Not Token = ")"
			If Value = "STR" Or Value  = "INT" Then
				Next1
			End If
			arr(2) = arr(2) + 1
			Next1
			If Token = "[" Then
				MatchString("[")
				MatchString("]")
			End If
			If Not Token = ")" Then MatchString(",")
		Loop
		MatchString(")")
		If Token = ":" Then
			MatchString(":")
			If Not Value = "INT" And Not Value = "STR" Then
				Abort("Undefined type (" & Value & ")")
			End If
			arr(1) = Token
			Next1
		Else
			arr(1) = "INT"
		End If
		AddEntry n, arr
		n = ""
		SkipBlock("SUB")
		If Value = "" Then Exit Do
	Loop
	StreamPos = 1
	ErrLn = 1
	Token = ""
	Value = ""
	GetChar
	Next1
End Sub

Sub PreParseLibs
	Dim File,LibText,LibFile,Counter,CurrentChar,LibName,FileName
	If Not Fso.GetFile(Fso.GetParentFolderName(WScript.ScriptFullName) & "\LIBS.INI").Size = 0 Then
		Set File = Fso.OpenTextFile(Fso.GetParentFolderName(WScript.ScriptFullName) & "\LIBS.INI")
		LibText = File.ReadAll
		Counter = 1
		CurrentChar = Mid(LibText,Counter,1)
		Do
			If CurrentChar = Chr(13) Or CurrentChar = "" Then
				FileName = Fso.GetParentFolderName(WScript.ScriptFullName) & "\LIBS\" & LibName
				If LibName <> "" Then
					If Not Fso.GetFile(FileName).Size = 0 Then
						Set LibFile = Fso.OpenTextFile(FileName)
						TextLib = LibFile.ReadAll
						LibFile.Close:Set LibFile = Nothing
						CounterLib = 1
						GetCharLib
						PreParseLib
					End If
				End If
				If CurrentChar = "" Then
					Exit Do
				End If
				LibName = ""
				CurrentChar = ""
				Counter = Counter + 1
			End If
			LibName = LibName & CurrentChar
			Counter = Counter + 1
			CurrentChar = Mid(LibText,Counter,1)
		Loop
	End If
End Sub

Sub PreParseLib
	Dim SubName
	Dim arr(2)
	arr(0) = "procedure"
	Dim n,x
	Do While CharLib <> ""
		If CharLib = "$" Then
			GetCharLib
			SkipWhiteSpaceLib
			GetNameLib
			If ValueLib = "SUB" Then
				SkipWhiteSpaceLib
				GetNameLib
				SubName = ValueLib
				SkipWhiteSpaceLib
				If CharLib = "(" Then
					SkipWhiteSpaceLib
					GetCharLib
					Do While CharLib <> ")"
						If CharLib = "," Then GetCharLib
						GetNameLib
						arr(2) = arr(2) + 1
						SkipWhitespaceLib
					Loop
					GetCharLib
					SkipWhiteSpaceLib
					If CharLib = ":" Then
						GetCharLib
						SkipWhiteSpaceLib
						GetNameLib
						If ValueLib = "STR" Then
							arr(1) = "STR"
						ElseIf ValueLib = "INT" Then
							arr(1) = "INT"
						Else
							arr(1) = ValueLib
						End If
					Else
						arr(1) = "INT"
					End If
					AddEntry SubName, arr
					arr(1) = ""
					arr(2) = 0
				End If
			Else
				If ValueLib = "CONST" Then
					SkipWhitespaceLib
					GetNameLib
					n = UCase(ValueLib)
					SkipWhitespaceLib
					If CharLib = "=" Then
						GetCharLib
						SkipWhitespaceLib
						GetNumLib
						x = ValueLib
						Constants.Add n, Array("INT",x)
					End If
				End If
			End If
		End If
		GetCharLib
	Loop
End Sub

Sub GetNameLib
	If IsAlpha(CharLib) Then
		ValueLib = CharLib
		GetCharLib
		Do While IsAlnum(CharLib)
			ValueLib = ValueLib & CharLib
			GetCharLib
		Loop
	End If
End Sub

Sub GetNumLib
	If IsDigit(CharLib) Then
		ValueLib = CharLib
		GetCharLib
		Do While IsDigit(CharLib)
			ValueLib = ValueLib & CharLib
			GetCharLib
		Loop
	End If
End Sub

Sub GetCharLib
	CharLib = Mid(TextLib,CounterLib,1)
	CounterLib = CounterLib + 1
End Sub

Sub SkipWhiteSpaceLib
	Do While IsWhite(CharLib)
		GetCharLib
	Loop
End Sub

'---------------------------------------------------------------------
'Mandatory And Boring Code
Sub Header
	EmitLn(	"format PE GUI 4.0" & VbCrLf &_
		"entry start" & VbCrLf &_
		"include 'fasm\include\win32a.inc'" & VbCrLf &_
		VbCrLf &_
		"section '.text' code readable writeable executable" & VbCrLf)
End Sub

Sub WriteMetadataHeader
	EmitLnR("section '.rsrc' resource data readable")
End Sub

Sub WriteMetadataDirectory(Icon,Version)
	If Icon And Version Then
		EmitLnR("directory RT_ICON,icons,\")
		EmitLnR("RT_GROUP_ICON,group_icons,\")
		EmitLnR("RT_VERSION,versions")
		EmitLnR("resource icons,1,LANG_NEUTRAL,icon_data")
		EmitLnR("resource group_icons,17,LANG_NEUTRAL,main_icon")
		EmitLnR("resource versions,1,LANG_NEUTRAL,version")
	ElseIf Icon Then
		EmitLnR("directory RT_ICON,icons,\")
		EmitLnR("RT_GROUP_ICON,group_icons")
		EmitLnR("resource icons,1,LANG_NEUTRAL,icon_data")
		EmitLnR("resource group_icons,17,LANG_NEUTRAL,main_icon")
	ElseIf Version Then
		EmitLnR("directory RT_VERSION,versions")
		EmitLnR("resource versions,1,LANG_NEUTRAL,version")
	End If
End Sub

Sub WriteIcon(Name)
	EmitLnR("icon main_icon,icon_data,'" & Name & "'")
End Sub

Sub WriteVersion(VersionString)
	EmitLnR("versioninfo version,VOS__WINDOWS32,VFT_APP,VFT2_UNKNOWN,LANG_ENGLISH+SUBLANG_DEFAULT,0" & VersionString)
End Sub

Sub Prolog
	PassNumber = 2
	PostLabel("start")
	EmitLnD("hHeap dd 0")
	EmitLn("invoke GetProcessHeap")
	EmitLn("MOV DWORD [hHeap], eax")
End Sub

Sub Epilog
	EmitLn("invoke ExitProcess,0" & VbCrLf)
End Sub

Sub Footer
	IncludeLibs
	EmitLn(	"section '.idata' import data readable writeable" & VbCrLf &_
		"library kernel32,'KERNEL32.DLL',\" & VbCrLf &_
		"	 user32,'USER32.DLL',\" & VbCrLf &_
		"	 gdi32,'GDI32.DLL',\" & VbCrLf &_
		"	 opengl,'OPENGL32.DLL',\" & VbCrLf &_
		"	 glu,'GLU32.DLL'" & VbCrLf &_
		"include 'fasm\include\api\kernel32.inc'" & VbCrLf &_
		"include 'fasm\include\api\user32.inc'" & VbCrLf &_
		"include 'fasm\include\api\gdi32.inc'" & VbCrLf &_
		"include 'fasm\include\api\opengl32.inc'")
End Sub

Sub IncludeLibs
	Dim Text,Counter,Name,Path,char
	Path = Fso.GetParentFolderName(WScript.ScriptFullName) & "\LIBS.INI"
	Counter = 1
	If Not Fso.GetFile(Path).Size = 0 Then
		Text = Fso.OpenTextFile(Path).ReadAll
		Do
			Do Until Mid(Text,Counter,1) = Chr(13) Or Counter > Len(Text)
				Name = Name & Mid(Text,Counter,1)
				Counter = Counter + 1
			Loop
			EmitLn("include 'LIBS\" & Name & "'")
			Name = ""
			Counter = Counter + 2
			If Counter > Len(Text) Then
				Exit Do
			End If
		Loop
	End If
End Sub

'---------------------------------------------------------------------
'Actual code :)
Sub Prog
	Header
	PreParseProcedures
	PreParseLibs
	Prolog
	WriteMetadata
	TopDecls
	Enumerations
	JmpToMainCode
	ClassDeclarations
	InitBlock
	MainBlock
	Epilog
	ProcDecl
	Footer
End Sub

Sub WriteMetadata
	Dim dict
	Dim arr,i
	Dim IconPresent, VersionPresent
	Dim VersionString
	If Value = "METADATA" Then
		Set dict = CreateObject("scripting.dictionary")
		MatchString("METADATA")
		
		Do While Value <> "ENDMETADATA"
			If Value = "ICON" Then
				IconPresent = True
				Next1
				MatchString(":")
				GetString
				dict.Add "ICON", Value
				Next1
			ElseIf Value = "FILEDESCRIPTION" Then
				VersionPresent = True
				Next1
				MatchString(":")
				GetString
				dict.Add "FileDescription", Value
				Next1
			ElseIf Value = "LEGALCOPYRIGHT" Then
				VersionPresent = True
				Next1
				MatchString(":")
				GetString
				dict.Add "LegalCopyright", Value
				Next1
			ElseIf Value = "FILEVERSION" Then
				VersionPresent = True
				Next1
				MatchString(":")
				GetString
				dict.Add "FileVersion", Value
				Next1
			ElseIf Value = "PRODUCTVERSION" Then
				VersionPresent = True
				Next1
				MatchString(":")
				GetString
				dict.Add "ProductVersion", Value
				Next1
			ElseIf Value = "ORIGINALFILENAME" Then
				VersionPresent = True
				Next1
				MatchString(":")
				GetString
				dict.Add "OriginalFilename", Value
				Next1
			ElseIf Token = "." Then
				MatchString(".")
				MatchString(".")
				MatchString(".")
			Else
				Abort("Unrecognized attribute: " & Value)
			End If
		Loop
		
		arr = dict.Keys
		If Not UBound(arr) = -1 Then
			WriteMetadataHeader
			WriteMetadataDirectory IconPresent, VersionPresent
			Do
				If arr(i) = "ICON" Then
					WriteIcon(dict.Item(arr(i)))
				Else
					VersionString = VersionString & ",\" & VbCrLf & "'" &  arr(i) & "','" & dict.Item(arr(i)) & "'"
				End If
				i = i + 1
			Loop While i <= UBound(arr)
			If VersionPresent Then WriteVersion(VersionString)
		End If
		
		MatchString("ENDMETADATA")
	End If
End Sub

Sub Enumerations
	Dim i
	Do While Value = "ENUMERATION"
		i = 0
		MatchString("ENUMERATION")
		Do While Value <> "ENDENUMERATION"
			If Value = "." Then
				MatchString(".")
				MatchString(".")
				MatchString(".")
			Else
				Constants.Add Value, Array("INT",i)
				i = i + 1
				Next1
			End If
		Loop
		MatchString("ENDENUMERATION")
	Loop
End Sub

Sub JmpToMainCode
	EmitLn("JMP Main")
End Sub

Sub ClassDeclarations
	Do While Value = "CLASS"
		MatchString("CLASS")
		Do While Value <> "ENDCLASS"
			If Value = "PROPERTY" Then
				DeclareProperty
			ElseIf Value = "METHOD" Then
				DeclareMethod
			ElseIf Value = "." Then
				DoPass
			Else
				Expected("Property or method declaration")
			End If
		Loop
		MatchString("ENDCLASS")
	Loop
End Sub

Sub DeclareProperty
	MatchString("PROPERTY")
	If Value = "INT" Or Value = "STR" Then
		Next1
	End If
	Next1
End Sub

Sub DeclareMethod
	Dim n
	MatchString("METHOD")
	n = Value
	Next1
	MatchString("(")
	'...
	MatchString(")")
	Block "",n
	MatchString("ENDMETHOD")
End Sub

Sub InitBlock
	PostLabel("Main")
	If Value = "INIT" Then
		MatchString("INIT")
		Block "",""
		MatchString("ENDINIT")
	End If
End Sub

Sub MainBlock
	Dim k
	MatchString("MAIN")
	k = LocDecls
	LocAllocMain(k)
	AllocateLocalArrays
	Block "",""
	MatchString("ENDMAIN")
	LocFreeMain(k)
	ClearParams
End Sub

Sub Allocate(n)
	EmitLnD("V_" & n & " dd 0")
End Sub

Sub Alloc(t,n)
	CheckDup(n)
	Allocate n
	AddEntry n, Array("variable",t,False)
End Sub

Sub AllocArray(t,n,i)
	CheckDup(n)
	Allocate n
	AddEntry n,Array("array",t,"")
	EmitLn("invoke HeapAlloc,[hHeap],HEAP_ZERO_MEMORY," & i * 4 + 4)
	EmitLn("MOV DWORD [V_" & n & "], eax")
	EmitLn("MOV DWORD [eax], " & i)
End Sub

Sub TopDecls
	Dim n,t:t = "INT"
	If Value = "GLOBAL" Then
		MatchString("GLOBAL")
		Do While Value = "DIM" Or Value = "CONST" Or Value = "."
			If Value = "DIM" Then
				MatchString("DIM")
				If Value = "INT" Or Value = "STR" Then
					t = Value
					Next1
				End If
				If IsKeyword(Value) Then Abort("Reserved keyword used as identifier ('" & Value & "')")
				n = Value
				Next1
				If Token = "[" Then
					MatchString("[")
					AllocArray t,n,Value
					Next1
					MatchString("]")
				Else
					Alloc t,n
				End If
				t = "INT"
				Do While Token = ","
					MatchString(",")
					If Value = "INT" Or Value = "STR" Then
						t = Value
						Next1
					End If
					n = Value
					Next1
					If Token = "[" Then
						MatchString("[")
						AllocArray t,n,Value
						Next1
						MatchString("]")
					Else
						Alloc t,n
					End If
					t = "INT"
				Loop
				If Token = ";" Then MatchString(";")
			ElseIf Value = "CONST" Then
				MatchString("CONST")
				If Value = "INT" Or Value = "STR" Then
					t = Value
					Next1
				End If
				n = Value
				Next1
				MatchString("=")
				If t = "INT" Then
					Constants.Add n, Array(t,Value)
					Next1
				Else
					Constants.Add n, Array(t,StringConst)
				End If
				t = "INT"
				Do While Token = ","
					MatchString(",")
					If Value = "INT" Or Value = "STR" Then
						t = Value
						Next1
					End If
					n = Value
					Next1
					MatchString("=")
					If t = "INT" Then
						Constants.Add n, Array(t,Value)
						Next1
					Else
						Constants.Add n, Array(t,StringConst)
					End If
					t = "INT"
				Loop
				If Token = ";" Then MatchString(";")
			Else
				DoPass
			End If
		Loop
		MatchString("ENDGLOBAL")
	End If
End Sub

Function StringConst
	Dim s,length,L
	If Token = Chr(34) Then
		Do While Not Look = Chr(34)
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString(Chr(34))
		MatchString(Chr(34))
		If Not StringTable.Exists(s) Then
			L = NewLabel
			StringTable.Add s, L
			length = Len(s)
			s = Replace(s,"'","',39,'")
			s = Replace(s,VbCrLf,"',13,10,'")
			EmitLnD(L & " dd " & length)
			EmitLnD("db '" & s & "',0")
		Else
			L = StringTable.Item(s)
		End If
		StringConst = L
	ElseIf Token = "'" Then
		Do While Not Look = "'"
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString("'")
		MatchString("'")
		If Not StringTable.Exists(s) Then
			L = NewLabel
			StringTable.Add s, L
			length = Len(s)
			s = Replace(s,VbCrLf,"',13,10,'")
			EmitLnD(L & " dd " & length)
			EmitLnD("db '" & s & "',0")
		Else
			L = StringTable.Item(s)
		End If
		StringConst = L
	Else
		Expected("Constant string")
	End If
End Function

Sub ProcDecl
	Do
		Select Case Value
			Case "SUB" : DoSub
			Case ""  : Exit Do
			Case Else: Abort("Unexpected data")
		End Select
	Loop
End Sub

Sub Assignement(n)
	Dim v,k
	CheckTable(n)
	If Not GetIdentType(n) = "variable" And Not IsParam(n) Then
		Abort("Incorrect identifier ('" & n & "')")
	End If
	Select Case Token
		Case "="
			MatchString("=")
			If GetDataType(n) = "INT" Then
				BoolExpression
				If IsParam(n) Then
					StoreParam(ParamNumber(n))
				Else
					Store(n)
				End If
			ElseIf GetDataType(n) = "STR" Then
				StringExpression
				If IsParam(n) Then
					If GetAdditionalInfo(n) = True Then
						Push
						FreeHeapBufferLoc(ParamNumber(n))
						Pop
					Else
						SetAdditionalInfo n,True
					End If
					StoreParam(ParamNumber(n))
				Else
					If GetAdditionalInfo(n) = True Then
						Push
						FreeHeapBuffer(n)
						Pop
					Else
						SetAdditionalInfo n,True
					End If
					Store(n)
				End If
			Else
				Abort("Unexpected data type: '" & GetDataType(n) & "'")
			End If
		Case "+"
			MatchString("+")
			Select Case Token
				Case "+"
					MatchString("+")
					Inc(n)
				Case "="
					AssignAdd(n)
			End Select
		Case "-"
			MatchString("-")
			Select Case Token
				Case "-"
					MatchString("-")
					Dec(n)
				Case "="
					AssignSub(n)
			End Select
		Case "*"
			AssignMul(n)
	End Select
End Sub

Sub AssignAdd(n)
	MatchString("=")
	CheckTable(n)
	If Not GetDataType(n) = "INT" Then Abort("Cannot assign and add to " & n & " (invalid type)")
	Expression
	If IsParam(n) Then
		EmitLn("ADD DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "], eax")
	Else
		EmitLn("ADD [V_" & n & "], eax")
	End If
End Sub

Sub AssignSub(n)
	MatchString("=")
	CheckTable(n)
	If Not GetDataType(n) = "INT" Then Abort("Cannot assign and subtract to " & n & " (invalid type)")
	Expression
	If IsParam(n) Then
		EmitLn("SUB DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "], eax")
	Else
		EmitLn("SUB [V_" & n & "], eax")
	End If
End Sub

Sub AssignMul(n)
	MatchString("*")
	MatchString("=")
	CheckTable(n)
	If Not GetDataType(n) = "INT" Then Abort("Cannot assign and multiply to " & n & " (invalid type)")
	Expression
	If IsParam(n) Then
		EmitLn("IMUL eax, DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
		EmitLn("MOV DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "], eax")
	Else
		EmitLn("IMUL eax, [V_" & n & "]")
		EmitLn("MOV [V_" & n & "], eax")
	End If
End Sub

Sub ArrayAssignement(n)
	MatchString("[")
	Expression
	ConvertArrayOffset
	Push
	MatchString("]")
	MatchString("=")
	If GetDataType(n) = "INT" Then
		BoolExpression
	ElseIf GetDataType(n) = "STR" Then
		StringExpression
		XchgTopMain
		FreeHeapBufferArray n
		XchgTopMain
	Else
		Abort("Unexpected data type: '" & GetDataType(n) & "'")
	End If
	StoreArray n
	Pop
End Sub

Sub Block(L,Ret)
	Dim n
	Do While InStr(EndKeywords,"|"&Value&"|") = 0
		Select Case Value
			Case ";" :Semi
			Case "IF" :DoIf L,Ret
			Case "SELECT" :DoSelect L,Ret
			Case "WHILE" :DoWhile Ret
			Case "DO" :DoLoop Ret
			Case "REPEAT" :DoRepeat Ret
			Case "BREAK" :DoBreak L
			Case "$" :InlineAsm
			Case "RETURN" :DoReturn(Ret)
			Case "END" :DoEnd
			Case "GOTO" :DoGoto
			Case "CALL" :DoCall
			Case "." :DoPass
			Case Else:
				n = Value
				Next1
				If GetIdentType(n) = "procedure" Then
					CallProc(n)
					If GetDataType(n) = "STR" Then
						FreeMainReg
					End If
				ElseIf GetIdentType(n) = "variable" Then
					Assignement(n)
				ElseIf GetIdentType(n) = "array" Then
					ArrayAssignement(n)
				ElseIf Token = ":" Then
					PostLabel("V_" & n)
					MatchString(":")
					AddEntry n, Array("l","","")
				ElseIf IsKeyword(n) Then
					Abort("'" & n & "' is misplaced")
				Else
					Undefined(n)
				End If
		End Select
		Semi
	Loop
End Sub

Sub Semi
	If Token = ";" Then
		MatchString(";")
	End If
End Sub

Sub DoCall
	Dim n
	MatchString("CALL")
	If Not InTable(Value) Then Undefined(Value)
	If GetIdentType(Value) <> "procedure" Then Abort(n & " is not a subroutine")
	n = Value
	Next1
	CallProc(n)
	If GetDataType(n) = "STR" Then
		FreeMainReg
	End If
End Sub

Sub CallProc(n)
	Dim nbytes:nbytes = ParamList
	If Not GetAdditionalInfo(n) = nbytes / 4 Then Abort("Wrong number of arguments while calling " & n &_
		VbCrLf & "given: " & nbytes / 4 & VbCrLf & "expected: " & GetAdditionalInfo(n))
	JmpToProc(n)
	CleanStack(nbytes)
End Sub

Function ParamList
	Dim n:n = 0
	MatchString("(")
	If Token <> ")" Then
		Param
		n = n + 1
		Do While Token = ","
			MatchString(",")
			Param
			n = n + 1
		Loop
	End If
	MatchString(")")
	ParamList = n * 4
End Function

Sub Param
	If Token = "," Or Token = ")" Then
		PushNull
	Else
		If Value = "ARRAY" Then
			MatchString("ARRAY")
			ArrayExpression
		ElseIf Token = Chr(34) Or Token = "'" Or GetDataType(Value) = "STR" Then
			StringExpression
		Else
			BoolExpression
		End If
		Push
	End If
End Sub

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
'Math Expressions
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub Factor
	Dim n
	If Token = "(" Then
		Next1
		BoolExpression
		MatchString(")")
	ElseIf Token = "@" Then
		MatchString("@")
		If GetIdentType(Value) <> "" Then
			If IsParam(Value) Then
				LoadLocalPointer(Value)
			Else
				LoadPointer(Value)
			End If
			Next1
		Else
			Undefined(Value)
		End If
	ElseIf Token = "#" Then
		MatchString("#")
		LoadConst(Value)
		Next1
	ElseIf Token = "x" Then
		n = Value
		Next1
		If GetIdentType(n) = "procedure" Then
			If GetDataType(n) = "STR" Then Abort("Type mismatch, procedure " & n & " is not of type INT")
			CallProc(n)
		ElseIf GetIdentType(n) = "array" Then
			If GetDataType(n) = "STR" Then Abort("Type mismatch, array " & n & " is not of type INT")
			MatchString("[")
			Expression
			ConvertArrayOffset
			MatchString("]")
			LoadArrayCell n
		ElseIf IsParam(n) Then
			If GetDataType(n) = "STR" Then Abort("Type mismatch, variable " & n & " is not of type INT")
			LoadParam(ParamNumber(n))
		ElseIf InTable(n) Then
			If GetDataType(n) = "STR" Then Abort("Type mismatch, variable " & n & " is not of type INT")
			LoadVar(n)
		ElseIf Constants.Exists(n) Then
			If Constants.Item(n)(0) = "STR" Then Abort("Type mismatch, constant " & n & " is not of type INT")
			LoadConstant(n)
		Else
			Undefined(n)
		End If
	ElseIf Token = "0" Then
		LoadConst(Value)
		Next1
	Else
		Expected("Math Factor")
	End If
End Sub

Sub NegFactor
	MatchString("-")
	If Token = "0" Then
		LoadConst("-" & Value)
		Next1
	Else
		Factor
		Negate
	End If
End Sub

Sub Multiply
	Next1
	If Token = "-" Then
		NegFactor
	Else
		Factor
	End If
	PopMul 
End Sub

Sub Divide
	Next1
	If Token = "-" Then
		NegFactor
	Else
		Factor
	End If
	PopDiv
End Sub

Sub Modulo
	Next1
	If Token = "-" Then
		NegFactor
	Else
		Factor
	End If
	PopModulo
End Sub

Sub Term
	If Token = "-" Then
		NegFactor
	Else
		Factor
	End If
	Do While IsMulop(Token)
		Push
		Select Case Token
			Case "*":Multiply
			Case "/":Divide
			Case "%":Modulo
		End Select
	Loop
End Sub

Sub Add
	Next1
	Term
	PopAdd
End Sub

Sub Subtract
	Next1
	Term
	PopSub
End Sub

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
'Boolean Expressions
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub CompareStringExpression
	StringExpression
	StringCompare
	PopSecondary
	PushFlags
	FreeMainReg
	FreeSecondaryReg
	PopFlags
End Sub

Sub CompareExpression
	Expression
	PopCompare
End Sub

Sub NextStringExpression
	Next1
	CompareStringExpression
End Sub

Sub NextExpression
	Next1
	CompareExpression
End Sub

Sub Equal
	NextExpression
	SetEqual
End Sub

Sub StringEqual
	NextStringExpression
	SetEqual
End Sub

Sub StringNotEqual
	NextStringExpression
	SetNEqual
End Sub

Sub LessOrEqual
	NextExpression
	SetLessOrEqual
End Sub

Sub NotEqual
	NextExpression
	SetNEqual
End Sub

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

Sub Relation
	If Token = Chr(34) Or GetDataType(Value) = "STR" Then
		StringExpression
		Push
		Select Case Token
			Case "=":StringEqual
			Case "<"
				Next1
				If Token = ">" Then
					StringNotEqual
				Else
					Expected("String operator")
				End If
			Case "!"
				MatchString("!")
				If Token = "=" Then
					StringNotEqual
				Else
					Expected("=")
				End If
			Case Else
				Expected("String operator")
		End Select
	Else
		Expression
		If IsRelop(Token) Then
			Push
			Select Case Token
				Case "=":Equal
				Case "<":Less
				Case ">":Greater
				Case "!"
					MatchString("!")
					If Token = "=" Then
						NotEqual
					Else
						Expected("=")
					End If
			End Select
		End If
	End If
End Sub

Sub NotFactor
	If Token = "!" Or Value = "NOT" Then
		Next1
		Relation
		NotIt
	Else
		Relation
	End If
End Sub

Sub BoolTerm
	NotFactor
	Do While Token = "&" Or Value = "AND"
		Push
		Next1
		NotFactor
		PopAnd
	Loop
End Sub

Sub BoolOr
	Next1
	BoolTerm
	PopOr
End Sub

Sub BoolXor
	Next1
	BoolTerm
	PopXor
End Sub

Sub BoolExpression
	BoolTerm
	Do While IsOrop(Token)
		Push
		Select Case Value
			Case "|":BoolOr
			Case "OR":BoolOr
			Case "~":BoolXor
			Case "XOR":BoolXor
		End Select
	Loop
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'String Expressions
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub FirstStringTerm
	dim n,s,L,length
	If Token = Chr(34) Then
		Do While Not Look = Chr(34)
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString(Chr(34))
		MatchString(Chr(34))
		If Not StringTable.Exists(s) Then
			L = NewLabel
			StringTable.Add s, L
			length = Len(s)
			s = Replace(s,"'","',39,'")
			s = Replace(s,VbCrLf,"',13,10,'")
			EmitLnD(L & " dd " & length)
			EmitLnD("db '" & s & "',0")
		Else
			L = StringTable.Item(s)
		End If
		
		AllocateHeapBuffer(Len(s)+5)
		If Len(s) <> 0 Then
			CopyStringToBuf(L)
		End If
	ElseIf Token = "'" Then
		Do While Not Look = "'"
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString("'")
		MatchString("'")
		If Not StringTable.Exists(s) Then
			L = NewLabel
			StringTable.Add s, L
			length = Len(s)
			s = Replace(s,VbCrLf,"',13,10,'")
			EmitLnD(L & " dd " & length)
			EmitLnD("db '" & s & "',0")
		Else
			L = StringTable.Item(s)
		End If
		
		AllocateHeapBuffer(Len(s)+5)
		If Len(s) <> 0 Then
			CopyStringToBuf(L)
		End If
	ElseIf Token = "x" Then
		n = Value
		Next1
		If GetIdentType(n) = "procedure" Then
			If Not GetDataType(n) = "STR" Then Abort(n & " does not return a string")
			CallProc(n)
		ElseIf GetIdentType(n) = "array" Then
			If Not GetDataType(n) = "STR" Then Abort(n & " is not a string array")
			MatchString("[")
			Expression
			ConvertArrayOffset
			MatchString("]")
			CopyStringFromArray n
		ElseIf InTable(n) Or IsParam(n) Then
			If Not GetDataType(n) = "STR" Then Abort(n & " is not of type STR")
			CopyStringVar(n)
		ElseIf Constants.Exists(n) Then
			If Not Constants.Item(n)(0) = "STR" Then Abort(n & " is not of type STR")
			CopyStringVar(n)
		Else
			Undefined(n)
		End If
	Else
		Expected("String")
	End If
End Sub

Sub AppendStringTerm
	dim n,s,L,length,NeedToFree
	NeedToFree = False
	If Token = Chr(34) Then
		Do While Not Look = Chr(34)
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString(Chr(34))
		MatchString(Chr(34))
		If Not StringTable.Exists(s) Then
			L = NewLabel
			StringTable.Add s, L
			length = Len(s)
			s = Replace(s,"'","',39,'")
			s = Replace(s,VbCrLf,"',13,10,'")
			EmitLnD(L & " dd " & length)
			EmitLnD("db '" & s & "',0")
		Else
			L = StringTable.Item(s)
		End If
		
		LoadLabel(L)
	ElseIf Token = "'" Then
		Do While Not Look = "'"
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString("'")
		MatchString("'")
		If Not StringTable.Exists(s) Then
			L = NewLabel
			StringTable.Add s, L
			length = Len(s)
			s = Replace(s,VbCrLf,"',13,10,'")
			EmitLnD(L & " dd " & length)
			EmitLnD("db '" & s & "',0")
		Else
			L = StringTable.Item(s)
		End If
		
		LoadLabel(L)
	ElseIf Token = "x" Then
		n = Value
		Next1
		If GetIdentType(n) = "procedure" Then
			If Not GetDataType(n) = "STR" Then Abort(n & " does not return a string")
			CallProc(n)
			NeedToFree = True
		ElseIf GetIdentType(n) = "array" Then
			If Not GetDataType(n) = "STR" Then Abort(n & " is not a string array")
			MatchString("[")
			Expression
			ConvertArrayOffset
			MatchString("]")
			LoadStringFromArray n
		ElseIf InTable(n) Or IsParam(n) Then
			If Not GetDataType(n) = "STR" Then Abort(n & " is not of type STR")
			LoadVar(n)
		ElseIf Constants.Exists(n) Then
			If Not Constants.Item(n)(0) = "STR" Then Abort(n & " is not of type STR")
			LoadConst(n)
		Else
			Undefined(n)
		End If
	Else
		Expected("String")
	End If
	Concat(NeedToFree)
End Sub

Sub StringExpression
	FirstStringTerm
	Do While Token = "&"
		MatchString("&")
		Push
		AppendStringTerm
	Loop
End Sub

Sub GetString
	Dim s
	If Value = "'" Then
		Do While Not Look = "'"
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString("'")
	ElseIf Value = Chr(34) Then
		Do While Not Look = Chr(34)
			If Look = Chr(10) Then ErrLn = ErrLn + 1
			s = s & Look
			GetChar
		Loop
		MatchString(Chr(34))
	Else
		Expected("String")
	End If
	Value = s
	Token = "STR"
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Array Expressions
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub ArrayExpression
	If GetIdentType(Value) <> "array" Then Abort("Array expected")
	If GetDataType(Value) = "STR" Then
		CopyStringArray(Value)
	Else
		CopyArray(Value)
	End If
	Next1
	MatchString("[")
	MatchString("]")
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Control Structures
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub DoIf(L,Ret)
	Dim Name,L1,L2
	MatchString("IF")
	L1 = NewLabel
	BoolExpression
	If Value = "THEN" Then MatchString("THEN")
	L2 = NewLabel
	BranchFalse(L2)
	Block L,Ret
	Branch(L1)
	PostLabel(L2)
	Do While Value = "ELSEIF"
		MatchString("ELSEIF")
		BoolExpression
		If Value = "THEN" Then MatchString("THEN")
		L2 = NewLabel
		BranchFalse(L2)
		Block L,Ret
		Branch(L1)
		PostLabel(L2)
	Loop
	If Value = "ELSE" Then
		MatchString("ELSE")
		Block L,Ret
		Branch(L1)
	End If
	MatchString("ENDIF")
	PostLabel(L1)
End Sub

Sub DoSelect(L,Ret)
	Dim L1,L2,t
	MatchString("SELECT")
	L1 = NewLabel
	t = GetDataType(Value)
	If t = "STR" Then
		StringExpression
	ElseIf t = "INT" Then
		BoolExpression
	End If
	Push
	Do While Value = "CASE"
		MatchString("CASE")
		If Value = "ELSE" Then
			MatchString("ELSE")
			Block L,Ret
			Branch(L1)
			Exit Do
		End If
		
		If t = "STR" Then
			StringExpression
			StringCompare
			PushFlags
			FreeMainReg
			PopFlags
		ElseIf t = "INT" Then
			Expression
			CompareTopOfStack
		End If
		L2 = NewLabel
		BranchIfFalse(L2)
		Block L,Ret
		Branch(L1)
		PostLabel(L2)
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
	MatchString("END")
	MatchString("(")
	If Token <> ")" Then
		Expression
		EndProgram("")
	Else
		EndProgram(0)
	End If
	MatchString(")")
End Sub

Sub DoGoto
	Dim p:p = False
	MatchString("GOTO")
	If Token = "(" Then
		MatchString("(")
		p = True
	End If
	Branch("V_" & Value)
	If p Then MatchString(")")
	Next1
End Sub

Sub DoPass
	MatchString(".")
	MatchString(".")
	MatchString(".")
	If Token = ";" Then MatchString(";")
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Procedure Handling
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub DoSub
	Dim Name,k,L1,freeargs:freeargs = False
	MatchString("SUB")
	Name = Value
	If IsKeyword(Value) Then Abort("Reserved keyword used as identifier (" & Value & ")")
	L1 = "RET" & Name
	Next1
	MatchString("(")
	FormalList
	MatchString(")")
	If Token = "(" Then
		MatchString("(")
		If Value = "STDCALL" Then
			MatchString("STDCALL")
			freeargs = True
		End If
		MatchString(")")
	End If
	If Token = ":" Then
		MatchString(":")
		If Value = "INT" Or Value = "STR" Then
			Next1
		Else
			Expected("Type Name")
		End If
	End If
	If DebugMode = 1 Then
		EmitLn("db '" & Name & "',0")
	End If
	PostLabel("V_" & Name)
	k = LocDecls
	LocAlloc(k)
	ProcHeader
	PushSavedRegisters
	AllocateLocalArrays
	Block "",L1
	LocFree k,L1
	PopSavedRegisters
	If freeargs Then
		Return(GetAdditionalInfo(Name))
	Else
		Return(0)
	End If
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
	Dim n,t:t = "INT"
	If Value = "INT" Or Value = "STR" Then
		t = Value
		Next1
	End If
	n = Value
	Next1
	If Value = "[" Then
		MatchString("[")
		MatchString("]")
		AddParam n,t,"array",-1
	Else
		AddParam n,t,"variable",True
	End If
End Sub

Function LocDecls
	Dim n
	Do While Value = "DIM"
		MatchString("DIM")
		LocDecl
		n = n + 1
		Do While Token = ","
			MatchString(",")
			LocDecl
			n = n + 1
		Loop
		If Token = ";" Then MatchString(";")
	Loop
	LocDecls = n
End Function

Sub LocDecl
	Dim t,data,n:t = "INT"
	If Value = "STR" Or Value = "INT" Then
		t = Value
		Next1
	End If
	n = Value
	Next1
	If Token = "[" Then
		MatchString("[")
		AddParam n,t,"array",Value
		Next1
		MatchString("]")
	Else
		AddParam n,t,"variable",False
	End If
End Sub

Sub AllocateLocalArrays
	Dim i,arr:arr = FormalParamST.Keys
	If Not UBound(arr) = -1 Then
		Do
			If GetIdentType(arr(i)) = "array" And Not GetAdditionalInfo(arr(i)) = -1 Then
				AllocateLocalArray ParamNumber(arr(i)),GetAdditionalInfo(arr(i))
			End If
			i = i + 1
		Loop While i <= UBound(arr)
	End If
End Sub

Sub AllocateGlobalArrays
	Dim i,arr:arr = ST.Keys
	If Not UBound(arr) = -1 Then
		Do
			If GetIdentType(arr(i)) = "array" And Not GetAdditionalInfo(arr(i)) = -1 Then
				AllocArray ParamNumber(arr(i)),GetAdditionalInfo(arr(i))
			End If
			i = i + 1
		Loop While i <= UBound(arr)
	End If
End Sub

Sub ClearParams
	FormalParamST.RemoveAll
End Sub

Function ParamNumber(n)
	ParamNumber = FormalParamST.Item(n)(0)
End Function

Function IsParam(n)
	IsParam = FormalParamST.Exists(n)
End Function

Sub AddParam(n,IdentType,DataType,AdditionalInfo)
	If IsKeyword(n) Then Abort("Reserved keyword used as identifier (" & n & ")")
	If InTable(n) Then Abort(n & " is already declared in the global scope")
	If IsParam(n) Then Duplicate(n)
	NumParams = NumParams + 1
	FormalParamST.Add n, Array(NumParams,IdentType,DataType,AdditionalInfo)
End Sub

Sub DoReturn(Ret)
	If Ret = "" Then Abort("RETURN Outside Of A Procedure")
	MatchString("RETURN")
	MatchString("(")
	If Token = ")" Then
		Clear
	Else
		If GetDataType(Right(Ret,Len(Ret) - 3)) = "STR" Then
			StringExpression
		Else
			BoolExpression
		End If
	End If
	MatchString(")")
	Branch(Ret)
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Math Code Generation Routines
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub Clear
	EmitLn("MOV eax, 0")
End Sub

Sub SecondaryToPrimary
	EmitLn("MOV eax, ebx")
End Sub

Sub PrimaryToSecondary
	EmitLn("MOV ebx, eax")
End Sub

Sub PopSecondary
	EmitLn("POP ebx")
End Sub

Sub PushSecondary
	EmitLn("PUSH ebx")
End Sub

Sub PushFlags
	EmitLn("PUSHFD")
End Sub

Sub PopFlags
	EmitLn("POPFD")
End Sub

Sub SetTrue
	EmitLn("MOV eax, 0xFFFFFFFF")
End Sub

Sub Negate
	EmitLn("NEG eax")
End Sub

Sub LoadConst(n)
	EmitLn("MOV eax, " & n)
End Sub

Sub LoadPointer(n)
	EmitLn("MOV eax, V_" & UCase(n))
End Sub

Sub LoadLocalPointer(n)
	EmitLn("MOV eax, ebp")
	EmitLn("ADD eax, " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4))
End Sub

Sub LoadVar(n)
	EmitLn("MOV eax, DWORD [V_" & n & "]")
End Sub

Sub LoadConstant(n)
	EmitLn("MOV eax, " & Constants.Item(n)(1))
End Sub

Sub LoadLabel(L)
	EmitLn("MOV eax, " & L)
End Sub

Sub Push
	EmitLn("PUSHD eax")
End Sub

Sub PushNull
	EmitLn("PUSHD 0")
End Sub

Sub Pop
	EmitLn("POPD eax")
End Sub

Sub PopAdd
	EmitLn("ADD eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

Sub PopSub
	EmitLn("SUB eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
	EmitLn("NEG eax")
End Sub

Sub PopMul
	EmitLn("IMUL DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

Sub PopDiv
	EmitLn("MOV ebx, DWORD [esp]")
	EmitLn("ADD esp, 4")
	EmitLn("XCHG eax, ebx")
	EmitLn("XOR edx, edx")
	EmitLn("IDIV ebx")
End Sub

Sub PopModulo
	EmitLn("MOV ebx, DWORD [esp]")
	EmitLn("ADD esp, 4")
	EmitLn("XCHG eax, ebx")
	EmitLn("XOR edx, edx")
	EmitLn("IDIV ebx")
	EmitLn("MOV eax, edx")
End Sub

Sub Store(n)
	EmitLn("MOV [V_" & n & "], eax")
End Sub

Sub StoreArray(n)
	If IsParam(n) Then
		EmitLn("MOV ebx, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
	Else
		EmitLn("MOV ebx, [V_" & n & "]")
	End If
	EmitLn("ADD ebx, DWORD [esp]")
	EmitLn("MOV DWORD [ebx], eax")
End Sub

Sub LoadArrayCell(n)
	If IsParam(n) Then
		EmitLn("MOV ebx, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
	Else
		EmitLn("MOV ebx, [V_" & n & "]")
	End If
	EmitLn("ADD ebx, eax")
	EmitLn("MOV eax, DWORD [ebx]")
End Sub

Sub ConvertArrayOffset
	EmitLn("SHL eax, 2")
	EmitLn("ADD eax, 4")
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Boolean Code Generation Routines
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub NotIt
	EmitLn("NOT eax")
End Sub

Sub PopAnd
	EmitLn("AND eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

Sub PopOr
	EmitLn("OR eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

Sub PopXor
	EmitLn("XOR eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

Sub PopCompare
	EmitLn("MOV ebx, DWORD [esp]")
	EmitLn("ADD esp, 4")
	EmitLn("CMP ebx, eax")
End Sub

Sub StringCompare
	Dim L1, L2
	L1 = NewLabel
	L2 = NewLabel
	EmitLn("MOV esi, DWORD [esp]")
	EmitLn("MOV ecx, DWORD [esi]")
	EmitLn("CMP ecx, 0")
	EmitLn("JE " & L1)
	EmitLn("INC ecx")
	EmitLn("MOV edi, eax")
	EmitLn("ADD esi, 4")
	EmitLn("ADD edi, 4")
	EmitLn("REPE CMPSB")
	EmitLn("JMP " & L2)
	PostLabel(L1)
	EmitLn("MOV ebx, DWORD [eax]")
	EmitLn("CMP ebx, 0")
	PostLabel(L2)
End Sub

Sub FreeMainReg
	EmitLn("invoke HeapFree,[hHeap],0,eax")
End Sub

Sub FreeSecondaryReg
	EmitLn("invoke HeapFree,[hHeap],0,ebx")
End Sub

Sub CompareTopOfStack
	EmitLn("CMP eax, DWORD [esp]")
End Sub

Sub SetEqual
	EmitLn("MOV eax, 0")
	EmitLn("SETE al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

Sub SetNEqual
	EmitLn("MOV eax, 0")
	EmitLn("SETNE al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

Sub SetGreater
	EmitLn("MOV eax, 0")
	EmitLn("SETG al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

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
	EmitLn("SETGE al")
	EmitLn("IMUL eax, 0xFFFFFFFF")
End Sub

Sub Inc(n)
	If Not GetIdentType(n) = "variable" Then Abort("Identifiers other than variables cannot be incremented")
	If IsParam(n) Then
		EmitLn("INC DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
	Else
		EmitLn("INC DWORD [V_" & n & "]")
	End If
End Sub

Sub Dec(n)
	If Not GetIdentType(n) = "variable" Then Abort("Identifiers other than variables cannot be decremented")
	If IsParam(n) Then
		EmitLn("DEC DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
	Else
		EmitLn("DEC DWORD [V_" & n & "]")
	End If
End Sub

Sub XchgTopMain
	EmitLn("XCHG eax, DWORD [esp]")
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Control Structures Code Generation Routines
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub Branch(l)
	EmitLn("JMP " & l)
End Sub

Sub BranchFalse(l)
	EmitLn("TEST eax, eax")
	EmitLn("JZ " & l)
End Sub

Sub BranchIfFalse(l)
	EmitLn("JNE " & l)
End Sub

Sub EndProgram(val)
	If val = "" Then
		EmitLn("invoke ExitProcess,eax")
	Else
		EmitLn("invoke ExitProcess," & val)
	End If
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Proc Handling Code Generation Routines
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub JmpToProc(n)
	EmitLn("CALL V_" & n)
End Sub

Sub ProcHeader
	EmitLn("PUSH ebp")
	EmitLn("MOV ebp, esp")
End Sub

Sub PushSavedRegisters
	'EmitLn("PUSH ebx")
	'EmitLn("PUSH edi")
	'EmitLn("PUSH esi")
End Sub

Sub PopSavedRegisters
	'EmitLn("POP esi")
	'EmitLn("POP edi")
	'EmitLn("POP ebx")
End Sub

Sub Return(k)
	EmitLn("RET " & k * 4)
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

Sub CleanArguments(k)
	If k > 0 Then
		EmitLn("POP ebx")
		EmitLn("ADD esp, " & k * 4)
		EmitLn("PUSH ebx")
	End If
End Sub

Sub LocAlloc(k)
	If k > 0 Then
		EmitLn("MOV ebx, DWORD [esp]")
		EmitLn("MOV DWORD [esp], 0")
		If k <> 1 Then
			EmitLn("SUB esp, " & (k - 1) * 4)
		End If
		EmitLn("PUSHD ebx")
	End If
End Sub

Sub LocFree(k,L)
	Dim i,arr
	arr = FormalParamST.Keys
	If Not UBound(arr) = -1 Then
		Do
			If GetDataType(arr(i)) = "STR" Then
				If GetIdentType(arr(i)) = "array" Then
					FreeStringArrayLoc(arr(i))
				End If
				FreeHeapBufferLoc(ParamNumber(arr(i)))
			End If
			i = i + 1
		Loop While i <= UBound(arr)
	End If
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

Sub FreeStringArrayLoc(n)
	Dim o
	o = ParamNumber(n)
	EmitLn("MOV ebx, DWORD [ebp + " & ((NumParams + 2) * 4) - (o * 4) & "]")
	EmitLn("MOV ecx, DWORD [ebx]")
	PostLabel("@@")
	EmitLn("PUSH ecx")
	EmitLn("ADD ebx, 4")
	EmitLn("invoke HeapFree,[hHeap],0,DWORD [ebx]")
	EmitLn("POP ecx")
	EmitLn("LOOP @b")
End Sub

Sub LocAllocMain(k)
	If k > 0 Then
		EmitLn("SUB esp, " & (k + 1) * 4)
		EmitLn("PUSH ebp")
		EmitLn("MOV ebp, esp")
	End If
End Sub

Sub LocFreeMain(k)
	If k > 0 Then
		EmitLn("MOV esp, ebp")
		EmitLn("POP ebp")
		EmitLn("ADD esp, " & (k + 1) * 4)
	End If
End Sub

Sub AllocateLocalArray(o,i)
	EmitLn("invoke HeapAlloc,[hHeap],HEAP_ZERO_MEMORY," & i * 4 + 4)
	EmitLn("MOV DWORD [ebp + " & ((NumParams + 2) * 4) - (o * 4) & "], eax")
	EmitLn("MOV DWORD [eax], " & i)
End Sub

Sub CopyArray(n)
	If IsParam(n) Then
		o = ((NumParams + 2) * 4) - (ParamNumber(n) * 4)
		EmitLn("MOV esi, DWORD [ebp + " & o & "]")
	Else
		EmitLn("MOV esi, [V_" & n & "]")
	End If
	EmitLn("MOV eax, DWORD [esi]")
	EmitLn("INC eax")
	EmitLn("SHL eax, 2")
	EmitLn("MOV ebx, eax")
	EmitLn("invoke HeapAlloc,[hHeap],HEAP_ZERO_MEMORY,eax")
	EmitLn("MOV edi, eax")
	EmitLn("MOV ecx, ebx")
	EmitLn("REP MOVSB")
End Sub

Sub CopyStringArray(n)
	Dim o
	If IsParam(n) Then
		o = ((NumParams + 2) * 4) - (ParamNumber(n) * 4)
		EmitLn("MOV esi, DWORD [ebp + " & o & "]")
	Else
		EmitLn("MOV esi, [V_" & n & "]")
	End If
	EmitLn("MOV eax, DWORD [esi]")
	EmitLn("INC eax")
	EmitLn("SHL eax, 2")
	EmitLn("invoke HeapAlloc,[hHeap],HEAP_ZERO_MEMORY,eax")
	EmitLn("MOV edi, eax")
	EmitLn("MOV ecx, DWORD [esi]")
	EmitLn("MOV DWORD [eax], ecx")
	EmitLn("PUSH eax")
	PostLabel("@@")
	EmitLn("PUSH ecx")
	EmitLn("ADD edi, 4")
	EmitLn("ADD esi, 4")
	
	EmitLn("MOV edx, DWORD [esi]")
	EmitLn("MOV edx, DWORD [edx]")
	EmitLn("ADD edx, 5")
	EmitLn("invoke HeapAlloc,[hHeap],HEAP_ZERO_MEMORY,edx")
	EmitLn("PUSH edi")
	EmitLn("PUSH esi")
	EmitLn("MOV edi, eax")
	EmitLn("MOV esi, DWORD [esi]")
	EmitLn("MOV ecx, DWORD [esi]")
	EmitLn("MOV DWORD [edi], ecx")
	CopyString
	EmitLn("POP esi")
	EmitLn("POP edi")
	EmitLn("MOV DWORD [edi], eax")
	
	EmitLn("POP ecx")
	EmitLn("LOOP @b")
	EmitLn("POP eax")
End Sub

Sub LoadStringFromArray(n)
	If IsParam(n) Then
		EmitLn("MOV esi, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
	Else
		EmitLn("MOV esi, [V_" & n & "]")
	End If
	EmitLn("ADD esi, eax")
	EmitLn("MOV esi, DWORD [esi]")
	EmitLn("MOV eax, esi")
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'String Code Generation Routines
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub AllocateHeapBuffer(n)
	EmitLn("invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, " & n)
End Sub

Sub FreeHeapBuffer(n)
	EmitLn("invoke HeapFree, [hHeap], 0, [V_" & n & "]")
End Sub

Sub FreeHeapBufferLoc(n)
	Dim Offset:Offset = ((NumParams + 2) * 4) - (n * 4)
	EmitLn("invoke HeapFree, [hHeap], 0, [ebp + " & Offset & "]")
End Sub

Sub FreeHeapBufferArray(n)
	Dim o
	If IsParam(n) Then
		o = ((NumParams + 2) * 4) - (ParamNumber(n) * 4)
		EmitLn("MOV ebx, DWORD [ebp + " & o & "]")
	Else
		EmitLn("MOV ebx, [V_" & n & "]")
	End If
	EmitLn("ADD ebx, eax")
	EmitLn("PUSH eax")
	EmitLn("invoke HeapFree,[hHeap],HEAP_ZERO_MEMORY,[ebx]")
	EmitLn("POP eax")
End Sub

Sub CopyStringToBuf(L)
	EmitLn("MOV esi, " & L)
	EmitLn("MOV ecx, DWORD [esi]")
	EmitLn("MOV DWORD [eax], ecx")
	EmitLn("MOV edi, eax")
	CopyString
End Sub

Sub CopyStringVar(n)
	If IsParam(n) Then
		EmitLn("MOV esi, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
	ElseIf Constants.Exists(n) Then
		EmitLn("MOV esi, " & Constants.Item(n)(1))
	Else
		EmitLn("MOV esi, [V_" & n & "]")
	End If
	EmitLn("MOV ebx, DWORD [esi]")
	EmitLn("ADD ebx, 5")
	EmitLn("invoke HeapAlloc,[hHeap], HEAP_ZERO_MEMORY, ebx")
	EmitLn("SUB ebx, 5")
	EmitLn("MOV DWORD [eax], ebx")
	EmitLn("MOV edi, eax")
	EmitLn("MOV ecx, ebx")
	CopyString
End Sub

Sub CopyStringFromArray(n)
	If IsParam(n) Then
		EmitLn("MOV esi, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) & "]")
	Else
		EmitLn("MOV esi, [V_" & n & "]")
	End If
	EmitLn("ADD esi, eax")
	EmitLn("MOV esi, DWORD [esi]")
	EmitLn("MOV ebx, DWORD [esi]")
	EmitLn("ADD ebx, 5")
	EmitLn("invoke HeapAlloc,[hHeap], HEAP_ZERO_MEMORY, ebx")
	EmitLn("SUB ebx, 5")
	EmitLn("MOV DWORD [eax], ebx")
	EmitLn("MOV edi, eax")
	EmitLn("MOV ecx, ebx")
	CopyString
End Sub

Sub Concat(WasABufferAllocated)
	EmitLn("MOV esi, eax")
	EmitLn("MOV eax, DWORD [esp]")
	EmitLn("MOV eax, DWORD [eax]")
	EmitLn("ADD eax, DWORD [esi]")
	EmitLn("ADD eax, 5")
	EmitLn("MOV edi, DWORD [esp]")
	EmitLn("invoke HeapReAlloc,[hHeap],HEAP_ZERO_MEMORY,edi,eax")
	EmitLn("MOV DWORD [esp], esi")
	EmitLn("MOV edi, eax")
	EmitLn("MOV ecx, DWORD [esi]")
	EmitLn("MOV ebx, ecx")
	EmitLn("ADD ebx, DWORD [edi]")
	EmitLn("ADD edi, DWORD [edi]")
	CopyString
	EmitLn("MOV DWORD [eax], ebx")
	If WasABufferAllocated Then
		EmitLn("PUSHD eax")
		EmitLn("invoke HeapFree, [hHeap], 0, DWORD [esp + 4]")
		EmitLn("POPD eax")
	End If
	EmitLn("ADD esp, 4")
End Sub

Sub CopyString
	EmitLn("ADD edi, 4")
	EmitLn("ADD esi, 4")
	EmitLn("REP MOVSB")
	EmitLn("MOV BYTE [edi], 0")
End Sub