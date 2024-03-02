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

Dim CharLib,CounterLib,ValueLib,TextLib

Dim ErrLn
Dim DebugMode
Dim PassNumber

Dim InMethod:InMethod = False
Dim current_class:current_class = ""

'---------------------------------------------------------------------
'Definition Of Keywords And Token Types
Dim KWList
KWList = Array(	"IF","THEN","ELSEIF","ELSE","ENDIF","WHILE","LOOP","DIM",_
		"GLOBAL","ENDGLOBAL","MAIN","ENDMAIN","DO","LOOP",_
		"REPEAT","UNTIL","BREAK","SELECT","CASE","ENDSELECT",_
		"SUB","ENDSUB","RETURN","END","GOTO","INT","STR",_
		"OR","XOR","AND","NOT","CALL","INIT","ENDINIT","METADATA",_
		"ENDMETADATA","ENUMERATION","ENDENUMERATION","CLASS","ENDCLASS",_
		"PROPERTY","METHOD","ENDMETHOD","SELF")
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
'Types Declarations
Dim TypeList:Set TypeList = CreateObject("Scripting.Dictionary")
'<TypeName> :: <BufferSize>
'The BufferSize needs to be -1 if there is no need for a buffer
TypeList.Add "INT", -1
TypeList.Add "STR", -1

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
	If FileName = "/?" Or FileName = "-help" Or FileName = "-h" Then
		DisplayHelp
		WScript.Quit
	End If
	If Not Fso.FileExists(FileName) Then
		Abort("File not found")
	End If
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
		CommandLine =	CurrentPath & "\fasm\FAsm.exe " & CurrentPath &_
				"\output.asm " & Left(WScript.Arguments.Item(0),_
				InStr(WScript.Arguments.Item(0),".") - 1) & ".exe"

	Else
		CommandLine =	CurrentPath & "\fasm\FAsm.exe " & CurrentPath &_
				"\output.asm " & Shell.CurrentDirectory & "\" &_
				Left(WScript.Arguments.Item(0),InStr(WScript.Arguments.Item(0),_
				".") - 1) & ".exe"
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

Sub DisplayHelp
	WScript.Echo "DBasic compiler - Written by JGN1722"
	WScript.Echo "Usage:"
	WScript.Echo "DBasic.vbs [option]|[filename.dbs]"
	WScript.Echo ""
	WScript.Echo "Options:"
	WScript.Echo "/? | -help | -h: Display this message"
End Sub

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
	WScript.Echo "(Ln: " & ErrLn & ") Error: " & s & "."
	WScript.Quit
End Sub

Sub Expected(s)
	Abort("'" & s & "' expected (instead of '" & Value & "')")
End Sub

Sub Undefined(n)
	If InMethod Then
		If IsProperty(current_class,n) Or IsMethod(current_class,n) Then
			Abort("Undefined identifier: " & n & VbCrLf &_
			      "You might have misspelled: SELF." & n)
		End If
	Else
		Abort("Undefined identifier ('" & n & "')")
	End If
End Sub

Sub Duplicate(n)
	Abort("Duplicate identifier ('" & n & "')")
End Sub

Function IsAlpha(c)
	If c <> "" Then IsAlpha = Asc(c) >= 65 And Asc(c) <= 90 Or Asc(c) >= 97 And Asc(c) <= 122 Or ASc(c) = 63 Or Asc(c) = 95
End Function

Function IsDigit(c)
	If c <> "" Then IsDigit = Asc(c) > 47 And Asc(c) < 58
End Function

Function IsAlNum(c)
	IsAlNum = IsAlpha(c) Or IsDigit(c)
End Function

Function IsAddop(c)
	IsAddop = c = "+" Or c = "-"
End Function

Function IsMulop(c)
	IsMulop = c = "*" Or c = "/" Or c = "%"
End Function

Function IsBitWiseOp(c)
	IsBitWiseOp = c = "<" Or c = ">"
End Function

Function IsOrop(c)
	IsOrop = c = "|" Or c = "~" Or c = "OR" Or c = "XOR"
End Function

Function IsRelop(c)
	IsRelop = c = "=" Or c = "<" Or c = ">" Or c = "!"
End Function

Function IsWhite(c)
	IsWhite = c = " " Or c = Chr(9) Or c = Chr(13) Or c = Chr(10)
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

Sub CheckIsKeyword(n)
	If IsKeyword(n) Then Abort("Reserved keyword used as identifier ('" & n & "')")
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

Function PredictValue(i)
	Dim pos_backup, token_backup, value_backup, look_backup
	pos_backup = StreamPos
	token_backup = Token
	value_backup = Value
	look_backup = Look
	Do While i <> 0
		Next1
		i = i - 1
	Loop
	PredictValue = Value
	StreamPos = pos_backup
	Token = token_backup
	Value = value_backup
	Look = look_backup
End Function

Function GetIdentType(n)
	If IsParam(n) Then
		GetIdentType = FormalParamST.Item(n)(1)
	ElseIf InTable(n) Then
		GetIdentType = ST.Item(n)(0)
	ElseIf Constants.Exists(n) Then
		GetIdentType = "constant"
	End If
End Function

Function GetDataType(n)
	Dim member_name,class_name
	If n = "SELF" Then
		GetDataType = current_class
	ElseIf InStr(n,".") <> 0 And n <> "" Then
		class_name = Left(n,InStr(n,".") - 1)
		member_name = Mid(n,InStr(n,".") + 1)
		If ClassST.Exists(class_name) Then GetDataType = ClassST.Item(class_name).Item(member_name)(1)
	ElseIf IsParam(n) Then
		GetDataType = FormalParamST.Item(n)(2)
	ElseIf InTable(n) Then
		GetDataType = ST.Item(n)(1)
	ElseIf Constants.Exists(n) Then
		GetDataType = Constants.Item(n)(0)
	End If
End Function

Function GetAdditionalInfo(n)
	Dim member_name,class_name
	If InStr(n,".") <> 0 Then
		class_name = Left(n,InStr(n,".") - 1)
		member_name = Mid(n,InStr(n,".") + 1)
		GetAdditionalInfo = ClassST.Item(class_name).Item(member_name)(2)
	ElseIf IsParam(n) Then
		GetAdditionalInfo = FormalParamST.Item(n)(3)
	ElseIf InTable(n) Then
		GetAdditionalInfo = ST.Item(n)(2)
	End If
End Function

Function Get2ndAdditionalInfo(n)
	Dim member_name,class_name
	If InStr(n,".") <> 0 Then
		class_name = Left(n,InStr(n,".") - 1)
		member_name = Mid(n,InStr(n,".") + 1)
		GetAdditionalInfo = ClassST.Item(class_name).Item(member_name)(3)
	ElseIf IsParam(n) Then
		GetAdditionalInfo = FormalParamST.Item(n)(4)
	ElseIf InTable(n) Then
		GetAdditionalInfo = ST.Item(n)(3)
	End If
End Function

Function GetPropertyPosition(class_name,n)
	GetPropertyPosition = ClassST.Item(class_name).Item(n)(3)
End Function

Function SetAdditionalInfo(n,v)
	If IsParam(n) Then
		FormalParamST.Item(n)(3) = v
	ElseIf InTable(n) Then
		ST.Item(n)(2) = v
	End If
End Function

Function IsType(n)
	IsType = TypeList.Exists(n)
End Function

Function GetTypeBufferSize(n)
	GetTypeBufferSize = TypeList.Item(n)
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
		RegisterClass
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

Sub RegisterClass
	Dim class_name, member_name, property_count,arg_count,t:t = "INT"
	Dim temp_dict:Set temp_dict = CreateObject("Scripting.Dictionary")
	
	MatchString("CLASS")
	If TypeList.Exists(Value) Then
		Abort("Already declared class: " & Value)
	End If
	CheckIsKeyword(Value)
	class_name = Value
	Next1
	
	property_count = 0
	arg_count = 0
	Do While Value <> "ENDCLASS"
		Select Case Value
			Case "PROPERTY"
				MatchString("PROPERTY")
				If IsType(Value) Then
					t = Value
					Next1
				End If
				
				If temp_dict.Exists(Value) Then
					Abort("Already declared property: " & Value)
				End If
				CheckIsKeyword(Value)
				
				temp_dict.Add Value, Array("property",t,False,property_count)
				property_count = property_count + 4
				
				Next1
				t = "INT"
			Case "METHOD"
				MatchString("METHOD")
				member_name = Value
				Next1
				
				MatchString("(")
				Do While Not Token = ")"
					If IsType(Value) Then Next1
					arg_count = arg_count + 1
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
					If Not IsType(Value) Then
						Abort("Undefined type (" & Value & ")")
					End If
					t = Value
					Next1
				End If
				
				temp_dict.Add member_name, Array("method",t,arg_count)
				
				SkipBlock("METHOD")
				t = "INT"
				arg_count = 0
			Case Else
				Expected("Method or property declaration")
		End Select
	Loop
	
	MatchString("ENDCLASS")
	
	ClassST.Add class_name,temp_dict
	TypeList.Add class_name, property_count
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
			If IsType(Value) Then Next1
			arr(2) = arr(2) + 1
			Next1
			If Token = "[" Then
				MatchString("[")
				MatchString("]")
			End If
			If Not Token = ")" Then MatchString(",")
		Loop
		MatchString(")")
		If Token = "(" Then
			MatchString("(")
			Next1
			MatchString(")")
		End If
		If Token = ":" Then
			MatchString(":")
			If Not IsType(Value) Then
				Abort("Undefined type (" & Value & ")")
			End If
			arr(1) = Value
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
	
	PassNumber = 2
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
						If ValueLib <> "STR" And ValueLib <> "INT" Then
							arr(2) = arr(2) + 1
						End If
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
	WriteMetadata
	TopDecls
	Enumerations
	CompileMethods
	Prolog
	AllocateGlobalArrays
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
				DoPass
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
				DoPass
			Else
				Constants.Add Value, Array("INT",i)
				i = i + 1
				Next1
			End If
		Loop
		MatchString("ENDENUMERATION")
	Loop
End Sub

Sub CompileMethods
	InMethod = True
	Do While Value = "CLASS"
		CompileClassMethods
	Loop
	current_class = ""
	InMethod = False
End Sub

Sub CompileClassMethods
	Dim class_name,method_name
	Dim arg_count, t:t = "INT"
	MatchString("CLASS")
	current_class = Value
	Next1
	
	Do While Value <> "ENDCLASS"
		If Value = "PROPERTY" Then
			MatchString("PROPERTY")
			If IsType(Value) Then
				Next1
			End If
			Next1
		Else
			DoMethod
		End If
	Loop
	
	MatchString("ENDCLASS")
End Sub

Sub InitBlock
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

Sub Allocate(n,i)
	If i = -1 Then
		EmitLnD("V_" & n & " dd 0")
	Else
		EmitLnD("V_" & n & " rd " & i)
	End If
End Sub

Sub Alloc(t,n)
	CheckDup(n)
	Allocate n,GetTypeBufferSize(t)
	AddEntry n, Array("variable",t,False)
End Sub

Sub AllocArray(t,n,i)
	CheckDup(n)
	Allocate n
	AddEntry n,Array("array",t,i)
End Sub

Sub AllocGlobalArraySpace(n,i)
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
				If IsType(Value) Then
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
					If IsType(Value) Then
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
				If IsType(Value) Then
					t = Value
					If t <> "INT" And t <> "STR" Then
						Abort("Incorrect constant type: " & t)
					End If
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
					If IsType(Value) Then
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
	Select Case Token
		Case "="
			MatchString("=")
			If GetDataType(n) = "INT" Then
				BoolExpression
				Store(n)
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
				Else
					Push
					FreeHeapBuffer(n)
					Pop
				End If
				Store(n)
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
			MatchString("*")
			AssignMul(n)
		Case "&"
			MatchString("&")
			AssignConcat(n)
	End Select
End Sub

Sub AssignAdd(n)
	MatchString("=")
	CheckTable(n)
	If Not GetDataType(n) = "INT" Then Abort("Cannot assign and add to " & n & " (invalid type)")
	Expression
	If IsParam(n) Then
		EmitLn("ADD DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "], eax")
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
		EmitLn("SUB DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "], eax")
	Else
		EmitLn("SUB [V_" & n & "], eax")
	End If
End Sub

Sub AssignMul(n)
	MatchString("=")
	CheckTable(n)
	If Not GetDataType(n) = "INT" Then Abort("Cannot assign and multiply to " & n & " (invalid type)")
	Expression
	If IsParam(n) Then
		EmitLn("IMUL DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
		EmitLn("MOV DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "], eax")
	Else
		EmitLn("IMUL [V_" & n & "]")
		EmitLn("MOV [V_" & n & "], eax")
	End If
End Sub

Sub AssignConcat(n)
	MatchString("=")
	CheckTable(n)
	If Not GetDataType(n) = "STR" Then Abort("Cannot assign and concatenate to " & n & " (invalid type)")
	LoadVar(n)
	Push
	AppendStringTerm
	Do While Token = "&"
		MatchString("&")
		Push
		AppendStringTerm
	Loop
	Store(n)
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

Sub PropertyAssignement(n)
	Dim class_name, property_name
	class_name = GetDataType(n)
	MatchString(".")
	property_name = Value
	Next1
	MatchString("=")
	If GetDataType(class_name & "." & property_name) = "INT" Then
		BoolExpression
	ElseIf GetDataType(class_name & "." & property_name) = "STR" Then
		StringExpression
		Push
		FreeHeapBufferProperty n, property_name
		Pop
	Else
		Abort("Unexpected data type: '" & GetDataType(class_name & "." & property_name) & "'")
	End If
	StoreProperty n, property_name
End Sub

Sub Block(L,Ret)
	Dim n
	Do While InStr(EndKeywords,"|" & Value & "|") = 0
		Select Case Value
			Case ";"	:Semi
			Case "IF"	:DoIf L,Ret
			Case "SELECT"	:DoSelect L,Ret
			Case "WHILE"	:DoWhile Ret
			Case "DO"	:DoLoop Ret
			Case "REPEAT"	:DoRepeat Ret
			Case "BREAK"	:DoBreak L
			Case "$"	:InlineAsm
			Case "RETURN"	:DoReturn(Ret)
			Case "END"	:DoEnd
			Case "GOTO"	:DoGoto
			Case "CALL"	:DoCall
			Case "."	:DoPass
			Case Else:
				n = Value
				Next1
				If GetIdentType(n) = "procedure" Then
					CallProc(n)
					If GetDataType(n) = "STR" Then
						FreeMainReg
					End If
				ElseIf Token = "." Then
					If IsMethod(GetDataType(n),PredictValue(1)) Then
						CallMethod(n)
					ElseIf IsProperty(GetDataType(n),PredictValue(1)) Then
						PropertyAssignement(n)
					Else
						Abort("Missing property or method: " & GetDataType(n) & "." & PredictValue(1))
					End If
				ElseIf GetIdentType(n) = "variable" Then
					Assignement(n)
				ElseIf GetIdentType(n) = "array" Then
					ArrayAssignement(n)
				ElseIf Token = ":" Then
					PostLabel("V_" & n)
					MatchString(":")
					AddEntry n, Array("label","","")
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
	Dim n,method_name
	MatchString("CALL")
	If Not InTable(Value) And Not IsParam(Value) Then
		If Value <> "SELF" Or Not InMethod Then Undefined(Value)
	End If
	If GetIdentType(Value) = "procedure" Then
		n = Value
		Next1
		CallProc(n)
		If GetDataType(n) = "STR" Then
			FreeMainReg
		End If
	Else
		n = Value
		Next1
		If Token = "." And IsMethod(GetDataType(n),PredictValue(1)) Then
			method_name = PredictValue(1)
			CallMethod(n)
			If GetDataType(GetDataType(n) & "." & method_name) = "STR" Then
				FreeMainReg
			End If
		Else
			Abort(n & " is not a procedure")
		End If
	End If
End Sub

Sub CallProc(n)
	Dim nbytes:nbytes = ParamList
	If Not GetAdditionalInfo(n) = nbytes / 4 Then Abort(_
		"Wrong number of arguments while calling " & n &_
		VbCrLf & "given: " & nbytes / 4 &_
		VbCrLf & "expected: " & GetAdditionalInfo(n))
	JmpToProc(n)
	CleanStack(nbytes)
End Sub

Sub CallMethod(n)
	Dim method_name
	MatchString(".")
	method_name = Value
	Next1
	Dim nbytes:nbytes = ParamList
	If Not GetAdditionalInfo(GetDataType(n) & "." & method_name) = nbytes / 4 Then Abort(_
		"Wrong number of arguments while calling " &_
		GetDataType(n) & "." & method_name & "()" &_
		VbCrLf & "given: " & nbytes / 4 &_
		VbCrLf & "expected: " & GetAdditionalInfo(GetDataType(n) & "." & method_name))
	If n = "SELF" And InMethod Then
		EmitLn("MOV eax, DWORD [ebp + 8]")
	Else
		LoadPointer(n)
	End If
	Push
	JmpToMethod(GetDataType(n) & "_V_" & method_name)
	CleanStack(nbytes + 4)
End Sub

Function ParamList
	Dim n
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

Sub Param
	Dim t
	If Token = "," Or Token = ")" Then
		PushNull
	Else
		If Value = "ARRAY" Then
			MatchString("ARRAY")
			LoadVar(Value)
			Next1
			MatchString("[")
			MatchString("]")
		ElseIf Token = Chr(34) Or Token = "'" Or GetDataType(Value) = "STR" Then
			StringExpression
		ElseIf Token = "x" And GetDataType(Value) <> "INT" Then
			t = GetDataType(GetDataType(Value) & "." & PredictValue(2))
			If t = "INT" Then
				BoolExpression
			ElseIf t = "STR" Then
				StringExpression
			ElseIf Not InTable(Value) Then
				Undefined(Value)
			Else
				Abort("Unexpected type: " & t)
			End If
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
Sub BitWiseFactor
	Dim n,o
	If Token = "(" Then
		Next1
		BoolExpression
		MatchString(")")
	ElseIf Token = "@" Then
		MatchString("@")
		If GetIdentType(Value) <> "" Then
			LoadPointer(Value)
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
			If GetDataType(n) <> "INT" Then Abort("Type mismatch, procedure " & n & " is not of type INT")
			CallProc(n)
		ElseIf Token = "." Then
			If IsProperty(GetDataType(n),PredictValue(1)) Then
				MatchString(".")
				If GetDataType(GetDataType(n) & "." & Value) <> "INT" Then Abort("Type mismatch, property " & n & "." & Value & " is not of type INT")
				LoadProperty n,Value
				Next1
			ElseIf IsMethod(GetDataType(n),PredictValue(1)) Then
				If GetDataType(GetDataType(n) & "." & PredictValue(1)) <> "INT" Then Abort("Type mismatch, method " & GetDataType(n) & "." & PredictValue(1) & " is not of type INT")
				CallMethod(n)
			Else
				Abort("Missing property or method: " & GetDataType(n) & "." & Value)
			End If
		ElseIf GetIdentType(n) = "array" Then
			If GetDataType(n) <> "INT" Then Abort("Type mismatch, array " & n & " is not of type INT")
			MatchString("[")
			Expression
			ConvertArrayOffset
			MatchString("]")
			LoadArrayCell n
		ElseIf InTable(n) Or IsParam(n) Then
			If GetDataType(n) <> "INT" Then Abort("Type mismatch, variable " & n & " is not of type INT")
			LoadVar(n)
		ElseIf Constants.Exists(n) Then
			If Constants.Item(n)(0) <> "INT" Then Abort("Type mismatch, constant " & n & " is not of type INT")
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

Sub ShiftLeft
	MatchString("<")
	MatchString("<")
	BitWiseFactor
	PopShiftLeft
End Sub

Sub ShiftRight
	MatchString(">")
	MatchString(">")
	BitWiseFactor
	PopShiftRight
End Sub

Sub Factor
	BitWiseFactor
	Do While IsBitWiseOp(Token)
		Push
		Select Case Token
			Case "<":ShiftLeft
			Case ">":ShiftRight
		End Select
	Loop
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
	Do While IsOrop(Value)
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
	dim n,o,s,L,length
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
			If GetDataType(n) <> "STR" Then Abort(n & " does not return a string")
			CallProc(n)
		ElseIf Value = "." Then
			If IsProperty(GetDataType(n),PredictValue(1)) Then
				MatchString(".")
				If n = "SELF" And InMethod Then
					EmitLn("MOV eax, DWORD [ebp + 8]")
				Else
					LoadPointer(n)
				End If
				If GetDataType(GetDataType(n) & "." & Value) <> "STR" Then Abort("Type mismatch, property " & n & "." & Value & " is not of type STR")
				CopyPropertyString n,Value
				Next1
			ElseIf IsMethod(GetDataType(n),PredictValue(1)) Then
				If GetDataType(GetDataType(n) & "." & PredictValue(1)) <> "STR" Then Abort("Type mismatch, method " & GetDataType(n) & "." & PredictValue(1) & " is not of type STR")
				CallMethod(n)
			Else
				Abort("Missing property or method: " & GetDataType(n) & "." & Value)
			End If
		ElseIf GetIdentType(n) = "array" Then
			If GetDataType(n) <> "STR" Then Abort(n & " is not a string array")
			MatchString("[")
			Expression
			ConvertArrayOffset
			MatchString("]")
			CopyStringFromArray n
		ElseIf InTable(n) Or IsParam(n) Then
			If GetDataType(n) <> "STR" Then Abort(n & " is not of type STR")
			CopyStringVar(n)
		ElseIf Constants.Exists(n) Then
			If Constants.Item(n)(0) <> "STR" Then Abort(n & " is not of type STR")
			CopyStringVar(n)
		Else
			Undefined(n)
		End If
	Else
		Expected("String")
	End If
End Sub

Sub AppendStringTerm
	dim n,o,s,L,length,NeedToFree
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
		ElseIf Value = "." Then
			If IsProperty(GetDataType(n),PredictValue(1)) Then
				MatchString(".")
				If GetDataType(GetDataType(n) & "." & Value) <> "STR" Then Abort("Type mismatch, property " & n & "." & Value & " is not of type STR")
				If n = "SELF" And InMethod Then
					EmitLn("MOV eax, DWORD [ebp + 8]")
				Else
					LoadPointer(n)
				End If
				o = GetPropertyPosition(GetDataType(n),Value)
				If o <> 0 Then
					EmitLn("MOV eax, DWORD [eax + " & o & "]")
				Else
					EmitLn("MOV eax, DWORD [eax]")
				End If
				Next1
			ElseIf IsMethod(GetDataType(n),PredictValue(1)) Then
				If GetDataType(GetDataType(n) & "." & PredictValue(1)) <> "STR" Then Abort("Type mismatch, method " & GetDataType(n) & "." & PredictValue(1) & " is not of type STR")
				CallMethod(n)
				NeedToFree = True
			Else
				Abort("Missing property or method: " & GetDataType(n) & "." & Value)
			End If
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
		Else
			Abort("Incorrect type: " & t)
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
	CheckIsKeyword(Value)
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
		If IsType(Value) Then
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
	If IsType(Value) Then
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
	If IsType(Value) Then
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
			If GetIdentType(arr(i)) = "array" Then
				AllocGlobalArraySpace arr(i),GetAdditionalInfo(arr(i))
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

Sub AddParam(n,DataType,IdentType,AdditionalInfo)
	CheckIsKeyword(n)
	If InTable(n) Then Abort(n & " is already declared in the global scope")
	If IsParam(n) Then Duplicate(n)
	If DataType = "INT" Or DataType = "STR" Then
		NumParams = NumParams + 1
	Else
		NumParams = NumParams + GetTypeBufferSize(DataType)
	End If
	FormalParamST.Add n, Array(NumParams,IdentType,DataType,AdditionalInfo)
End Sub

Sub DoReturn(Ret)
	Dim t
	If Ret = "" Then Abort("RETURN Outside Of A Procedure")
	MatchString("RETURN")
	MatchString("(")
	If Token = ")" Then
		Clear
	Else
		If InMethod Then
			t = GetDataType(current_class & "." & Right(Ret,Len(Ret) - 3))
		Else
			t = GetDataType(Right(Ret,Len(Ret) - 3))
		End If
		If t = "STR" Then
			StringExpression
		ElseIf t = "INT" Then
			BoolExpression
		Else
			Abort("The only returnable types are INT and STR")
		End If
	End If
	MatchString(")")
	Branch(Ret)
End Sub

'---------------------------------------------------------------------
'---------------------------------------------------------------------
'Methods And Classes Handling
'---------------------------------------------------------------------
'---------------------------------------------------------------------
Sub DoMethod
	Dim Name,k,L1
	MatchString("METHOD")
	Name = Value
	L1 = "RET" & Name
	Next1
	MatchString("(")
	FormalList
	MatchString(")")
	If Token = ":" Then
		MatchString(":")
		If IsType(Value) Then
			Next1
		Else
			Expected("Type Name")
		End If
	End If
	If DebugMode = 1 Then
		EmitLn("db '" & class_name & "_" & Name & "',0")
	End If
	PostLabel(current_class & "_V_" & Name)
	k = LocDecls
	LocAlloc(k)
	ProcHeader
	PushSavedRegisters
	AllocateLocalArrays
	Block "",L1
	LocFree k,L1
	PopSavedRegisters
	MatchString("ENDMETHOD")
	Return(0)
	ClearParams
End Sub

Function AdjustOffset
	If InMethod Then
		AdjustOffset = 4
	Else
		AdjustOffset = 0
	End If
End Function

Function IsMethod(class_name,n)
	IsMethod = False
	If ClassST.Exists(class_name) Then
		If ClassST.Item(class_name).Exists(n) Then
			If ClassST.Item(class_name).Item(n)(0) = "method" Then
				IsMethod = True
			End If
		End If
	End If
End Function

Function IsProperty(class_name,n)
	IsProperty = False
	If ClassST.Exists(class_name) Then
		If ClassST.Item(class_name).Exists(n) Then
			If ClassST.Item(class_name).Item(n)(0) = "property" Then
				IsProperty = True
			End If
		End If
	End If
End Function

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
	If IsParam(n) Then
		EmitLn("MOV eax, ebp")
		EmitLn("ADD eax, " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset)
	Else
		EmitLn("MOV eax, V_" & UCase(n))
	End If
End Sub

Sub LoadVar(n)
	If IsParam(n) Then
		EmitLn("MOV eax, DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
	Else
		EmitLn("MOV eax, DWORD [V_" & n & "]")
	End If
End Sub

Sub LoadProperty(instance_name,property)
	Dim o
	If instance_name = "SELF" And InMethod Then
		EmitLn("MOV eax, DWORD [ebp + 8]")
	Else
		LoadPointer(n)
	End If
	o = GetPropertyPosition(GetDataType(instance_name),property)
	If o <> 0 Then
		EmitLn("ADD eax, " & o)
	End If
	EmitLn("MOV eax, DWORD [eax]")
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

Sub PopShiftLeft
	EmitLn("SHL DWORD [esp], eax")
	EmitLn("MOV eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

Sub PopShiftRight
	EmitLn("SHR DWORD [esp], eax")
	EmitLn("MOV eax, DWORD [esp]")
	EmitLn("ADD esp, 4")
End Sub

Sub Store(n)
	If IsParam(n) Then
		EmitLn("MOV DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "], eax")
	Else
		EmitLn("MOV DWORD [V_" & n & "], eax")
	End If
End Sub

Sub StoreArray(n)
	If IsParam(n) Then
		EmitLn("MOV ebx, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
	Else
		EmitLn("MOV ebx, [V_" & n & "]")
	End If
	EmitLn("ADD ebx, DWORD [esp]")
	EmitLn("MOV DWORD [ebx], eax")
End Sub

Sub StoreProperty(n,property_name)
	Dim o
	If n = "SELF" And InMethod Then
		EmitLn("MOV ebx, DWORD [ebp + 8]")
	ElseIf IsParam(n) Then
		o = ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset
		EmitLn("MOV ebx, ebp")
		EmitLn("ADD ebx, " & o)
	Else
		EmitLn("MOV ebx, V_" & n)
	End If
	o = GetPropertyPosition(GetDataType(n),property_name)
	If o <> 0 Then
		EmitLn("ADD ebx, " & o)
	End If
	EmitLn("MOV DWORD [ebx], eax")
End Sub

Sub LoadArrayCell(n)
	If IsParam(n) Then
		EmitLn("MOV ebx, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
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
		EmitLn("INC DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
	Else
		EmitLn("INC DWORD [V_" & n & "]")
	End If
End Sub

Sub Dec(n)
	If Not GetIdentType(n) = "variable" Then Abort("Identifiers other than variables cannot be decremented")
	If IsParam(n) Then
		EmitLn("DEC DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
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

Sub JmpToMethod(n)
	EmitLn("CALL " & n)
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
	Dim i,ii,arr,a,instance_name,class_name
	arr = FormalParamST.Keys
	If Not UBound(arr) = -1 Then
		Do
			If GetDataType(arr(i)) = "STR" And GetAdditionalInfo(arr(i)) <> True Then
				If GetIdentType(arr(i)) = "array" Then
					FreeStringArrayLoc(arr(i))
				End If
				FreeHeapBufferLoc(ParamNumber(arr(i)))
			ElseIf GetDataType(arr(i)) <> "INT" And GetDataType(arr(i)) <> "STR" Then
				instance_name = arr(i)
				class_name = GetDataType(instance_name)
				a = ClassST.Item(class_name).Keys
				If Not UBound(a) = -1 Then
					Do
						If GetDataType(class_name & "." & ClassST.Item(class_name).Item(a(ii))) = "STR" Then FreeHeapBufferProperty instance_name,a(ii)
						ii = ii + 1
					Loop While ii <= UBound(a)
					ii = 0
				End If
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
	EmitLn("MOV ebx, DWORD [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
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
	EmitLn("MOV DWORD [ebp + " & ((NumParams + 2) * 4) - (o * 4) + AdjustOffset & "], eax")
	EmitLn("MOV DWORD [eax], " & i)
End Sub

Sub LoadStringFromArray(n)
	If IsParam(n) Then
		EmitLn("MOV esi, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
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
	Dim Offset:Offset = ((NumParams + 2) * 4) - (n * 4) + AdjustOffset
	EmitLn("invoke HeapFree, [hHeap], 0, [ebp + " & Offset & "]")
End Sub

Sub FreeHeapBufferArray(n)
	Dim o
	If IsParam(n) Then
		o = ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset
		EmitLn("MOV ebx, DWORD [ebp + " & o & "]")
	Else
		EmitLn("MOV ebx, [V_" & n & "]")
	End If
	EmitLn("ADD ebx, eax")
	EmitLn("PUSH eax")
	EmitLn("invoke HeapFree,[hHeap],HEAP_ZERO_MEMORY,[ebx]")
	EmitLn("POP eax")
End Sub

Sub FreeHeapBufferProperty(n,property)
	Dim o,p
	p = GetPropertyPosition(GetDataType(n),property)
	If n = "SELF" And InMethod Then
		EmitLn("MOV ebx, DWORD [ebp + 8]")
		EmitLn("ADD ebx, " & p)
	ElseIf IsParam(n) Then
		o = ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset
		EmitLn("MOV ebx, ebp")
		EmitLn("ADD ebx, " & o + p)
	Else
		EmitLn("MOV ebx, V_" & n)
		If p <> 0 Then EmitLn("ADD ebx, " & p)
	End If
	EmitLn("invoke HeapFree,[hHeap],HEAP_ZERO_MEMORY,[ebx]")
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
		EmitLn("MOV esi, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
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
		EmitLn("MOV esi, [ebp + " & ((NumParams + 2) * 4) - (ParamNumber(n) * 4) + AdjustOffset & "]")
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

Sub CopyPropertyString(instance_name,property)
	If instance_name = "SELF" And InMethod Then
		EmitLn("MOV esi, DWORD [ebp + 8]")
	ElseIf IsParam(instance_name) Then
		EmitLn("MOV esi, ebp")
		EmitLn("ADD esi, " & ((NumParams + 2) * 4) - (ParamNumber(instance_name) * 4) + AdjustOffset)
	Else
		EmitLn("MOV esi, V_" & instance_name)
	End If
	EmitLn("ADD esi, " & GetPropertyPosition(GetDataType(instance_name),property))
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
