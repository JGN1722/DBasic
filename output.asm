format PE GUI 4.0
entry start
include 'fasm\include\win32a.inc'

section '.text' code readable writeable executable

start:
invoke GetProcessHeap
MOV DWORD [hHeap], eax
SUB esp, 12
PUSH ebp
MOV ebp, esp
CALL V_GETARGCOUNT
PUSHD eax
MOV eax, 1
MOV ebx, DWORD [esp]
ADD esp, 4
CMP ebx, eax
MOV eax, 0
SETE al
IMUL eax, 0xFFFFFFFF
TEST eax, eax
JZ L1
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 13
MOV esi, L2
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
MOV [V_FILENAME], eax
MOV eax, 0
MOV [V_EXISTING?], eax
JMP L0
L1:
MOV eax, 1
PUSHD eax
CALL V_GETCOMMANDLINEARGUMENT
ADD esp, 4
MOV [V_FILENAME], eax
MOV eax, -1
MOV [V_EXISTING?], eax
JMP L0
L0:
MOV eax, 0
MOV [V_MODIFIED?], eax
MOV eax, 100
PUSHD eax
MOV eax, 100
PUSHD eax
MOV eax, 300
PUSHD eax
MOV eax, 600
PUSHD eax
MOV esi, [V_FILENAME]
MOV ebx, DWORD [esi]
ADD ebx, 5
invoke HeapAlloc,[hHeap], HEAP_ZERO_MEMORY, ebx
SUB ebx, 5
MOV DWORD [eax], ebx
MOV edi, eax
MOV ecx, ebx
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV eax, V_WINDOWCALLBACK
PUSHD eax
CALL V_OPENWINDOW
ADD esp, 24
MOV [V_WINDOWID], eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, 240
PUSHD eax
MOV eax, 585
PUSHD eax
CALL V_CREATEEDIT
ADD esp, 20
MOV [V_EDITID], eax
MOV eax, DWORD [V_EXISTING?]
TEST eax, eax
JZ L4
MOV eax, DWORD [V_EDITID]
PUSHD eax
MOV esi, [V_FILENAME]
MOV ebx, DWORD [esi]
ADD ebx, 5
invoke HeapAlloc,[hHeap], HEAP_ZERO_MEMORY, ebx
SUB ebx, 5
MOV DWORD [eax], ebx
MOV edi, eax
MOV ecx, ebx
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_READFILE
ADD esp, 4
PUSHD eax
CALL V_SETCOMPONENTTEXT
ADD esp, 8
JMP L3
L4:
L3:
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
CALL V_CREATEMENU
ADD esp, 4
MOV [V_MENUID], eax
CALL V_CREATESUBMENU
MOV [V_FILEMENUID], eax
MOV eax, DWORD [V_FILEMENUID]
PUSHD eax
MOV eax, 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 9
MOV esi, L5
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENUITEM
ADD esp, 12
MOV eax, DWORD [V_FILEMENUID]
PUSHD eax
CALL V_APPENDMENUSEPARATOR
ADD esp, 4
MOV eax, DWORD [V_FILEMENUID]
PUSHD eax
MOV eax, 1
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 9
MOV esi, L6
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENUITEM
ADD esp, 12
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, DWORD [V_MENUID]
PUSHD eax
MOV eax, DWORD [V_FILEMENUID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 9
MOV esi, L7
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENU
ADD esp, 16
CALL V_CREATESUBMENU
MOV [V_EDITMENUID], eax
MOV eax, DWORD [V_EDITMENUID]
PUSHD eax
MOV eax, 2
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 9
MOV esi, L8
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENUITEM
ADD esp, 12
MOV eax, DWORD [V_EDITMENUID]
PUSHD eax
MOV eax, 3
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 12
MOV esi, L9
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENUITEM
ADD esp, 12
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, DWORD [V_MENUID]
PUSHD eax
MOV eax, DWORD [V_EDITMENUID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 9
MOV esi, L10
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENU
ADD esp, 16
CALL V_CREATESUBMENU
MOV [V_COMMANDMENUID], eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, DWORD [V_MENUID]
PUSHD eax
MOV eax, DWORD [V_COMMANDMENUID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 13
MOV esi, L11
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENU
ADD esp, 16
CALL V_CREATESUBMENU
MOV [V_HELPMENUID], eax
MOV eax, DWORD [V_HELPMENUID]
PUSHD eax
MOV eax, 4
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 13
MOV esi, L12
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENUITEM
ADD esp, 12
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, DWORD [V_MENUID]
PUSHD eax
MOV eax, DWORD [V_HELPMENUID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 9
MOV esi, L13
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_APPENDMENU
ADD esp, 16
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, 2
PUSHD eax
MOV eax, 65
PUSHD eax
CALL V_KEYBOARDSHORTCUT
ADD esp, 16
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, 1
PUSHD eax
MOV eax, 2
PUSHD eax
MOV eax, 87
PUSHD eax
CALL V_KEYBOARDSHORTCUT
ADD esp, 16
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, 2
PUSHD eax
MOV eax, 2
PUSHD eax
MOV eax, 83
PUSHD eax
CALL V_KEYBOARDSHORTCUT
ADD esp, 16
L14:
CALL V_HANDLEWINDOWEVENTS
MOV DWORD [ebp + 12], eax
MOV eax, DWORD [ebp + 12]
PUSHD eax
MOV eax, 0
MOV ebx, DWORD [esp]
ADD esp, 4
CMP ebx, eax
MOV eax, 0
SETE al
IMUL eax, 0xFFFFFFFF
TEST eax, eax
JZ L14
L15:
CALL V_CLOSEAPPLICATION
MOV esp, ebp
POP ebp
ADD esp, 12
invoke ExitProcess,0

V_CLOSEAPPLICATION:
PUSH ebp
MOV ebp, esp
MOV eax, DWORD [V_MODIFIED?]
TEST eax, eax
JZ L17
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 58
MOV esi, L19
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 12
MOV esi, L20
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV eax, 48
PUSHD eax
MOV eax, 4
ADD eax, DWORD [esp]
ADD esp, 4
PUSHD eax
CALL V_MSGBOX
ADD esp, 12
PUSHD eax
MOV eax, 6
MOV ebx, DWORD [esp]
ADD esp, 4
CMP ebx, eax
MOV eax, 0
SETE al
IMUL eax, 0xFFFFFFFF
TEST eax, eax
JZ L21
CALL V_SAVEFILE
JMP L18
L21:
L18:
JMP L16
L17:
L16:
invoke ExitProcess,0
MOV eax, 0
RETCLOSEAPPLICATION:
MOV esp, ebp
POP ebp
RET
V_SAVEFILE:
PUSH ebp
MOV ebp, esp
MOV eax, DWORD [V_EXISTING?]
NOT eax
TEST eax, eax
JZ L23
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 29
MOV esi, L24
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 10
MOV esi, L25
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV eax, 16
PUSHD eax
CALL V_MSGBOX
ADD esp, 12
JMP L22
L23:
MOV esi, [V_FILENAME]
MOV ebx, DWORD [esi]
ADD ebx, 5
invoke HeapAlloc,[hHeap], HEAP_ZERO_MEMORY, ebx
SUB ebx, 5
MOV DWORD [eax], ebx
MOV edi, eax
MOV ecx, ebx
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV eax, DWORD [V_EDITID]
PUSHD eax
CALL V_GETCOMPONENTTEXT
ADD esp, 4
PUSHD eax
CALL V_WRITEFILE
ADD esp, 8
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV esi, [V_FILENAME]
MOV ebx, DWORD [esi]
ADD ebx, 5
invoke HeapAlloc,[hHeap], HEAP_ZERO_MEMORY, ebx
SUB ebx, 5
MOV DWORD [eax], ebx
MOV edi, eax
MOV ecx, ebx
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_SETWINDOWTITLE
ADD esp, 8
MOV eax, 0
MOV [V_MODIFIED?], eax
JMP L22
L22:
MOV eax, 0
RETSAVEFILE:
MOV esp, ebp
POP ebp
RET
V_WINDOWCALLBACK:
PUSH ebp
MOV ebp, esp
MOV eax, DWORD [ebp + 12]
PUSHD eax
MOV eax, 2
CMP eax, DWORD [esp]
JNE L27
MOV eax, 0
PUSHD eax
CALL V_POSTQUITMESSAGE
ADD esp, 4
JMP L26
L27:
MOV eax, 5
CMP eax, DWORD [esp]
JNE L28
MOV eax, DWORD [V_EDITID]
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
CALL V_WINDOWWIDTH
ADD esp, 4
PUSHD eax
MOV eax, 15
SUB eax, DWORD [esp]
ADD esp, 4
NEG eax
PUSHD eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
CALL V_WINDOWHEIGHT
ADD esp, 4
PUSHD eax
MOV eax, 60
SUB eax, DWORD [esp]
ADD esp, 4
NEG eax
PUSHD eax
CALL V_RESIZECOMPONENT
ADD esp, 20
JMP L26
L28:
MOV eax, 273
CMP eax, DWORD [esp]
JNE L29
MOV eax, DWORD [ebp + 16]
PUSHD eax
MOV eax, 0
CMP eax, DWORD [esp]
JNE L31
CALL V_SAVEFILE
JMP L30
L31:
MOV eax, 1
CMP eax, DWORD [esp]
JNE L32
CALL V_CLOSEAPPLICATION
JMP L30
L32:
MOV eax, 4
CMP eax, DWORD [esp]
JNE L33
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 48
MOV esi, L34
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 18
MOV esi, L35
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV eax, 64
PUSHD eax
CALL V_MSGBOX
ADD esp, 12
JMP L30
L33:
MOV eax, 1024
PUSHD eax
MOV eax, 65536
IMUL DWORD [esp]
ADD esp, 4
CMP eax, DWORD [esp]
JNE L36
MOV eax, DWORD [V_MODIFIED?]
NOT eax
TEST eax, eax
JZ L38
MOV eax, -1
MOV [V_MODIFIED?], eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 6
MOV esi, L39
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV esi, [V_FILENAME]
MOV ebx, DWORD [esi]
ADD ebx, 5
invoke HeapAlloc,[hHeap], HEAP_ZERO_MEMORY, ebx
SUB ebx, 5
MOV DWORD [eax], ebx
MOV edi, eax
MOV ecx, ebx
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
MOV esi, eax
MOV eax, DWORD [esp]
MOV eax, DWORD [eax]
ADD eax, DWORD [esi]
ADD eax, 5
MOV edi, DWORD [esp]
invoke HeapReAlloc,[hHeap],HEAP_ZERO_MEMORY,edi,eax
MOV DWORD [esp], esi
MOV edi, eax
MOV ecx, DWORD [esi]
MOV ebx, ecx
ADD ebx, DWORD [edi]
ADD edi, DWORD [edi]
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
MOV DWORD [eax], ebx
PUSHD eax
invoke HeapFree, [hHeap], 0, DWORD [esp + 4]
POPD eax
ADD esp, 4
PUSHD eax
CALL V_SETWINDOWTITLE
ADD esp, 8
JMP L37
L38:
L37:
JMP L30
L36:
L30:
JMP L26
L29:
MOV eax, 786
CMP eax, DWORD [esp]
JNE L40
CALL V_GETACTIVEWINDOW
PUSHD eax
MOV eax, DWORD [ebp + 8]
MOV ebx, DWORD [esp]
ADD esp, 4
CMP ebx, eax
MOV eax, 0
SETE al
IMUL eax, 0xFFFFFFFF
TEST eax, eax
JZ L42
MOV eax, DWORD [ebp + 16]
PUSHD eax
MOV eax, 0
CMP eax, DWORD [esp]
JNE L44
MOV eax, DWORD [V_EDITID]
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, DWORD [V_EDITID]
PUSHD eax
CALL V_GETTEXTLENGTH
ADD esp, 4
PUSHD eax
CALL V_SELECTTEXT
ADD esp, 12
JMP L43
L44:
MOV eax, 1
CMP eax, DWORD [esp]
JNE L45
CALL V_CLOSEAPPLICATION
JMP L43
L45:
MOV eax, 2
CMP eax, DWORD [esp]
JNE L46
CALL V_SAVEFILE
JMP L43
L46:
L43:
JMP L41
L42:
L41:
JMP L26
L40:
MOV eax, DWORD [ebp + 20]
PUSHD eax
MOV eax, DWORD [ebp + 16]
PUSHD eax
MOV eax, DWORD [ebp + 12]
PUSHD eax
MOV eax, DWORD [ebp + 8]
PUSHD eax
CALL V_DEFAULTMESSAGEHANDLING
ADD esp, 16
JMP RETWINDOWCALLBACK
JMP L26
L26:
MOV eax, 0
RETWINDOWCALLBACK:
MOV esp, ebp
POP ebp
POP ebx
ADD esp, 16
PUSH ebx
RET
include 'LIBS\IO.LIB'
include 'LIBS\WINDOW.LIB'
include 'LIBS\COMMANDLINE.LIB'
include 'LIBS\FILE.LIB'
include 'LIBS\SUBPROCESS.LIB'
section '.idata' import data readable writeable
library kernel32,'KERNEL32.DLL',\
	 user32,'USER32.DLL',\
	 ole32,'OLE32.DLL'
import  ole32,CoInitialize,'CoInitialize'
include 'fasm\include\api\kernel32.inc'
include 'fasm\include\api\user32.inc'

section '.data' data readable writeable
hHeap dd 0
V_WINDOWID dd 0
V_EDITID dd 0
V_MENUID dd 0
V_FILEMENUID dd 0
V_HELPMENUID dd 0
V_EDITMENUID dd 0
V_COMMANDMENUID dd 0
V_FILENAME dd 0
V_EXISTING? dd 0
V_MODIFIED? dd 0
L2 dd 8
db 'New File',0
L5 dd 4
db 'Save',0
L6 dd 4
db 'Exit',0
L7 dd 4
db 'File',0
L8 dd 4
db 'Find',0
L9 dd 7
db 'Replace',0
L10 dd 4
db 'Edit',0
L11 dd 8
db 'Commands',0
L12 dd 8
db 'About...',0
L13 dd 4
db 'Help',0
L19 dd 53
db 'This file has been modified. Do you want to save it ?',0
L20 dd 7
db 'Warning',0
L24 dd 24
db 'This file does not exist',0
L25 dd 5
db 'error',0
L34 dd 43
db 'This is a Win32 notepad created with DBasic',0
L35 dd 13
db 'About Minipad',0
L39 dd 1
db '*',0
