format PE GUI 4.0
entry start
include 'fasm\include\win32a.inc'

section '.text' code readable writeable executable

start:
invoke GetProcessHeap
MOV DWORD [hHeap], eax
JMP Main
Main:
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
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 15
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
MOV eax, 600
PUSHD eax
MOV eax, 300
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
MOV eax, L3
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
ADD esp, 4
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
CALL V_EDITCOMPONENT
ADD esp, 20
MOV [V_EDITID], eax
MOV eax, DWORD [V_EDITID]
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, 1
SUB eax, DWORD [esp]
ADD esp, 4
NEG eax
PUSHD eax
CALL V_SETEDITMAXLENGHT
ADD esp, 8
MOV eax, DWORD [V_EXISTING?]
TEST eax, eax
JZ L5
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
JMP L4
L5:
L4:
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
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 11
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
MOV eax, DWORD [V_FILEMENUID]
PUSHD eax
MOV eax, 1
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
CALL V_APPENDMENUITEM
ADD esp, 12
MOV eax, DWORD [V_FILEMENUID]
PUSHD eax
CALL V_APPENDMENUSEPARATOR
ADD esp, 4
MOV eax, DWORD [V_FILEMENUID]
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
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, DWORD [V_MENUID]
PUSHD eax
MOV eax, DWORD [V_FILEMENUID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 9
MOV esi, L9
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
MOV eax, 3
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
CALL V_APPENDMENUITEM
ADD esp, 12
MOV eax, DWORD [V_EDITMENUID]
PUSHD eax
MOV eax, 4
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 12
MOV esi, L11
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
MOV esi, L12
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
MOV eax, DWORD [V_COMMANDMENUID]
PUSHD eax
MOV eax, 5
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 17
MOV esi, L13
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
MOV eax, DWORD [V_COMMANDMENUID]
PUSHD eax
MOV eax, 6
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 14
MOV esi, L14
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
MOV eax, DWORD [V_COMMANDMENUID]
PUSHD eax
MOV eax, 7
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 14
MOV esi, L15
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
MOV eax, DWORD [V_COMMANDMENUID]
PUSHD eax
MOV eax, 8
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 14
MOV esi, L16
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
MOV eax, DWORD [V_COMMANDMENUID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 13
MOV esi, L17
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
MOV eax, 9
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 13
MOV esi, L18
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
MOV eax, DWORD [V_HELPMENUID]
PUSHD eax
MOV eax, 10
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 12
MOV esi, L19
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
MOV esi, L20
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
CALL V_CREATESTATUSBAR
ADD esp, 4
MOV [V_STATUSBARID], eax
MOV eax, DWORD [V_STATUSBARID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 16
MOV esi, L21
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_SETCOMPONENTTEXT
ADD esp, 8
MOV eax, 0
PUSHD eax
MOV eax, 0
PUSHD eax
CALL V_CREATEFONT
ADD esp, 8
MOV DWORD [ebp + 8], eax
MOV eax, DWORD [V_EDITID]
PUSHD eax
MOV eax, DWORD [ebp + 8]
PUSHD eax
CALL V_SETFONT
ADD esp, 8
L22:
CALL V_HANDLEWINDOWEVENTS
PUSHD eax
MOV eax, 0
MOV ebx, DWORD [esp]
ADD esp, 4
CMP ebx, eax
MOV eax, 0
SETE al
IMUL eax, 0xFFFFFFFF
TEST eax, eax
JZ L22
L23:
MOV esp, ebp
POP ebp
ADD esp, 12
invoke ExitProcess,0

V_CLOSEAPPLICATION:
PUSH ebp
MOV ebp, esp
MOV eax, DWORD [V_MODIFIED?]
TEST eax, eax
JZ L25
MOV eax, DWORD [V_EXISTING?]
TEST eax, eax
JZ L27
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 58
MOV esi, L29
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 12
MOV esi, L30
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
JZ L31
CALL V_SAVEFILE
JMP L28
L31:
L28:
MOV eax, -1
JMP RETCLOSEAPPLICATION
JMP L26
L27:
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 138
MOV esi, L33
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 12
MOV esi, L30
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
JZ L34
MOV eax, -1
JMP RETCLOSEAPPLICATION
JMP L32
L34:
L32:
MOV eax, 0
JMP RETCLOSEAPPLICATION
JMP L26
L26:
JMP L24
L25:
MOV eax, -1
JMP RETCLOSEAPPLICATION
JMP L24
L24:
MOV eax, 0
RETCLOSEAPPLICATION:
MOV esp, ebp
POP ebp
RET 0
V_SAVEFILE:
PUSH ebp
MOV ebp, esp
MOV eax, DWORD [V_EXISTING?]
NOT eax
TEST eax, eax
JZ L36
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 29
MOV esi, L37
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 10
MOV esi, L38
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
JMP L35
L36:
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
JMP L35
L35:
MOV eax, 0
RETSAVEFILE:
MOV esp, ebp
POP ebp
RET 0
V_WINDOWCALLBACK:
PUSH ebp
MOV ebp, esp
MOV eax, DWORD [V_STATUSBARID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 8
MOV esi, L39
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV eax, DWORD [V_EDITID]
PUSHD eax
CALL V_GETCURSORLINE
ADD esp, 4
PUSHD eax
CALL V_CSTRU
ADD esp, 4
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
MOV eax, L40
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
ADD esp, 4
PUSHD eax
MOV eax, DWORD [V_EDITID]
PUSHD eax
CALL V_GETCURSORCOL
ADD esp, 4
PUSHD eax
CALL V_CSTRU
ADD esp, 4
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
CALL V_SETCOMPONENTTEXT
ADD esp, 8
MOV eax, DWORD [ebp + 12]
PUSHD eax
MOV eax, WM_CLOSE
CMP eax, DWORD [esp]
JNE L42
CALL V_CLOSEAPPLICATION
TEST eax, eax
JZ L44
MOV eax, 0
PUSHD eax
CALL V_POSTQUITMESSAGE
ADD esp, 4
JMP L43
L44:
L43:
JMP L41
L42:
MOV eax, WM_SIZE
CMP eax, DWORD [esp]
JNE L45
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
MOV eax, 80
SUB eax, DWORD [esp]
ADD esp, 4
NEG eax
PUSHD eax
CALL V_RESIZECOMPONENT
ADD esp, 20
MOV eax, DWORD [V_STATUSBARID]
PUSHD eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
CALL V_WINDOWHEIGHT
ADD esp, 4
PUSHD eax
MOV eax, 20
SUB eax, DWORD [esp]
ADD esp, 4
NEG eax
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
CALL V_WINDOWWIDTH
ADD esp, 4
PUSHD eax
MOV eax, 20
PUSHD eax
CALL V_RESIZECOMPONENT
ADD esp, 20
JMP L41
L45:
MOV eax, WM_COMMAND
CMP eax, DWORD [esp]
JNE L46
MOV eax, DWORD [ebp + 16]
PUSHD eax
MOV eax, 0
CMP eax, DWORD [esp]
JNE L48
MOV eax, DWORD [V_EXISTING?]
TEST eax, eax
JZ L50
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
JMP L49
L50:
L49:
JMP L47
L48:
MOV eax, 1
CMP eax, DWORD [esp]
JNE L51
CALL V_SAVEFILE
JMP L47
L51:
MOV eax, 2
CMP eax, DWORD [esp]
JNE L52
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
MOV eax, WM_CLOSE
PUSHD eax
MOV eax, 0
PUSHD eax
MOV eax, 0
PUSHD eax
CALL V_SENDMESSAGE
ADD esp, 16
JMP L47
L52:
MOV eax, 9
CMP eax, DWORD [esp]
JNE L53
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 48
MOV esi, L54
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 18
MOV esi, L55
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
JMP L47
L53:
MOV eax, 10
CMP eax, DWORD [esp]
JNE L56
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 34
MOV esi, L57
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 12
MOV esi, L19
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
JMP L47
L56:
MOV eax, EN_UPDATE
PUSHD eax
MOV eax, 65536
IMUL DWORD [esp]
ADD esp, 4
CMP eax, DWORD [esp]
JNE L58
MOV eax, DWORD [V_MODIFIED?]
NOT eax
TEST eax, eax
JZ L60
MOV eax, -1
MOV [V_MODIFIED?], eax
MOV eax, DWORD [V_WINDOWID]
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 6
MOV esi, L61
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
MOV eax, DWORD [V_FILENAME]
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
ADD esp, 4
PUSHD eax
MOV eax, L3
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
ADD esp, 4
PUSHD eax
CALL V_SETWINDOWTITLE
ADD esp, 8
JMP L59
L60:
L59:
JMP L47
L58:
MOV eax, EN_MAXTEXT
PUSHD eax
MOV eax, 65536
IMUL DWORD [esp]
ADD esp, 4
CMP eax, DWORD [esp]
JNE L62
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 17
MOV esi, L63
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 10
MOV esi, L64
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
JMP L47
L62:
L47:
JMP L41
L46:
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
JMP L41
L41:
MOV eax, 0
RETWINDOWCALLBACK:
MOV esp, ebp
POP ebp
RET 16
include 'LIBS\IO.LIB'
include 'LIBS\WINDOW.LIB'
include 'LIBS\COMMANDLINE.LIB'
include 'LIBS\FILE.LIB'
include 'LIBS\CONVERT.LIB'
include 'LIBS\STRINGS.LIB'
section '.idata' import data readable writeable
library kernel32,'KERNEL32.DLL',\
	 user32,'USER32.DLL',\
	 gdi32,'GDI32.DLL',\
	 opengl,'OPENGL32.DLL',\
	 glu,'GLU32.DLL'
include 'fasm\include\api\kernel32.inc'
include 'fasm\include\api\user32.inc'
include 'fasm\include\api\gdi32.inc'
include 'fasm\include\api\opengl32.inc'

section '.data' data readable writeable
hHeap dd 0
V_WINDOWID dd 0
V_EDITID dd 0
V_STATUSBARID dd 0
V_MENUID dd 0
V_FILEMENUID dd 0
V_HELPMENUID dd 0
V_EDITMENUID dd 0
V_COMMANDMENUID dd 0
V_FILENAME dd 0
V_EXISTING? dd 0
V_MODIFIED? dd 0
L2 dd 10
db '<New File>',0
L3 dd 10
db ' - Minipad',0
L6 dd 6
db 'Reload',0
L7 dd 4
db 'Save',0
L8 dd 4
db 'Exit',0
L9 dd 4
db 'File',0
L10 dd 4
db 'Find',0
L11 dd 7
db 'Replace',0
L12 dd 4
db 'Edit',0
L13 dd 12
db 'Set commands',0
L14 dd 9
db 'Command 1',0
L15 dd 9
db 'Command 2',0
L16 dd 9
db 'Command 3',0
L17 dd 8
db 'Commands',0
L18 dd 8
db 'About...',0
L19 dd 7
db 'License',0
L20 dd 4
db 'Help',0
L21 dd 11
db 'Ln 1, Col 1',0
L29 dd 53
db 'This file has been modified. Do you want to save it ?',0
L30 dd 7
db 'Warning',0
L33 dd 118
db 'You',39,'re about to close a file that doesn',39,'t exist. All the text you',39,'ve written will be lost. Do you still want to exit ?',0
L37 dd 24
db 'This file does not exist',0
L38 dd 5
db 'error',0
L39 dd 3
db 'Ln ',0
L40 dd 6
db ', Col ',0
L54 dd 43
db 'This is a Win32 notepad created with DBasic',0
L55 dd 13
db 'About Minipad',0
L57 dd 29
db 'Do whatever the fuck you want',0
L61 dd 1
db '*',0
L63 dd 12
db 'Out of space',0
L64 dd 5
db 'Error',0

section '.rsrc' resource data readable
directory RT_ICON,icons,\
RT_GROUP_ICON,group_icons,\
RT_VERSION,versions
resource icons,1,LANG_NEUTRAL,icon_data
resource group_icons,17,LANG_NEUTRAL,main_icon
resource versions,1,LANG_NEUTRAL,version
icon main_icon,icon_data,'EXAMPLES\Minipad\minipad.ico'
versioninfo version,VOS__WINDOWS32,VFT_APP,VFT2_UNKNOWN,LANG_ENGLISH+SUBLANG_DEFAULT,0,\
'FileDescription','DBasic notepad',\
'LegalCopyright','No rights reserved',\
'FileVersion','1.0',\
'ProductVersion','1.0',\
'OriginalFilename','MINIPAD.EXE'
