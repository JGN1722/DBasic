format PE GUI 4.0
entry start
include 'fasm\include\win32a.inc'

section '.text' code readable writeable executable

start:
invoke GetProcessHeap
MOV DWORD [hHeap], eax
MOV eax, 0
PUSHD eax
MOV eax, 1
SUB eax, DWORD [esp]
ADD esp, 4
NEG eax
MOV [V_X], eax
MOV eax, 0
PUSHD eax
MOV eax, 1
SUB eax, DWORD [esp]
ADD esp, 4
NEG eax
MOV [V_Y], eax
MOV eax, DWORD [V_X]
PUSHD eax
MOV eax, DWORD [V_Y]
MOV ebx, DWORD [esp]
ADD esp, 4
CMP ebx, eax
MOV eax, 0
SETG al
IMUL eax, 0xFFFFFFFF
TEST eax, eax
JZ L1
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 27
MOV esi, L2
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 5
PUSHD eax
PUSHD 0
CALL V_MSGBOX
ADD esp, 12
JMP L0
L1:
MOV eax, DWORD [V_X]
PUSHD eax
MOV eax, DWORD [V_Y]
MOV ebx, DWORD [esp]
ADD esp, 4
CMP ebx, eax
MOV eax, 0
SETB al
IMUL eax, 0xFFFFFFFF
TEST eax, eax
JZ L4
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 28
MOV esi, L5
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 5
PUSHD eax
PUSHD 0
CALL V_MSGBOX
ADD esp, 12
JMP L0
L4:
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 8
MOV esi, L6
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 5
PUSHD eax
PUSHD 0
CALL V_MSGBOX
ADD esp, 12
JMP L0
L0:
invoke ExitProcess,0

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
V_X dd 0
V_Y dd 0
L2 dd 17
db 'Everything',39,'s okay',0
L3 dd 0
db '',0
L5 dd 18
db 'Man, that',39,'s fucked',0
L6 dd 3
db '...',0

