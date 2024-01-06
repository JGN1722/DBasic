format PE GUI 4.0
entry start
include 'fasm\include\win32a.inc'

section '.text' code readable writeable executable

start:
invoke GetProcessHeap
MOV DWORD [hHeap], eax
JMP Main
Main:
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 32
MOV esi, L0
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
invoke HeapAlloc, [hHeap], HEAP_ZERO_MEMORY, 115
MOV esi, L1
MOV ecx, DWORD [esi]
MOV DWORD [eax], ecx
MOV edi, eax
ADD edi, 4
ADD esi, 4
REP MOVSB
MOV BYTE [edi], 0
PUSHD eax
CALL V_RUN
ADD esp, 8
invoke ExitProcess,0

include 'LIBS\IO.LIB'
include 'LIBS\SUBPROCESS.LIB'
section '.idata' import data readable writeable
library kernel32,'KERNEL32.DLL',\
	 user32,'USER32.DLL'
include 'fasm\include\api\kernel32.inc'
include 'fasm\include\api\user32.inc'

section '.data' data readable writeable
hHeap dd 0
L0 dd 27
db 'C:/Windows/System32/cmd.exe',0
L1 dd 110
db ' /c "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe" https://www.youtube.com/watch?v=dQw4w9WgXcQ',0

