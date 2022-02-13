!IFNDEF m
m = pb
!ENDIF

MachCFlags = /DPORTABLE_BYTECODE /DFEATURE_WINDOWS
MachLDFlags = 
MachStackSize = 0x200000
MachRebase =

!INCLUDE Makefile.nt
