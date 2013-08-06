Module param_module
implicit none


integer, parameter :: sp=kind(1.0e00)
integer, parameter :: hp=kind(1.0d00)
!
! double precision integers:
!

integer, parameter :: fp=hp


integer, parameter :: SEDTYP_NONCOHESIVE_TOTALLOAD = 0
integer, parameter :: SEDTYP_NONCOHESIVE_SUSPENDED = 1
integer, parameter :: SEDTYP_COHESIVE              = 2

End module param_module
