!=======================================================================
! CMAQ - hydrodynamic model
!=======================================================================
!
! !DESCRIPTION:
!
!! @mainpage CMAQ
!! CMAQ is a mess!
!!
!! @author Johannes Bieser 
!!
!
! !INTERFACE:
   module cmaq_driver
!
! !USES:
!    use AverageVar, only : AvrT, AvrS, AvrU, AvrV, AvrCumZ, AvrAv, AvrAh, AvrSm
!
   IMPLICIT NONE
!
! !PUBLIC MEMBER FUNCTIONS:
!!  default: all is private
   private
   public init_cmaq, do_mainstep_cmaq, finalize_cmaq
!    public AvrT, AvrS, AvrU, AvrV, AvrCumZ, AvrAv, AvrAh, AvrSm
!
!=======================================================================
   contains
!=======================================================================
!
! !INTERFACE:
   subroutine init_cmaq(grd_ID,SDATE,STIME,EDATE,ETIME)
   implicit none
   integer         ,INTENT(IN)  :: SDATE, STIME, EDATE, ETIME
   character(len=*), INTENT(IN) :: grd_ID
!
! !DESCRIPTION:
!
! !USES:
!   use CommonVar,  only : Restart
!   use ConfigVar,  only : RunNumber, DayLength, dt
!   use TimeVar,    only : Day, Month, Year, Hour, Minute, Second
!   use TimeVar,    only : dayE, monthE, StepOfIteration, SecondOfYear
!   use AverageVar, only : AverageDT
!
! !INPUT PARAMETERS:
! !LOCAL VARIABLES:
!
!-----------------------------------------------------------------------

!   call ReadConfig

!=============   IMPORTANT FOR COUPLING !!  ================
   ! override namelist settings with global definitions

!===========================================================
!   call init


   return

   end subroutine init_cmaq
!
!=======================================================================
!
! !INTERFACE:
   subroutine do_mainstep_cmaq(hours)
   
!
! !DESCRIPTION:
!
! !USES:
   implicit none
!
! !INPUT PARAMETERS:
   integer, intent(in) :: hours
!
! !LOCAL VARIABLES:
!
!-----------------------------------------------------------------------


!      call cmaq_step

   return

   end subroutine do_mainstep_cmaq
!
!=======================================================================
!
! !INTERFACE:
   subroutine finalize_cmaq
!
! !DESCRIPTION:
!
! !USES:
   implicit none
!
!-----------------------------------------------------------------------

   call finish

!    print*,'-----------------------------'
!    print*,'---     END  CMAQ       ---'
!    print*,'-----------------------------'
   return

   end subroutine finalize_cmaq
!
!=======================================================================
   end module cmaq_driver
!=======================================================================

!-----------------------------------------------------------------------
! Copyright (C) 2014 - Markus Kreus                                    !
!-----------------------------------------------------------------------
