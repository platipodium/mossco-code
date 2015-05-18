!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Lagrangian trajectories of the particles drifting under         !!!
!!!       influence of current fields and (turbulent) duffisivity    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! !INTERFACE:
   PROGRAM TRACER
!
! !DESCRIPTION:
!
! !CONVENTIONS:
!  M    = i = North South !=lat = Y = V
!  N    = j = West East   !=lon = X = U
!  ILO  = k = Top Bottom  !=km  = Z = W
!
! !USES:
!$ use omp_lib
   use tracer_driver
!
   IMPLICIT NONE
!
! !LOCAL VARIABLES:
   integer           :: istep_main, nstep_main, dt_main
!
!-----------------------------------------------------------------------

!*****************************************************************
!  initialisation
!*****************************************************************
   print*,'-----------------------------'
   print*,'---      INITIALIZE       ---'
   print*,'-----------------------------'
   call init_tracer

   dt_main    = dt*3600 ! main timestep in seconds !!
   nstep_main = ltg_max ! number of iterations


!*****************************************************************
!  main time-loop
!*****************************************************************
   print*,'-----------------------------'
   print*,'---   STARTING MAINLOOP   ---'
   print*,'-----------------------------'
   do istep_main = 1, nstep_main
      call do_mainstep_tracer(dt_main)
   enddo

!*****************************************************************
!  finish
!*****************************************************************
   print*,'-----------------------------'
   print*,'---        FINALIZE       ---'
   print*,'-----------------------------'
   call finalize_tracer

!=======================================================================
   end ! PROGRAM TRACER
!=======================================================================

