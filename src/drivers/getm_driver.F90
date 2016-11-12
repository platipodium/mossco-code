#include "cppdefs.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: getm_driver - driver routines for GETM
!
! !INTERFACE:
   module getm_driver
!
! !DESCRIPTION:
!
! !USES:
   IMPLICIT NONE

   interface
      subroutine tracer_diffusion(f,hn,AH_method,AH_const,AH_Prt,AH_stirr_const, &
                                  phymix)
         use domain, only: imin,imax,jmin,jmax,kmax
         IMPLICIT NONE
         REALTYPE,intent(in)           :: hn(I3DFIELD)
         integer,intent(in)            :: AH_method
         REALTYPE,intent(in)           :: AH_const,AH_Prt,AH_stirr_const
         REALTYPE,intent(inout)        :: f(I3DFIELD)
         REALTYPE,dimension(:,:,:),pointer,intent(out),optional :: phymix
      end subroutine tracer_diffusion
   end interface

   private
!
! !PUBLIC DATA MEMBERS:
   public do_transport,do_transport_3d
   public zero_gradient_3d_bdy
!
! !REVISION HISTORY:
!  Original author(s): Knut Klingbeil
!
!EOP
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: do_transport() - transport of 2D fields
!
! !INTERFACE:
   subroutine do_transport(f)
!
! !DESCRIPTION:
!
! !USES:
   use domain      , only: imin,imax,jmin,jmax,az,H
   use m2d         , only: dtm,Uint,Vint
   use advection   , only: do_advection,HALFSPLIT,P2_PDM
   use variables_3d, only: Dn,Dun,Dvn,sseo
   use halo_zones  , only: update_2d_halo,wait_halo,H_TAG
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
! !INPUT/OUPUT PARAMETERS:
   REALTYPE,dimension(E2DFIELD),intent(inout) :: f
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(E2DFIELD) :: Dold
   REALTYPE,parameter           :: AH=_ZERO_
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'do_transport() # ',Ncall
#endif

!  Cannot extract layer heights from grid coordinates, because these are
!  already updated.
!  For several timesteps we need to store Dires and calculate new
!  D[old|[u|v]n] based on Dires.
!  For several timesteps [U|V]int is inconsistent.
   Dold = sseo + H
   call update_2d_halo(f,f,az,imin,jmin,imax,jmax,H_TAG)
   call wait_halo(H_TAG)
   call do_advection(dtm,f,Uint,Vint,Dun,Dvn,Dold,Dn,HALFSPLIT,P2_PDM,AH,H_TAG)

#ifdef DEBUG
   write(debug,*) 'Leaving do_transport()'
   write(debug,*)
#endif
   return

   end subroutine do_transport
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: do_transport_3d() - transport of 3D fields
!
! !INTERFACE:
   subroutine do_transport_3d(f,ws)
!
! !DESCRIPTION:
!
! !USES:
   use domain      ,only: imin,imax,jmin,jmax,kmax,az
   use m3d         ,only: dt,cnpar
   use les         ,only: les_mode,LES_BOTH,LES_TRACER
   use advection_3d,only: do_advection_3d
   use variables_3d,only: uu,vv,ww,hun,hvn,ho,hn,nuh
   use halo_zones  ,only: update_3d_halo,wait_halo,H_TAG
   use advection   ,only: HALFSPLIT
   use util        ,only: P2_PDM,NEUMANN,FLUX
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,dimension(I3DFIELD),intent(in)    :: ws
!
! !INPUT/OUPUT PARAMETERS:
   REALTYPE,dimension(I3DFIELD),intent(inout) :: f
!
! !REVISION HISTORY:
!  Original Author(s): Knut Klingbeil
!
! !LOCAL VARIABLES
   REALTYPE,dimension(0:kmax) :: sour,Taur,ws1d
   integer                    :: i,j
   integer                    :: AH_method=2
   REALTYPE                   :: AH_const=1.4d-7
   REALTYPE                   :: AH_Prt=_TWO_
   REALTYPE                   :: AH_stirr_const=_ONE_
!
!EOP
!-----------------------------------------------------------------------
!BOC
#ifdef DEBUG
   integer, save :: Ncall = 0
   Ncall = Ncall+1
   write(debug,*) 'do_transport_3d() # ',Ncall
#endif
#ifdef GETM_SLICE_MODEL
   j = jmax/2
#endif

!  see comments in do_transport()
   call update_3d_halo(f,f,az,imin,jmin,imax,jmax,kmax,H_TAG)
   call wait_halo(H_TAG)
   call do_advection_3d(dt,f,uu,vv,ww,hun,hvn,ho,hn,HALFSPLIT,P2_PDM,P2_PDM,_ZERO_,H_TAG)

   if (les_mode.eq.LES_BOTH .or. les_mode.eq.LES_TRACER) then

      call update_3d_halo(f,f,az,imin,jmin,imax,jmax,kmax,H_TAG)
      call wait_halo(H_TAG)
      call tracer_diffusion(f,hn,AH_method,AH_const,AH_Prt,AH_stirr_const)
   end if

   sour = _ZERO_
   Taur = 1.d15
   ws1d(0   ) = _ZERO_
   ws1d(kmax) = _ZERO_
#ifndef GETM_SLICE_MODEL
   do j=jmin,jmax
#endif
      do i=imin,imax
         if (az(i,j) .eq. 1) then
!           Do advection step due to settling or rising
            ws1d(1:kmax-1) = _HALF_ * ( ws(i,j,1:kmax-1) + ws(i,j,2:kmax) )
            call adv_center(kmax,dt,hn(i,j,:),hn(i,j,:),ws1d,FLUX,FLUX, &
                            _ZERO_,_ZERO_,P2_PDM,1,f(i,j,:))
            call diff_center(kmax,dt,cnpar,1,hn(i,j,:),NEUMANN,NEUMANN, &
                             _ZERO_,_ZERO_,nuh(i,j,:),sour,sour,Taur,   &
                             f(i,j,:),f(i,j,:))
         end if
      end do
#ifndef GETM_SLICE_MODEL
   end do
#else
   f(:,j+1,:) = f(:,j,:)
#endif

#ifdef DEBUG
   write(debug,*) 'Leaving do_transport_3d()'
   write(debug,*)
#endif
   return

   end subroutine do_transport_3d
!EOC
!-----------------------------------------------------------------------


   subroutine zero_gradient_3d_bdy(f,hackmax)
   use domain, only: imin,imax,jmin,jmax,kmax,az,au,av
   use halo_zones  ,only: update_3d_halo,wait_halo,H_TAG

   REALTYPE,dimension(I3DFIELD),intent(inout) :: f
   REALTYPE, intent(in) :: hackmax
   REALTYPE,dimension(0:kmax) :: hackmaxvec
   logical :: clip=.false.
   ! hackmax: if negative, do not change boundary state, otherwise clip
   integer :: i,j
#ifdef GETM_SLICE_MODEL
   j = jmax/2
#endif

   clip = hackmax > 0.0
   hackmaxvec(:) = hackmax
!
!***************************************************************
!
!           LEVEL1 'clip',clip

   ! a halo update is necessary here to be fully consistent in parallel
   call update_3d_halo(f,f,az,imin,jmin,imax,jmax,kmax,H_TAG)
   call wait_halo(H_TAG)

   ! set zero-gradient in x-direction
#ifndef GETM_SLICE_MODEL
   do j=jmin,jmax
#endif
     do i=imin,imax
       ! western boundary
       if ((au(i,j) .eq. 2) .and. (au(i-1,j) .eq. 0)) then
         if (clip) then
           f(i,j,:) = min(hackmaxvec,f(i+1,j,:))
         else
           f(i,j,:) = f(i+1,j,:)
         end if
       end if
       ! eastern boundary
       if ((au(i-1,j) .eq. 2) .and. (au(i,j) .eq. 0)) then
         if (clip) then
           f(i,j,:) = min(hackmaxvec,f(i-1,j,:))
         else
           f(i,j,:) = f(i-1,j,:)
         end if
       end if
     end do
#ifndef GETM_SLICE_MODEL
   end do
#endif

   ! set zero-gradient in y-direction
!
!***************************************************************
!
#ifndef GETM_SLICE_MODEL
   do j=jmin,jmax
#endif
     do i=imin,imax
       ! southern boundary
       if ((av(i,j) .eq. 2) .and. (av(i,j-1) .eq. 0)) then
         if (clip) then
           f(i,j,:) = min(hackmaxvec,f(i,j+1,:))
         else
           f(i,j,:) = f(i,j+1,:)
         end if
       end if
       ! northern boundary
       if ((av(i,j-1) .eq. 2) .and. (av(i,j) .eq. 0)) then
         if (clip) then
           f(i,j,:) = min(hackmaxvec,f(i,j-1,:))
         else
           f(i,j,:) = f(i,j-1,:)
         end if
       end if
     end do
#ifndef GETM_SLICE_MODEL
   end do
#endif

   end subroutine zero_gradient_3d_bdy


   end module getm_driver

!-----------------------------------------------------------------------
! Copyright (C) 2013 - Knut Klingbeil                                  !
!-----------------------------------------------------------------------
