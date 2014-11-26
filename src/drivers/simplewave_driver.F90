!! This computer program is part of MOSSCO. 
!! @copyright Copyright 2014, Helmholtz-Zentrum Geesthacht
!! @author Knut Klingbeil, IOW

!
! MOSSCO is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License v3+.  MOSSCO is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY.  Consult the file
! LICENSE.GPL or www.gnu.org/licenses/gpl-3.0.txt for the full license terms.

!-----------------------------------------------------------------------

#define REALTYPE real(kind=selected_real_kind(13))
#define _ZERO_   0.0d0
#define _QUART_  0.25d0
#define _HALF_   0.5d0
#define _ONE_    1.0d0
#define _TWO_    2.0d0

   module simplewave_driver

   IMPLICIT NONE
   private
!
! !PUBLIC DATA MEMBERS:
   public wind2waveHeight,wind2wavePeriod,wavePeriod2waveNumber,wbbl_tauw,wbbl_rdrag
   integer,public,parameter :: WBBL_DATA2=1
   integer,public,parameter :: WBBL_SOULSBY05=2
   integer,public           :: waves_bbl_method=WBBL_DATA2
!
! !PRIVATE DATA MEMBER:
   REALTYPE,parameter :: grav=9.81d0
   REALTYPE,parameter :: avmmol=1.8d-6
   REALTYPE,parameter :: kappa=0.4d0
!
! !REVISION HISTORY:
!  These routines are taken from GETM's wave module.
!  Original author(s): Ulf Graewe
!                      Knut Klingbeil
!
!-----------------------------------------------------------------------

   contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wind2waveHeight - estimates significant wave height from wind
!
! !INTERFACE:
   REALTYPE function wind2waveHeight(wind,depth,fetch)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)          :: wind,depth
   REALTYPE,intent(in),optional :: fetch
!
! !DESCRIPTION:
!  Calculates significant wave height (Hm0).
!  If fetch is not provided, unlimited fetch will be assumed.
!  See page 250 in Holthuijsen (2007).
!
! !LOCAL VARIABLES
   REALTYPE           :: depthstar,fetchstar,waveHeightstar
   REALTYPE           :: wind2,windm2,tanhk3dm3,limiter
   REALTYPE,parameter :: waveHeightstar8 = 0.24d0
   REALTYPE,parameter :: k1 = 4.14d-4
   REALTYPE,parameter :: m1 = 0.79d0
   REALTYPE,parameter :: k3 = 0.343d0
   REALTYPE,parameter :: m3 = 1.14d0
   REALTYPE,parameter :: p  = 0.572d0
!
!EOP
!-----------------------------------------------------------------------
!BOC

   wind2 = wind*wind
   windm2 = _ONE_ / wind2

!  dimensionless depth
   depthstar = grav * depth * windm2

   tanhk3dm3 = tanh(k3*depthstar**m3)

   if (present(fetch)) then
!     dimensionless fetch
      fetchstar = grav * fetch * windm2
      limiter = tanh(k1*fetchstar**m1 / tanhk3dm3)
   else
      limiter = _ONE_
   end if

!  dimensionless significant wave height
   waveHeightstar = waveHeightstar8 * (tanhk3dm3*limiter)**p

!  significant wave height
   wind2waveHeight = wind2 * waveHeightstar / grav

   end function wind2waveHeight
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wind2wavePeriod - estimates peak wave period from wind
!
! !INTERFACE:
   REALTYPE function wind2wavePeriod(wind,depth,fetch)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)          :: wind,depth
   REALTYPE,intent(in),optional :: fetch
!
! !DESCRIPTION:
!  Calculates peak wave period.
!  If fetch is not provided, unlimited fetch will be assumed.
!  See page 250 in Holthuijsen (2007).
!  The peak wave period can be empirically related to the significant
!  wave period (Holthuijsen Eqs. (4.2.7) and (4.2.9)).
!
! !LOCAL VARIABLES
   REALTYPE           :: depthstar,fetchstar,wavePeriodstar
   REALTYPE           :: windm2,tanhk4dm4,limiter
   REALTYPE,parameter :: wavePeriodstar8 = 7.69d0
   REALTYPE,parameter :: k2 = 2.77d-7
   REALTYPE,parameter :: m2 = 1.45d0
   REALTYPE,parameter :: k4 = 0.10d0
   REALTYPE,parameter :: m4 = 2.01d0
   REALTYPE,parameter :: q  = 0.187d0
!
!EOP
!-----------------------------------------------------------------------
!BOC

   windm2 = _ONE_ / (wind*wind)

!  dimensionless depth
   depthstar = grav * depth * windm2

   tanhk4dm4 = tanh(k4*depthstar**m4)

   if (present(fetch)) then
!     dimensionless fetch
      fetchstar = grav * fetch * windm2
      limiter = tanh(k2*fetchstar**m2 / tanhk4dm4)
   else
      limiter = _ONE_
   end if

!  dimensionless peak wave period
   wavePeriodstar = wavePeriodstar8 * (tanhk4dm4*limiter)**q

!  peak wave period
   wind2wavePeriod = wind * wavePeriodstar / grav

   end function wind2wavePeriod
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wavePeriod2waveNumber - approximates wave number from wave period
!
! !INTERFACE:
   REALTYPE function wavePeriod2waveNumber(period,depth)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in) :: period,depth
!
! !DESCRIPTION:
!  x=k*D=kD,y=omega/sqrt(g/D)=omegastar
!  y=sqrt(x*tanh(x)),y(1)=0.8727=omegastar1
!  x'=lg(x),(dx'|dx)=1/(x*ln(10))
!  y'=lg(y),(dy'|dy)=1/(y*ln(10))
!  m'(x)=(dy'|dx')=(dy'|dy)*(dy|dx)*(dx|dx')=x/y*m(x)
!  m(x)=(dy|dx)=0.5*[tanh(x)+x*(1-tanh(x)**2)]/sqrt(x*tanh(x))
!  m(1)=0.677,m'(1)=0.77572=slopestar1
!  y'=lg(y(1))+m'(1)*x' <=> y=y(1)*[x**m'(1)] <=> x=(y/y(1))**(1/m'(1))
!  shallow: y=x       => x<=y(1)**(1/(1  -m'(1)))=0.5449  => y<=0.5449
!  deep   : y=sqrt(x) => x>=y(1)**(1/(0.5-m'(1)))=1.63865 => y>=1.28
!
!  For alternatives see Holthuijsen (2007) page 124
!  (Eckart, 1952 and Fenton, 1988)
!
! !LOCAL VARIABLES
   REALTYPE           :: omega,omegastar,omegastar2,kD
   REALTYPE,parameter :: sqrtgrav_rec = _ONE_/sqrt(grav)
   REALTYPE,parameter :: omegastar1_rec = _ONE_/0.8727d0
   REALTYPE,parameter :: slopestar1_rec = _ONE_/0.77572d0
   REALTYPE,parameter :: one5th = _ONE_/5
   REALTYPE,parameter :: pi=3.1415926535897932384626433832795029d0
!
!EOP
!-----------------------------------------------------------------------
!BOC

   omega = _TWO_ * pi / period ! radian frequency
   omegastar = omega * sqrt(depth) * sqrtgrav_rec ! non-dimensional radian frequency
   omegastar2 = omegastar*omegastar

!!   approximation by Knut
!!   (errors less than 5%)
!   if ( omegastar .gt. 1.28d0 ) then
!!     deep-water approximation
!      kD = omegastar**2
!   else if ( omegastar .lt. 0.5449d0 ) then
!!     shallow-water approximation
!      kD = omegastar
!   else
!!     tangential approximation in loglog-space for full dispersion relation
!      kD = (omegastar1_rec * omegastar) ** slopestar1_rec
!   end if

!  approximation by Soulsby (1997, page 71) (see (18) in Lettmann et al., 2009)
!  (errors less than 1%)
   if ( omegastar .gt. _ONE_ ) then
      kD = omegastar2 * ( _ONE_ + one5th*exp(_TWO_*(_ONE_-omegastar2)) )
   else
      kD = omegastar * ( _ONE_ + one5th*omegastar2 )
   end if

   wavePeriod2waveNumber = kD / depth

   end function wavePeriod2waveNumber
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wbbl_tauw - calculates wave-only bottom stress
!
! !INTERFACE:
   REALTYPE function wbbl_tauw(waveT,waveH,waveK,depth,z0,wbl)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in)           :: waveT,waveH,waveK,depth,z0
!
! !OUTPUT PARAMETERS:
   REALTYPE,intent(out),optional :: wbl
!
! !DESCRIPTION:
!
! !LOCAL VARIABLES:
   REALTYPE           :: Hrms,omegam1,uorb,aorb,Rew,tauwr,tauws,tauwl
   logical,save       :: first=.true.
   REALTYPE,save      :: avmmolm1
   REALTYPE,parameter :: sqrthalf=sqrt(_HALF_)
   REALTYPE,parameter :: pi=3.1415926535897932384626433832795029d0
   REALTYPE,parameter :: oneovertwopi=_HALF_/pi
   REALTYPE,parameter :: Rew_crit = 5.0d5 ! (Stanev et al., 2009)
   !REALTYPE,parameter :: Rew_crit = 1.5d5 ! (Soulsby & Clarke, 2005)
   REALTYPE,parameter :: ar = 0.24d0 ! 0.26d0
   REALTYPE,parameter :: as = 0.24d0 ! 0.22d0
!
!EOP
!-----------------------------------------------------------------------
!BOC

   if (first) then
      avmmolm1 = _ONE_ / avmmol
      first = .false.
   end if

   if (waveT .gt. _ZERO_) then
      Hrms = sqrthalf * waveH
      omegam1 = oneovertwopi * waveT
!     wave orbital velocity amplitude at bottom (peak orbital velocity, ubot in SWAN)
      uorb = _HALF_ * Hrms / ( omegam1*sinh(waveK*depth) )
!     wave orbital excursion
      aorb = omegam1 * uorb
!     wave Reynolds number
      Rew = aorb * uorb * avmmolm1

!     Note (KK): We do not calculate fw alone, because for small
!                uorb this can become infinite.

!     KK-TODO: For combined wave-current flow, the decision on
!              turbulent or laminar flow depends on Rew AND Rec!
!              (Soulsby & Clarke, 2005)
!              However, here we decide according to Lettmann et al. (2009).
!              (Or do we want to assume always turbulent currents?)
      if ( Rew .gt. Rew_crit ) then
!        wave friction factor for rough turbulent flow
         !fwr = 1.39d0 * (aorb/z0(i,j))**(-0.52d0)
         tauwr = _HALF_ * 1.39d0 * (omegam1/z0)**(-0.52d0) * uorb**(2-0.52d0)
!        wave friction factor for smooth turbulent flow
         !fws = 0.0521d0 * Rew**(-0.187d0)
         tauws = _HALF_ * (omegam1*avmmolm1)**(-0.187d0) * uorb**(2-2*0.187d0)

!        Note (KK): For combined wave-current flow, the decision on
!                   rough or smooth flow depends on the final taubmax.
!                   (Soulsby & Clarke, 2005)
!                   However, here we decide according to Stanev et al. (2009).
!                   (as for wave-only flow)
!        wave friction factor
         !fw = max( fwr , fws )
!        wave-only bottom stress
         !tauw = _HALF_ * fw * uorb**2
         wbbl_tauw = max( tauwr , tauws )
      else
!        wave friction factor for laminar flow
         !fwl = _TWO_ * Rew**(-_HALF_)
         !fw = fwl
         tauwl = uorb / sqrt(omegam1*avmmolm1)
         wbbl_tauw = tauwl
      end if

!     bbl thickness (Soulsby & Clarke, 2005)
      if (present(wbl)) wbl = max( 12.0d0*z0 , ar*omegam1*sqrt(wbbl_tauw) )

   else
      wbbl_tauw = _ZERO_
      if (present(wbl)) wbl = 12.0d0*z0
   end if

   end function wbbl_tauw
!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: wbbl_rdrag - calculates mean bottom friction
!
! !INTERFACE:
   REALTYPE function wbbl_rdrag(tauc,tauw,rdragc,vel,depth,wbbl,z0)

! !USES:
   IMPLICIT NONE
!
! !INPUT PARAMETERS:
   REALTYPE,intent(in) :: tauc,tauw,rdragc,vel,depth,wbbl,z0
!
! !DESCRIPTION:
!  rough => total stress => includes form drag (Whitehouse, 2000, page 57)
!  smooth => skin-friction
!
! !LOCAL VARIABLES:
   REALTYPE :: taue_vel,lnT1m1,lnT2,T3,A1,A2,sqrtcd,cd
!
!EOP
!-----------------------------------------------------------------------
!BOC

   select case(waves_bbl_method)
      case (WBBL_DATA2) ! DATA2 formula for rough flow (Soulsby, 1995, 1997)
         wbbl_rdrag = (_ONE_ + 1.2d0*(tauw/(tauc+tauw))**3.2d0) * rdragc
      case (WBBL_SOULSBY05) ! Soulsby & Clarke (2005) for rough flow
         taue_vel = ( tauc**2 + tauw**2 ) ** _QUART_
!!        extension by Malarkey & Davies (2012)
!         taue_vel = (tauc**2 + tauw**2 + _TWO_*tauc*tauw*cos(angle))**_QUART_
         lnT1m1 = _ONE_ / log( wbbl / z0 )
         lnT2 = log( depth / wbbl )
         T3 = taue_vel / vel
         A1 = _HALF_ * T3 * (lnT2-_ONE_) * lnT1m1
         A2 = kappa * T3 * lnT1m1
         sqrtcd = sqrt(A1**2 + A2) - A1
         cd = sqrtcd*sqrtcd
         wbbl_rdrag = cd * vel
   end select

   end function wbbl_rdrag
!EOC
!-----------------------------------------------------------------------
   end module simplewave_driver

