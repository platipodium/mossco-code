program BioDriver

! this program is a driver for calculating the biological effects of microphytobenthos and macrofauna
! on the sediment flux.
! An interface (Macrofauna_interface) has been inbroduced to handle various macrofauna species within it.
! At the moment only Macoma bathica has been implemented. In the following, microphytobenthos and macrofauan
! are initialized, set, run and finalized. The biological bioturbation effect (currently only the biological
! effects on the sediment erodibility and the critical bed shear stress are only included.
! however, many other bilogical effects including biogenic structures, direct bio-resuspension and deposition
! are included as templates in macrofauna class. They can be activated, when required.

use Macrofauna_interface
use Microphytobenthos_class

implicit none

type (microphytobenthos)  :: Micro
type (BioturbationEffect) :: Total_Bioturb
real (fp)    :: tau
real (fp)    :: Erod

! setting up microphytobenthos effects
call init_microphyt(Micro)
call set_microphyt(Micro)
call run_microphyt(Micro)

! setting up macrofauna effect (currently only Macoma balthica)
call Macrofanua_init (Total_Bioturb)
call Macrofanua_set ()
call Macrofanua_run (Total_Bioturb)

! assuming an abiotic critical bed shear stress (tau) and erodibility
tau = 1.9
Erod = 0.00006


write (*,*) 'Abitotic critical tau =' , tau, 'Abiotic Erodibility = ', Erod
write (*,*)

 tau = tau * Micro%TauEffect(1,1,1)
 Erod = Erod * Micro%ErodibilityEffect(1,1,1)


write (*,*) 'critical tau (microphytobenthos) =' , tau
write (*,*)
write (*,*) 'Erodibility (microphytobenthos) =',Erod
write (*,*)

open (unit = 12, file= 'result.out',action = 'write ', status = 'replace')

write (12,*) 'critical tau (microphytobenthos) =' ,tau
write (12,*) 'Biotic erodibility=', Micro%ErodibilityEffect
write (12,*) 'Biotic Critical bed shear stress effect= ',Micro%TauEffect


tau = tau * Total_Bioturb%TauEffect(1,1,1)
Erod = Erod * Total_Bioturb%ErodibilityEffect(1,1,1)

write (*,*) 'tau (macrofaunau and microphytobenthos) =' ,tau,' Both Biotic Critical bed shear stress effect= '& 
       &       ,Total_Bioturb%TauEffect(1,1,1), 'Both Biotic erodibility',Total_Bioturb%ErodibilityEffect(1,1,1)

write (*,*)

write (12,*) 'tau (macrofaunau and microphytobenthos) =' ,tau

write (12,*) 'Both Biotic effects on the critical bed shear stress effect= ',Total_Bioturb%TauEffect

write (12,*) 'Both Biotic erodibility =', Total_Bioturb%ErodibilityEffect(1,1,1)

! finalizing the classes
 call fin_micropyht(Micro)
 call Macrofanua_fin(Total_Bioturb)

 close (12)

 end program BioDriver
