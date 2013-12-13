module macrofauna_class
! this module defines an abstract object "Macrofauna" as subcalss of BenthosEffect superclass.
! Every  macrofauana species (object) must be instantiated from this class and
! override the original methods.

use BenthosEffect_class
use BioTypes

implicit none

type                    , extends(statevariable)  :: Mc_statevariable
 real (fp)              , pointer :: Intensity=> null ()
end type  Mc_statevariable

type , public , abstract, extends (BenthosEffect) :: Macrofauna

type (Mc_statevariable)  ,pointer :: StateVar=> null ()
type (BioturbationEffect),pointer :: Bioturbation=> null ()
type (BiogenicStrctures) ,pointer :: Biogenic=> null ()
type (DirectBioeffect)   ,pointer :: DirectBioturbation=> null ()

end type Macrofauna


end module macrofauna_class
