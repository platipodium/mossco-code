# Recipe #31: GETM component special features

For interaction with other coupled components, we added in the IOW branch of GETM
and in its MOSSCO component some features that are documented her

## Compile options

 * `-D_NEW_DAF_`:
 * `-D_TEST_TRACERFLUXES_`: Add flux diagnostics for all tracers 
 * `-D_NO_SEALEVEL_CHECK_`: Do not check for negative sealevel (which occurs
     whenever any transported variable becomes undefined)
 * `-D_GETM_EXPORT_EULERIAN_VELOCITIES_`: Do not include Stokes drift in your 
     velocity output, but use Lagrangian velocities
