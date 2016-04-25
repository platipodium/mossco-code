# Recipe #20: Running multiple instance of FABM

This document describes how to run multiple instances of the pelagic FABM component.

## Coupling

Use the `instance:` facility in your coupling specification (the `.yaml` file) to create two
instances of the `fabm_pelagic` component.

        instances:
        - fabm_pelagic_1: fabm_pelagic
        - fabm_pelagic_2: fabm_pelagic

## Component configuration

The default for every instance of `fabm_pelagic` is to look for a file `fabm_pelagic.nml`
for configuration; also the FABM framework is, by default, initialized with the contents of
the file `fabm.nml`

If, however, there exists a file identical to the name of your fabm_pelagic instance, then
this alternate file is used for configuration.  In the above example, MOSSCO will read
two different configuration files

        fabm_pelagic_1.nml
        fabm_pelagic_2.nml

## FABM configuration

Each of the fabm_pelagic instances will, by default, initialize its FABM models with the contents of
the file `fabm.nml`.  You can change this name by using the `fabm_nml` paramter in the
component's configuration file, for example

        cat /Users/lemmen/temp/bengrid/fabm_pelagic_1.nml
            &fabm_pelagic
              dt=100.
              dt_min=1.0
              albedo_const=0.0
              ode_method=3
              fabm_nml='fabm_1.nml'
            /

and a different configuration

        cat /Users/lemmen/temp/bengrid/fabm_pelagic_2.nml
            &fabm_pelagic
              dt=1000.
              dt_min=10.0
              albedo_const=0.2
              ode_method=2
              fabm_nml='fabm_2.nml'
            /

## Feedback

This is an experimental feature implemented March 2016.  Please provide feedback!
