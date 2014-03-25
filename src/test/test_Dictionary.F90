!> @file test_Dictionary.F90
!! @brief test mossco_dictionary module
!! @author Richard Hofmeister
!!
!! create dictionary, add keys and dump dictionary

program test_Netcdf
use mossco_dictionary

type(type_mossco_dictionary) :: dict

write(0,*) 'create dictionary and add keys'
!dict => mossco_dictionary_create()

call dict%set_value(key='Richard',value='Klingbeil')
call dict%set_value(key='Carsten',value='Lemmen')

write(0,*) 'dump dictionary:'
call dict%dump()

write(0,*) 'correct key Richard in dictionary'
call dict%set_value(key='Richard',value='Hofmeister')

write(0,*) 'dump dictionary:'
call dict%dump()

write(0,*) 'finished dictionary test'

end program
