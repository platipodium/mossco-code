!> @file test_Dictionary.F90
!! @brief test mossco_dictionary module
!! @author Richard Hofmeister
!!
!! create dictionary, add keys and dump dictionary

program test
use mossco_dictionary

type(type_mossco_dictionary) :: dict,dict2
character(len=20)            :: string

write(0,*) 'create dictionary and add keys'
!dict => mossco_dictionary_create()

call dict%set_value(key='Richard',string='Klingbeil')
call dict%set_value(key='Carsten',string='Lemmen')

write(0,*) 'dump dictionary:'
call dict%dump()
write(0,*) '----------------'

call dict2%set_value(key='Kai',string='Wirtz')
call dict2%set_value(key='Hans',string='Burchard')
call dict2%set_value(key='Frank',string='Koesters')
call dict2%set_value(key='years',integer=3)

call dict%add_dictionary(key='again',dictionary=dict2)
write(0,*) 'dump dictionary:'
call dict%dump()
write(0,*) '----------------'

write(0,*) 'correct key Richard in dictionary'
call dict%set_value(key='Richard',string='Hofmeister')

write(0,*) 'dump dictionary:'
call dict%dump()
write(0,*) '----------------'
write(0,*)

write(0,*) 'key years has top be of type integer (=3):',dict2%get_type('years')

call dict2%get_value('Hans',string=string)
write(0,*) 'last name of Hans: '//trim(string)
write(0,*)

write(0,*) 'finished dictionary test'

end program
