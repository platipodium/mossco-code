module mossco_dictionary
private

public type_mossco_dictionary, mossco_create_dictionary

type :: type_key
  integer :: type
  integer :: index
  character(len=256) :: name
end type

type :: type_value
  character(len=256) :: value
  type(type_key)     :: key
end type

type :: type_mossco_dictionary
  type(type_mossco_dictionary),dimension(:),pointer :: dictionaries => null()
  type(type_value),dimension(:),pointer             :: values => null()
  type(type_key),dimension(:),pointer               :: keys => null()
contains
  procedure :: set_value
  procedure :: get_value
  procedure :: get_key
  procedure :: add_dictionary
  procedure :: get_dictionaries
  procedure :: get_values
  procedure :: dump
  procedure :: key_is_present
end type

#define _VALUE_ 1
#define _DICTIONARY_ 2

contains

function mossco_dictionary_create() result(dictionary)
type(type_mossco_dictionary),pointer :: dictionary

allocate(dictionary)
end function mossco_dictionary_create

function key_is_present(dict,keyname) result(check)
class(type_mossco_dictionary) :: dict
logical                       :: check
integer                       :: k
character(len=*)              :: keyname
check=.false.
do k=1,ubound(dict%keys,1)
  if (trim(keyname)==trim(dict%keys(k)%name)) check=.true.
end do
end function key_is_present

subroutine set_value(dict,key,value)
class(type_mossco_dictionary)  :: dict
character(len=*), intent(in)   :: value
character(len=*), intent(in)   :: key
integer                        :: curlen,curlenkeys
type(type_key),dimension(:),pointer,save   :: oldkeys,newkeys
type(type_key),pointer         :: curkey
type(type_value),dimension(:),pointer,save :: oldvalues,newvalues

! select key and prepare values list
if (associated(dict%values)) then
  if (dict%key_is_present(key)) then
    write(0,*) '  key is present, overwriting'
    curkey => dict%get_key(key)
    if (curkey%type /= _VALUE_) then
      write(0,*) '  key refers to dictionary'
      stop
    end if
  else
    !extend list and generate new key
    write(0,*) '  generate new key '
    curlen = ubound(dict%values,1)
    oldvalues => dict%values
    allocate(newvalues(curlen+1))
    newvalues(1:curlen) = oldvalues(1:curlen)
    dict%values => newvalues
    deallocate(oldvalues)

    curlenkeys = ubound(dict%keys,1)
    oldkeys => dict%keys
    allocate(newkeys(curlenkeys+1))
    newkeys(1:curlenkeys) = oldkeys(1:curlenkeys)
    dict%keys => newkeys
    deallocate(oldkeys)
    curkey => dict%keys(curlenkeys+1)
    curkey%index=curlen+1
  end if
else
  if (.not.associated(dict%keys)) then
    write(0,*) '  initialise keys'
    allocate(dict%keys(1))
    curkey => dict%keys(1)
  else
    write(0,*) '  generate new key'
    curlenkeys = ubound(dict%keys,1)
    oldkeys => dict%keys
    allocate(newkeys(curlenkeys+1))
    newkeys(1:curlenkeys) = oldkeys(1:curlenkeys)
    dict%keys => newkeys
    deallocate(oldkeys)
    curkey => dict%keys(curlenkeys+1)
  end if
  allocate(dict%values(1))
  curkey%index = 1
end if
curkey%type = _VALUE_
curkey%name = trim(key)
dict%values(curkey%index)%value=trim(value)
dict%values(curkey%index)%key = curkey
end subroutine set_value


function get_value(dict,key) result(value)
class(type_mossco_dictionary)   :: dict
character(len=256)              :: value
character(len=256)              :: key
end function get_value


subroutine add_dictionary(dict,dictionary)
class(type_mossco_dictionary) :: dict
type(type_mossco_dictionary)  :: dictionary
end subroutine add_dictionary


subroutine get_dictionaries(dict)
class(type_mossco_dictionary) :: dict
end subroutine get_dictionaries


subroutine get_values(dict)
class(type_mossco_dictionary) :: dict
end subroutine get_values

function get_key(dict,keyname) result(key)
class(type_mossco_dictionary) :: dict
character(len=*)              :: keyname
type(type_key),pointer        :: key
integer                       :: k
key => null()
do k=1,ubound(dict%keys,1)
  if (dict%keys(k)%name == trim(keyname)) then
    key => dict%keys(k)
    continue
  end if
end do
end function get_key


subroutine dump(dict,indent)
class(type_mossco_dictionary) :: dict
character(len=*), optional    :: indent
character(len=16)             :: curindent
integer                       :: k,v,d

curindent='--              '
if (present(indent)) curindent=trim(curindent)//trim(indent)
do k=1,ubound(dict%keys,1)
  if (dict%keys(k)%type == _VALUE_) then
    write(0,*) trim(curindent)//trim(dict%keys(k)%name)//' = '//trim(dict%values(dict%keys(k)%index)%value)
  else
    if (dict%keys(k)%type == _DICTIONARY_) &
        call dict%dictionaries(dict%keys(k)%index)%dump(indent='--')
  end if
end do
end subroutine dump

end module mossco_dictionary
