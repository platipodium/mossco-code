module mossco_dictionary
private

public type_mossco_dictionary, mossco_create_dictionary

type :: type_key
  integer :: type
  integer :: index
  character(len=256) :: name
end type

#define _DICTIONARY_ -1
#define _UNKNOWN_ 0
#define _INTEGER_ 3
#define _LOGICAL_ 4
#define _STRING_ 1
#define _REAL_ 2

type :: type_value
  character(len=256) :: string
  integer            :: integer
  real               :: real
  logical            :: logical
  type(type_key)     :: key
end type

type :: type_mossco_dictionary
  type(type_mossco_dictionary),dimension(:),pointer :: dictionaries => null()
  type(type_value),dimension(:),pointer             :: values => null()
  type(type_key),dimension(:),pointer               :: keys => null()
  integer                                           :: type_dictionary=_DICTIONARY_
  integer                                           :: type_string=_STRING_
  integer                                           :: type_real=_REAL_
  integer                                           :: type_integer=_INTEGER_
  integer                                           :: type_logical=_LOGICAL_
  integer                                           :: type_unknown=_UNKNOWN_
contains
  procedure :: set_value
  procedure :: get_value
  procedure :: get_key
  procedure :: get_type
  procedure :: add_dictionary
  procedure :: dump
  procedure :: key_is_present
end type

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

subroutine set_value(dict,key,string,logical,integer,real)

implicit none

class(type_mossco_dictionary)  :: dict
character(len=*),intent(in)    :: key
character(len=*),intent(in),optional :: string
logical         ,intent(in),optional :: logical
integer         ,intent(in),optional :: integer
real            ,intent(in),optional :: real

integer                        :: curlen,curlenkeys
type(type_key),dimension(:),pointer,save   :: oldkeys,newkeys
type(type_key),pointer         :: curkey
type(type_value),dimension(:),pointer,save :: oldvalues,newvalues

! select key and prepare values list
if (associated(dict%values)) then
  if (dict%key_is_present(key)) then
    write(0,*) '  key is present, overwriting'
    curkey => dict%get_key(key)
    if (curkey%type < 0) then
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
curkey%name = trim(key)
if (present(string)) then
  dict%values(curkey%index)%string=trim(string)
  curkey%type = dict%type_string
endif
if (present(integer)) then
  dict%values(curkey%index)%integer=integer
  curkey%type = dict%type_integer
endif
if (present(real)) then
  dict%values(curkey%index)%real=real
  curkey%type = dict%type_real
endif
if (present(logical)) then
  dict%values(curkey%index)%logical=logical
  curkey%type = dict%type_logical
endif
dict%values(curkey%index)%key = curkey
end subroutine set_value


function get_type(dict,key) result(valuetype)
class(type_mossco_dictionary) :: dict
character(len=*)              :: key
integer                       :: k,valuetype

valuetype=dict%type_unknown
do k=1,ubound(dict%keys,1)
  if (trim(key)==trim(dict%keys(k)%name)) then
    valuetype = dict%keys(k)%type
    continue
  end if
end do
end function get_type


subroutine get_value(dict,key,real,integer,logical,string)
class(type_mossco_dictionary) :: dict
character(len=*),optional     :: string
integer         ,optional     :: integer
real            ,optional     :: real
logical         ,optional     :: logical
character(len=*)              :: key
integer                       :: k

do k=1,ubound(dict%keys,1)
  if (trim(key)==trim(dict%keys(k)%name)) then
    if (present(string)) string=dict%values(dict%keys(k)%index)%string
    if (present(real)) real=dict%values(dict%keys(k)%index)%real
    if (present(logical)) logical=dict%values(dict%keys(k)%index)%logical
    if (present(integer)) integer=dict%values(dict%keys(k)%index)%integer
  end if
end do
end subroutine get_value


subroutine add_dictionary(dict,key,dictionary)

implicit none

class(type_mossco_dictionary) :: dict
type(type_mossco_dictionary)  :: dictionary
character(len=*)              :: key

integer                       :: k,d
type(type_key),dimension(:),pointer,save   :: oldkeys,newkeys
type(type_key),pointer         :: curkey
type(type_mossco_dictionary),dimension(:),pointer,save :: oldvalues,newvalues
integer                        :: curlen,curlenkeys

! select key and prepare values list
if (associated(dict%dictionaries)) then
  if (dict%key_is_present(key)) then
    write(0,*) '  key is present'
    stop
  else
    !extend list and generate new key
    write(0,*) '  generate new key '
    curlen = ubound(dict%dictionaries,1)
    oldvalues => dict%dictionaries
    allocate(newvalues(curlen+1))
    newvalues(1:curlen) = oldvalues(1:curlen)
    dict%dictionaries => newvalues
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
  allocate(dict%dictionaries(1))
  curkey%index = 1
end if
curkey%type = dict%type_dictionary
curkey%name = trim(key)
dict%dictionaries(curkey%index)=dictionary
end subroutine add_dictionary

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
  select case(dict%keys(k)%type)
  case(_STRING_)
    write(0,*) trim(curindent)//trim(dict%keys(k)%name)//' : '//trim(dict%values(dict%keys(k)%index)%string)
  case(_INTEGER_)
    write(0,*) trim(curindent)//trim(dict%keys(k)%name)//' : ',dict%values(dict%keys(k)%index)%integer
  case(_REAL_)
    write(0,*) trim(curindent)//trim(dict%keys(k)%name)//' : ',dict%values(dict%keys(k)%index)%real
  case(_LOGICAL_)
    write(0,*) trim(curindent)//trim(dict%keys(k)%name)//' : ',dict%values(dict%keys(k)%index)%logical
  case(_DICTIONARY_)
    call dict%dictionaries(dict%keys(k)%index)%dump(indent='--')
    end select
end do
end subroutine dump

end module mossco_dictionary
