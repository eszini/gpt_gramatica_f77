module cap_shared
use logging
use end_routine
use string
use miscmod
implicit none

contains
    subroutine check_cap_prod_pointer(the_value, key, expand_range)
    integer (kind=2), intent(in) :: the_value
    logical, intent(in), optional :: expand_range
    character (len=*) :: key
    integer :: maxidx
    
        maxidx=0
        if(expand_range) then
            maxidx=1
        end if
    
    
        if(the_value<=maxidx) then
            call end_program(trim(key) // " - prod_pointer " // &
            "value of " // trim(itos(int(the_value))) // " is invalid.")
        end if
        
    
    end subroutine check_cap_prod_pointer

end module cap_shared
