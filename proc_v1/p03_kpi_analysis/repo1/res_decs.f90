module res_decs
use foshydi2com
implicit none

    ! TODO: If a "not allocated" error appears on resource_id, the fix
    ! will be to call allocate_resource_ids_array first. If it's 
    ! attempting to read the array at the point of the error, you'll 
    ! need to find the right place to call it from to ensure it's 
    ! properly initialized.  Once that's done,  this comment can be 
    ! deleted.
    ! John Reynolds, 07182024
    integer (kind=2), allocatable :: resource_id(:)
contains

    subroutine allocate_resource_ids_array(num_elements)
        integer (kind=2), intent(in) :: num_elements
        
        if(.not. allocated(resource_id)) then
            allocate(resource_id(num_elements))
        endif

    end subroutine allocate_resource_ids_array
    
end module res_decs