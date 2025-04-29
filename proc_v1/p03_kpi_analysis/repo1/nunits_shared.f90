module nunits_shared
    use end_routine
    ! NUNITS comes from foshydi2 common block and is 
    ! now called nunits_allshared (used
    ! get_nunits/set_nunits to get/set. Use nunits_allshared
    ! only for variable allocation at declaration.
    implicit none
    
    integer (kind=2) :: nunits_allshared

    contains
        function GET_NUNITS()
            integer (kind=2) :: GET_NUNITS
            if(nunits_allshared==0) then
                call end_program("cl_units_read_ext:0002 - nunits_allshared " // &
                "not set.")
            end if
            
            GET_NUNITS = nunits_allshared
            
        end function GET_NUNITS
        subroutine set_nunits(nunits_value)
            integer (kind=2), intent(in) :: nunits_value
            nunits_allshared=nunits_value
        end subroutine set_nunits
        

end module nunits_shared