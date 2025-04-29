module midasmod_decs
implicit none
integer (kind=2) :: mvalue_of_last_variable
contains
      subroutine set_last_variable(val)
        integer (kind=2), intent(in) :: val
        mvalue_of_last_variable=val
      end subroutine set_last_variable
      
      function get_last_variable()
         integer (kind=2) :: get_last_variable
         get_last_variable = mvalue_of_last_variable
      end function get_last_variable
end module midasmod_decs