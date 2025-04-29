module msgoutpt_decs
use msgoutpt_data
implicit none

contains
      function get_short_form_variables(varval)
         real :: varval(:)

         integer (kind=2) :: get_short_form_variables, i

         do i = 0, variables_used-1
            msgoutpt_elements%output_variable(i+1) = &
                varval(variable_list(i))
         enddo
         get_short_form_variables = variables_used
      end function get_short_form_variables
      
      

      
end module msgoutpt_decs