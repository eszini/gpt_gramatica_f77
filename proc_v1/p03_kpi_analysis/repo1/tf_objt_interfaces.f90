module tf_objt_interfaces
implicit none
      interface get_number_of_active_groups
        function get_number_of_active_groups()
            integer (kind=2) :: get_number_of_active_groups
        end function get_number_of_active_groups
      end interface get_number_of_active_groups
end module tf_objt_interfaces