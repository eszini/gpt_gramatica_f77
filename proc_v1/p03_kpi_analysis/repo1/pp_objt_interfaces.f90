module pp_objt_interfaces
implicit none
    interface
        function yes_run_transact()
            logical (kind=1) :: yes_run_transact
        end function yes_run_transact
    end interface
end module pp_objt_interfaces