module sf_objt_interfaces
implicit none
    interface
        function peak_month(yr)
        real :: peak_month
        integer (kind=2) :: yr
        end function peak_month
    end interface
end module sf_objt_interfaces