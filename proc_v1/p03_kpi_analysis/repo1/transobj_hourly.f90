module transobj_hourly
implicit none
    logical (kind=1) :: hrly_after_trans_not_open=.TRUE.
    logical (kind=1):: m_HOURLY_TRANSACTION_NOT_OPEN=.TRUE.
    contains
    function get_hourly_transaction_not_open()
        logical (kind=1) :: get_hourly_transaction_not_open
        get_hourly_transaction_not_open=m_HOURLY_TRANSACTION_NOT_OPEN
    end function get_hourly_transaction_not_open
    
    subroutine set_hourly_transaction_not_open(value)
    logical (kind=1), intent(in) :: value
    logical (kind=1) :: savevalue
    m_HOURLY_TRANSACTION_NOT_OPEN=value
    savevalue=m_HOURLY_TRANSACTION_NOT_OPEN ! For debugger visibility
    end subroutine set_hourly_transaction_not_open
end module transobj_hourly
