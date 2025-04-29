module globecom
implicit none

    integer (kind=2) :: globecom_year
    integer (kind=2) :: gc_end_point,mstudy_period=0,gc_endyr, &
                    gc_last_study_year,m_extension_period=0, &
                    gc_last_extension_year, gc_max_years

    integer (kind=2), parameter ::  last_available_monthly_year=5
       
    integer (kind=2) :: stored_current_year, &
                         stored_end_year
contains
    subroutine set_globecom_study_period(v)
        integer (kind=2), intent(in) :: V
        mstudy_period=v
    end subroutine set_globecom_study_period
    function get_globecom_study_period()
        integer (kind=2) :: get_globecom_study_period
        get_globecom_study_period=mstudy_period
    end function get_globecom_study_period
    
    function get_extension_period()
        integer (kind=2) :: get_extension_period
        get_extension_period=m_extension_period
    end function get_extension_period
    subroutine set_extension_period(the_value)
        integer (kind=2), intent(in) :: the_value
        m_extension_period=the_value
    end subroutine set_extension_period
end module globecom


