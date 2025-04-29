module mod_base_year
implicit none
integer (kind=2) :: m_base_yr=0
contains
      function get_base_year()
         use run_period_data
         integer (kind=2) :: get_base_year
         get_base_year = m_base_yr
      end function get_base_year
      subroutine set_base_year(by)
         integer (kind=2) :: by
         m_base_yr=by
      end subroutine set_base_year
end module mod_base_year