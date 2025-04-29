module kootenay
implicit none
    logical (kind=1) :: m_west_kootenay_active=.false.
contains

      function get_west_kootenay_power_active()
        logical (kind=1) :: get_west_kootenay_power_active
         get_west_kootenay_power_active = m_west_kootenay_active
      end function get_west_kootenay_power_active
      
      subroutine set_west_kootenay_power_active(active)
        logical (kind=1) :: active
        m_west_kootenay_active=active
      end subroutine set_west_kootenay_power_active
      
      function STORE_WKP_ACTIVE(UTILITY_TYPE, active)
         implicit none
         
         character (len=1) :: UTILITY_TYPE
         logical(kind=1) :: active, STORE_WKP_ACTIVE
         
         active = (UTILITY_TYPE == 'W')
         IF(ACTIVE) then
            UTILITY_TYPE = 'I'
         endif
                  
         CALL set_west_kootenay_power_active(active)
         store_wkp_active=active
      end function store_wkp_active
   
   
end module kootenay
