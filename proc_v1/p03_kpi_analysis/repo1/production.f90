module production
implicit none
      integer(kind=2) :: mPRODUCTION_PERIODS_IN=0
      character(len=1) :: m_production_periods_chr
contains
      subroutine set_production_periods_in(rval)
        integer(kind=2) :: rval
        mPRODUCTION_PERIODS_IN=rval
      end subroutine set_production_periods_in
      
      function get_PRODUCTION_PERIODS_in()
         integer(kind=2) :: get_PRODUCTION_PERIODS_in
         get_PRODUCTION_PERIODS_in = mPRODUCTION_PERIODS_IN
      end function get_PRODUCTION_PERIODS_in
      
      subroutine set_production_periods(r_production_periods_chr)
         character (len=1) :: r_production_periods_chr
		 integer (kind=2) :: month
         month=12
         m_production_periods_chr = r_production_periods_chr
         call set_production_periods_in(month)
         
         if(m_production_periods_chr == 'a') then
            call set_production_periods_in(int(1,2))
         endif

      end subroutine set_production_periods
      
end module production