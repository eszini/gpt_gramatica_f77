module month_extra
implicit none
   ! TODO:  Create initialization routine for this array. That
   ! prevents having to make each value the same length to avoid
   ! compatibility warnings.
   CHARACTER (len=20) :: PERIOD_NAME(12)=(/ &
     'January             ',  'February            ', &
     'March               ',  'April               ', &
     'May                 ',  'June                ', &
     'July                ',  'August              ', &
     'September           ',  'October             ', &
     'November            ',  'December            '/)
     
     character (len=9) :: el_MONTH=" "
     character (len=20) :: el_MONTH_NAME
     ! TODO:  Look into combining this array with PERIOD_NAME
     ! and possibly other linked arrays into a key-based
     ! collection
     INTEGER (kind=2) :: HOURS_IN_EACH_MONTH(12)= &
       (/744,672,744,720,744,720,744,744,720,744,720,744/)
     integer (kind=2) :: BEGIN_FISCAL_YEAR
     character (len=1) :: FISCAL_YEAR_ACTIVE_STR="F"
contains
     function IS_FISCAL_YEAR_ACTIVE(R_BEGIN_FISCAL_YEAR, &
        R_ONLY_FISCAL_YEAR)
         logical (kind=1) :: IS_FISCAL_YEAR_ACTIVE
         integer (kind=2) :: R_BEGIN_FISCAL_YEAR
         logical (kind=1) :: R_ONLY_FISCAL_YEAR
         
         R_BEGIN_FISCAL_YEAR = BEGIN_FISCAL_YEAR
         R_ONLY_FISCAL_YEAR =    FISCAL_YEAR_ACTIVE_STR == 'O'
         IS_FISCAL_YEAR_ACTIVE = FISCAL_YEAR_ACTIVE_STR /= 'F'
      end function IS_FISCAL_YEAR_ACTIVE
      
     function get_month_name(idx)
        character (len=20) :: get_month_name
        integer (kind=2), intent(in) :: idx
        get_month_name=PERIOD_NAME(idx)
     end function get_month_name
     
     function get_HOURS_IN_PERIOD(MONTH)
         integer (kind=2) :: get_HOURS_IN_PERIOD
         integer (kind=2), intent(in) :: month
         get_HOURS_IN_PERIOD = HOURS_IN_EACH_MONTH(MONTH)
     end function get_HOURS_IN_PERIOD
end module month_extra