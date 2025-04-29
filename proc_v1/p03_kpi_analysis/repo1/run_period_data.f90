      module run_period_data
         implicit none
         integer (kind=2) :: base_yr=0,end_yr=0
         character (len=5) :: scename
         contains
      function get_endyr()
         integer (kind=2) :: get_endyr
         get_endyr = end_yr
      end function get_endyr
      end module