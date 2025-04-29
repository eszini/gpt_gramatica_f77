!
! change log
!
! 2024_04_09
! replaced character*256 to character (len=256)
!

module clreport_decs
implicit none
	  INTERFACE lwr
	  subroutine lwr(reportname, reportnum)
		implicit none
		character (len=256), intent(in) :: reportname
		integer, intent(in)              :: reportnum
	  end subroutine lwr
	  end INTERFACE lwr
      
      real (kind=4) :: ROR_CAPACITY=0
      real (kind=4) :: SYSTEM_PROD_BY_TG_BY_MWH_ord
end module clreport_decs

