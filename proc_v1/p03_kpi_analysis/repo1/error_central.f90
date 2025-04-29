!***********************************************************************
! Subrotine to use error intrinsic procedure in one module. This must be
! used for case on errors that will not stop the program.-
! Created: 2024-08-05 
!***********************************************************************
module error_central
implicit none
contains
subroutine call_error(er_message)
   character (len=1024) :: er_message
   call error(er_message)
end subroutine call_error
end module 