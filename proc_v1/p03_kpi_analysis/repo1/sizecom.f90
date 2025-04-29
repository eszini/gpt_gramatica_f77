!     ******************************************************************
!     Sizecom.obj
!     
!     Created: 05/22/2024 
!     Original Author : Mark S GErber as .MON file
!     Migration Author: Pablo A. Bilbao
!     ******************************************************************
module sizecom
implicit none
      integer, parameter :: screen_msg_len=136
      character (len=screen_msg_len) :: screen_messages
      integer (kind=4), parameter :: all_versions=0, &
         lf77_version=1, lf95_version=2, win95_version=3
end module sizecom

