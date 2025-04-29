module WEEK_INTERP_I2
    implicit none
    integer(kind=2) :: INTERP_CM(52)
    integer(kind=2) :: FIRST_WEEK(12)
    save :: INTERP_CM
    save :: FIRST_WEEK
end module WEEK_INTERP_I2

module WEEK_INTERP
    implicit none
    real :: INTERP_WT(52)
    save :: INTERP_WT
end module WEEK_INTERP

