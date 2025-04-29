!
     module poolcom
     implicit none
!
!    Pooling variables
!
! COMMON /POOLING_COMMON/ &
!
        real :: CLASS_ASSIGNED_ENERGY(2)
        real :: CLASS_ASS_FOSSIL_COST(2)
        real :: CLASS_ASS_PURCHASE_COST(2)
        real :: CLASS_ASS_NUCLEAR_COST(2)
        real :: CLASS_ASSIGNED_VARIABLE_COST(2)
        real :: CLASS_ASSIGNED_FIXED_COST(2)
        real :: P_CLASS_ASSIGNED_ENERGY(2)
        real :: P_CLASS_ASSIGNED_COST(2)
        real :: P_CLASS_ASSIGNED_FIXED_COST(2)
        real :: CLASS_ASSIGNED_EMISS(5,2)
        real :: CLASS_ASSIGNED_MMBTUS(2)
        real :: P_CLASS_ASS_ECON_SELL(2)
        real :: P_CLASS_ASS_ECON_BUY(2)
        real :: P_CLASS_ASS_ECON_COST(2)
        real :: P_CLASS_ASS_ECON_REV(2)

     end module poolcom
!
!
