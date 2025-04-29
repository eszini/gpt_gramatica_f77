module class_canadian
        type t_ns_class_canadian
        real :: PROVINCIAL_CAPITAL_TAX
        real :: FEDERAL_CAPITAL_TAX
        real :: PROVINCIAL_CAPITAL_TAX_RATE
        real :: FEDERAL_CAPITAL_TAX_RATE
        real :: PROVINCIAL_CAP_TAX_DEDUCTION
        real :: FEDERAL_CAPITAL_TAX_DEDUCTION
        real :: PROVINCIAL_CAP_TAX_ADDENDUM
        real :: FEDERAL_CAPITAL_TAX_ADDENDUM
        end type t_ns_class_canadian
        
        ! SAVE required by Lahey, and should be implicit per the Fortran standard.
        type(t_ns_class_canadian), save :: ns_class_canadian ! Namespace
end module class_canadian