module amt2
implicit none
    type t_ns_amt2

    real ::  AMTBKPREF(2), &
                   AMTNEGBKPREF(2), &
                   AVAILCREDIT(2)
    end type t_ns_amt2
    
    ! SAVE required by Lahey, and should be implicit per the Fortran 
    !standard.
    type(t_ns_amt2), save :: ns_amt2 ! amt2 namespace
    
end module amt2