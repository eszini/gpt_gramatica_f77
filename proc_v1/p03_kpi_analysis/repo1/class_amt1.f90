module class_amt1
implicit none
    type t_class_amt1
        real :: amtbkinc=0,amtunadjinc=0,amtincome=0,amtcredit=0
        real :: creditused
        real :: amtoffsetinc=0,amtforwardloss=0,amtnoltaken=0,amtitcused=0
        real :: altmintax=0
        real :: ace_dep=0
    end type t_class_amt1
    type (t_class_amt1), save :: ns_class_amt1
end module class_amt1