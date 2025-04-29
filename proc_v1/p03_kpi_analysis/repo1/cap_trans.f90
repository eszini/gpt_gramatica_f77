module cap_trans
    implicit none
    type t_ns_cap_trans
     integer (kind=2), allocatable :: TRANSACTION_GROUP(:)
    end type t_ns_cap_trans
    type(t_ns_cap_trans), save :: ns_cap_trans
    
end module cap_trans
