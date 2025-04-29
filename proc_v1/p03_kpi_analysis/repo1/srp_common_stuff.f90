module srp_common_stuff
implicit none
      type t_srp_common_stuff
      REAL :: SRP_RATIO=0.
      real :: BTL_FEDERAL_TAXES
      real :: BTL_STATE_TAXES
      end type t_srp_common_stuff
      type(t_srp_common_stuff), save :: ns_srp_common_stuff
end module srp_common_stuff