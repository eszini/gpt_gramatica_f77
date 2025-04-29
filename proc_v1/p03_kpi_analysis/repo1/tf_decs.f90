module tf_decs
use msgoutpt_data
implicit none



      type t_ns_tf_decs
        character (len=1), allocatable :: REPORT_THIS_GROUP(:)
        real (kind=8), allocatable :: MONTHLY_TRANS_LOAD_HYDRO_MWH(:)
      end type t_ns_tf_decs
      type(t_ns_tf_decs), save :: ns_tf_decs
      type t_ns_tf_objt
        integer (kind=2), allocatable ::  TG_2_PLANNING_AREA(:)
      end type t_ns_tf_objt
      type t_ns_mtf ! Contains declarations for manage_transaction_forecasts routine.
        integer (kind=2), allocatable ::  TG_2_PLANNING_AREA(:)
      end type t_ns_mtf
      type(t_ns_tf_objt) :: ns_tf_objt
      type(t_ns_mtf) :: ns_mtf
      real (kind=8), allocatable :: HOURLY_HYDRO_TF(:)

integer (kind=2) :: TRANS_POSITION=0
contains

      function GET_REPORT_TRANS_GROUP(R_TRANS_GROUP)
        logical (kind=1) :: GET_REPORT_TRANS_GROUP
        integer (kind=2) :: R_TRANS_GROUP

        
        ! Avoid spurious Lahey warning
        if(ns_tf_decs%REPORT_THIS_GROUP(R_TRANS_GROUP) == 'T') then
            GET_REPORT_TRANS_GROUP = .true.
        else
            GET_REPORT_TRANS_GROUP=.false.
        end if
      end function GET_REPORT_TRANS_GROUP
      

end module tf_decs

