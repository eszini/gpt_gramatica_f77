module cla_decs
use PROD_ARRAYS_DIMENSIONS
implicit none
       
       
       REAL*4 &
               RAMP_RATE, &
               RAMP_DOWN_RATE, &
               FOR_FREQUENCY, &
               FOR_DURATION, &
               EMERGENCY_CAPACITY, &
               EMERGENCY_HEATRATE
       character (len=6) :: BASECASE_MARKET_AREA_ID
       
type t_cla_fuel
real (kind=4), allocatable :: S_FUEL_DELIVERY(:)
real (kind=4), allocatable :: S_FUEL_DELIVERY_2(:)
real (kind=4), allocatable :: S_FUEL_DELIVERY_3(:)
end type t_cla_fuel
type (t_cla_fuel), save :: ns_cla_fuel

  real :: CONSTANT_POLY_ord, &
               FIRST_POLY_ord, &
               SECOND_POLY_ord, &
               THIRD_POLY_ord
type t_cla_ox
      real (kind=4) :: &
            NOX_VOM(MAX_CL_UNITS),& 
            NOX_FOM(MAX_CL_UNITS),& 
            SOX_VOM(MAX_CL_UNITS),& 
            SOX_FOM(MAX_CL_UNITS),& 
            CO2_VOM(MAX_CL_UNITS),& 
            CO2_FOM(MAX_CL_UNITS),& 
            HG_VOM(MAX_CL_UNITS),& 
            HG_FOM(MAX_CL_UNITS),& 
            OTHER3_VOM(MAX_CL_UNITS),& 
            OTHER3_FOM(MAX_CL_UNITS)
end type t_cla_ox

type(t_cla_ox), save :: ns_cla_ox







type t_ns_cla_decs
integer (kind=2) :: capacity_market_pointer_acl(MAX_CL_UNITS)=0
! TODO: Rename UNITS variable to number_of_units, when safe to do so.

real (kind=4), allocatable :: MON_MDS_CL_CLASS_PURCHASES(:,:,:)
real (kind=4), allocatable :: MON_MDS_CL_CLASS_FUEL_COST(:,:,:)
real (kind=4), allocatable :: MON_MDS_NF_FUEL_OWNED_BC(:,:,:)
real (kind=4), allocatable :: MON_MDS_NF_FUEL_LEASED_BC(:,:,:)
real (kind=4), allocatable :: MON_MDS_CL_CAP_PURCHASES(:,:,:)
real (kind=4), allocatable :: MON_MDS_CL_CLASS_REVENUE(:,:,:)
real (kind=4), allocatable :: MON_MDS_ICAP_REV_BY_CLASS(:,:)
real (kind=4), allocatable :: MON_MDS_CL_CAP_REVENUE(:,:,:)




integer (kind=2) :: MAX_CAP_LIMITED_CLASS_ID_NUM
integer (kind=2) :: asset_cls_loc=0
integer (kind=2), allocatable :: ASSET_CLASS_ptr(:)


character(kind=1) ::  REPORT_THIS_UNIT(MAX_CL_UNITS)

end type t_ns_cla_decs

    type(t_ns_cla_decs), save :: ns_cla_decs



end module cla_decs