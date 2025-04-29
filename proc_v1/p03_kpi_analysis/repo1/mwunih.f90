module mwunih
! Adapted from mwunih** common blocks in prod3com.mon (blocks
! were named based on their type.
use hesi_gendecs
use prod_arrays_dimensions
implicit none
    real :: SO2(MAX_CL_UNITS)
    real :: NOX(MAX_CL_UNITS)
    real :: PARTICULATES(MAX_CL_UNITS)
    real :: EMIS_OTH2(MAX_CL_UNITS),EMIS_OTH3(MAX_CL_UNITS)
    real :: MINLOD,INHEAT(MAX_DISPATCH_BLOCKS)
    real :: FUEL_BTU_COST(MAX_CL_UNITS),MWBLOK(MAX_DISPATCH_BLOCKS)
    real :: SEC_SO2(MAX_CL_UNITS),SEC_NOX_BK1(MAX_CL_UNITS)
    real :: SEC_NOX_BK2(MAX_CL_UNITS),SEC_CO2(MAX_CL_UNITS)
    real :: SEC_OTH2(MAX_CL_UNITS),SEC_OTH3(MAX_CL_UNITS)
    real :: PRIM_NOX_BK2(MAX_CL_UNITS)
    
    integer (kind=2) :: UNIT(MAX_DISPATCH_BLOCKS)
    integer (kind=2) :: BLKNO(MAX_DISPATCH_BLOCKS)
    integer (kind=2) :: SHADOW_UNIT_NUMBER(MAX_CL_UNITS)
    integer (kind=2) :: TIE_CONSTRAINT_GROUP(MAX_CL_UNITS)
    
    logical (kind=1) :: MAINT(MAX_CL_UNITS)
    
    integer (kind=2) :: FUEL_LIMITED(MAX_DISPATCH_BLOCKS)
    

end module mwunih
