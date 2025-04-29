module fuelcat
use end_routine
use miscmod
implicit none
integer, parameter :: FC_COAL=1, FC_GAS=2, &
    FC_OIL=3, FC_NUCLEAR=4, FC_WATER=5, &
    FC_OTHER=6, FC_RESERVED_7=7, &
    FC_RESERVED_8=8, FC_RESERVED_9=9, &
    FC_WHOLESALE_SALES=10, &
    FC_WHOLESALE_PURCHASES=11

contains
function GetFuelCategoryName(FuelCategoryID)
integer, intent(in) :: FuelCategoryID
character (len=256) :: result
character (len=256) :: GetFuelCategoryName

! NUM_FUEL_CATEGORIES:   1 = COAL
!                       2 = GAS
!                       3 = OIL
!                       4 = NUCLEAR
!                       5 = WATER
!                       6 = OTHER
!                       7-9 = RESERVED
!                       10 =  WHOLESALE SALES
!                       11 =  WHOLESALE PURCHASES
    result=" "
    select case (FuelCategoryID)
        case (FC_COAL)
            result="COAL"
        case (FC_GAS)
            result="GAS"
        case (FC_OIL)
            result="OIL"
        case (FC_NUCLEAR)
            result="NUCLEAR"
        case (FC_WATER)
            result="WATER"
        case (FC_OTHER)
            result="OTHER"
        case (FC_RESERVED_7)
            result="RESERVED_7"
        case (FC_RESERVED_8)
            result="RESERVED_8"
        case (FC_RESERVED_9)
            result="RESERVED_9"
        case (FC_WHOLESALE_SALES)
            result="WHOLESALE_SALES"
        case (FC_WHOLESALE_PURCHASES)
            result="WHOLESALE_PURCHASES"
        case DEFAULT
            er_message="fuelcat:0001 - Unknown/unsupported fuel category of " &
                // trim(itos(FuelCategoryID))
            call end_program(er_message)
    end select
    
    GetFuelCategoryName=result

end function GetFuelCategoryName
end module fuelcat