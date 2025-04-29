module cl_object_shared
implicit none
        real (kind=4) :: FIRST_POLY_cla, SECOND_POLY_cla, THIRD_POLY_cla
        real (kind=4) :: BlendedEnthalpyUp_cla
        real (kind=4) :: BlendedEnthalpyLo_cla
        real (kind=4) :: BlendedSO2Up_cla
        real (kind=4) :: FuelTransportationCost_clos(4), NAME_PLATE_CAPACITY
        real (kind=4) :: EmissRedRate_clos(5)
        real (kind=4) :: EmissMaxRate_clos(5)
        real (kind=4) :: MaxEnergyLimit_clos(3), EnrgPatternPointer

        character (len=18) :: expense_collection_ord
        real (kind=4) :: cl_ai_energy_rate_ord
        integer (kind=2) :: CL_AI_ENERGY_ESCALATOR_ord
        real :: ai_cl_adr_life_ord=0
        CHARACTER*10 :: FUEL_BLENDING_IS_cla
        character (len=30) :: CO2_BASIN_NAME_cla
        real (kind=4) :: winter_total_capacity_cla
        real (kind=4) :: EXCESS_ENERGY_SALES_cla
        integer (kind=8) :: HESI_UNIT_ID_NUM ! shared by cla_objt and derivatives


        


end module cl_object_shared