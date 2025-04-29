! Extracted from cla_objt
module screen_interface
    use screen_interface_data
    use cla_decs
    use esrn_decs
    use miscmod
    use aitvaprod
    use cl_screen_data_mod
    use prim_mover_idx
    use capacity_options_fixed_vars
    use foshydi2com
    use cl_object_shared
    use capacity_options_alloc_vars
    use spcapexvariables
    use cl_screen_data_mod
    use cl_object_shared

    
implicit none
contains
     subroutine READ_CL_SCREEN_DATA(PRODUCTION_DATA_POINTER, array_size, &
        RESOURCE_TYPE_loc, CLOPTS, TAO, is_hw_unit)
      implicit none
      integer (kind=2) :: array_size
      integer (kind=2) :: PRODUCTION_DATA_POINTER(1:array_size)

      integer (kind=2) ::  CL_RESOURCE_ID
      real (kind=4) :: DISPATCH_MULT
      
      real(kind=4) :: AI_CL_TAX_LIFE_ord 
      character (len=5) :: TEMP_APPLY_NOX_SEASON_DATE
      CHARACTER*2    &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY
      integer (kind=2) :: CL_AI_CAPACITY_ESCALATOR_ord
      INTEGER (kind=8) :: H2_UNIT_ID_NUM_ord
      INTEGER (kind=4) :: POWERDAT_PLANT_ID ! Int*4?
      
      INTEGER (kind=8) :: TRANS_BUS_ID
      character (len=2) :: cl_load_type
      character (len=2) :: resource_type_loc(:)
      integer (kind=2) :: CLOPTS, TAO 
      logical (kind=1) :: is_hw_unit(:)
      integer :: ocub, rtub

         character (len=2) :: rt
         integer (kind=2) :: STATE_2_GAS_REGION_LOOKUP ! External
         logical (kind=1) :: YES_GSP_IS_ST_TG ! External
         INTEGER (KIND=2) :: STATE_ID_LOOKUP ! External
         integer (kind=2) :: GET_BELONGS_TO_GROUP ! External
         character (len=1024) :: file_name
         logical :: rt_ne_oc, hwu
         

         integer (kind=2), save :: OPTIONS_COUNTER=0
         integer (kind=2) :: ai_cl_remaining_life_ord
         character (len=6) :: TEMP_PRIM_FUEL_TYPE_STR
         integer (kind=2) :: temp_i2
         integer :: ub
         character (len=6) :: temp_char6
         INTEGER (kind=2) :: M,S, &
               RETURN_CL_OPTIONS_NO, &
               DELETE
         real (kind=4) :: dispadj_ord, DISPADJ2_ord
         character (len=18) :: EXPENSE_ASSIGNMENT_ord
         ocub=ubound(is_hw_unit,1)
         rtub=ubound(resource_type_loc, 1)
         

         CALL GET_BASE_PN_RECORDS(BASE_PN_RECORDS)
         PROD_PNTR = MAX(PROD_PNTR,TAO, &
                          BASE_PN_RECORDS)
         if(allocated(S_FUELMX)) then
            deallocate(S_FUELMX)
         end if


         if(allocated(S_PBTUCT)) then
            deallocate(S_PBTUCT)
         end if

         if(allocated(S_PFESCR)) then
            deallocate(S_PFESCR)
         end if

         if(allocated(S_SBTUCT)) then
            deallocate(S_SBTUCT)
         end if

         if(allocated(S_RPS_PROGRAM_NUMBER)) then
            deallocate(S_RPS_PROGRAM_NUMBER)
         end if

         if(allocated(S_RPS_PERCENT)) then
            deallocate(S_RPS_PERCENT)
         end if

         if(allocated(S_SFESCR)) then
            deallocate(S_SFESCR)
         end if

         if(allocated(S_FUELADJ_IN)) then
            deallocate(S_FUELADJ_IN)
         end if

         if(allocated(S_FUELADJ)) then
            deallocate(S_FUELADJ)
         end if

         if(allocated(S_MNRATE)) then
            deallocate(S_MNRATE)
         end if

         if(allocated(S_INTER_BLOCKS)) then
            deallocate(S_INTER_BLOCKS)
         end if

         if(allocated(S_EFOR)) then
            deallocate(S_EFOR)
         end if

         if(allocated(S_VCPMWH_IN)) then
            deallocate(S_VCPMWH_IN)
         end if

         if(allocated(S_OMESCR)) then
            deallocate(S_OMESCR)
         end if

         if(allocated(S_FIXED_COST_IN)) then
            deallocate(S_FIXED_COST_IN)
         end if

         if(allocated(S_FIXED_COST_ESCALATOR)) then
            deallocate(S_FIXED_COST_ESCALATOR)
         end if

         if(allocated(S_MONTHLY_CAPACITY_POINTER)) then
            deallocate(S_MONTHLY_CAPACITY_POINTER)
         end if

         if(allocated(S_HR_FACTOR)) then
            deallocate(S_HR_FACTOR)
         end if

         if(allocated(S_INPUT_MW)) then
            deallocate(S_INPUT_MW)
         end if

         if(allocated(S_CAP_PLANNING_FAC)) then
            deallocate(S_CAP_PLANNING_FAC)
         end if

         if(allocated(S_COEFF)) then
            deallocate(S_COEFF)
         end if

         if(allocated(ns_screen_data%P_SO2)) then
            deallocate(ns_screen_data%P_SO2)
         end if

         if(allocated(ns_screen_data%P_NOX)) then
            deallocate(ns_screen_data%P_NOX)
         end if

         if(allocated(P_PARTICULATES)) then
            deallocate(P_PARTICULATES)
         end if

         if(allocated(P_EMIS_OTH2)) then
            deallocate(P_EMIS_OTH2)
         end if

         if(allocated(P_EMIS_OTH3)) then
            deallocate(P_EMIS_OTH3)
         end if

         if(allocated(AHR)) then
            deallocate(AHR)
         end if

         if(allocated(PROD_PNTR_BY_CL_UNITS)) then
            deallocate(PROD_PNTR_BY_CL_UNITS)
         end if

         if(allocated(S_UNITNM)) then
            deallocate(S_UNITNM)
         end if

         if(allocated(ns_screen_data%LDTYPE)) then
            deallocate(ns_screen_data%LDTYPE)
         end if

        if(allocated(S_ANNUAL_CL_FIXED_COST)) then
            deallocate(S_ANNUAL_CL_FIXED_COST)
        end if

        if(allocated(S_TRANSACTION_GROUP_ID)) then
            deallocate(S_TRANSACTION_GROUP_ID)
        end if

        if(allocated(S_EMISSION_MARKET_LINK)) then
            deallocate(S_EMISSION_MARKET_LINK)
        end if

        if(allocated(S_ANNUAL_CL_FIXED_COST_ESC)) then
            deallocate(S_ANNUAL_CL_FIXED_COST_ESC)
        end if

         if(allocated(P_FUEL_DELIVERY)) then
            deallocate(P_FUEL_DELIVERY)
         end if

         if(allocated(P_FUEL_DELIVERY_2)) then
            deallocate(P_FUEL_DELIVERY_2)
         end if

         if(allocated(P_FUEL_DELIVERY_3)) then
            deallocate(P_FUEL_DELIVERY_3)
         end if

         if(allocated(START_UP_COSTS)) then
            deallocate(START_UP_COSTS)
         end if

         if(allocated(START_UP_COSTS_ESCALATION)) then
            deallocate(START_UP_COSTS_ESCALATION)
         end if

         if(allocated(MIN_DOWN_TIME)) then
            deallocate(MIN_DOWN_TIME)
         end if

         if(allocated(MIN_UP_TIME)) then
            deallocate(MIN_UP_TIME)
         end if

         if(allocated(S_FUEL_DELIVERY)) then
            deallocate(S_FUEL_DELIVERY)
         end if

         if(allocated(S_FUEL_DELIVERY_2)) then
            deallocate(S_FUEL_DELIVERY_2)
         end if

         if(allocated(S_FUEL_DELIVERY_3)) then
            deallocate(S_FUEL_DELIVERY_3)
         end if

         if(allocated(APPLY_NOX_SEASON_DATE)) then
            deallocate(APPLY_NOX_SEASON_DATE)
         end if

         if(allocated(S_PRIMARY_MOVER)) then
            deallocate(S_PRIMARY_MOVER)
         end if

         if(allocated(S_UNIT_GAS_REGION_INDEX)) then
            deallocate(S_UNIT_GAS_REGION_INDEX)
         end if

         if(allocated(State_Index)) then
            deallocate(State_Index)
         end if

         if(allocated(NOX_SEASON_DATE)) then
            deallocate(NOX_SEASON_DATE)
         end if

         if(allocated(NOX_CONTROL_PERCENT)) then
            deallocate(NOX_CONTROL_PERCENT)
         end if

         if(allocated(PRIMARY_MOVER_STR)) then
            deallocate(PRIMARY_MOVER_STR)
         end if

         if(allocated(CO2_CONTROL_PERCENT)) then
            deallocate(CO2_CONTROL_PERCENT)
         end if

         if(allocated(HG_CONTROL_PERCENT)) then
            deallocate(HG_CONTROL_PERCENT)
         end if

         if(allocated(OTHER3_CONTROL_PERCENT)) then
            deallocate(OTHER3_CONTROL_PERCENT)
         end if

         if(allocated(NOX_CONTROL_DATE)) then
            deallocate(NOX_CONTROL_DATE)
         end if

         if(allocated(CO2_CONTROL_DATE)) then
            deallocate(CO2_CONTROL_DATE)
         end if

         if(allocated(HG_CONTROL_DATE)) then
            deallocate(HG_CONTROL_DATE)
         end if

         if(allocated(OTHER3_CONTROL_DATE)) then
            deallocate(OTHER3_CONTROL_DATE)
         end if

         if(allocated(MONTHLY_MUST_RUN_VECTOR)) then
            deallocate(MONTHLY_MUST_RUN_VECTOR)
         end if

         if(allocated(SOX_CONTROL_PERCENT)) then
            deallocate(SOX_CONTROL_PERCENT)
         endif
         if (allocated(SOX_CONTROL_DATE)) then
            deallocate(SOX_CONTROL_DATE)
         endif
         if(allocated(sox_vom)) then
            deallocate(SOX_VOM)
         endif
         
         if(allocated(sox_fom)) then
            deallocate(sox_fom)
         endif
         
         if(allocated(NOX_VOM)) then
            deallocate(NOX_VOM)
         endif
         
         if(allocated(NOX_FOM)) then
            deallocate(NOX_FOM)
         endif
         
         if(allocated(CO2_VOM )) then
            deallocate(CO2_VOM)
         endif

         if(allocated(CO2_FOM)) then
            deallocate(CO2_FOM)
         endif

         if(allocated(HG_VOM)) then
            deallocate(HG_VOM)
         endif
         
         if(allocated(HG_FOM)) then
            deallocate(HG_FOM)
         endif
         
         if(allocated(OTHER3_VOM)) then
            deallocate(OTHER3_VOM)
         endif
 
         if(allocated(OTHER3_FOM)) then
            deallocate(OTHER3_FOM)
         endif

! OPERATIONS FILE
         ALLOCATE(S_FUELMX(clopts))
         ALLOCATE(S_PBTUCT(clopts))
         ALLOCATE(S_PFESCR(clopts))
         ALLOCATE(S_SBTUCT(clopts))
         ALLOCATE(S_RPS_PROGRAM_NUMBER(clopts))
         ALLOCATE(S_RPS_PERCENT(clopts))
         ALLOCATE(S_SFESCR(clopts))
         ALLOCATE(S_FUELADJ_IN(clopts))
         ALLOCATE(S_MNRATE(clopts,12))
         ALLOCATE(S_INTER_BLOCKS(clopts,3))
         ALLOCATE(S_FUELADJ(clopts))
         ALLOCATE(S_EFOR(clopts))
         ALLOCATE(S_VCPMWH_IN(clopts))
         ALLOCATE(S_OMESCR(clopts))
         ALLOCATE(S_FIXED_COST_IN(clopts))
         ALLOCATE(S_FIXED_COST_ESCALATOR(clopts))
         ALLOCATE(S_MONTHLY_CAPACITY_POINTER(clopts))
         ALLOCATE(S_HR_FACTOR(clopts))
         ALLOCATE(S_INPUT_MW(2,clopts))
         ALLOCATE(S_CAP_PLANNING_FAC(clopts))
         ALLOCATE(S_COEFF(3,clopts), &
                     ns_screen_data%P_SO2(clopts), &
                     ns_screen_data%P_NOX(clopts), &
                     P_PARTICULATES(clopts), &
                     P_EMIS_OTH2(clopts), &
                     P_EMIS_OTH3(clopts))
         ALLOCATE(AHR(clopts))
         ALLOCATE(PROD_PNTR_BY_CL_UNITS(PROD_PNTR))
         ALLOCATE(S_UNITNM(clopts))
         ALLOCATE(ns_screen_data%LDTYPE(clopts))
         ALLOCATE(S_ANNUAL_CL_FIXED_COST(clopts))
         ALLOCATE(S_TRANSACTION_GROUP_ID(clopts))
         ALLOCATE(S_EMISSION_MARKET_LINK(clopts))
         ALLOCATE(S_ANNUAL_CL_FIXED_COST_ESC(clopts))
         ALLOCATE(P_FUEL_DELIVERY(clopts))
         ALLOCATE(START_UP_COSTS(clopts))
         ALLOCATE(START_UP_COSTS_ESCALATION(clopts))
         ALLOCATE(MIN_DOWN_TIME(clopts))
         ALLOCATE(MIN_UP_TIME(clopts))
         ALLOCATE(P_FUEL_DELIVERY_2(clopts))
         ALLOCATE(P_FUEL_DELIVERY_3(clopts))
         ALLOCATE(S_FUEL_DELIVERY(clopts))
         ALLOCATE(S_FUEL_DELIVERY_2(clopts))
         ALLOCATE(s_fuel_delivery_3(clopts))
         ALLOCATE(APPLY_NOX_SEASON_DATE(clopts), &
                  S_PRIMARY_MOVER(clopts), &
                  S_UNIT_GAS_REGION_INDEX(clopts), &
                  State_Index(clopts), &
                  NOX_SEASON_DATE(clopts), &
                  NOX_CONTROL_PERCENT(clopts), &
                  PRIMARY_MOVER_STR(clopts), &
                  CO2_CONTROL_PERCENT(clopts), &
                  HG_CONTROL_PERCENT(clopts), &
                  OTHER3_CONTROL_PERCENT(clopts), &
                  NOX_CONTROL_DATE(clopts), &
                  CO2_CONTROL_DATE(clopts), &
                  HG_CONTROL_DATE(clopts), &
                  OTHER3_CONTROL_DATE(clopts), &
                  MONTHLY_MUST_RUN_VECTOR(clopts), &
                  SOX_CONTROL_PERCENT(clopts), &
                  SOX_CONTROL_DATE(clopts), &
                  SOX_VOM(clopts), &
                  SOX_FOM(clopts), &
                  NOX_VOM(clopts), &
                  NOX_FOM(clopts), &
                  CO2_VOM(clopts), &
                  CO2_FOM(clopts), &
                  HG_VOM(clopts), &
                  HG_FOM(clopts), &
                  OTHER3_VOM(clopts), &
                  OTHER3_FOM(clopts))
! READ THE CORRESPONDING EXPANSION PLANT RECORDS
         S_FUEL_DELIVERY=0.0

         S_UNIT_GAS_REGION_INDEX = 0
         SUNITS = 0
         S_INTER_BLOCKS = 0.0
         PROD_PNTR_BY_CL_UNITS = -1 ! 111413.
         inquire(unit=13, NAME=file_name)
       

         DO OPTIONS_COUNTER = 1, TAO
            ! merge2issue
            !Reworked IF statement because a "cannot mix string
            ! and logic operations" spurious error wouldn't go away
            ! otherwise.
            ! TODO: looks ok but code is ugly. Rework.
            rtub=ubound(resource_type_loc, 1)
            if(rtub<OPTIONS_COUNTER) then
                er_message="screen_interface:0002 - " // &
                "resource_type_loc only has " // &
                trim(itos(rtub)) // " elements,  but is attempting " // &
            "to access element " // trim(itos(int(OPTIONS_COUNTER)))
                call end_program(er_message)

            end if
            
            rt=resource_type_loc(OPTIONS_COUNTER)
            if(trim(rt)=="") then
                er_message="screen_interface:0008 - rt is empty."
                call end_program(er_message)
            end if
            
            rt_ne_oc=(rt .ne. 'CL')

            if(OPTIONS_COUNTER>ocub) then
                er_message="screen_interface:0001 - " // &
                "is_hw_unit only has " // &
                trim(itos(ocub)) // "elements,  but will attempt " // &
             " to access element " // trim(itos(int(OPTIONS_COUNTER)))
                call end_program(er_message)

            else
            
                hwu=is_hw_unit(OPTIONS_COUNTER)
            end if
            ! rt_ne_oc=.false/.true.
            ! hwu=/.false.
             
            IF(rt_ne_oc .OR. hwu) then ! Preventing READ, below
                CYCLE ! Legacy doesn't cycle first time
            end if
            
            SUNITS = SUNITS + 1
            ! sunits=/1
            ! ub=/1329
            
            ! options_counter=1/
            ! sunits=1/
            ub=ubound(production_data_pointer,1)
            if(ub<options_counter) then
                er_message="screen_interface:0007 - range check " // &
                "error in production_data_pointer with upper " // &
                "bound of " // trim(itos(int(ub))) // ". There was " // &
                "an attempt to access element " // &
                trim(itos(int(OPTIONS_COUNTER))) // &
                " (options_counter)."
                call end_program(er_message)
            end if

            PROD_PNTR_BY_CL_UNITS( &
                     PRODUCTION_DATA_POINTER(OPTIONS_COUNTER)) = SUNITS
            
            file_name=get_filename_from_unit(13)

            ! TODO:  Protect read by placing inside its own routine.
            ! MW_INSIDE_FENCE read here

            if(.not. is_unit_open(13)) then
                call end_program("screen_interface:0013 - " // &
                "unit 13 was not open and a read was about to be " // &
                "performed.")
            end if
            
            READ(13,REC=PRODUCTION_DATA_POINTER(OPTIONS_COUNTER)) &
           DELETE,S_UNITNM(SUNITS), &
           CL_LOAD_TYPE, &
           EXPENSE_ASSIGNMENT_ord, &
           EXPENSE_COLLECTION_ord, &
           GENGRP, CAP_FRAC_OWN_PAD, &
           ONLIMO,OFLIMO, &
           S_FUELMX(SUNITS), &
           S_PBTUCT(SUNITS),S_PFESCR(SUNITS), &
           S_SBTUCT(SUNITS), &
           S_SFESCR(SUNITS), &
           S_FUELADJ_IN(SUNITS), &
           S_EFOR(SUNITS), &
           (S_MNRATE(SUNITS,M),M=1,12), &
           S_VCPMWH_IN(SUNITS),S_OMESCR(SUNITS), &
           S_FIXED_COST_IN(SUNITS), &
           S_FIXED_COST_ESCALATOR(SUNITS), &
           DISPADJ_ord,S_HR_FACTOR(SUNITS), &
           (S_INPUT_MW(M,SUNITS),M=1,2), &
           S_CAP_PLANNING_FAC(SUNITS), &
           (S_COEFF(M,SUNITS),M=1,3), &
           CL_AI_CAPACITY_RATE_ord, &
           CL_AI_CAPACITY_ESCALATOR_ord, &
           CL_AI_ENERGY_RATE_ord, &
           CL_AI_ENERGY_ESCALATOR_ord, &
           ns_screen_data%P_SO2(SUNITS), &
           ns_screen_data%P_NOX(SUNITS), &
           P_PARTICULATES(SUNITS), &
           CL_POOL_FRAC_OWN, &
           P_EMIS_OTH2(SUNITS), &
           P_EMIS_OTH3(SUNITS), &
           DISPADJ2_ord, &
           CL_RESOURCE_ID, &
           FUEL_SUPPLY_ID, &
           P_NOX_BK2, &
           SEC_FUEL_EMISS_PTR, &
           EMISS_FUEL_COST, &
           EMISS_FUEL_ESCAL, &
           EMISS_FUEL_EMISS_PTR, &
           EMISS_BLENDING_RATE, &
           PRIM_FUEL_EMISS_PTR, &
           TEMP_PRIM_FUEL_TYPE_STR, &
           SEC_FUEL_TYPE, &
           EMISS_FUEL_TYPE, &
           TIE_CONSTRAINT_GROUP, & 
           S_MONTHLY_CAPACITY_POINTER(SUNITS), & 
           S_ANNUAL_CL_FIXED_COST(SUNITS), & 
           S_ANNUAL_CL_FIXED_COST_ESC(SUNITS), & 
           MAINT_DAYS, &
                 DISPATCH_MULT, &
                 excess_energy_sales_cla, &
                 AI_CL_REMAINING_LIFE_ord, &
                 AI_CL_TAX_LIFE_ord, &
                 AI_CL_ADR_LIFE_ord, &
                 ASSET_CLASS_NUM, &
                 ASSET_CLASS_VECTOR, &
                 INTRA_COMPANY_CLASS_ID, &
                 INTRA_COMPANY_TRANSACTION, &
                 SPECIAL_UNIT_ID, &
                 MINIMUM_CAPACITY_FACTOR, &
                 MAXIMUM_CAPACITY_FACTOR, &
                 S_TRANSACTION_GROUP_ID(SUNITS), &
                 DAY_TYPE_ID, & ! 19968
                 UNIT_ACTIVE, & ! "o Non" should be "Y"?
                 BASECASE_PLANT_ID, &
                 BASECASE_UNIT_ID, &
                 BASECASE_MARKET_AREA_ID, &
                 BASECASE_TRANS_AREA_ID, &
                 BASECASE_PLANT_NERC_SUB_ID, &
                 BASECASE_PLANT_OWNER_ID, &
                 UTILITY_OWNED,  &
                 MONTHLY_FUEL_INDEX, &
                 START_UP_COSTS(SUNITS), &
                 START_UP_LOGIC, &
                 RAMP_RATE, &
                 MIN_DOWN_TIME(SUNITS), &
                 report_this_unit_ord, & !  CH*5
                 P_FUEL_DELIVERY(SUNITS), &
                 P_FUEL_DELIVERY_2(SUNITS), &
                 P_FUEL_DELIVERY_3(SUNITS), &
                 MIN_UP_TIME(SUNITS), &
                 HESI_UNIT_ID_NUM, &
                 FOR_FREQUENCY, &
                 FOR_DURATION, &
                 winter_total_capacity_cla, &
                 CONTRIBUTES_TO_SPIN, & !  CH*5
                 H2_UNIT_ID_NUM_ord, &
                 POWERDAT_PLANT_ID, &
          TEMP_APPLY_NOX_SEASON_DATE, & 
                 NOX_SEASON_DATE(SUNITS), &
                 RAMP_DOWN_RATE, &
                 NOX_CONTROL_PERCENT(SUNITS), &
                 NOX_CONTROL_DATE(SUNITS), &
                 EMERGENCY_CAPACITY, &
                 EMERGENCY_HEATRATE, &
                 MARKET_RESOURCE, & 
                 PRIMARY_MOVER_STR(SUNITS), &
                 COUNTY, &
                 STATE_PROVINCE, &
                 NOX_VOM(SUNITS), &
                 NOX_FOM(SUNITS), &
                 S_FUEL_DELIVERY(SUNITS), &
                 S_FUEL_DELIVERY_2(SUNITS), &
                 S_FUEL_DELIVERY_3(SUNITS), &
                 CONSTANT_POLY_ord, &
                 FIRST_POLY_cla, &
                 SECOND_POLY_cla, &
                 THIRD_POLY_cla, &
                 USE_POLY_HEAT_RATES, & 
                 NEWGEN_UNIT_STATUS, &
                 MONTHLY_MUST_RUN_VECTOR(SUNITS), &
                 S_EMISSION_MARKET_LINK(SUNITS), &
                 WVPA_RATE_TRACKER, & 
                 ALLOW_DECOMMIT, & 
                 MIN_SPIN_CAP, &
                 MAX_SPIN_CAP, &
                 WVPA_RES_TRACKER, &
                 WVPA_FUEL_TRACKER, &
                 MONTE_OR_FREQ,     &
                 WVPA_MEM_TRACKER, &
                 MONTHLY_DECOMMIT_VECTOR, &
                 TRANS_BUS_ID, &
                 SOX_CONTROL_PERCENT(SUNITS), &
                 SOX_CONTROL_DATE(SUNITS), &
                 SOX_VOM(SUNITS), &
                 SOX_FOM(SUNITS), &
                 LATITUDE, &
                 LONGITUDE, &
                 MW_INSIDE_FENCE, &
                 S_INTER_BLOCKS(SUNITS,1), &
                 S_INTER_BLOCKS(SUNITS,2), &
                 S_INTER_BLOCKS(SUNITS,3), &
                 ns_screen_data%FIRST_YEAR_DECOM_AVAIL,  & !  155
!                        Decommissioning base yr cost
                 ns_screen_data%DECOM_BASE_YR_COST_ord, & 
                 ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
                 ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR,   & 
                 AGGREGATE_THIS_UNIT_ord,   &
                 ns_screen_data%NAME_PLATE_CAPACITY_cls, &
                 FUEL_BLENDING_IS_cla,   &
                 ns_screen_data%FuelRatio(1), &
                 ns_screen_data%FuelRatio(2), &
                 ns_screen_data%FuelRatio(3),         & !  164
                 ns_screen_data%FuelRatio(4), &
                 ns_screen_data%FuelRatio(5),         & !  166
                 ns_screen_data%BlendableFuelsPtr(1), & !  167
                 ns_screen_data%BlendableFuelsPtr(2), & !  168
                 ns_screen_data%BlendableFuelsPtr(3), &
                 ns_screen_data%BlendableFuelsPtr(4), &
                 FuelTransportationCost_clos(1), &
                 FuelTransportationCost_clos(2),  & !  172
                 FuelTransportationCost_clos(3), &
                 FuelTransportationCost_clos(4), &
                 BlendedEnthalpyUp_cla, &
                 BlendedEnthalpyLo_cla, &
                 BlendedSO2Up_cla, & !  177
                 ns_screen_data%BettermentProjectID, &
                 ns_screen_data%DECOM_CONTINUING_COST, &
                 ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
                 EnrgPatternPointer, & !  181
                 ns_screen_data%EMISSION_DATA_UNITS, &
                 EmissRedRate_clos(1), &
                 EmissRedRate_clos(2), &
                 EmissRedRate_clos(3), &
                 EmissRedRate_clos(4),  & !  186
                 EmissRedRate_clos(5), &
                 EmissMaxRate_clos(1), &
                 EmissMaxRate_clos(2), &
                 EmissMaxRate_clos(3), &
                 EmissMaxRate_clos(4), &
                 EmissMaxRate_clos(5), &
                 MaxEnergyLimit_clos(1), &
                 MaxEnergyLimit_clos(2), &
                 MaxEnergyLimit_clos(3), &
                 ns_screen_data%TECH_TYPE, &
                 ns_screen_data%LINKED_BETTERMENT_OPTION, &
                 CO2_CONTROL_PERCENT(SUNITS), &
                 HG_CONTROL_PERCENT(SUNITS), &
                 OTHER3_CONTROL_PERCENT(SUNITS), &
                 CO2_CONTROL_DATE(SUNITS), &
                 HG_CONTROL_DATE(SUNITS), &
                 OTHER3_CONTROL_DATE(SUNITS), &
                 CO2_VOM(SUNITS), &
                 CO2_FOM(SUNITS), &
                 HG_VOM(SUNITS), &
                 HG_FOM(SUNITS), &
                 OTHER3_VOM(SUNITS), &
                 OTHER3_FOM(SUNITS),  &
                 MARKET_FLOOR, &
                 MARKET_CEILING,  & !  211
                 START_UP_COSTS_ESCALATION(SUNITS),  & !  212
                 CAPACITY_MARKET_TYPE, &
                 CAPACITY_MARKET_MONTH, &
                 capacity_market_pointer_ocl, &
                 CAPACITY_MARKET_COST_ASSIGN, &
                 CAPACITY_MARKET_EXP_COLLECT, &
                 CAPACITY_MARKET_COIN_ADJ_FACT, &
                 PRIMARY_FUEL_CATEGORY, &
                 SECONDARY_FUEL_CATEGORY, & !  220
                 EMISSIONS_FUEL_CATEGORY, &
                 S_RPS_PERCENT(SUNITS), &
                 RETIREMENT_CANDIDATE, &
                 RETROFIT_CANDIDATE, &
                 RETROFIT_PROJECT_ID, &
                 co2_basin_name_cla, &
                 CO2_PIPELINE_DISTANCE, &
                 STATE_PROV_NAME, & !  228
                 CO2_RETRO_HEAT_MULT, &
                 CO2_RETRO_CAP_MULT, &
                 THERMAL_GUID, &
                 ns_screen_data%THERMAL_PARENT_ID, & !  232
                 THERMAL_AGGREGATED_UNIT, &
                 FIRST_RETIREMENT_YEAR, &
                 UNIT_TYPE, &
                 UNIT_TYPE_CATEGORY, &
                 S_RPS_PROGRAM_NUMBER(SUNITS)



            CALL UPC(TEMP_PRIM_FUEL_TYPE_STR,PRIM_FUEL_TYPE_STR)

            TEMP_I2 = STATE_2_GAS_REGION_LOOKUP(STATE_PROVINCE)
            IF(YES_GSP_IS_ST_TG(TEMP_I2)) THEN
               TEMP_CHAR6 = STATE_PROVINCE
            ELSE
               TEMP_CHAR6 = STATE_PROVINCE(1:2)
            ENDIF
            S_UNIT_GAS_REGION_INDEX(SUNITS) = &
                                  STATE_2_GAS_REGION_LOOKUP(TEMP_CHAR6)
            ! state_province=PA_01/PA_01

            TEMP_CHAR6 = State_Province(1:2)
            TEMP_CHAR6=realtrim(TEMP_CHAR6)
            ! temp_char6=asdf/PA
            if(len_trim(temp_char6)==0) then
                call end_program("screen_interface:0012 - " // &
                "State_province value of empty string is " // &
                "invalid.")
            end if

            ST_TG = STATE_ID_LOOKUP(TEMP_CHAR6)

            State_Index(SUNITS) = ST_TG


            S_PRIMARY_MOVER(SUNITS) = &
                             FUEL_TYPE_2_PRIM_MOVER(PRIM_FUEL_TYPE_STR)

! 10/03/02. FOR REGIONAL CONSOLIDATION. NOTE REASSIGNMENT.
      
            APPLY_NOX_SEASON_DATE(SUNITS) = &
                                        TEMP_APPLY_NOX_SEASON_DATE(1:1)
            
            S_TRANSACTION_GROUP_ID(SUNITS) = &
                GET_BELONGS_TO_GROUP(S_TRANSACTION_GROUP_ID(SUNITS))

            AHR(SUNITS) = (S_COEFF(1,SUNITS)*S_INPUT_MW(1,SUNITS) + &
               (S_COEFF(2,SUNITS)+S_COEFF(3,SUNITS))* &
               (S_INPUT_MW(2,SUNITS) - S_INPUT_MW(1,SUNITS))/2.) &
               / S_INPUT_MW(2,SUNITS)

            NOX_SEASON_DATE(SUNITS) = NOX_SEASON_DATE(SUNITS) + 10000

            NOX_CONTROL_PERCENT(SUNITS) = &
                  MIN(100.,MAX(0.,1.-NOX_CONTROL_PERCENT(SUNITS)/100.))
            NOX_CONTROL_DATE(SUNITS) = NOX_CONTROL_DATE(SUNITS) + 10000

            SOX_CONTROL_PERCENT(SUNITS) = &
                  MIN(100.,MAX(0.,1.-SOX_CONTROL_PERCENT(SUNITS)/100.))
            SOX_CONTROL_DATE(SUNITS) = SOX_CONTROL_DATE(SUNITS) + 10000

            CO2_CONTROL_PERCENT(SUNITS) = &
                  MIN(100.,MAX(0.,1.-CO2_CONTROL_PERCENT(SUNITS)/100.))
            CO2_CONTROL_DATE(SUNITS) = CO2_CONTROL_DATE(SUNITS) + 10000
            HG_CONTROL_PERCENT(SUNITS) = &
                   MIN(100.,MAX(0.,1.-HG_CONTROL_PERCENT(SUNITS)/100.))
            HG_CONTROL_DATE(SUNITS) = HG_CONTROL_DATE(SUNITS) + 10000
            OTHER3_CONTROL_PERCENT(SUNITS) = &
                   MIN(100., &
                        MAX(0.,1.-OTHER3_CONTROL_PERCENT(SUNITS)/100.))
            OTHER3_CONTROL_DATE(SUNITS) = &
                                    OTHER3_CONTROL_DATE(SUNITS) + 10000
            ns_screen_data%LDTYPE(SUNITS) = CL_LOAD_TYPE(1:1)

         ENDDO

      end subroutine READ_CL_SCREEN_DATA
                                          
end module screen_interface
