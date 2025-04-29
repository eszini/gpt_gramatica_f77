!
!     Last change: msg 2/27/2017 8:50:22 AM
      SUBROUTINE DISPAT(SEASONAL_SYSTEM_CAP,HRF, &
          MAINTENANCE_RATE,CALL_DISPATCH_REPORT,DISP_BTU_COST, &
        DISP_BTU_TAX_ADDER,BTU_TAX_ACTIVE,FUEL_INVENTORY_ACTIVE, &
        USE_SECONDARY_FUEL,ISEAS)
!
      use debugtrace
      use spindriftlib
      use prod_arrays_dimensions
      use irec_endpoint_control
      use spcapexvariables
      use rptreccontrol
      use grx_planning_routines
      use hesi
      use p_fuel_annl
      use mwunih
      use sizecom
      use foshydi2com
      use mod_base_year
      use screen_interface_data
      use prod2com
      use prodcom
      use spcapexvariables
      use nunits_shared
      use program_state
     
      
      implicit none
      integer, save :: file_trace_sca=0 ! SP_CAPEX_OUTPUT routine only.
      integer, save :: file_trace_dispat=0 ! Tracing in file_trace_dispat routine proper (only).
      integer, save :: file_trace_gmdcu=0 ! Tracing in ENTRY GET_MONTHLY_DISP_COST_BY_UNIT
      integer, save :: sp_capex_output_count=0
      real :: HRF(MAX_CL_UNITS)
      real (kind=4) :: HESI_UNIT_ID_NUM_ord
      REAL (KIND=4) :: NonFuelDispatchCost(MAX_CL_UNITS,2), &
          FuelDispatchCostMultiplier(MAX_CL_UNITS,2)
      CHARACTER (len=1) ::  COUNTRY,FIRST_BLOCK_DISP_SWITCH
      CHARACTER (len=6) ::  MARKET_ID
      INTEGER (kind=2) ::  LAMDA(MAX_DISPATCH_BLOCKS)
      INTEGER (kind=2) ::  PROCMETH,I,L,M,N,NB,UN,ITEMP,PRODUCTION_METHOD, &
      ISEAS,MARKET_AREA_LOOKUP,GET_PRIMARY_MOVER,R_UN, &
                R_BLK(2),R_TG,UZ_PRIMARY_MOVER
      INTEGER (kind=4) ::  FMT1
      REAL (kind=4) ::  SYSTEM_CUM_CAP(0:MAX_DISPATCH_BLOCKS), &
      TRANS_GROUP_CUM_CAP(0:256,0:MAX_DISPATCH_BLOCKS),RMK,RFT,RPM, &
             GET_HEAT_RATE_FACTOR,TEMP_R,R_DISPATCH(2,12), &
             R_LOCAL_COEFF(3),R_DISP_BTU_COST,R_DISP_BTU_TAX_ADDER, &
             FUEL_SCEN_MULT, &
             GET_SCENARIO_COAL_PRICES,GET_SCENARIO_GAS_PRICES, &
             GET_SCENARIO_OIL_PRICES,GET_SCENARIO_URANIUM_PRICES,TEMP_R4
!
! TRANSACTION GROUP VARIABLES FOR DISPATCH REPORT
!
      REAL (kind=4) ::  GROUP_CUM_CAP(0:256),GROUP_SEGMENT_NUM(0:256), &
          GROUP_TOTAL_CAP(0:256), &
          R_HEAT_RATE,BLOCK_CAP

      INTEGER (kind=2) ::  get_TRANSACTION_GROUP, & ! External
          GET_TRANS_GROUP_POSITION, &
        R_BLOCK,J, &
        LOCAL_DATE,BASE_DATE, &
        GET_NEWGEN_INDEX
!
! CHANGED TO REAL ON 5/14/92
!
      REAL ::  SEASONAL_SYSTEM_CAP,LOCAL_YEAR
!
      REAL ::  RTEMP,DISP_COST, &
      PRIM_TOTAL_EMISSION_COST(MAX_CL_UNITS), &
      R_PRIM_TOTAL_EMISSION_COST, &
           STEMP,CURRENT_LAMDA,MAINTENANCE_RATE(MAX_CL_UNITS), &
           COST_CONVERSION,HEAT_CONVERSION,HEAT_RATE,DISP_ADJUSTER, &
           DISP_BTU_COST(MAX_CL_UNITS),DISP_BTU_TAX_ADDER(MAX_CL_UNITS), &
           DISPATCHING_BTU_COST,RTEMP1,TEMP_DISPATCH_MULT,GET_VAR, &
           SAVE_HEATRATE(MAX_DISPATCH_BLOCKS),TOTAL_DELIVERY_COST, &
           GET_MAINTENANCE_RATE_FOR_MONTH,BLENDED_BTU_COST(*), &
           ng_unit_status ! Newgen_unit_status
      REAL ::  GET_COST_CONVERSION,GET_HEAT_CONVERSION
!
! ADDED 12/30/92 FOR FUEL LIMITS
!
      LOGICAL (kind=4) ::  FUEL_INVENTORY_ACTIVE
      CHARACTER (len=20) ::  UNIT_NAME
!
      INTEGER (kind=2) ::  SHADOW_UN
      REAL ::  SHADOW_DISPATCH_ADDER(:)
      ALLOCATABLE :: SHADOW_DISPATCH_ADDER
      LOGICAL (kind=1) ::  BTU_TAX_ACTIVE,USE_SECONDARY_FUEL(*)
      REAL ::  F_MIX,S_MIX,NOX_RATE
      LOGICAL (kind=1) ::  CALL_DISPATCH_REPORT
      LOGICAL (kind=1) ::  SEASONAL_DISPATCH_REPORT, &
          SEASONAL_DISPATCH_REPORT_ACTIVE,REPORT_THIS_DISPATCH_PERIOD, &
        REPORT_NEW_SUPPLY_PERIOD,TEMP_L1,GET_CL_BASECASE_MARKET_ID, &
        FUEL_POINTERS_USED,EXISTING_UNIT

      REAL ::  ADJ_TOTAL_CAP,R_ADJ_TOTAL_CAP
      SAVE ADJ_TOTAL_CAP ! SPEEDS-UP MC ROUTINE
      LOGICAL (kind=1) ::  REASON1,REASON2,REASON3,REASON4,REASON5
!
      INTEGER (kind=2) ::  DISPATCH_ORDER_HEADER,DISP_ORDER_NO
      INTEGER (kind=2) ::  TRANS_DISP_ORDER_NO
      INTEGER (kind=2) ::  TRANS_DISPATCH_RPT_HEADER,NEW_SUPPLY_CURVE_NO, &
          NEW_SUPPLY_CURVE_HEADER
      SAVE DISP_ORDER_NO,TRANS_DISP_ORDER_NO,SAVE_HEATRATE,NEW_SUPPLY_CURVE_NO
      INTEGER ::  DISP_ORDER_REC
      INTEGER ::  TRANS_DISP_ORDER_REC,NEW_SUPPLY_CURVE_REC
      SAVE DISP_ORDER_REC,TRANS_DISP_ORDER_REC,NEW_SUPPLY_CURVE_REC
      LOGICAL (kind=1) ::  TRANS_DISP_ORDER_RPT_NOT_OPEN=.TRUE. , &
          NEW_SUPPLY_CURVE_NOT_OPEN=.TRUE. 
      REAL (KIND=4) :: CUM_CAP,RPT_CAP
      CHARACTER (len=2) :: FIRST=' 1' ,SECOND=' 2' 
      CHARACTER (len=22) :: REPORT_UNIT_NAME
      LOGICAL (kind=1) :: DISP_ORDER_REPORT_NOT_OPEN=.TRUE. , &
          SOUTHERN_COMPANY=.FALSE. ,YES_SECOND_FUEL_EMIS_DISPATCH, &
                  SECOND_FUEL_EMIS_DISPATCH,NEW_SUPPLY_CURVE_REPORT
      CHARACTER (len=1) :: DISPATCH_RPT_TYPE
!
      REAL ::  DISPATCH_EMIS_ADDER
!  END COMMON BLOCK SECTION
      INTEGER (kind=2) :: R_UNIT(*)
      INTEGER (kind=2) :: R_BLKNO(*),ECBLK,R_NUNITS,R_NBLOK2, &
          R_SINGLE_UNIT,CLASS,GET_ASSET_CLASS_NUM
          
      REAL ::   BLOCK_DISP_COST(:,:),R_BLOCK_DISP_COST(nunits_allshared,2), &
      SECOND_DISP_COST(:,:),R_1ST_DISP_COST,R_2ND_DISP_COST, &
             R_1ST_FUEL,R_1ST_NON_FUEL,R_2ND_FUEL, &
             R_2ND_NON_FUEL,EMIS_DISPATCH_1, &
             EMIS_DISPATCH_2,EMIS_DISPATCH_3,EMIS_DISPATCH_4, &
             EMIS_DISPATCH_5,TRANS_DISPATCH_EMIS_ADDER, &
             TOTAL_S_DELIVERY_COST
      ALLOCATABLE :: BLOCK_DISP_COST,SECOND_DISP_COST
      SAVE BLOCK_DISP_COST,SECOND_DISP_COST
!
      INTEGER (kind=2) ::  UPPER_TRANS_GROUP, &
          GET_MAX_TRANS_ID_USED,MAX_BLOCKS_PER_TRANS(:), &
          TRANS,R_MAX_TRANS_BLOCK
      ALLOCATABLE :: MAX_BLOCKS_PER_TRANS
      SAVE           MAX_BLOCKS_PER_TRANS
      REAL (kind=4) ::  TRANS_GROUP_SCARCITY_VALUE,LOCAL_SCARCITY_COST, &
          GLOBAL_SCARCITY_COST,TRANS_GLOBAL_SCARCITY_VALUE
      INTEGER (kind=2) ::  T_GROUP
      INTEGER (kind=4) ::  VALUES_TO_SET
      CHARACTER (len=9) ::  REPORT_PERIOD,LOADS_MONTH_NAME
      REAL (kind=4) ::  SOX_RATE
      CHARACTER (len=35) ::  GET_TRANSACTION_GROUP_NAME
      CHARACTER (len=40) ::  TRANS_GROUP_NAME
      character (len=35) :: TRANS_NAME
      CHARACTER (len=15) ::  LEFT_JUSTIFY_I2_IN_STR
      INTEGER (KIND=2) :: MUST_RUN_BLOCKS,BLK1_DISPATCH,BLK2_DISPATCH, &
          BLK0,BLK1,BLK2,PassPos,IBK1,IBK2
      INTEGER (KIND=2) , ALLOCATABLE, DIMENSION(:) ::  LAMDA_BLK1, &
          BLKNO_BLK1,UNIT_BLK1,LAMDA_BLK2,BLKNO_BLK2,UNIT_BLK2,LAMDA_MR, &
                BLKNO_MR,UNIT_MR,LAMDA_TEMP
      INTEGER (KIND=4) , ALLOCATABLE, DIMENSION(:) :: PosBLK0,PosBLK1,PosBLK2
      REAL (KIND=4) , ALLOCATABLE, DIMENSION(:) :: SAVE_HEATRATE_BLK1,&
          SHADOW_DISPATCH_ADDER_BLK1,INHEAT_BLK1,MWBLOK_BLK1, &
        SAVE_HEATRATE_BLK2,SHADOW_DISPATCH_ADDER_BLK2, &
        INHEAT_BLK2,MWBLOK_BLK2,SAVE_HEATRATE_MR, &
        SHADOW_DISPATCH_ADDER_MR,INHEAT_MR,MWBLOK_MR
        
      LOGICAL (KIND=1) , ALLOCATABLE, DIMENSION(:) :: first_blk_used, &
          SECOND_BLK_PASSED
      LOGICAL (KIND=1) , SAVE :: CAPEX_THERMAL_RPT_NOT_OPEN=.TRUE.
      INTEGER (KIND=2) :: CAPEX_THERMAL_RPT_HEADER,DAY_TYPE_FOR_RESOURCE
      INTEGER (KIND=2) , SAVE :: CAPEX_THERMAL_NO
      INTEGER (KIND=4) , SAVE :: CAPEX_THERMAL_REC
      REAL (KIND=4) :: MIN_CAP, MAX_CAP,MARKET_AREA,NEW_SUPPLY_VARIABLES(16)
      LOGICAL (KIND=1) :: V_LOG
      CHARACTER (LEN=6) :: BASECASE_MARKET_AREA_ID
      CHARACTER (LEN=8) :: SP_BASECASE_MARKET_AREA_ID
      REAL (KIND=4) :: GET_POWERDAT_PLANT_ID,HESI_SECOND_UNIT_ID_NUM, &
          POWERDAT_PLANT_ID,SPCapExResourceOption
      INTEGER (KIND=4) :: OnLineYr,OnLineMo,OffLineYr,OffLineMo
      REAL (KIND=4) :: GET_UNIT_START_UP_COSTS,GET_MIN_UP_TIME, &
          GET_MIN_DOWN_TIME,YEAR_MONTH
      LOGICAL (KIND=1) :: GET_BASECASE_MARKET_AREA_ID,GET_CUBIC_HEAT_CURVE
      CHARACTER (LEN=16) :: SAC_UNIT_NAME,SAC_STATION_GROUP
      REAL (KIND=4) :: A_CO,B_CO,C_CO,D_CO,UNIT_START_UP_COSTS, &
          MIN_UPTIME,MIN_DOWNTIME
      CHARACTER (LEN=35) :: GET_GROUP_NAME
      INTEGER (KIND=2) :: TG_POSITION,GET_EMISSION_MARKET_LINK
      CHARACTER (LEN=6) :: EV_TRANS_AREA_NAME
      LOGICAL (kind=1) :: PASS_TO_ENERPRISE=.TRUE. 
      REAL (kind=4) :: LOCAL_FUEL,GET_PBTUCT_SAVE,LOCAL_DISPATCH_ADDER, &
          GET_DISPADJ_SAVE,TEMP_P_FUEL_DELIVERY, &
        TEMP_P_FUEL_DELIVERY_2,TEMP_P_FUEL_DELIVERY_3
      REAL (KIND=4) :: BTUDispatchAdder

      integer, save :: timeshere=0
      integer (kind=4) :: EP ! Contents of endpoint() for debugger
      ! TODO: prim_mover is declared floating-point so that it can 
      ! be written to report without formatting.  Fix by assigning
      ! to a real variable before write, casting, or adding formatting.
      real (kind=4) :: prim_mover 
      HESI_SECOND_UNIT_ID_NUM=0
      
      if(file_trace_dispat==0) then
        ! This traces ONLY in the dispat routine and none of its
        ! ENTRY entry points.
        file_trace_dispat=open_trace("dispat.trace", rq_dispat)
        call write_trace_message(file_trace_dispat, &
         "Tracing calls to DISPAT routine only (non of its subENTRYs)")
      end if
      IF(ISEAS == 1) THEN
         UPPER_TRANS_GROUP = GET_MAX_TRANS_ID_USED()
         IF(ALLOCATED(MAX_BLOCKS_PER_TRANS)) then
             DEALLOCATE(MAX_BLOCKS_PER_TRANS)
         end if
         ! 0 IS THE MAX OF ALL GROUPS
         ALLOCATE(MAX_BLOCKS_PER_TRANS(0:UPPER_TRANS_GROUP)) 
         MAX_BLOCKS_PER_TRANS = 0
      ENDIF

      SEASONAL_SYSTEM_CAP = 0.
      IF(get_nunits() < 1) RETURN
      SECOND_FUEL_EMIS_DISPATCH = YES_SECOND_FUEL_EMIS_DISPATCH()
!
      COST_CONVERSION = GET_COST_CONVERSION()
      HEAT_CONVERSION = GET_HEAT_CONVERSION()
      PRODUCTION_METHOD = PROCMETH()
      IF(ALLOCATED(BLOCK_DISP_COST)) then
          DEALLOCATE(BLOCK_DISP_COST,SECOND_DISP_COST)
      end if
      
      ALLOCATE(BLOCK_DISP_COST(get_nunits(),2), &
          SECOND_DISP_COST(get_nunits(),2))
      IF(ALLOCATED(SHADOW_DISPATCH_ADDER)) then
          DEALLOCATE(SHADOW_DISPATCH_ADDER)
      end if
      ALLOCATE(SHADOW_DISPATCH_ADDER(NBLOK))

!
      ADJ_TOTAL_CAP = 0.
      BLOCK_DISP_COST = 0.
      SECOND_DISP_COST = 0.
      MUST_RUN_BLOCKS = 0
      BLK1_DISPATCH = 0
      BLK2_DISPATCH = 0
      PRIM_TOTAL_EMISSION_COST = 0.0
      NonFuelDispatchCost = 0.0
      FuelDispatchCostMultiplier = 0.0
      call write_trace_int2(file_trace_dispat, "NBLOK", NBLOK)

      DO NB = 1, NBLOK
         UN = UNIT(NB)
         call write_trace_int2(file_trace_dispat, "8. UN", UN)
         LAMDA(NB) = 32000
         SHADOW_DISPATCH_ADDER(NB) = 0.
!
         IF(ISEAS == 1) THEN
            TRANS = get_TRANSACTION_GROUP(UN)
! 08/13/03. DOUBLE INDEX ADDED.
            TRANS = GET_TRANS_GROUP_POSITION(TRANS)
            MAX_BLOCKS_PER_TRANS(TRANS) = &
                MAX_BLOCKS_PER_TRANS(TRANS) + 1
            MAX_BLOCKS_PER_TRANS(0) = &
                MAX(MAX_BLOCKS_PER_TRANS(0), &
                MAX_BLOCKS_PER_TRANS(TRANS))
         ENDIF

            EMIS_DISPATCH_1 = &
                TRANS_DISPATCH_EMIS_ADDER(1,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_2 = &
                TRANS_DISPATCH_EMIS_ADDER(2,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_3 = &
                TRANS_DISPATCH_EMIS_ADDER(3,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_4 = &
                TRANS_DISPATCH_EMIS_ADDER(4,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_5 = &
                TRANS_DISPATCH_EMIS_ADDER(5,ISEAS,globecom_year,UN)
!
! GAT. 1/31/94.
         IF(BLKNO(NB) == 2) THEN
            MWBLOK(NB) = MW(2,UN) - MW(1,UN)
         ELSE
            MWBLOK(NB) = MW(1,UN)
         ENDIF
!
         IF(ONLINE(UN) .LE. DATE2 .AND. OFLINE(UN) .GE. DATE1) THEN
            IF(SHADOW_UNIT_NUMBER(UN) == 0 .OR. &
                UN < SHADOW_UNIT_NUMBER(UN)) THEN
               SEASONAL_SYSTEM_CAP = SEASONAL_SYSTEM_CAP + MWBLOK(NB)
            ENDIF
            IF((MAINTENANCE_RATE(UN) /= 1. .AND. EAVAIL(UN) /= 0. &
                .AND. (MWBLOK(NB) /= 0. .OR. LDTYPE(UN) == 'L')) .OR. &
                     (EAVAIL(UN) /= 0. .AND. CALL_DISPATCH_REPORT)) THEN

               IF(USE_SECONDARY_FUEL(UN) .OR. &
                   ((SOUTHERN_COMPANY .OR. SECOND_FUEL_EMIS_DISPATCH) &
                    .AND. FUELMX(UN) < 0.) ) THEN
                  F_MIX = 0.
                  S_MIX = 1.
               ELSE
                  F_MIX = ABS(FUELMX(UN))
                  S_MIX = 1. - F_MIX
               ENDIF
               IF(BLKNO(NB) == 2) THEN
                  DISP_ADJUSTER = DISPADJ2(UN)
                  NOX_RATE = F_MIX * PRIM_NOX_BK2(UN) + &
                      S_MIX * SEC_NOX_BK2(UN)
               ELSE
                  DISP_ADJUSTER = DISPADJ(UN)
                  NOX_RATE = F_MIX * NOX(UN) + S_MIX * SEC_NOX_BK1(UN)
               ENDIF
!
               IF(BTU_TAX_ACTIVE) THEN
                  DISPATCHING_BTU_COST = DISP_BTU_COST(UN) + &
                      DISP_BTU_TAX_ADDER(UN)
               ELSE
                  DISPATCHING_BTU_COST = DISP_BTU_COST(UN)
               ENDIF
               IF(BTU_TAX_ACTIVE) THEN
                  BTUDispatchAdder = DISP_BTU_TAX_ADDER(UN)
               ELSE
                  BTUDispatchAdder = 0.
               ENDIF
!
               TEMP_DISPATCH_MULT = DISPATCH_MULT(UN)
               IF(TEMP_DISPATCH_MULT < 0.) THEN ! 030506. ADDED MONTH.
                  get_var_called_from="picadisp:305"
                  TEMP_DISPATCH_MULT = &
                      GET_VAR(TEMP_DISPATCH_MULT,globecom_year,UNITNM(UN))
                  IF(TEMP_DISPATCH_MULT < 0.) THEN
                     TEMP_DISPATCH_MULT = &
                         GET_VAR(TEMP_DISPATCH_MULT,ISEAS,UNITNM(UN))
                  ENDIF
               ENDIF
!
               SAVE_HEATRATE(NB) = INHEAT(NB) * HRF(UN)
!
               PRIM_TOTAL_EMISSION_COST(UN) = 0.001 * &
               TEMP_DISPATCH_MULT * SAVE_HEATRATE(NB) * ( &
               EMIS_DISPATCH_1 * (F_MIX * SO2(UN) + &
               S_MIX * SEC_SO2(UN)) + EMIS_DISPATCH_2 * NOX_RATE + &
                        EMIS_DISPATCH_3 &
               *(F_MIX*PARTICULATES(UN)+S_MIX*SEC_CO2(UN)) &
               +EMIS_DISPATCH_4 *(F_MIX * EMIS_OTH2(UN)  &
               +S_MIX * SEC_OTH2(UN)) + EMIS_DISPATCH_5 &
               *(F_MIX * EMIS_OTH3(UN) + S_MIX * SEC_OTH3(UN)))
!
               NonFuelDispatchCost(UN,MAX(int(1,2),BLKNO(NB))) = &
                (TEMP_DISPATCH_MULT * COST_CONVERSION * SAVE_HEATRATE(NB) *  &  
                 BTUDispatchAdder) + 10.*(VCPMWH(UN) + DISP_ADJUSTER + &
                 PRIM_TOTAL_EMISSION_COST(UN))
                 
               FuelDispatchCostMultiplier(UN,MAX(int(1,2),BLKNO(NB))) = &
                   (TEMP_DISPATCH_MULT * COST_CONVERSION * SAVE_HEATRATE(NB))

               RTEMP = TEMP_DISPATCH_MULT * (SAVE_HEATRATE(NB) * &
               (DISPATCHING_BTU_COST + EMIS_DISPATCH_1 * (F_MIX * SO2(UN) + &
               S_MIX * SEC_SO2(UN)) + EMIS_DISPATCH_2 * NOX_RATE + &
               EMIS_DISPATCH_3 *(F_MIX*PARTICULATES(UN)+ &
               S_MIX*SEC_CO2(UN)) + EMIS_DISPATCH_4 &
               *(F_MIX * EMIS_OTH2(UN) + S_MIX * SEC_OTH2(UN)) + &
               EMIS_DISPATCH_5 *(F_MIX * EMIS_OTH3(UN) + S_MIX &
               * SEC_OTH3(UN)))*COST_CONVERSION + &
                        10.*(VCPMWH(UN) + DISP_ADJUSTER) )
!
               CALL GET_TOTAL_S_DELIVERY_COST(TOTAL_S_DELIVERY_COST,UN)
!
               STEMP = TEMP_DISPATCH_MULT * (SAVE_HEATRATE(NB) * &
                   (SBTUCT(UN) + TOTAL_S_DELIVERY_COST + &
                EMIS_DISPATCH_1 * S_MIX * SEC_SO2(UN) + &
                EMIS_DISPATCH_2 * S_MIX * SEC_NOX_BK2(UN) + &
                EMIS_DISPATCH_3 * S_MIX*SEC_CO2(UN) + &
                EMIS_DISPATCH_4 * S_MIX * SEC_OTH2(UN) + &
                EMIS_DISPATCH_5 * S_MIX * SEC_OTH3(UN))* &
                COST_CONVERSION + 10.*(VCPMWH(UN) + DISP_ADJUSTER) )
               IF(PRODUCTION_METHOD== 4) RTEMP = RTEMP + 10.*FUELADJ(UN)

! THE SHADOW_DISPATCH_ADDER IS USED TO ENSURE THAT THE SHADOW UNIT 
! FOLLOWS THE MAIN UNIT IN THE DISPATCH ORDER

               SHADOW_UN = SHADOW_UNIT_NUMBER(UN)
               IF(SHADOW_UN > 0 .AND. UN > SHADOW_UN) THEN
             RTEMP1 = BLOCK_DISP_COST(SHADOW_UN,MAX(BLKNO(NB),int(1,2)))
                  IF(RTEMP1 > RTEMP) THEN
                     SHADOW_DISPATCH_ADDER(NB) = (RTEMP1 - RTEMP)/10.
                     RTEMP = RTEMP1
                  ENDIF
               ELSE
!
                  ADJ_TOTAL_CAP = ADJ_TOTAL_CAP + MWBLOK(NB) &
                      * (1.-MAINTENANCE_RATE(UN))
!
               ENDIF
               BLOCK_DISP_COST(UN,MAX(BLKNO(NB),int(1,2))) = RTEMP
               SECOND_DISP_COST(UN,MAX(BLKNO(NB),int(1,2))) = STEMP
! LAMDA SET HERE
               LAMDA(NB) = MAX(-31999,NINT(MIN(31999.,RTEMP)))
            ENDIF
         ENDIF
         IF(BLKNO(NB) == 0 .AND. LAMDA(NB) < 32000) THEN
            MUST_RUN_BLOCKS = MUST_RUN_BLOCKS + 1
         ELSEIF(BLKNO(NB) == 1 .AND. LAMDA(NB) < 32000) THEN
            BLK1_DISPATCH = BLK1_DISPATCH + 1
         ELSE
            BLK2_DISPATCH = BLK2_DISPATCH + 1
         ENDIF
      ENDDO
      CALL RW_PROCESS_MESSAGES()
!
! TRYING TO INPROVE THE SORT TIME OF THE DISPATCH ROUTINE
!
      SCREEN_MESSAGES = TRIM(LOADS_MONTH_NAME(ISEAS))//"-Dispatch Order"
      CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      CALL DISPLAY_TIME
!
      IF(get_nunits() > 2000 .AND. ISEAS <= 2) THEN
         IF(ALLOCATED(LAMDA_BLK1))DEALLOCATE(LAMDA_BLK1,BLKNO_BLK1, &
             INHEAT_BLK1,MWBLOK_BLK1,UNIT_BLK1,SAVE_HEATRATE_BLK1, &
            SHADOW_DISPATCH_ADDER_BLK1)
         IF(BLK1_DISPATCH > 0) THEN
            ALLOCATE(LAMDA_BLK1(BLK1_DISPATCH),BLKNO_BLK1(BLK1_DISPATCH),&
            INHEAT_BLK1(BLK1_DISPATCH),MWBLOK_BLK1(BLK1_DISPATCH), &
            UNIT_BLK1(BLK1_DISPATCH),&
            SAVE_HEATRATE_BLK1(BLK1_DISPATCH), &
            SHADOW_DISPATCH_ADDER_BLK1(BLK1_DISPATCH))
         ENDIF
         IF(ALLOCATED(LAMDA_BLK2)) DEALLOCATE(LAMDA_BLK2,BLKNO_BLK2, &
            INHEAT_BLK2,MWBLOK_BLK2,UNIT_BLK2,SAVE_HEATRATE_BLK2, &
            SHADOW_DISPATCH_ADDER_BLK2)
         IF(BLK2_DISPATCH > 0) THEN
            ALLOCATE(LAMDA_BLK2(BLK2_DISPATCH),BLKNO_BLK2(BLK2_DISPATCH), &
            INHEAT_BLK2(BLK2_DISPATCH),MWBLOK_BLK2(BLK2_DISPATCH), &
                  UNIT_BLK2(BLK2_DISPATCH),SAVE_HEATRATE_BLK2(BLK2_DISPATCH), &
                  SHADOW_DISPATCH_ADDER_BLK2(BLK2_DISPATCH))
         ENDIF
         IF(ALLOCATED(LAMDA_MR)) DEALLOCATE(LAMDA_MR,BLKNO_MR,INHEAT_MR, &
         MWBLOK_MR,UNIT_MR,SAVE_HEATRATE_MR,SHADOW_DISPATCH_ADDER_MR)
         IF(MUST_RUN_BLOCKS > 0) THEN
            ALLOCATE(LAMDA_MR(MUST_RUN_BLOCKS),BLKNO_MR(MUST_RUN_BLOCKS), &
            INHEAT_MR(MUST_RUN_BLOCKS),MWBLOK_MR(MUST_RUN_BLOCKS), &
                  UNIT_MR(MUST_RUN_BLOCKS),SAVE_HEATRATE_MR(MUST_RUN_BLOCKS), &
                  SHADOW_DISPATCH_ADDER_MR(MUST_RUN_BLOCKS))
         ENDIF
!
         BLK2 = 0
         BLK1 = 0
         BLK0 = 0
         DO NB = 1, NBLOK
            UN = UNIT(NB)
            call write_trace_int2(file_trace_dispat, "8. UN", UN)
            IF(BLKNO(NB) == 0 .AND. LAMDA(NB) < 32000) THEN
               BLK0 = BLK0 + 1
               LAMDA_MR(BLK0) = LAMDA(NB)
               BLKNO_MR(BLK0) = BLKNO(NB)
               INHEAT_MR(BLK0) = INHEAT(NB)
               MWBLOK_MR(BLK0) = MWBLOK(NB)
               UNIT_MR(BLK0) = UNIT(NB)
               SAVE_HEATRATE_MR(BLK0) = SAVE_HEATRATE(NB)
               SHADOW_DISPATCH_ADDER_MR(BLK0)=SHADOW_DISPATCH_ADDER(NB)
            ELSEIF(BLKNO(NB) == 1 .AND. LAMDA(NB) < 32000) THEN
               BLK1 = BLK1 + 1
               LAMDA_BLK1(BLK1) = LAMDA(NB)
               BLKNO_BLK1(BLK1) = BLKNO(NB)
               INHEAT_BLK1(BLK1) = INHEAT(NB)
               MWBLOK_BLK1(BLK1) = MWBLOK(NB)
               UNIT_BLK1(BLK1) = UNIT(NB)
               SAVE_HEATRATE_BLK1(BLK1) = SAVE_HEATRATE(NB)
               SHADOW_DISPATCH_ADDER_BLK1(BLK1) = SHADOW_DISPATCH_ADDER(NB)
            ELSE
               BLK2 = BLK2 + 1
               LAMDA_BLK2(BLK2) = LAMDA(NB)
               BLKNO_BLK2(BLK2) = BLKNO(NB)
               INHEAT_BLK2(BLK2) = INHEAT(NB)
               MWBLOK_BLK2(BLK2) = MWBLOK(NB)
               UNIT_BLK2(BLK2) = UNIT(NB)
               SAVE_HEATRATE_BLK2(BLK2) = SAVE_HEATRATE(NB)
               SHADOW_DISPATCH_ADDER_BLK2(BLK2) = SHADOW_DISPATCH_ADDER(NB)
            ENDIF
         ENDDO
!
         allocate(first_blk_used(MAX_CL_UNITS))
         first_blk_used = .FALSE.
         PassPos = 1
!
! BY PASS IF THERE ARE NOT ANY MUST RUN UNITS
!
         IF(BLK0 > 0) THEN
            IF(ALLOCATED(PosBLK0)) DEALLOCATE(PosBLK0)
            ALLOCATE(PosBLK0(BLK0))

            CAll QuickI(INT(BLK0,4),LAMDA_MR,PosBLK0) 
            DO PassPos = 1, BLK0
               UN = UNIT_MR(PosBLK0(PassPos))
               call write_trace_int2(file_trace_dispat, "7. UN", UN)
               if(first_blk_used(UN)) then
               WRITE(SCREEN_MESSAGES,"(1x,I5,1x,a)") UN,UNITNM(UN)
              CALL MG_CLEAR_LINE_WRITE(8,59,63,TRIM(SCREEN_MESSAGES), &
                  ALL_VERSIONS,0)
                  ! TODO: Get rid of this pause.
                  pause
               endif
               first_blk_used(UN) = .TRUE.
               LAMDA(PassPos) = LAMDA_MR(PosBLK0(PassPos))
               UNIT(PassPos) = UNIT_MR(PosBLK0(PassPos))
               BLKNO(PassPos) = BLKNO_MR(PosBLK0(PassPos))
               INHEAT(PassPos) = INHEAT_MR(PosBLK0(PassPos))
               MWBLOK(PassPos) = MWBLOK_MR(PosBLK0(PassPos))
               SAVE_HEATRATE(PassPos) = &
                   SAVE_HEATRATE_MR(PosBLK0(PassPos))
               SHADOW_DISPATCH_ADDER(PassPos) = &
                   SHADOW_DISPATCH_ADDER_MR(PosBLK0(PassPos))
            ENDDO
            CALL RW_PROCESS_MESSAGES()
            IF(ALLOCATED(PosBLK0)) DEALLOCATE(PosBLK0)
         ENDIF
!
! PROCESS FIRST BLOCKS
!
         IF(BLK1 > 0) THEN
            IF(ALLOCATED(PosBLK1)) DEALLOCATE(PosBLK1)
            ALLOCATE(PosBLK1(BLK1))
            ALLOCATE(LAMDA_TEMP(BLK1_DISPATCH))
            LAMDA_TEMP = LAMDA_BLK1
            CAll QuickI(INT(BLK1,4),LAMDA_TEMP,PosBLK1) 
            DO I = 1, BLK1
               LAMDA_BLK1(I) = LAMDA_TEMP(PosBLK1(I))
            ENDDO
            DEALLOCATE(LAMDA_TEMP)
            CALL RW_PROCESS_MESSAGES()
         ENDIF
!
! PROCESS SECOND BLOCKS IF THERE ARE ANY
!
         IF(BLK2 > 0) THEN
            ALLOCATE(LAMDA_TEMP(BLK2_DISPATCH))
            LAMDA_TEMP = LAMDA_BLK2
            IF(ALLOCATED(PosBLK2)) DEALLOCATE(PosBLK2)
            ALLOCATE(PosBLK2(BLK2))
            CAll QuickI(INT(BLK2,4),LAMDA_TEMP,PosBLK2) 
            DO I = 1, BLK2
               LAMDA_BLK2(I) = LAMDA_TEMP(PosBLK2(I))
            ENDDO
            DEALLOCATE(LAMDA_TEMP)
            ALLOCATE(SECOND_BLK_PASSED(BLK2))
            SECOND_BLK_PASSED = .FALSE.
            CALL RW_PROCESS_MESSAGES()
         ENDIF
         IBK1 = 1
         IBK2 = 1
         DO
!

            IF(IBK1 <= BLK1 .AND. IBK2 <= BLK2) THEN
               IF(LAMDA_BLK1(IBK1) <= MINVAL(LAMDA_BLK2(IBK2:BLK2)))THEN
                  UN = UNIT_BLK1(PosBLK1(IBK1))
              call write_trace_int2(file_trace_dispat, "1. IBK1", IBK1)
              call write_trace_int2(file_trace_dispat, "6. UN", UN)
                  first_blk_used(UN) = .TRUE.
                  LAMDA(PassPos) = LAMDA_BLK1(IBK1)
                  UNIT(PassPos) = UNIT_BLK1(PosBLK1(IBK1))
                  BLKNO(PassPos) = BLKNO_BLK1(PosBLK1(IBK1))
                  INHEAT(PassPos) = INHEAT_BLK1(PosBLK1(IBK1))
                  MWBLOK(PassPos) = MWBLOK_BLK1(PosBLK1(IBK1))
                  SAVE_HEATRATE(PassPos) = &
                      SAVE_HEATRATE_BLK1(PosBLK1(IBK1))
                  SHADOW_DISPATCH_ADDER(PassPos) = &
                      SHADOW_DISPATCH_ADDER_BLK1(PosBLK1(IBK1))
                  PassPos = PassPos + 1
                  IBK1 = IBK1 + 1
               ELSE
                  DO I = IBK2, BLK2
!
                     IF(LAMDA_BLK1(IBK1) <= LAMDA_BLK2(I) .OR. &
                         SECOND_BLK_PASSED(I)) then
                         CYCLE
                     end if
                     UN = UNIT_BLK2(PosBLK2(I))
                     call write_trace_int2(file_trace_dispat, "4. I", I)
                   call write_trace_int2(file_trace_dispat, "4. UN", UN)
                     IF(first_blk_used(UN)) THEN
                        LAMDA(PassPos) = LAMDA_BLK2(I)
                        UNIT(PassPos) = UNIT_BLK2(PosBLK2(I))
                        BLKNO(PassPos) = BLKNO_BLK2(PosBLK2(I))
                        INHEAT(PassPos) = INHEAT_BLK2(PosBLK2(I))
                        MWBLOK(PassPos) = MWBLOK_BLK2(PosBLK2(I))
                        SAVE_HEATRATE(PassPos) = &
                            SAVE_HEATRATE_BLK2(PosBLK2(I))
                        SHADOW_DISPATCH_ADDER(PassPos) = &
                            SHADOW_DISPATCH_ADDER_BLK2(PosBLK2(I))
                        PassPos = PassPos + 1
                        SECOND_BLK_PASSED(I) = .TRUE.
                        LAMDA_BLK2(I) = 32000
                     ENDIF
                  ENDDO
! RESET THE VALUE IBK2 TO THE FIRST SECOND BLOCK THAT HASN'T BEEN DISPATCHED
                  DO I = 1, BLK2
!
                     IF(.NOT. SECOND_BLK_PASSED(I)) EXIT
                  ENDDO
                  IBK2 = I
!
! PASS THE FIRST BLOCK
!

                  UN = UNIT_BLK1(PosBLK1(IBK1))
                  call write_trace_int2(file_trace_dispat, "3. UN", UN)
                  first_blk_used(UN) = .TRUE.
                  LAMDA(PassPos) = LAMDA_BLK1(IBK1)
                  UNIT(PassPos) = UNIT_BLK1(PosBLK1(IBK1))
                  BLKNO(PassPos) = BLKNO_BLK1(PosBLK1(IBK1))
                  INHEAT(PassPos) = INHEAT_BLK1(PosBLK1(IBK1))
                  MWBLOK(PassPos) = MWBLOK_BLK1(PosBLK1(IBK1))
                  SAVE_HEATRATE(PassPos) = &
                      SAVE_HEATRATE_BLK1(PosBLK1(IBK1))
                  SHADOW_DISPATCH_ADDER(PassPos) = &
                      SHADOW_DISPATCH_ADDER_BLK1(PosBLK1(IBK1))
                  PassPos = PassPos + 1
                  IBK1 = IBK1 + 1
               ENDIF
            ELSE
               IF(IBK1 <= BLK1) THEN
                  DO I = IBK1, BLK1
                     LAMDA(PassPos) = LAMDA_BLK1(I)
                     UNIT(PassPos) = UNIT_BLK1(PosBLK1(I))
                     BLKNO(PassPos) = BLKNO_BLK1(PosBLK1(I))
                     INHEAT(PassPos) = INHEAT_BLK1(PosBLK1(I))
                     MWBLOK(PassPos) = MWBLOK_BLK1(PosBLK1(I))
                     SAVE_HEATRATE(PassPos) = &
                         SAVE_HEATRATE_BLK1(PosBLK1(I))
                     SHADOW_DISPATCH_ADDER(PassPos) = &
                         SHADOW_DISPATCH_ADDER_BLK1(PosBLK1(I))
                     PassPos = PassPos + 1

                  ENDDO
               ENDIF
               IF(BLK2 > 0) THEN
                  DO I = 1, BLK2
                     IF(SECOND_BLK_PASSED(I)) CYCLE
                     LAMDA(PassPos) = LAMDA_BLK2(I)
                     UNIT(PassPos) = UNIT_BLK2(PosBLK2(I))
                     BLKNO(PassPos) = BLKNO_BLK2(PosBLK2(I))
                     INHEAT(PassPos) = INHEAT_BLK2(PosBLK2(I))
                     MWBLOK(PassPos) = MWBLOK_BLK2(PosBLK2(I))
                     SAVE_HEATRATE(PassPos) = &
                         SAVE_HEATRATE_BLK2(PosBLK2(I))
                     SHADOW_DISPATCH_ADDER(PassPos) = &
                         SHADOW_DISPATCH_ADDER_BLK2(PosBLK2(I))
                     PassPos = PassPos + 1
!
                  ENDDO
               ENDIF
               EXIT
            ENDIF
         ENDDO
         IF(ALLOCATED(PosBLK1)) DEALLOCATE(PosBLK1)
         IF(ALLOCATED(PosBLK2)) DEALLOCATE(PosBLK2)
         IF(ALLOCATED(SECOND_BLK_PASSED)) DEALLOCATE(SECOND_BLK_PASSED)
         IF(ALLOCATED(first_blk_used)) DEALLOCATE(first_blk_used)
         IF(ALLOCATED(LAMDA_BLK1)) DEALLOCATE(LAMDA_BLK1,&
             BLKNO_BLK1,INHEAT_BLK1,MWBLOK_BLK1, &
             UNIT_BLK1,SAVE_HEATRATE_BLK1, &
            SHADOW_DISPATCH_ADDER_BLK1)
            
         IF(ALLOCATED(LAMDA_BLK2)) DEALLOCATE(LAMDA_BLK2, &
             BLKNO_BLK2,INHEAT_BLK2,MWBLOK_BLK2,UNIT_BLK2, &
             SAVE_HEATRATE_BLK2, &
            SHADOW_DISPATCH_ADDER_BLK2)
            
         IF(ALLOCATED(LAMDA_MR)) DEALLOCATE(LAMDA_MR,BLKNO_MR, &
             INHEAT_MR,MWBLOK_MR,UNIT_MR,SAVE_HEATRATE_MR, &
             SHADOW_DISPATCH_ADDER_MR)
      ELSE
         CALL DISPATCH_SORT(NBLOK,LAMDA,BLKNO,INHEAT,MWBLOK,UNIT, &
             SAVE_HEATRATE,SHADOW_DISPATCH_ADDER)
      ENDIF
      CALL DISPLAY_TIME
      SCREEN_MESSAGES ="Dispatch Order Finished"
      CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
!
! THE BELOW MAY NOT BE NEEDED 11/17/04
!
      NBLOCK = NBLOK
      DO WHILE ((NBLOCK > 0) .AND. (LAMDA(NBLOCK) > 31999))
         NBLOCK = NBLOCK - 1
      ENDDO
      CALL RW_PROCESS_MESSAGES()

      SEASONAL_DISPATCH_REPORT_ACTIVE = SEASONAL_DISPATCH_REPORT()

      IF((CALL_DISPATCH_REPORT .OR. REPORT_THIS_DISPATCH_PERIOD(ISEAS)) &
          .AND. NBLOCK > 0 .AND. .NOT. TESTING_PLAN) THEN

         GROUP_CUM_CAP = 0.
         GROUP_SEGMENT_NUM = 0.
         GROUP_TOTAL_CAP = 0.
         SYSTEM_CUM_CAP = 0.
         TRANS_GROUP_CUM_CAP = 0.
         IF(DISP_ORDER_REPORT_NOT_OPEN .AND. &
             (DISPATCH_RPT_TYPE() == 'A' .OR. &
             DISPATCH_RPT_TYPE() == 'B')) THEN
            DISP_ORDER_NO = DISPATCH_ORDER_HEADER(DISP_ORDER_REC)
            DISP_ORDER_REPORT_NOT_OPEN = .FALSE.
         ENDIF
         IF(TRANS_DISP_ORDER_RPT_NOT_OPEN .AND. &
             (DISPATCH_RPT_TYPE() == 'T' .OR. &
             DISPATCH_RPT_TYPE() == 'B')) THEN
            TRANS_DISP_ORDER_NO = &
                TRANS_DISPATCH_RPT_HEADER(INT(60,2),TRANS_DISP_ORDER_REC)
            TRANS_DISP_ORDER_RPT_NOT_OPEN = .FALSE.
         ENDIF
!
         LOCAL_YEAR = FLOAT(globecom_year+get_BASE_YEAR())
         IF(CALL_DISPATCH_REPORT) THEN
            REPORT_PERIOD = 'Annual'
         ELSE
            REPORT_PERIOD = LOADS_MONTH_NAME(ISEAS)
         ENDIF
!
! TESTING REPORT FOR DISPATCH ORDER
!

!
! CALCULATE THE CAPACITY ISSUES
!
         CUM_CAP = 0.
         call write_trace_int2(file_trace_dispat, "2. NBLOCK", NBLOCK)
         call write_trace_message(file_trace_dispat, "2. Iterating.")
         DO I = 1, NBLOCK
            call write_trace_int2(file_trace_dispat, "2. I", I)
            UN = UNIT(I)
            call write_trace_int2(file_trace_dispat, "2. UN became", UN)
            T_GROUP = get_TRANSACTION_GROUP(UN)
            BLOCK_CAP = MWBLOK(I)*EAVAIL(UN)*(1.-MAINTENANCE_RATE(UN))
            GROUP_TOTAL_CAP(T_GROUP)=GROUP_TOTAL_CAP(T_GROUP)+BLOCK_CAP
            TRANS_GROUP_CUM_CAP(T_GROUP,I) = GROUP_TOTAL_CAP(T_GROUP)
            CUM_CAP = CUM_CAP + BLOCK_CAP
            SYSTEM_CUM_CAP(I) = CUM_CAP
            SYSTEM_CUM_CAP(0) = SYSTEM_CUM_CAP(0) + BLOCK_CAP
         ENDDO
         CALL RW_PROCESS_MESSAGES()
!
         CUM_CAP = 0.
         call write_trace_int2(file_trace_dispat, "NBLOCK", NBLOCK)
         call write_trace_message(file_trace_dispat, "1. Iterating.")
          DO I = 1, NBLOCK
            UN = UNIT(I)
            call write_trace_int2(file_trace_dispat, "1. I", I)
            call write_trace_int2(file_trace_dispat, "UN became", UN)
            
            T_GROUP = get_TRANSACTION_GROUP(UN)
            DISP_COST = FLOAT(LAMDA(I))/10.
            RTEMP = SAVE_HEATRATE(I)/HEAT_CONVERSION
            UNIT_NAME = UNITNM(UN)


            EMIS_DISPATCH_1 = &
            TRANS_DISPATCH_EMIS_ADDER(1,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_2 = &
               TRANS_DISPATCH_EMIS_ADDER(2,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_3 = &
            TRANS_DISPATCH_EMIS_ADDER(3,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_4 = &
            TRANS_DISPATCH_EMIS_ADDER(4,ISEAS,globecom_year,UN)
            EMIS_DISPATCH_5 = &
                 TRANS_DISPATCH_EMIS_ADDER(5,ISEAS,globecom_year,UN)


             IF(USE_SECONDARY_FUEL(UN) .OR. &
                 ((SOUTHERN_COMPANY .OR. SECOND_FUEL_EMIS_DISPATCH) &
                 .AND. FUELMX(UN) < 0.) ) THEN
                F_MIX = 0.
                S_MIX = 1.
             ELSE
                F_MIX = ABS(FUELMX(UN))
                S_MIX = 1. - F_MIX
             ENDIF
             IF(BTU_TAX_ACTIVE) THEN
                DISPATCHING_BTU_COST = DISP_BTU_COST(UN) + &
                    DISP_BTU_TAX_ADDER(UN)
             ELSE
                DISPATCHING_BTU_COST = DISP_BTU_COST(UN)
             ENDIF
            IF(BLKNO(I) == 2) THEN
               DISP_ADJUSTER = DISPADJ2(UN)
               NOX_RATE = F_MIX * PRIM_NOX_BK2(UN) + &
                   S_MIX * SEC_NOX_BK2(UN)
            ELSE
               DISP_ADJUSTER = DISPADJ(UN)
               NOX_RATE = F_MIX * NOX(UN) + S_MIX * SEC_NOX_BK1(UN)
            ENDIF
            SOX_RATE = F_MIX * SO2(UN) + S_MIX * SEC_SO2(UN)
            IF(SHADOW_DISPATCH_ADDER(I) /= 0.) &
                DISP_ADJUSTER = SHADOW_DISPATCH_ADDER(I)
!
            TEMP_DISPATCH_MULT = DISPATCH_MULT(UN)
            IF(TEMP_DISPATCH_MULT < 0.) THEN ! 030506. ADDED MONTH.
               get_var_called_from="picadisp:758"
               TEMP_DISPATCH_MULT = &
                   GET_VAR(TEMP_DISPATCH_MULT,globecom_year,UNITNM(UN))
               IF(TEMP_DISPATCH_MULT < 0.) THEN
               get_var_called_from="picadisp:758"
                  TEMP_DISPATCH_MULT = &
                      GET_VAR(TEMP_DISPATCH_MULT,ISEAS,UNITNM(UN))
               ENDIF
            ENDIF
!
! 10/19/01
!
            TEMP_L1 = GET_CL_BASECASE_MARKET_ID(UN,MARKET_ID)
            RMK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
!
            RFT = GET_PRIMARY_MOVER(UN)
!
            IF(BLKNO(I) < 2) THEN
               REPORT_UNIT_NAME = UNIT_NAME//FIRST
            ELSE
               REPORT_UNIT_NAME = UNIT_NAME//SECOND
            ENDIF
            CUM_CAP = CUM_CAP + MWBLOK(I)

            LOCAL_SCARCITY_COST = TRANS_GROUP_SCARCITY_VALUE( &
                T_GROUP,ISEAS,globecom_year, &
                TRANS_GROUP_CUM_CAP(T_GROUP,I), &
                GROUP_TOTAL_CAP(T_GROUP))
                
            GLOBAL_SCARCITY_COST = TRANS_GLOBAL_SCARCITY_VALUE(ISEAS, &
                globecom_year,SYSTEM_CUM_CAP(I),SYSTEM_CUM_CAP(0))
                
            IF(DISPATCH_RPT_TYPE() == 'A' &
                .OR. DISPATCH_RPT_TYPE() == 'B') THEN
               DISP_ORDER_REC = RPTREC(DISP_ORDER_NO)
               
                 WRITE(DISP_ORDER_NO,REC=DISP_ORDER_REC) PRT_ENDPOINT(), &
                 LOCAL_YEAR,REPORT_PERIOD,REPORT_UNIT_NAME,FLOAT(I), &
                 MWBLOK(I),CUM_CAP,SAVE_HEATRATE(I),DISP_COST,RTEMP * &
                 DISPATCHING_BTU_COST,VCPMWH(UN),DISP_ADJUSTER, &
                  RTEMP * SOX_RATE * EMIS_DISPATCH_1,RTEMP * NOX_RATE * &
                  EMIS_DISPATCH_2, &
                  RTEMP * (EMIS_DISPATCH_3 *(F_MIX*PARTICULATES(UN)+&
                  S_MIX*SEC_CO2(UN)) + EMIS_DISPATCH_4 *(F_MIX * &
                  EMIS_OTH2(UN) + &
                  S_MIX * SEC_OTH2(UN)) + EMIS_DISPATCH_5 *&
                  (F_MIX * EMIS_OTH3(UN) + S_MIX * SEC_OTH3(UN))), &
                  TEMP_DISPATCH_MULT, &
                  SOX_RATE,2000.*EMIS_DISPATCH_1,NOX_RATE,2000.* &
                  EMIS_DISPATCH_2,DISPATCHING_BTU_COST,100.* &
                  MAINTENANCE_RATE(UN), & 
! DERATED MAINTENANCE CAP
                  100.*(1.-EAVAIL(UN)),MWBLOK(I)*(1.-MAINTENANCE_RATE(UN)) &
                  *EAVAIL(UN),LOCAL_SCARCITY_COST,FLOAT(T_GROUP), &
                  GLOBAL_SCARCITY_COST,GLOBAL_SCARCITY_COST + &
                  LOCAL_SCARCITY_COST,RMK,RFT
               DISP_ORDER_REC = DISP_ORDER_REC + 1
            ENDIF
!
! TRANSACTION GROUP REPORT
!
            IF(DISPATCH_RPT_TYPE() == 'T' .OR. DISPATCH_RPT_TYPE() == 'B') THEN
               T_GROUP = get_TRANSACTION_GROUP(UN)
               TRANS_GROUP_NAME = GET_TRANSACTION_GROUP_NAME(T_GROUP)// &
                   " "//LEFT_JUSTIFY_I2_IN_STR(T_GROUP)
               GROUP_SEGMENT_NUM(T_GROUP)=GROUP_SEGMENT_NUM(T_GROUP)+1.
               GROUP_CUM_CAP(T_GROUP) = GROUP_CUM_CAP(T_GROUP) + &
                   MWBLOK(I)*EAVAIL(UN)*(1.-MAINTENANCE_RATE(UN))
               TRANS_DISP_ORDER_REC = RPTREC(TRANS_DISP_ORDER_NO)
                 WRITE(TRANS_DISP_ORDER_NO,REC=TRANS_DISP_ORDER_REC)  &
                    PRT_ENDPOINT(),LOCAL_YEAR,TRANS_GROUP_NAME, &
                    REPORT_PERIOD,REPORT_UNIT_NAME, &
                    GROUP_SEGMENT_NUM(T_GROUP), &
                    MWBLOK(I),GROUP_CUM_CAP(T_GROUP), &
                    SAVE_HEATRATE(I),DISP_COST,RTEMP * DISPATCHING_BTU_COST, &
                    VCPMWH(UN), &
                    DISP_ADJUSTER,RTEMP * SOX_RATE * EMIS_DISPATCH_1, &
                    RTEMP * NOX_RATE * EMIS_DISPATCH_2, &
                    RTEMP * (EMIS_DISPATCH_3 *(F_MIX*PARTICULATES(UN) &
                    + S_MIX*SEC_CO2(UN)) &
                    + EMIS_DISPATCH_4 *(F_MIX * EMIS_OTH2(UN) + &
                    S_MIX * SEC_OTH2(UN)) + EMIS_DISPATCH_5 *&
                    (F_MIX * EMIS_OTH3(UN) &
                    + S_MIX * SEC_OTH3(UN))),DISPATCH_MULT(UN),&
                    SOX_RATE,2000.*EMIS_DISPATCH_1,NOX_RATE,2000.* &
                    EMIS_DISPATCH_2, &
                    DISPATCHING_BTU_COST,FLOAT(I),CUM_CAP,100.* &
                    MAINTENANCE_RATE(UN),100.*(1.-EAVAIL(UN)), &
                    MWBLOK(I)*(1.-MAINTENANCE_RATE(UN))* &
                    EAVAIL(UN),LOCAL_SCARCITY_COST,FLOAT(T_GROUP), &
                    GLOBAL_SCARCITY_COST, &
                    GLOBAL_SCARCITY_COST + LOCAL_SCARCITY_COST,RMK, &
                    RFT,FLOAT(BLKNO(I))
               TRANS_DISP_ORDER_REC = TRANS_DISP_ORDER_REC + 1
            ENDIF
         ENDDO
       ENDIF
      CALL RW_PROCESS_MESSAGES()
!
!      DEALLOCATE(BLOCK_DISP_COST, !!! 3/7/95. GAT. !!!
!
 9330 DEALLOCATE(SHADOW_DISPATCH_ADDER)
!
! ADDED 08/18/03.
!
      CALL MG_LOCATE_WRITE(18,70," ",3,2)
!
      RETURN

      ENTRY GET_PRIM_TOTAL_EMISSION_COST(R_NUNITS,R_PRIM_TOTAL_EMISSION_COST)

         R_PRIM_TOTAL_EMISSION_COST = PRIM_TOTAL_EMISSION_COST(R_NUNITS)
      RETURN

      ENTRY SP_CAPEX_OUTPUT(ISEAS,HRF, &
          MAINTENANCE_RATE,DISP_BTU_COST,USE_SECONDARY_FUEL,&
          BLENDED_BTU_COST)
       sp_capex_output_count=sp_capex_output_count+1
       if (file_trace_sca==0) then
        file_trace_sca=open_trace("sp_capex_output.trace", rq_sca)
        call write_trace_message(file_trace_sca, &
            "sp_capex_output function")
       end if
       call write_trace_int(file_trace_sca, "sp_capex_output_count", &
        sp_capex_output_count)
        
!
!
      IF(get_nunits() < 1) RETURN
      SECOND_FUEL_EMIS_DISPATCH = YES_SECOND_FUEL_EMIS_DISPATCH()
!
      COST_CONVERSION = GET_COST_CONVERSION()
      HEAT_CONVERSION = GET_HEAT_CONVERSION()
      PRODUCTION_METHOD = PROCMETH()
      REPORT_PERIOD = LOADS_MONTH_NAME(ISEAS)
      IF(ALLOCATED(BLOCK_DISP_COST)) &
          DEALLOCATE(BLOCK_DISP_COST,SECOND_DISP_COST)
      ALLOCATE(BLOCK_DISP_COST(get_nunits(),2), &
      SECOND_DISP_COST(get_nunits(),2))
      IF(ALLOCATED(SHADOW_DISPATCH_ADDER) )then
          DEALLOCATE(SHADOW_DISPATCH_ADDER)
      end if
      ALLOCATE(SHADOW_DISPATCH_ADDER(NBLOK))
!
      BLOCK_DISP_COST = 0.
      SECOND_DISP_COST = 0.
      SHADOW_DISPATCH_ADDER = 0.
!
      DO NB = 1, NBLOK
         UN = UNIT(NB)
         call write_trace_int2(file_trace_sca, "9. UN", UN)
         if(globecom_year==0) then
            call end_program("picadisp:0002 - " // &
            "globecom_year value of zero is invalid.")
            
         end if
         EMIS_DISPATCH_1 = TRANS_DISPATCH_EMIS_ADDER(1,ISEAS, &
            globecom_year,UN)
         EMIS_DISPATCH_2 = TRANS_DISPATCH_EMIS_ADDER(2,ISEAS, & 
            globecom_year,UN)
         EMIS_DISPATCH_3 = TRANS_DISPATCH_EMIS_ADDER(3,ISEAS, &
            globecom_year,UN)
         EMIS_DISPATCH_4 = TRANS_DISPATCH_EMIS_ADDER(4,ISEAS, &
            globecom_year,UN)
         EMIS_DISPATCH_5 = TRANS_DISPATCH_EMIS_ADDER(5,ISEAS, &
            globecom_year,UN)
!

               IF(USE_SECONDARY_FUEL(UN) .OR. &
                   ((SOUTHERN_COMPANY .OR. SECOND_FUEL_EMIS_DISPATCH) &
                   .AND. FUELMX(UN) < 0.) ) THEN
                  F_MIX = 0.
                  S_MIX = 1.
               ELSE
                  F_MIX = ABS(FUELMX(UN))
                  S_MIX = 1. - F_MIX
               ENDIF
               IF(BLKNO(NB) == 2) THEN
                  DISP_ADJUSTER = DISPADJ2(UN)
                  NOX_RATE = F_MIX * PRIM_NOX_BK2(UN) + &
                      S_MIX * SEC_NOX_BK2(UN)
               ELSE
                  DISP_ADJUSTER = DISPADJ(UN)
                  NOX_RATE = F_MIX * NOX(UN) + S_MIX * SEC_NOX_BK1(UN)
               ENDIF
!
               DISPATCHING_BTU_COST = DISP_BTU_COST(UN)
!
               TEMP_DISPATCH_MULT = DISPATCH_MULT(UN)
               IF(TEMP_DISPATCH_MULT < 0.) THEN ! 030506. ADDED MONTH.
                  get_var_called_from="picadisp:936"
                  TEMP_DISPATCH_MULT = GET_VAR(TEMP_DISPATCH_MULT, &
                      globecom_year,UNITNM(UN))
                  IF(TEMP_DISPATCH_MULT < 0.) THEN
                     TEMP_DISPATCH_MULT = GET_VAR(TEMP_DISPATCH_MULT, &
                         ISEAS,UNITNM(UN))
                  ENDIF
               ENDIF
!
               SAVE_HEATRATE(NB) = INHEAT(NB) * HRF(UN)
!
               RTEMP = TEMP_DISPATCH_MULT * (SAVE_HEATRATE(NB) * &
                   (DISPATCHING_BTU_COST + EMIS_DISPATCH_1 * &
                   (F_MIX * SO2(UN) + &
               S_MIX * SEC_SO2(UN)) + EMIS_DISPATCH_2 * NOX_RATE + &
               EMIS_DISPATCH_3 *(F_MIX*PARTICULATES(UN)+ &
               S_MIX*SEC_CO2(UN)) + EMIS_DISPATCH_4 *(F_MIX * &
               EMIS_OTH2(UN) + S_MIX * SEC_OTH2(UN)) + &
               EMIS_DISPATCH_5 *(F_MIX * EMIS_OTH3(UN) + &
               S_MIX * SEC_OTH3(UN)))*COST_CONVERSION + &
               10.*(VCPMWH(UN) + DISP_ADJUSTER) )
!
               CALL GET_TOTAL_S_DELIVERY_COST(TOTAL_S_DELIVERY_COST,UN)
!
               STEMP = TEMP_DISPATCH_MULT * (SAVE_HEATRATE(NB) * &  
                   (SBTUCT(UN) + TOTAL_S_DELIVERY_COST + &
                   EMIS_DISPATCH_1 * S_MIX * SEC_SO2(UN) + &
                   EMIS_DISPATCH_2 * S_MIX * SEC_NOX_BK2(UN) + &
                   EMIS_DISPATCH_3 * S_MIX*SEC_CO2(UN) + &
                   EMIS_DISPATCH_4 * S_MIX * SEC_OTH2(UN) + &
                   EMIS_DISPATCH_5 * S_MIX * SEC_OTH3(UN))* &
                   COST_CONVERSION + &
                   10.*(VCPMWH(UN) + DISP_ADJUSTER) )
               IF(PRODUCTION_METHOD== 4) RTEMP = RTEMP + 10.*FUELADJ(UN)

! THE SHADOW_DISPATCH_ADDER IS USED TO ENSURE THAT THE SHADOW UNIT 
! FOLLOWS
!  THE MAIN UNIT IN THE DISPATCH ORDER

               SHADOW_UN = SHADOW_UNIT_NUMBER(UN)
               IF(SHADOW_UN > 0 .AND. UN > SHADOW_UN) THEN
             RTEMP1 = BLOCK_DISP_COST(SHADOW_UN,MAX(BLKNO(NB),int(1,2)))
                  IF(RTEMP1 > RTEMP) THEN
                     SHADOW_DISPATCH_ADDER(NB) = (RTEMP1 - RTEMP)/10.
                     RTEMP = RTEMP1
                  ENDIF
               ENDIF
               BLOCK_DISP_COST(UN,MAX(BLKNO(NB),int(1,2))) = RTEMP
               SECOND_DISP_COST(UN,MAX(BLKNO(NB),int(1,2))) = STEMP
!
      ENDDO
      CALL RW_PROCESS_MESSAGES()
!
! TRYING TO INPROVE THE SORT TIME OF THE DISPATCH ROUTINE
!
      TEMP_R4 = 0.
      LOCAL_YEAR = FLOAT(globecom_year+get_BASE_YEAR())
      SCREEN_MESSAGES = TRIM(LOADS_MONTH_NAME(ISEAS))//"-CapEx Thermal"
      CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      CALL DISPLAY_TIME
         IF(CAPEX_THERMAL_RPT_NOT_OPEN) THEN
            CAPEX_THERMAL_NO = &
                CAPEX_THERMAL_RPT_HEADER(INT(120,2),CAPEX_THERMAL_REC)
            CAPEX_THERMAL_RPT_NOT_OPEN = .FALSE.
         ENDIF
         call write_trace_int2(file_trace_sca,& 
      "iterating on NBLOK", NBLOK)
          DO I = 1, NBLOK
            IF(BLKNO(I) > 1)  then
                call write_trace_message(file_trace_sca, "1. CYCLE")
                CYCLE
            end if
            UN = UNIT(I)
            call write_trace_int2(file_trace_sca, "10. UN", UN)
            IF(.not.(ONLINE(UN)<=DATE2 .AND. OFLINE(UN)>=DATE1)) CYCLE
            T_GROUP = get_TRANSACTION_GROUP(UN)
           TG_POSITION = MAX(int(1,2),GET_TRANS_GROUP_POSITION(T_GROUP))
            DISP_COST = BLOCK_DISP_COST(UN,1)/10.
            RTEMP = SAVE_HEATRATE(I)/HEAT_CONVERSION

            EMIS_DISPATCH_1 = TRANS_DISPATCH_EMIS_ADDER(1,ISEAS, & 
                                globecom_year,UN)
            EMIS_DISPATCH_2 = TRANS_DISPATCH_EMIS_ADDER(2,ISEAS, & 
         globecom_year,UN)
            EMIS_DISPATCH_3 = TRANS_DISPATCH_EMIS_ADDER(3,ISEAS, &
         globecom_year,UN)
            EMIS_DISPATCH_4 = TRANS_DISPATCH_EMIS_ADDER(4,ISEAS, & 
         globecom_year,UN)
            EMIS_DISPATCH_5 = TRANS_DISPATCH_EMIS_ADDER(5,ISEAS, & 
         globecom_year,UN)

             IF(USE_SECONDARY_FUEL(UN) .OR. &
                 ((SOUTHERN_COMPANY .OR. SECOND_FUEL_EMIS_DISPATCH) &
                 .AND. FUELMX(UN) < 0.) ) THEN
                F_MIX = 0.
                S_MIX = 1.
             ELSE
                F_MIX = ABS(FUELMX(UN))
                S_MIX = 1. - F_MIX
             ENDIF
            IF(BLKNO(I) == 2) THEN
               DISP_ADJUSTER = DISPADJ2(UN)
               NOX_RATE = F_MIX * PRIM_NOX_BK2(UN) + &
                   S_MIX * SEC_NOX_BK2(UN)
            ELSE
               DISP_ADJUSTER = DISPADJ(UN)
               NOX_RATE = F_MIX * NOX(UN) + S_MIX * SEC_NOX_BK1(UN)
            ENDIF
            SOX_RATE = F_MIX * SO2(UN) + S_MIX * SEC_SO2(UN)
            IF(SHADOW_DISPATCH_ADDER(I) /= 0.) &
                DISP_ADJUSTER = SHADOW_DISPATCH_ADDER(I)
            DISPATCHING_BTU_COST = DISP_BTU_COST(UN)
!
            TEMP_DISPATCH_MULT = DISPATCH_MULT(UN)
               IF(TEMP_DISPATCH_MULT < 0.) THEN 
                  get_var_called_from="picadisp:1045"
                  TEMP_DISPATCH_MULT = &
                      GET_VAR(TEMP_DISPATCH_MULT,globecom_year,UNITNM(UN))
                  IF(TEMP_DISPATCH_MULT < 0.) THEN
                  get_var_called_from="picadisp:1049"
                     TEMP_DISPATCH_MULT = GET_VAR(TEMP_DISPATCH_MULT, &
                        ISEAS,UNITNM(UN))
                  ENDIF
               ENDIF
!
! 10/19/01
!
            TEMP_L1 = GET_CL_BASECASE_MARKET_ID(UN,MARKET_ID)
            MARKET_AREA = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
!
            prim_mover = GET_PRIMARY_MOVER(UN)
            call write_trace_real(file_trace_sca, "prim_mover", &
                prim_mover)

!
            SPCapExResourceOption = MIN(int(1.,4), &
                INDEX(SP_NEWGEN_UNIT_STATUS(UN),"SPCapEx"))
            IF(BLKNO(I) == 0 .OR. SPCapExResourceOption == 1.) THEN
               MIN_CAP = MW(1,UN)
               MAX_CAP = MW(2,UN)
            ELSE
               MIN_CAP = 0.
               MAX_CAP = MW(2,UN)
            ENDIF
            V_LOG=GET_CL_BASECASE_MARKET_ID(UN,BASECASE_MARKET_AREA_ID)
            call write_trace_message(file_trace_sca, "sp_capex_output " // &
                "calling  GET_HESI_UNIT_ID_NUM with: ")
            call write_trace_int2(file_trace_sca, "UN", UN)
            call write_trace_real4(file_trace_sca, &
                "HESI_SECOND_UNIT_ID_NUM", HESI_SECOND_UNIT_ID_NUM)
                
            if ( int(prim_mover)==1 .and. &
                int(hesi_second_unit_id_num)==1 .and. UN==407 &
                .and. sp_capex_output_count==205 &
            ) then
!                stop "sequence error picadisp:0004."
            end if
            
            HESI_UNIT_ID_NUM_ord = &
               GET_HESI_UNIT_ID_NUM(UN,HESI_SECOND_UNIT_ID_NUM)
               
            call write_trace_real4(file_trace_sca, "HESI_UNIT_ID_NUM", &
                HESI_UNIT_ID_NUM_ord)
                
            POWERDAT_PLANT_ID = GET_POWERDAT_PLANT_ID(UN)
            SP_BASECASE_MARKET_AREA_ID = BASECASE_MARKET_AREA_ID
!
            OnLineYr = ONLINE(UN)/100.
            OnLineMo = ONLINE(UN) - 100.*OnLineYr
            IF(OnLineYr >= 100) THEN
               OnLineYr = 2000 + OnLineYr - 100
            ELSE
               OnLineYr = 1900 + OnLineYr
            ENDIF
            OffLineYr = OFLINE(UN)/100.
            OffLineMo = OFLINE(UN)-100.*OffLineYr
            IF(OffLineYr >= 100) THEN
               OffLineYr = 2000 + OffLineYr - 100
            ELSE
               OffLineYr = 1900 + OffLineYr
            ENDIF
            UNIT_START_UP_COSTS = GET_UNIT_START_UP_COSTS(globecom_year,UN,ISEAS)
!
            V_LOG = GET_BASECASE_MARKET_AREA_ID(EV_TRANS_AREA_NAME,TG_POSITION)
            V_LOG = GET_CUBIC_HEAT_CURVE(UN,A_CO,B_CO,C_CO,D_CO)
!
            MIN_UPTIME = GET_MIN_UP_TIME(UN)
            MIN_DOWNTIME = GET_MIN_DOWN_TIME(UN)
!
            IF(BLKNO(I) == 0 .OR. SPCapExResourceOption == 1.) THEN

               SAC_UNIT_NAME = UNITNM(UN)
               SAC_STATION_GROUP = 'NEW.'
            ELSE
               SAC_UNIT_NAME = UNITNM(UN)
               SAC_STATION_GROUP = 'EXISTING.UNIT'
            ENDIF
            IF(PASS_TO_ENERPRISE) THEN
               LOCAL_FUEL = GET_PBTUCT_SAVE(UN)
               LOCAL_DISPATCH_ADDER = GET_DISPADJ_SAVE(UN)
            ELSE
               LOCAL_FUEL = RTEMP * DISPATCHING_BTU_COST
               LOCAL_DISPATCH_ADDER = DISP_ADJUSTER
            ENDIF
!
! 080306
!
            TEMP_P_FUEL_DELIVERY = GET_P_FUEL_DELIVERY(UN)
            TEMP_P_FUEL_DELIVERY_2 = GET_P_FUEL_DELIVERY_2(UN)
            TEMP_P_FUEL_DELIVERY_3 = GET_P_FUEL_DELIVERY_3(UN)

! TRANSACTION GROUP REPORT
! 050108. THIS IS USED TO DRIVER HORIZONS INTERACTIVE SUPPLY CURVE.

            IF(NEW_SUPPLY_CURVE_REPORT()) THEN
               IF(REPORT_NEW_SUPPLY_PERIOD(ISEAS)) THEN
                  IF(NEW_SUPPLY_CURVE_NOT_OPEN) THEN

                     NEW_SUPPLY_CURVE_NO = &
                         NEW_SUPPLY_CURVE_HEADER(INT(17,2), &
                         NEW_SUPPLY_CURVE_REC)
                     NEW_SUPPLY_CURVE_NOT_OPEN = .FALSE.
                  ENDIF
!
                  RPM = FLOAT(UZ_PRIMARY_MOVER(UN))
                  T_GROUP = get_TRANSACTION_GROUP(UN)
                  TRANS_NAME = GET_TRANSACTION_GROUP_NAME(T_GROUP)// &
                  " "//LEFT_JUSTIFY_I2_IN_STR(T_GROUP)
                  ng_unit_status = FLOAT(GET_NEWGEN_INDEX(UN))
                  YEAR_MONTH = FLOAT(OnLineYr*100 + OnLineMo)
                  NEW_SUPPLY_VARIABLES = 0.0
                  NEW_SUPPLY_CURVE_REC = RPTREC(NEW_SUPPLY_CURVE_NO)
                  timeshere=timeshere+1
                  ep=PRT_ENDPOINT()
                     WRITE(NEW_SUPPLY_CURVE_NO,REC=NEW_SUPPLY_CURVE_REC) &
                         ep,LOCAL_YEAR,REPORT_PERIOD,TRANS_NAME, &
                        UNITNM(UN),HESI_UNIT_ID_NUM_ord,MAX_CAP, &
                        SAVE_HEATRATE(I),SAVE_HEATRATE(I)* &
                        DISPATCHING_BTU_COST * 0.001, &
                        VCPMWH(UN),SAVE_HEATRATE(I) * .001 * &
                        SOX_RATE*EMIS_DISPATCH_1, &
                        SAVE_HEATRATE(I) * .001 * NOX_RATE* &
                        EMIS_DISPATCH_2,SAVE_HEATRATE(I) * .001 * &
                        EMIS_DISPATCH_3 * &
                        (F_MIX*PARTICULATES(UN) + S_MIX*SEC_CO2(UN)), &
                        SAVE_HEATRATE(I) * .001 * EMIS_DISPATCH_4 * &
                        (F_MIX * EMIS_OTH2(UN) + S_MIX * SEC_OTH2(UN)), &
                        ng_unit_status,YEAR_MONTH,RPM,prim_mover, &
                        DISPATCH_MULT(UN),100.*MAINTENANCE_RATE(UN), & 
                        100.*(1.-EAVAIL(UN)), & 
                        HESI_SECOND_UNIT_ID_NUM 
                  NEW_SUPPLY_CURVE_REC = NEW_SUPPLY_CURVE_REC + 1
               ENDIF
!
            ELSE
                 WRITE(CAPEX_THERMAL_NO,REC=CAPEX_THERMAL_REC) &
                 PRT_ENDPOINT(),LOCAL_YEAR,FLOAT(ISEAS),SP_UNIT_NAME(UN), & 
                      FLOAT(T_GROUP),SAVE_HEATRATE(I), &
                      DISP_COST,LOCAL_FUEL,VCPMWH(UN),LOCAL_DISPATCH_ADDER, &
                      RTEMP * SOX_RATE * EMIS_DISPATCH_1,RTEMP * &
                      NOX_RATE * EMIS_DISPATCH_2, &
                      RTEMP * (EMIS_DISPATCH_3 *(F_MIX*PARTICULATES(UN) + &
                      S_MIX*SEC_CO2(UN)) + EMIS_DISPATCH_4 * &
                      (F_MIX * EMIS_OTH2(UN) + S_MIX * SEC_OTH2(UN)) + &
                      EMIS_DISPATCH_5 *(F_MIX * EMIS_OTH3(UN) &
                      + S_MIX * SEC_OTH3(UN))), &
                      DISPATCH_MULT(UN),SOX_RATE, & 
                      2000.*EMIS_DISPATCH_1,NOX_RATE,2000.* &
                      EMIS_DISPATCH_2,DISPATCHING_BTU_COST,100.* &
                      ! 15 DERATED MAINTENANCE CAP
                      MAINTENANCE_RATE(UN), &  

                      100.*(1.-EAVAIL(UN)),MARKET_AREA, &
                      prim_mover,FLOAT(OnLineYr), &
                      FLOAT(OffLineYr),FLOAT(BLKNO(I)), &
                      MIN_CAP,MAX_CAP,FIXED_COST(UN), &  ! 24 $/KW-YEAR
! COSTS
                      RTEMP * EMIS_DISPATCH_3 *(F_MIX*PARTICULATES(UN) &
                          + S_MIX*SEC_CO2(UN)), &  ! 25
                      RTEMP * EMIS_DISPATCH_4 *(F_MIX * EMIS_OTH2(UN) + &
                          S_MIX * SEC_OTH2(UN)), &
                      RTEMP * EMIS_DISPATCH_5 *(F_MIX * EMIS_OTH3(UN) + &
                       S_MIX * SEC_OTH3(UN)), &
! RATES
                      F_MIX * PARTICULATES(UN) + S_MIX * &
                      SEC_CO2(UN),F_MIX * EMIS_OTH2(UN) + S_MIX * &
                      SEC_OTH2(UN), &
                      F_MIX * EMIS_OTH3(UN) + S_MIX * SEC_OTH3(UN), & 
! EMISSION MARKET COST
                      2000. * EMIS_DISPATCH_3, &
                      2000. * EMIS_DISPATCH_4,2000. &
                      * EMIS_DISPATCH_5,foshyd%CAP_PLANNING_FAC(UN), &
                      HESI_UNIT_ID_NUM_ord, &
                          HESI_SECOND_UNIT_ID_NUM,POWERDAT_PLANT_ID, &
                          FLOAT(CL_RESOURCE_ID(UN)), &
                      FLOAT(DAY_TYPE_FOR_RESOURCE(UN)), &
                      FLOAT(OnLineMo),FLOAT(OffLineMo), &
                      SPCapExResourceOption, &   ! 41 42
                      DISPATCHING_BTU_COST,BLENDED_BTU_COST(UN),RTEMP &
                          * BLENDED_BTU_COST(UN), & 
                      FIRST_YEAR_DECOM_AVAIL_capex(UN), &
                          DECOM_CONTINUING_COST(UN), &
                          DECOM_CONT_COST_ESCALATION(UN), &
                      ANNUAL_ENERGY_PLANNING_FACTOR_capex(UN), &
                      NAME_PLATE_CAPACITY_capex(UN),MW(1,UN),MW(2,UN), &
                      A_CO,B_CO,C_CO,D_CO, &   
! 49 ! 50 !55
                      UNIT_START_UP_COSTS,MIN_UPTIME,MIN_DOWNTIME, &
                      FuelRatio_capex(1,UN),FuelRatio_capex(2,UN), &
                      FuelRatio_capex(3,UN), &
                      FuelRatio_capex(4,UN), &
! 164
                      FuelRatio_capex(5,UN),BlendableFuelsPtr(1,UN), &
                      BlendableFuelsPtr(2,UN),BlendableFuelsPtr(3,UN), & 
                      BlendableFuelsPtr(4,UN), &
                      FuelTransportationCost(1,UN), &
                      FuelTransportationCost(2,UN),  & ! 172
                      FuelTransportationCost(3,UN), &
                      FuelTransportationCost(4,UN), &
                      BlendedEnthalpyUp(UN),BlendedEnthalpyLo(UN), &
                      BlendedSO2Up(UN),BettermentProjectID(UN), &
                      TEMP_R4, & ! DECOM_CONTINUING_COST(UN),
                      FLOAT(GET_EMISSION_MARKET_LINK(UN)), &
                      EnrgPatternPointer_capex(UN),EmissRedRate(1,UN), &
                      EmissRedRate(2,UN),EmissRedRate(3,UN), &
                      EmissRedRate(4,UN), &    ! 186
                      EmissRedRate(5,UN),EmissMaxRate(1,UN), &
                      EmissMaxRate(2,UN),EmissMaxRate(3,UN), &
                      EmissMaxRate(4,UN), &
                      EmissMaxRate(5,UN),MaxEnergyLimit(1,UN), &
                      MaxEnergyLimit(2,UN),MaxEnergyLimit(3,UN), &
                      TEMP_P_FUEL_DELIVERY, &
                      TEMP_P_FUEL_DELIVERY_2, &
                      TEMP_P_FUEL_DELIVERY_3, &
                      EMISSION_DATA_UNITS(UN),  &   ! LEN = 1
                      AGGREGATE_THIS_UNIT(UN),  &   ! LEN = 1
                      SP_BASECASE_MARKET_AREA_ID,  & ! LEN = 8
                      GET_GROUP_NAME(TG_POSITION), & ! LEN = 35
                      SAC_UNIT_NAME, &               ! LEN = 16
                      EV_TRANS_AREA_NAME, &          ! LEN = 6
                      SAC_STATION_GROUP, &           ! LEN = 16
                      FUEL_BLENDING_IS(UN), & 
                      TECH_TYPE(UN), &               ! LEN = 20
                      LINKED_BETTERMENT_OPTION(UN)   ! LEN = 1
! RECORD LENGTH = 64 + 4 X 92 + 99 = 531
!
! END WRITE FOR CAPEX
               CAPEX_THERMAL_REC = CAPEX_THERMAL_REC + 1
            ENDIF
         ENDDO
         DEALLOCATE(BLOCK_DISP_COST, &
             SECOND_DISP_COST,&
             SHADOW_DISPATCH_ADDER)
      RETURN

      ENTRY GET_BLOCK_DISP_COST(R_UNIT,R_BLKNO,R_BLOCK_DISP_COST,R_NBLOK2)


         DO NB = 1, R_NBLOK2
            UN = R_UNIT(NB)
            ECBLK = MAX(int(1,2),R_BLKNO(NB))
            R_BLOCK_DISP_COST(UN,ECBLK) = BLOCK_DISP_COST(UN,ECBLK)
         ENDDO
!
      RETURN
!

      ENTRY GET_DISP_COST_FOR_A_UNIT(R_SINGLE_UNIT,R_1ST_DISP_COST, &
          R_2ND_DISP_COST)

         R_1ST_DISP_COST = BLOCK_DISP_COST(R_SINGLE_UNIT,1)/10.
         R_2ND_DISP_COST = BLOCK_DISP_COST(R_SINGLE_UNIT,2)/10.
         ! TODO: Remove code in IF block
         IF(.FALSE. ) then
         RFT = GET_PRIMARY_MOVER(R_SINGLE_UNIT)
         IF(RFT == 2) THEN
             R_1ST_DISP_COST = (500 * FuelDispatchCostMultiplier(UN,&
                 MAX(int(1,2),BLKNO(NB)))) &
                + NonFuelDispatchCost(UN,MAX(int(1,2),BLKNO(NB)))
             R_1ST_DISP_COST = R_1ST_DISP_COST/10.
         ENDIF
         Endif
!
      RETURN

      ENTRY GET_DISP_PARAM_FOR_A_UNIT(R_SINGLE_UNIT, &
      R_DISP_BTU_COST, &
      R_1ST_FUEL, &
      R_1ST_NON_FUEL, &
      R_2ND_FUEL,&
      R_2ND_NON_FUEL)

         RFT = GET_PRIMARY_MOVER(R_SINGLE_UNIT)

             R_DISP_BTU_COST = DISP_BTU_COST(R_SINGLE_UNIT)
             R_1ST_FUEL = FuelDispatchCostMultiplier(R_SINGLE_UNIT,1)*0.1
             R_1ST_NON_FUEL = NonFuelDispatchCost(R_SINGLE_UNIT,1)*0.1
             R_2ND_FUEL = FuelDispatchCostMultiplier(R_SINGLE_UNIT,2)*0.1
             R_2ND_NON_FUEL = NonFuelDispatchCost(R_SINGLE_UNIT,2)*0.1

      RETURN

      ENTRY GET_SECOND_COST_FOR_A_UNIT(R_SINGLE_UNIT, &
          R_1ST_DISP_COST,R_2ND_DISP_COST)

         R_1ST_DISP_COST = SECOND_DISP_COST(R_SINGLE_UNIT,1)/10.
         R_2ND_DISP_COST = SECOND_DISP_COST(R_SINGLE_UNIT,2)/10.
!
      RETURN

      ENTRY GET_ADJ_TOTAL_CAP(R_ADJ_TOTAL_CAP)

         R_ADJ_TOTAL_CAP = ADJ_TOTAL_CAP
      RETURN
!

      ENTRY TELL_DISPATCH_SOUTHERN_STATUS

         SOUTHERN_COMPANY = .TRUE.
      RETURN
!

      ENTRY DEALLOCATE_DISPAT_ARRAYS

         IF(ALLOCATED(BLOCK_DISP_COST)) then
             DEALLOCATE(BLOCK_DISP_COST,SECOND_DISP_COST)
         end if
      RETURN

      ENTRY GET_MAX_TRANS_BLOCKS_USED(R_MAX_TRANS_BLOCK)

         R_MAX_TRANS_BLOCK= MAX_BLOCKS_PER_TRANS(0)
      RETURN

      ENTRY GET_MAX_BLOCKS_PER_TRANS(R_TG,R_MAX_TRANS_BLOCK)

         R_MAX_TRANS_BLOCK= MAX_BLOCKS_PER_TRANS(R_TG)
      RETURN

      ENTRY GET_INHEAT_AND_FUEL(R_HEAT_RATE,R_BLOCK)

         R_HEAT_RATE = SAVE_HEATRATE(R_BLOCK)
      RETURN
!
! 7/5/02.
!

      ENTRY GET_MONTHLY_DISP_COST_BY_UNIT( &
          R_UN,R_LOCAL_COEFF,R_DISPATCH) ! 2x12

      if(file_trace_gmdcu==0) then
         file_trace_gmdcu=open_trace( &
            "GET_MONTHLY_DISP_COST_BY_UNIT.trace", rq_gmdcu)
         call write_trace_message(file_trace_gmdcu, &
            "Tracing GET_MONTHLY_DISP_COST_BY_UNIT")
      end if
      call write_trace_message(file_trace_gmdcu, "Arguments:")
      call write_trace_int2(file_trace_gmdcu, "R_UN", R_UN)
      call write_trace_real4s(file_trace_gmdcu, "LOCAL_COEFFS", &
        R_LOCAL_COEFF)

        
      UN = R_UN
      EXISTING_UNIT = .TRUE.
      BASE_DATE = (globecom_year + get_BASE_YEAR() - 1900) * 100
!
      RFT = GET_PRIMARY_MOVER(UN)
      IF(RFT > 3 .OR. RFT < 1) RFT = 1
!
      DO J = 1, 12
         if(un<=1) then
            call end_program("picadisp:0001 - " // &
             "UN is invalid before calling get_mrx_delivery_cost.")
         end if         
         
         CALL GET_MRX_DELIVERY_COST(UN, &
             TOTAL_DELIVERY_COST,J,globecom_year, &
             FUEL_POINTERS_USED,EXISTING_UNIT)
!
         FUEL_SCEN_MULT = 1.0
         IF(RFT == 1) THEN
            FUEL_SCEN_MULT =  GET_SCENARIO_COAL_PRICES(globecom_year,J)
         ELSEIF(RFT == 2) THEN
            FUEL_SCEN_MULT =  GET_SCENARIO_GAS_PRICES(globecom_year,J)
         ELSEIF(RFT == 3) THEN
            FUEL_SCEN_MULT =  GET_SCENARIO_OIL_PRICES(globecom_year,J)
         ELSEIF(RFT == 4) THEN
        FUEL_SCEN_MULT =  GET_SCENARIO_URANIUM_PRICES(globecom_year,J)
         ENDIF
!
         DISPATCHING_BTU_COST = TOTAL_DELIVERY_COST * FUEL_SCEN_MULT
!
         LOCAL_DATE = BASE_DATE + J
!
         DO I = 1, 2

            EMIS_DISPATCH_1 = &
             TRANS_DISPATCH_EMIS_ADDER(1,J,globecom_year,UN)
            EMIS_DISPATCH_2 = &
              TRANS_DISPATCH_EMIS_ADDER(2,J,globecom_year,UN)
            EMIS_DISPATCH_3 = &
               TRANS_DISPATCH_EMIS_ADDER(3,J,globecom_year,UN)
            EMIS_DISPATCH_4 = &
             TRANS_DISPATCH_EMIS_ADDER(4,J,globecom_year,UN)
            EMIS_DISPATCH_5 = &
             TRANS_DISPATCH_EMIS_ADDER(5,J,globecom_year,UN)

            IF(ONLINE(UN) <= LOCAL_DATE .AND. OFLINE(UN) >= LOCAL_DATE) THEN
               IF(GET_MAINTENANCE_RATE_FOR_MONTH(UN,J) /= 1. .AND.  &
               EAVAIL(UN) /= 0.) THEN

                  IF(USE_SECONDARY_FUEL(UN) .OR. &
                      ((SOUTHERN_COMPANY .OR. SECOND_FUEL_EMIS_DISPATCH) &
                      .AND. FUELMX(UN) < 0.) ) THEN
                     F_MIX = 0.
                     S_MIX = 1.
                  ELSE
                     F_MIX = ABS(FUELMX(UN))
                     S_MIX = 1. - F_MIX
                  ENDIF

                  TEMP_R4 = GET_DISPADJ_SAVE(UN)
                  IF(TEMP_R4 < 0.0) THEN
                     TEMP_R4 = GET_VAR(RTEMP,J,UNITNM(UN))
                     IF(TEMP_R4 < 0.0) THEN
                        DISP_ADJUSTER = GET_VAR(RTEMP,J,UNITNM(UN))
                     ELSE
                        DISP_ADJUSTER = TEMP_R4
                     ENDIF
                  ELSE
                     DISP_ADJUSTER = TEMP_R4
                  ENDIF
                  NOX_RATE = F_MIX * NOX(UN) + S_MIX * SEC_NOX_BK1(UN)

!
                  TEMP_DISPATCH_MULT = DISPATCH_MULT(UN)
                  IF(TEMP_DISPATCH_MULT < 0.) THEN ! 030506. ADDED MONTH.
                     TEMP_DISPATCH_MULT = &
                         GET_VAR(TEMP_DISPATCH_MULT,globecom_year,UNITNM(UN))
                     IF(TEMP_DISPATCH_MULT < 0.) THEN
                        TEMP_DISPATCH_MULT = &
                            GET_VAR(TEMP_DISPATCH_MULT,J,UNITNM(UN))
                     ENDIF
                  ENDIF
!
!

!
                   TEMP_R = R_LOCAL_COEFF(I+1)*GET_HEAT_RATE_FACTOR(UN)
!
! 10/05/02. $/MWH Units.
!
                  R_DISPATCH(I,J) = (TEMP_DISPATCH_MULT * &
                  (TEMP_R * & ! 07/03/03. FOR IPL
                       (DISPATCHING_BTU_COST + &
                        EMIS_DISPATCH_1 * (F_MIX * SO2(UN) + &
                        S_MIX * SEC_SO2(UN)) + EMIS_DISPATCH_2 * NOX_RATE + &
                        EMIS_DISPATCH_3 *(F_MIX*PARTICULATES(UN)+ &
                        S_MIX*SEC_CO2(UN)) + EMIS_DISPATCH_4 *(F_MIX * &
                        EMIS_OTH2(UN) + &
                        S_MIX * SEC_OTH2(UN)) + EMIS_DISPATCH_5 * &
                        (F_MIX * EMIS_OTH3(UN) + S_MIX * SEC_OTH3(UN)))* &
                        COST_CONVERSION)/10. + (VCPMWH(UN) + DISP_ADJUSTER) )

               ENDIF
            ENDIF
         ENDDO
      ENDDO
      CALL RW_PROCESS_MESSAGES()
      RETURN
      END

      SUBROUTINE DISPATCH_SORT(NBLOK,LAMDA,BLKNO,&
      INHEAT,MWBLOK,UNIT,SAVE_HEATRATE,SHADOW_DISPATCH_ADDER)

!
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines

      INTEGER (KIND=2) :: NBLOK,L,M,I,N,ITEMP
      INTEGER (KIND=2) :: LAMDA(NBLOK),BLKNO(NBLOK),UNIT(NBLOK)
      REAL (KIND=4) :: SAVE_HEATRATE(NBLOK),SHADOW_DISPATCH_ADDER(NBLOK), &
      MWBLOK(NBLOK),INHEAT(NBLOK),RTEMP
      LOGICAL (KIND=4) :: REASON1,REASON2,REASON3,REASON4,REASON5
!

!     THIS SECTION OF THE PROGRAM CALCULATES THE LOADING ORDER OF
!       THE DISPATCHING BLOCKS.  THIS SECTION IS PATTERNED AFTER THE
!     SORT IN THE CREORD SUBROUTINE IN PCS

! TO CHANGE THE PROMOD PROBLEM CAUSED BY INCREMENTAL HEAT RATES GOING
! LOWER (THE FIT TO I/O DATA PRODUCES NEGATIVE COEFFICIENTS WHICH LEAD 
! TO
! DECLINING INCREMENTAIL HEAT RATES), THE UNITS SHOULD BE SHORTED FIRST 
! BY
! BLOCK NUMBER THEN BY INCREMENT COST.
! I DON'T THINK THIS SHOULD BE DONE.  10/4/88
! AFTER SEEING COMMONWEALTH EDISON'S DISPATCHING ORDER I CHANGED MY MIND
! AND IMPLEMENTE A PRE SORT ON BLOCK NUMBER
!
         L = NBLOK - 1
         IF(L >= 1) THEN
            DO I=1,L
               M=I
               N=I+1
   20          REASON4 = .FALSE.

               IF(M > 0) THEN
!
! UNIT TO BE FLIPPED IS A MUST RUN
!
                  REASON1 = BLKNO(M) /= 0 .AND. BLKNO(N) == 0 .AND. &
                      LAMDA(M) /= 32000 .AND. LAMDA(N) /= 32000
!
! BOTH UNITS ARE MUST RUNS AND THE LAMDA IS LOWER
!
                  REASON2 = BLKNO(M) == 0 .AND. BLKNO(N) == 0 .AND. &
                      LAMDA(M) > LAMDA(N)
!
! DON'T FLIP BLOCK 1 AND BLOCK 2 OF THE SAME UNIT
!
                  REASON3 = BLKNO(M) /= 0 .AND. BLKNO(N) /= 0 .AND. &
                      LAMDA(M) > LAMDA(N) .AND. UNIT(M) /= UNIT(N)
!
! THE PROMOD REASON FOR FLIPPING A DECREASING INCREMENTAL HEAT RATE
! CHECK TO SEE IF BLOCK 2  HAS STOPPED BECAUSE IT HIT ITS BLOCK 1
!
                  IF(M > 1) REASON4 = LAMDA(N) < LAMDA(M-1) .AND. &
                      UNIT(M) == UNIT(M-1)
!
! REASON 5 IS MOVE ALL UNITS WITH LAMDAS GE 32000 TO THE BOTTOM OF THE
! STACK
!
                  REASON5 = LAMDA(M) > LAMDA(N) .AND. LAMDA(M) > 31999
!
                  IF(REASON1 .OR. REASON2 .OR. REASON3 .OR. REASON4 .OR. &
                    REASON5) THEN
!        THEN INTERCHANGES THE ARRAY POSITIONS
                     ITEMP=LAMDA(M)
                     LAMDA(M)=LAMDA(N)
                     LAMDA(N)=ITEMP
                     RTEMP=MWBLOK(M)
                     MWBLOK(M)=MWBLOK(N)
                     MWBLOK(N)=RTEMP
                     ITEMP=UNIT(M)
                     UNIT(M)=UNIT(N)
                     UNIT(N)=ITEMP
!
                     RTEMP=INHEAT(M)
                     INHEAT(M)=INHEAT(N)
                     INHEAT(N)=RTEMP
!
                     RTEMP=SAVE_HEATRATE(M)
                     SAVE_HEATRATE(M)=SAVE_HEATRATE(N)
                     SAVE_HEATRATE(N)=RTEMP
!
                     ITEMP = BLKNO(M)
                     BLKNO(M) = BLKNO(N)
                     BLKNO(N) = ITEMP
                     RTEMP = SHADOW_DISPATCH_ADDER(M)
                     SHADOW_DISPATCH_ADDER(M) = SHADOW_DISPATCH_ADDER(N)
                     SHADOW_DISPATCH_ADDER(N) = RTEMP
                     N = M
                     M = M - 1
                     GOTO 20
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      RETURN
      END SUBROUTINE

!
      FUNCTION UZ_PRIMARY_MOVER(R_UNIT_NO)
        use params
        use prim_mover_idx
        use program_state


      INTEGER (kind=2) ::  UZ_PRIMARY_MOVER
      INTEGER (kind=2) ::  R_UNIT_NO, &
      GET_PRIMARY_MOVER_INDEX,PM,FT, &
      MAX_FUEL_TYPES=6 ,MAX_PROD_TYPES=18 , &
      GET_PRIMARY_MOVER ! External
! END DATA DECLARATIONS
         GET_PRIMARY_MOVER_INDEX_called_from="picadisp:0005"
         PM = GET_PRIMARY_MOVER_INDEX(R_UNIT_NO)
         IF(PM == pm_steam_TPR) THEN
            FT = MIN(GET_PRIMARY_MOVER(R_UNIT_NO),MAX_FUEL_TYPES)
            UZ_PRIMARY_MOVER = MIN(int(PM + FT + 4,2),MAX_PROD_TYPES)
         ELSE
            UZ_PRIMARY_MOVER = PM
         ENDIF
      RETURN
      END
