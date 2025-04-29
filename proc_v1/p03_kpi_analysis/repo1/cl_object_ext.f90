      SUBROUTINE CL_OBJECT
      use end_routine, only: end_program, er_message
      use logging
      use miscmod
      use filemod
      use prim_mover_idx
      use cla_decs
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use p_fuel_annl
      use data_integrity
      use screen_interface
      use dsex_obj_interfaces
      use cl_screen_data_mod
      use spcapexvariables
      use cl_data
      use cl_object_shared
      use offline_trace


      implicit none

      integer (kind=2) :: online_loc, ofline_loc
      CHARACTER (len=30) :: DESC
      real :: EMISS_BLND_RT_ORD
      real :: mw(2)
      real :: DISPADJ2_ord, DISPADJ_ord
      integer :: powerdat_plid
      real  :: dispatch_mult_ord
      INTEGER (kind=2) :: IREC,INUNIT,DELETE,LRECL=1159
      INTEGER (kind=2) :: UNIT_NO,R_ACTIVE_CL_RECORDS
      INTEGER (kind=2) :: MONTHLY_CAP_ptr
      INTEGER (kind=2) :: MONTHLY_FUEL_idx
      integer (kind=2) :: RETROFIT_proj_ID
      INTEGER :: IOS,IOS_BASE
      CHARACTER (len=2) :: CL_LOAD_TYPE
      CHARACTER (len=2) :: UNIT_TYPE_ord
      CHARACTER (len=2) :: UNIT_TYPE_cat

      CHARACTER*5 OVERLAY_FAMILY_NAME,FOSSILFL
      CHARACTER(len=36) :: thrm_GUID
      CHARACTER*48 UNIT_NAME
      CHARACTER(len=30) :: CO2_BASIN_nm





      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      logical (kind=4) :: file_exists

      CHARACTER*2048 :: RECLN
! DECLARATION FOR /FOSSIL FILE/
      LOGICAL*1 PHASE_I
      CHARACTER (LEN=1) :: PHASE_I_STR

      INTEGER (kind=2) :: GENGRP_ord,ONLIMO_ord,OFLIMO_ord
      integer (kind=2) :: ONLIYR,OFLIYR
      integer (kind=2) :: PRESCR,SDESCR,VRESCR,FIXED_COST_ESCALATOR, &
                AI_CAPACITY_ESCALATOR,AI_ENERGY_ESCALATOR

      integer (kind=2) :: AI_REMAINING_LIFE,TIE_GROUP
      integer (kind=2) :: ASSET_CLASS_NUM_ord,ASSET_CLASS_Vctr
      integer (kind=2) :: INTRA_COMPANY_CLsid
      integer (kind=2) :: NOX_SEASON_DATE_ord, &
               NOX_CONTROL_DATE_ord, &
               SOX_CONTROL_DATE_ord, &
               CO2_CONTROL_DATE_ord, &
               HG_CONTROL_DATE_ord, &
               OTHER3_CONTROL_DATE_ord

      integer (kind=2) :: MONTHLY_MUST_RUN_VECTOR_ord, &
               MONTHLY_DECOMMIT_VECTOR_ord

      integer (kind=2) :: EMISSION_MARKET_LINK, &
                FIRST_RETIREMENT_Yr, &
                RPS_PROGRAM_NUMBER, &
                GET_RPS_PROGRAM_POSITION

      character (len=20) :: spl_UNIT_ID

        ! INTRA_COMPANY_xACTION is INTRA_COMPANY_TRANSACTION
            CHARACTER (len=1) :: INTRA_COMPANY_xACTION
            character (len=1) :: UNIT_ACTIVE_ord
            character (len=1) :: REPORT_UNIT_ord
            character (len=1) :: UTILITY_pWNED, &
                  START_LOGIC_ord, &
                  CONTRIBs_TO_SPIN, &
                  APPLY_NOX_SSN_DT_ord, &
                  MrkT_RESOURCE, &
                  USE_POLY_HT_RATES, &
                  WVPA_RT_TRACKER, &
                  WVPA_rs_TRACKER, &
                  WVPA_FL_TRACKER, &
                  WVPA_MEM_TKER_ord, &
                  ALLOW_DECOMMIT_ord, &
                  MONTE_OR_FREQ_ord, &
                  PRIMARY_FUEL_CAT, &
                  SECONDARY_FUEL_CAT, &
                  EMISSIONS_FUEL_CAT, &
                  RETIRMENT_CANDIDATE, &
                  rFIT_CANDIDATE, &
                  THRML_AGGREGATED_UNIT


       CHARACTER (len=6) :: BASECASE_PLNT_ID
       character (len=6) :: BASECASE_UNITid
       character (len=6) :: BASECASE_MARKET_AREAid
       character (len=6) :: BASECASE_TRANS_AREAid
       character (len=6) :: BASECASE_PLANT_NERC_SUBid
       character (len=6) :: BASECASE_PLANT_OWNERid
       character (len=6) :: PRIM_MOVER_STR
       
      CHARACTER(len=20) :: COUNTY_ord, NEWGEN_UNIT_STATiS, &
        STATE_Pvnc_NAME
      CHARACTER(len=6) :: STATE_or_PROVINCE

      REAL AI_TAX_LIFE
      real :: AI_ADR_TAX_LIFE, &
          START_UP_COSTS_ord
      integer (kind=2) :: TRANSACTION_GROUP_ID, DAY_TYP_ID

      real :: ANNUAL_CL_FIXED_COST_ord, CO2_PIPELINE_DISTANCE_ord


      INTEGER*2 res_id_loc,P_FUEL_SUPPLY_ID
! END 5/14/92 CHANGES
      INTEGER*2 ANNUAL_CL_FIXED_COST_ESC_ord
      
      REAL :: FRAOWN,FUELMX_ord,PRFCST,SEFCST,EFOR_loc
      real :: FUELADJ_ord,MAINRT(12)
      real :: VARCST
      real :: HRFACT,COEFF(3),FIXED_COST,AI_CAPACITY_RATE
      real :: AI_ENERGY_RATE,POOL_FRAC_OWN,DISP_ADJ2_OUT
      real :: MAINT_DAYS_ord, DISPATH_MULT_ord 
      real :: MIN_DOWN_TIME_ord

      real :: ramp_rate_ord, RAMP_DOWN_RATE_ord
      real :: p_fuel_dlv, &
          p_fuel_dlv_2_ord,p_fuel_dlv_3_ord
      real :: MIN_UP_TIME_ord



      real :: S_FUEL_DELIVERY_ord
      real :: S_FUEL_DELIVERY_2_ord,S_FUEL_DELIVERY_3_ord
      real :: FOR_FREQ, &
           FOR_DURATN
      real :: WINTER_ttL_CAPACITY

      real :: const_poly, fst_poly, scnd_poly
      real :: thrd_poly
      real :: NOX_CONTROL_PERCENT_ord, &
          SOX_CONTROL_PERCENT_ord, &
          CO2_CONTROL_PERCENT_ord, &
          HG_CONTROL_PERCENT_ord, &
          OTHER3_CONTROL_PERCENT_ord, &
      EMERGENCY_CAPACITY_ord



          real ::  EMERGENCY_HEATRATE_ord, &
          NOX_VOM_ord, &
          NOX_FOM_ord, SOX_VOM_ord, SOX_FOM_ord, CO2_VOM_ord, &
          CO2_FOM_ord, HG_VOM_ord, &
          HG_FOM_ord, OTHER3_VOM_ord, OTHER3_FOM_ord, &
          LATITUDE_ord, LONGITUDE_ord, &
          MW_INSIDE_FENCE_ord, &
          MIN_SPIN_CAP_ord, MAX_SPIN_CAP_ord

      real :: &
           INTER_BLOCKS(3), &
                  RPS_CONTRIBUTION_PERCENT
      real :: START_UP_COSTS_ESCLN
      real :: mkt_floor, mkt_ceiling
! DECLARATION FOR EMISSIONS ITEMS
    real :: p_so2_loc
      REAL P_NOX_BK1,P_CO2, &
       P_NOX_BK2_OUT
    real :: P_OTHER2,P_OTHER3

      integer (kind=2) :: EMISS_FUEL_ESCln
      integer (kind=2) :: SEC_EMISS_PR, EMISS_FUEL_EMISS_PR
      integer (kind=2) :: PRIM_FUEL_EMISS_PR


       ! DECLARATION FOR /ENERGY LIMITED RESOURCE FILE/
! 5/20/92 ADDED EL GROUP CAPABILITY-GT
      REAL :: PLANNING_FACTOR
      CHARACTER*1 :: EXPENSE_ASSGMENT,EXPENSE_cln
! ADDED 3/10/93 FOR CLINTON TAX 
      character (len=1) :: FUEL_TYPES(2)
      character(len=6) :: PRIM_Ft
      character*3 temp_expense_collection
      CHARACTER*22 FILE_TYPE/'Capacity-limited Units'/
      INTEGER*2 NUCLEAR_UNITS_ACTIVE,R_NUM_OF_NUCLEAR_UNITS
      SAVE NUCLEAR_UNITS_ACTIVE
      CHARACTER*1 NUCLEAR
      PARAMETER (NUCLEAR='N')

      INTEGER(kind=8) ::  &
                H2_UNIT_ID_NUM, &
                TRANS_BUSid, &
                Therm_parent_id




! MULTI-CL FILE VARIABLES

      INTEGER FILE_NUMBER,FILE_ID
      INTEGER MAX_CL_FILES,R_MAX_CL_FILES
      PARAMETER (MAX_CL_FILES=36) ! note file 24 is for SPCapEx
      INTEGER*2 :: ACTIVE_CL_RECORDS(0:MAX_CL_FILES-1)=0
      INTEGER*2 :: NUC_UNITS_IN_FILE(0:MAX_CL_FILES-1)=0
      INTEGER*2 :: NUC_UNITS_IN_OL_FILE(0:MAX_CL_FILES-1)=0

      CHARACTER*2 :: FOSSILOL(0:MAX_CL_FILES-1)='BC'
      INTEGER*2 NUC_RECORD_POSITION(200,0:MAX_CL_FILES-1)
      CHARACTER*5 CL_FILE_BASE_NAMES(0:MAX_CL_FILES-1), &
                  VOID_CHR,BASE_FILE_NAME
      CHARACTER*2 CL_FILE_CODES(0:MAX_CL_FILES-1)/ &
                                    'CL','C1','C2','C3','C4','C5', &
                                    'C6','C7','C8','C9','C0', &
                                    'CA','CB','CC','CD','CE','CG', &
                                    'CI','CJ','CK','CM','CP','CQ', &
                                    'XC','CS','CU','CV','CW','CX', &
        'CY', &
                                    'EJ','EK','ER','ET','MX','MY'/, &
                  FILE_CODE

      INTEGER (kind=2) :: cap_mkt_ptr=6
      real :: CO2_RETRO_CAP_MULT_ord
      real :: CO2_RETRO_HEAT_MULT_ord
      real (kind=4) :: CAP_MARKET_COIN_ADJ_FACT
      CHARACTER (len=5) :: &
        CAP_MARKET_TYPE, &
    CAP_mkt_MONTH, &
    cap_mkt_exp_collect

      CHARACTER(len=20) :: CAPACITY_MKT_COST_ASSIGN

      logical, save :: cl_21_hit=.false., cl_31_hit=.false.

      CHARACTER (len=6) :: CL_FILE_BINARY_NAMES(0:MAX_CL_FILES-1)=(/ &
       'FOSIL ', &
       'FOSIL1', 'FOSIL2', 'FOSIL3','FOSIL4','FOSIL5','FOSIL6',&
        'FOSIL7','FOSIL8','FOSIL9','FOSIL0','FOSILA','FOSILB',&
        'FOSILC','FOSILD','FOSILE','FOSILF','FOSILG','FOSILH',&
        'FOSILI','FOSILJ','FOSILK','FOSILL','FOSILM','FOSILN',&
        'FOSILO','FOSILP','FOSILQ','CapExF','FOSILR','FOSILS',&
        'FOSILU','FOSILV','FOSILW','FOSILX','FOSILY'/)
      character*6 :: BINARY_FILE_NAME
      LOGICAL ACTIVE_BASE_CL_FILES(0:MAX_CL_FILES-1)/ &
                                             MAX_CL_FILES*.FALSE./, &
              ACTIVE_OVERLAY_CL_FILES(0:MAX_CL_FILES-1)/ &
                                                 MAX_CL_FILES*.FALSE./
      LOGICAL*1 CL_FILES_ARE_ACTIVE/.FALSE./,R_CL_FILES_ARE_ACTIVE
      CHARACTER*256 COMMAND1,COMMAND2
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
      !Probably declared elsewhere.
      CHARACTER (LEN=20)  :: tTYPE
      real (kind=4) :: BettermentProjId
      real (kind=4) :: BlendSo2Up
      real (kind=4) :: fuel_trans_cost(4)
      integer (kind=2) :: I
      real (kind=4) :: decom_cont_cost
      


      character (len=6) :: pm_str_loc
      real (kind=4) :: BlendedEnthUp,BlendedEnthLo
      real :: MINIMUM_CAPACITY_FACTOR_ord,MAXIMUM_CAPACITY_FACTOR_ord
      real (kind=4) :: name_plate_cap_loc
      character*256 :: misc_character_string
      character (len=1024) :: other_filename
      character (len=512) :: basename


      CHARACTER (LEN=1) :: AGGREGT_THIS_UNIT, &
                           EMISSN_DATA_UNITS, &
                           LINKED_BTRMNT_OPTION
      CHARACTER(len=10) :: FUEL_BLENDNG_IS

      logical :: wroteyet=.false.

       UTILITY_pWNED=""
! *********************************************************************

!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.



! CONVERT THE (FOSSIL-FUELED) CAPACITY-LIMITED-UNITS FILE
! ***************************************************************
      ENTRY CL_MAKEBIN
! ***************************************************************
      VOID_CHR = FOSSILFL(CL_FILE_BASE_NAMES)

      IF(.NOT. LAHEY_LF95()) &
             CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      NUCLEAR_UNITS_ACTIVE = 0


       if(.not. cl_21_hit) then
        cl_21_hit=.true.
        inquire(unit=10, NAME=file_name)
               ! call write_log_entry("cla_objt:0021", &
       ! "Reading " // trim(file_name) // " (unit 10) ")
       endif

      DO FILE_ID = 0, MAX_CL_FILES-1
         IF(.NOT. SP_CAPEX_ACTIVE() .AND. FILE_ID == 23) CYCLE
         ACTIVE_BASE_CL_FILES(FILE_ID) = .FALSE.
         BASE_FILE_NAME = CL_FILE_BASE_NAMES(FILE_ID)

         ! Prevent spurious Lahey compiler warning about
         ! use of uninitialized variable in call to
         ! int_to_string().
         misc_character_string=""

         ! Get file handle
         misc_character_string=int_to_string(FILE_ID)

         IF(INDEX(BASE_FILE_NAME,'NONE') /= 0) CYCLE
         FILE_CODE = CL_FILE_CODES(FILE_ID)
         BINARY_FILE_NAME = CL_FILE_BINARY_NAMES(FILE_ID)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//FILE_CODE// &
                                     "B"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            SPCapExOption = (FILE_ID == 23)
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
              CALL MG_CLEAR_LINE_WRITE(16,30,34,trim(BASE_FILE_NAME), &
                                                        ALL_VERSIONS,0)
            ENDIF
            ACTIVE_BASE_CL_FILES(FILE_ID) = .TRUE.
            CL_FILES_ARE_ACTIVE = .TRUE.
            OPEN(10,FILE=FILE_NAME)
            other_filename=trim(OUTPUT_DIRECTORY())// &
                           "BC"//trim(BINARY_FILE_NAME)//".BIN"

            OPEN(11,FILE=other_filename, &
                         ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            IREC = 0
               inquire(unit=10, NAME=file_name)
               ! call write_log_entry("cla_objt:0020", &
       ! "Reading " // trim(just_the_filename(file_name)) // &
       ! " (unit 10) ")

               inquire(unit=11, NAME=file_name)
               ! call write_log_entry("cla_objt:0025", &
       ! "Reading " // trim(just_the_filename(file_name)) // &
       ! " (unit 11) ")
            ! BCFOSIL.BIN and similar
            READ(10,*) DELETE

            DO
               AI_CAPACITY_RATE = 0.
               AI_CAPACITY_ESCALATOR = 0.
               AI_ENERGY_RATE = 0.
               AI_ENERGY_ESCALATOR = 0.
               AI_REMAINING_LIFE = 0
               P_SO2_loc = 0.
               P_NOX_BK1 = 0.
               P_CO2 = 0.
               P_OTHER2 = 0.
               P_OTHER3 = 0.
               POOL_FRAC_OWN = 100.
               PHASE_I = .TRUE.
               P_FUEL_SUPPLY_ID = 0
               DISPADJ2_ord  = -99999.
               P_NOX_BK2 = -99999.
               SEC_EMISS_PR = -1
               EMISS_FUEL_COST = 0.
               EMISS_FUEL_ESCln = 0
               EMISS_FUEL_EMISS_PR = 0
               EMISS_BLND_RT_ORD = 0.
               PRIM_FUEL_EMISS_PR = 0
               PRIM_Ft = 'T'
               FUEL_TYPES(1) = 'T'
               FUEL_TYPES(2) = 'T'
               TIE_GROUP = 0
               MONTHLY_CAP_ptr = 0
               ANNUAL_CL_FIXED_COST_ord = 0.
               ANNUAL_CL_FIXED_COST_ESC_ord = 0
               MAINT_DAYS_ord = 0.
               dispatch_mult_ord = 1.
               EXCESS_ENERGY_SALES_cla = 0.
               AI_TAX_LIFE = 99.
               AI_ADR_TAX_LIFE = 99.
               ASSET_CLASS_NUM_ord = 0
               ASSET_CLASS_Vctr = 0
               INTRA_COMPANY_CLsid = -1
               INTRA_COMPANY_xACTION = 'N'
               MINIMUM_CAPACITY_FACTOR_ord = 0.
               MAXIMUM_CAPACITY_FACTOR_ord = 8800.
               TRANSACTION_GROUP_ID = 1
               DAY_TYP_ID = 0
               UNIT_ACTIVE_ord = 'T'
               REPORT_UNIT_ord = 'T'
               BASECASE_PLNT_ID = 'BLANK ' ! CHAR*6
               BASECASE_UNITid = 'BLANK ' ! CHAR*6
               BASECASE_MARKET_AREAid = 'BLANK ' ! CHAR*6
               BASECASE_TRANS_AREAid = 'BLANK ' ! CHAR*6
               BASECASE_PLANT_NERC_SUBid = 'BLANK ' ! CHAR*6
               BASECASE_PLANT_OWNERid = 'BLANK ' ! CHAR*6
               UTILITY_pWNED = 'U'
               MONTHLY_FUEL_idx = 0
               START_UP_COSTS_ord = 0.
               START_LOGIC_ord = 'F'
               CONTRIBs_TO_SPIN = 'F'
               ramp_rate_ord = 999999.
               RAMP_DOWN_RATE_ord = -999999.
               MIN_DOWN_TIME_ord = 0.0
               MIN_UP_TIME_ord = 0.0
           ! Johncheck - other one using original vars.
               p_fuel_dlv = 0.0
               p_fuel_dlv_2_ord = 0.0
               p_fuel_dlv_2_ord = 0.0
            ! Johncheck - other one using original vars.
               S_FUEL_DELIVERY_ord = 0.0
               S_FUEL_DELIVERY_2_ord = 0.0
               S_FUEL_DELIVERY_3_ord = 0.0
               HESI_UNIT_ID_NUM = 0
               H2_UNIT_ID_NUM = 0
               FOR_FREQ = 0.
               FOR_DURATN = 0.
               WINTER_ttL_CAPACITY = 0.

           !Johncheck
               CO2_CONTROL_PERCENT_ord = 0.
               HG_CONTROL_PERCENT_ord = 0.
               OTHER3_CONTROL_PERCENT_ord = 0.
               NOX_CONTROL_PERCENT_ord = 0.
               SOX_CONTROL_PERCENT_ord = 0.
               NOX_CONTROL_DATE_ord = 9001
               SOX_CONTROL_DATE_ord = 9001
               CO2_CONTROL_DATE_ord = 9001
               HG_CONTROL_DATE_ord = 9001
               OTHER3_CONTROL_DATE_ord = 9001
               APPLY_NOX_SSN_DT_ord = 'F'
               NOX_SEASON_DATE_ord = 0150

               EMERGENCY_CAPACITY_ord = 0.
               EMERGENCY_HEATRATE_ord = 0.
               MrkT_RESOURCE = 'F'

               !Johncheck
               pm_str_loc = 'PMNONE'
               COUNTY_ord = 'BLANK'
               NEWGEN_UNIT_STATiS = 'BLANK'
               STATE_or_PROVINCE = '#N/A'

               ! Johncheck
               NOX_VOM_ord = 0.
               NOX_FOM_ord = 0.
               SOX_VOM_ord = 0.
               SOX_FOM_ord = 0.
               CO2_VOM_ord = 0.
               CO2_FOM_ord = 0.
               HG_VOM_ord = 0.
               HG_FOM_ord = 0.
               OTHER3_VOM_ord = 0.
               OTHER3_FOM_ord = 0.
               LATITUDE_ord = 0.
               LONGITUDE_ord = 0.
               MW_INSIDE_FENCE_ord = 0.

               USE_POLY_HT_RATES = 'F'
               WVPA_RT_TRACKER = 'N'
               WVPA_rs_TRACKER = 'N'
               WVPA_FL_TRACKER = 'N'
               WVPA_MEM_TKER_ord = 'M'
               MONTE_OR_FREQ_ord = 'G'
               LINKED_BTRMNT_OPTION = 'F'

               ALLOW_DECOMMIT_ord = 'T'

               MONTHLY_MUST_RUN_VECTOR_ord = 0
               MONTHLY_DECOMMIT_VECTOR_ord = 0
               TRANS_BUSid = 0
               EMISSION_MARKET_LINK = -9999
               FIRST_RETIREMENT_Yr = 1901
               RPS_PROGRAM_NUMBER = 0

               MIN_SPIN_CAP_ord = 0.
               MAX_SPIN_CAP_ord = 9999.

               mkt_floor = 0.
               mkt_ceiling = 0.
               START_UP_COSTS_ESCLN = 0.

               INTER_BLOCKS(1) = -99.
               INTER_BLOCKS(2) = -99.
               INTER_BLOCKS(3) = -99.
               name_plate_cap_loc = -99.
               ns_screen_data%FIRST_YEAR_DECOM_AVAIL = 2100.
               ns_screen_data%DECOM_BASE_YR_COST_ord = 0.
               ns_screen_data%DECOMMISSIONING_COST_ESCALATION = 0.
               ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR = 0.
               AGGREGT_THIS_UNIT = "F"

               ns_screen_data%FuelRatio = 0.
               ns_screen_data%BlendableFuelsPtr = 0.
               fuel_trans_cost = 0.
               BlendedEnthUp = 0.
               BlendedEnthLo = 0.
               BlendSo2Up = 0.
               BettermentProjId = 0.
               decom_cont_cost = 0.
               ns_screen_data%DECOM_CONTINUING_COST_ESCALATION = 0.
               ns_screen_data%EnrgPatternPointer = 0.
               EMISSN_DATA_UNITS = 'l'
               ns_screen_data%EmissRedRate = 0.
               ns_screen_data%EmissMaxRate = 0.
               ns_screen_data%MaxEnergyLimit= 0.
               tTYPE = " "
               FUEL_BLENDNG_IS = 'Primary'
           !Johncheck
               FUELMX_ord = 1.
               CAP_MARKET_TYPE = 'NONE'
               CAP_MARKET_COIN_ADJ_FACT = 1.0
               PRIMARY_FUEL_CAT = ' '
               SECONDARY_FUEL_CAT = ' '
               EMISSIONS_FUEL_CAT = ' '
               RPS_CONTRIBUTION_PERCENT = 0.0
               RETIRMENT_CANDIDATE = 'F'
               rFIT_CANDIDATE = 'F'

               thrm_GUID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
               Therm_parent_id = 123456789
               THRML_AGGREGATED_UNIT = 'F'


               DO
             res_id_loc=irec+1
            file_name=trim(get_filename_from_unit(10))
          ! call write_log_entry("cla_objt:0092", "Reading RECLEN " // &
       ! "from " // trim(file_name) // " (unit 10).")

                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /=0) EXIT
                  IF(RECLN(1:1) == '7') EXIT



         call check_string_corrupt("cla_objt:1047.C", RECLN, int(0,2), &
            "Corruption detected!")

                  RECLN=trim(RECLN)// &
               ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                  ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'

         call check_string_corrupt("cla_objt:1047.d", RECLN, int(0,2), &
            "Corruption detected!")

! VARIABLES THAT CAN NOT PROPAGATE LEFT TO RIGHT

                  spl_UNIT_ID = " "
                  CO2_RETRO_CAP_MULT_ord = 1
                  CO2_RETRO_HEAT_MULT_ord = 1
                  FIRST_RETIREMENT_Yr = 1901


! first read
! capacity_market_pointer : CAPACITY_MARKET_POINTER = 6
! first_retirement_year : FIRST_RETIREMENT_YEAR = 2200

                  ! Avoid spurious Lahey initialization warning
                  ! on these few variables by initializing before
                  ! write.
                  ofliyr=0
                  onliyr=0
                  oflimo_ord=0
                  onlimo_ord=0
                  dispadj_ord=0

                  ! Johncheck (for _loc variables)
                  READ(RECLN,*) DELETE,UNIT_NAME,CL_LOAD_TYPE, &
                  EXPENSE_ASSGMENT,TEMP_EXPENSE_COLLECTION,GENGRP_ord, &
          FRAOWN, &
          ONLIMO_ord,ONLIYR,OFLIMO_ord,OFLIYR, & !  10
          FUELMX_ord,PRFCST,PRESCR, &
          SEFCST,SDESCR,FUELADJ_ord,EFOR_loc, &
          MAINRT,VARCST,VRESCR,  & !  31
          FIXED_COST,FIXED_COST_ESCALATOR, &
          DISPADJ_ord,HRFACT,MW,PLANNING_FACTOR,COEFF,DESC, &  !42
          P_SO2_loc,P_NOX_BK1,P_CO2,POOL_FRAC_OWN, &
          P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2_ord, & !50
          res_id_loc,P_FUEL_SUPPLY_ID,P_NOX_BK2, &
          SEC_EMISS_PR,EMISS_FUEL_COST,EMISS_FUEL_ESCln, &
          EMISS_FUEL_EMISS_PR,EMISS_BLND_RT_ORD, &
          PRIM_FUEL_EMISS_PR, &
          PRIM_Ft,                          & !   60
          FUEL_TYPES,TIE_GROUP, &
          MONTHLY_CAP_ptr, &
          AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
          AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
          AI_REMAINING_LIFE, &
          ANNUAL_CL_FIXED_COST_ord,                     & !  70
          ANNUAL_CL_FIXED_COST_ESC_ord, &
          MAINT_DAYS_ord,dispatch_mult_ord, &
          EXCESS_ENERGY_SALES_cla, &
          AI_TAX_LIFE, &
          AI_ADR_TAX_LIFE, &
          ASSET_CLASS_NUM_ord, &
          ASSET_CLASS_Vctr, &
          INTRA_COMPANY_CLsid, &
          INTRA_COMPANY_xACTION, & !  80
          spl_UNIT_ID, &
          MINIMUM_CAPACITY_FACTOR_ord, &
          MAXIMUM_CAPACITY_FACTOR_ord, &
          TRANSACTION_GROUP_ID, &
          DAY_TYP_ID, &
          UNIT_ACTIVE_ord, &
          BASECASE_PLNT_ID, &
          BASECASE_UNITid, &
          BASECASE_MARKET_AREAid, &
          BASECASE_TRANS_AREAid, & !  90
          BASECASE_PLANT_NERC_SUBid, &
          BASECASE_PLANT_OWNERid, &
          UTILITY_pWNED, &
          MONTHLY_FUEL_idx, &
          START_UP_COSTS_ord, &
          START_LOGIC_ord, &
          ramp_rate_ord, &
          MIN_DOWN_TIME_ord, &
          REPORT_UNIT_ord, &
          p_fuel_dlv, & !  100
          p_fuel_dlv_2_ord, &
          p_fuel_dlv_3_ord, &
          MIN_UP_TIME_ord, &
          HESI_UNIT_ID_NUM,  & !  104
          FOR_FREQ, & !  105
          FOR_DURATN, & !  106
          WINTER_ttL_CAPACITY, &
          CONTRIBs_TO_SPIN, &
          H2_UNIT_ID_NUM,      & !  109
          POWERDAT_PLID,   & !  110
          APPLY_NOX_SSN_DT_ord,  & !  111
          NOX_SEASON_DATE_ord,    & !  112
          RAMP_DOWN_RATE_ord,      & !  113
          NOX_CONTROL_PERCENT_ord, &
          NOX_CONTROL_DATE_ord, &
          EMERGENCY_CAPACITY_ord, &
          EMERGENCY_HEATRATE_ord, &
          MrkT_RESOURCE, &
          pm_str_loc, &
          COUNTY_ord,              & !  120
          STATE_or_PROVINCE, &
          NOX_VOM_ord, &
          NOX_FOM_ord, &
          S_FUEL_DELIVERY_ord, &
          S_FUEL_DELIVERY_2_ord,   & !  125
          S_FUEL_DELIVERY_3_ord, &
          const_poly, &
          fst_poly, &
          scnd_poly, &
          thrd_poly,          & !  130
          USE_POLY_HT_RATES, &
          NEWGEN_UNIT_STATiS, &
          MONTHLY_MUST_RUN_VECTOR_ord, &
          EMISSION_MARKET_LINK, &
          WVPA_RT_TRACKER,    & !  135
          ALLOW_DECOMMIT_ord, &
          MIN_SPIN_CAP_ord, &
          MAX_SPIN_CAP_ord, &
          WVPA_rs_TRACKER, &
          WVPA_FL_TRACKER,  & !  140
          MONTE_OR_FREQ_ord, &
          WVPA_MEM_TKER_ord, &
          MONTHLY_DECOMMIT_VECTOR_ord, &
          TRANS_BUSid, &
          SOX_CONTROL_PERCENT_ord, &
          SOX_CONTROL_DATE_ord, &
          SOX_VOM_ord, &
          SOX_FOM_ord, &
          LATITUDE_ord, &
          LONGITUDE_ord,          & !  150
          MW_INSIDE_FENCE_ord, &
          INTER_BLOCKS, &    ! 152-154
           ns_screen_data%FIRST_YEAR_DECOM_AVAIL,     & !  155
          ns_screen_data%DECOM_BASE_YR_COST_ord,            & !  156
          ns_screen_data%DECOMMISSIONING_COST_ESCALATION, &
          ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR, &
          AGGREGT_THIS_UNIT,              & !  159
          name_plate_cap_loc, &
          FUEL_BLENDNG_IS, &
          ns_screen_data%FuelRatio,         & !  162-166
          ns_screen_data%BlendableFuelsPtr, & !  167-170
          fuel_trans_cost, & !  171-174
          BlendedEnthUp, & !  175
          BlendedEnthLo, &
          BlendSo2Up, & !  177
          BettermentProjId, &
          decom_cont_cost, &
          ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
          ns_screen_data%EnrgPatternPointer, & !  181
          EMISSN_DATA_UNITS, &
          ns_screen_data%EmissRedRate, & !  183-187
          ns_screen_data%EmissMaxRate, &
          ns_screen_data%MaxEnergyLimit, &
          tTYPE, & !  196
          LINKED_BTRMNT_OPTION, & !  197
          CO2_CONTROL_PERCENT_ord, &
          HG_CONTROL_PERCENT_ord, &
          OTHER3_CONTROL_PERCENT_ord, &
          CO2_CONTROL_DATE_ord, &
          HG_CONTROL_DATE_ord, &
          OTHER3_CONTROL_DATE_ord, &
          CO2_VOM_ord, & !  204
          CO2_FOM_ord, &
          HG_VOM_ord, &
          HG_FOM_ord, &
          OTHER3_VOM_ord, &
          OTHER3_FOM_ord, & !  209
          mkt_floor, &
          mkt_ceiling,  & !  211
          START_UP_COSTS_ESCLN,  & !  212
          CAP_MARKET_TYPE, &
          CAP_mkt_MONTH, &
          cap_mkt_ptr, &
          CAPACITY_MKT_COST_ASSIGN, &
          cap_mkt_exp_collect, &
          CAP_MARKET_COIN_ADJ_FACT, &
          PRIMARY_FUEL_CAT, &
          SECONDARY_FUEL_CAT, & !  220
          EMISSIONS_FUEL_CAT, &
          RPS_CONTRIBUTION_PERCENT, &
          RETIRMENT_CANDIDATE, &
          rFIT_CANDIDATE, &
          RETROFIT_proj_ID, &
          CO2_BASIN_nm, &
          CO2_PIPELINE_DISTANCE_ord, &
          STATE_Pvnc_NAME, & !  228
          CO2_RETRO_HEAT_MULT_ord, &
          CO2_RETRO_CAP_MULT_ord, &
          thrm_GUID, &
          Therm_parent_id, & !  232
          THRML_AGGREGATED_UNIT, &
          FIRST_RETIREMENT_Yr, &
          UNIT_TYPE_ord, &
          UNIT_TYPE_cat, &
          RPS_PROGRAM_NUMBER
     call check_string_corrupt("cla_objt:1047.B", UNIT_TYPE_ord, &
      IREC, "Corruption detected!")

                  IF(IOS /= 0) THEN
                     CL_VARIABLES = "BAD"
                     READ(RECLN,*,IOSTAT=IOS)DELETE,CL_VARIABLES
                     DO I = 1, 100
                        IF(INDEX(CL_VARIABLES(I),"BAD") /= 0) THEN
        call end_program("cla_objt:0001 - Found BAD variable " // &
       "entry at cl_variables(" // trim(itos(int(I))) // ")")


                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
                  IF(SPCapExOption) then
                      NEWGEN_UNIT_STATiS = 'SPCapEx'
                  endif
                  IF(RAMP_DOWN_RATE_ord == -999999.) THEN
                     RAMP_DOWN_RATE_ord = ramp_rate_ord
                  ENDIF
                  IF(DISPADJ2_ord == -99999.) THEN
                     DISP_ADJ2_OUT = DISPADJ_ord
                  ELSE
                     DISP_ADJ2_OUT = DISPADJ2_ord
                  ENDIF


                  IF(P_NOX_BK2 == -99999.) THEN
                     P_NOX_BK2_OUT = P_NOX_BK1
                  ELSE
                     P_NOX_BK2_OUT = P_NOX_BK2
                  ENDIF
                  IF(INDEX(TEMP_EXPENSE_COLLECTION,'BTL') /= 0) THEN
                     EXPENSE_cln = 'X'
                  ELSE
                     EXPENSE_cln = TEMP_EXPENSE_COLLECTION(1:1)
                  ENDIF
                  IREC = IREC + 1
               IF(CL_LOAD_TYPE(1:1) == NUCLEAR .AND. DELETE < 8 .AND. &
        (UNIT_ACTIVE_ord == 'T' .OR. UNIT_ACTIVE_ord == 't')) THEN
                     NUC_UNITS_IN_FILE(FILE_ID) = &
                                         NUC_UNITS_IN_FILE(FILE_ID) + 1
                     NUC_RECORD_POSITION(NUC_UNITS_IN_FILE(FILE_ID), &
                                                        FILE_ID) = IREC
                     NUCLEAR_UNITS_ACTIVE = NUCLEAR_UNITS_ACTIVE + 1
                  ENDIF


            file_name=trim(get_filename_from_unit(11))
            ! call write_log_entry("cla_objt:0028", "Writing to " // &
       ! trim(file_name) // " (unit 11).")
       if(index(file_name, "BCFOSIL")>0) then
        file_name=file_name ! Debugstop
       end if


       if(cap_mkt_ptr>24000) then
        call end_program("cl_objt_ext:0001 - about to write " // &
            "invalid value of " // trim(itos(int(cap_mkt_ptr))) // &
            " to " // &
            trim(file_name) // " for " // &
            "cap_mkt_ptr (cap_market_pointer).")
       end if
! John is here




 WRITE(11,REC=IREC) DELETE,UNIT_NAME,CL_LOAD_TYPE, &
        EXPENSE_ASSGMENT,EXPENSE_cln, GENGRP_ord,FRAOWN, &
        ONLIMO_ord,ONLIYR,OFLIMO_ord,OFLIYR,FUELMX_ord,PRFCST, &
        PRESCR, SEFCST, &
        SDESCR,FUELADJ_ord,EFOR_loc,MAINRT,VARCST,VRESCR, FIXED_COST, &
       FIXED_COST_ESCALATOR, DISPADJ_ord,HRFACT,MW,PLANNING_FACTOR,COEFF, &
       AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,  &
       AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, AI_REMAINING_LIFE, &
       P_SO2_loc,P_NOX_BK1,P_CO2,POOL_FRAC_OWN, P_OTHER2,P_OTHER3, &
       PHASE_I_STR,DISP_ADJ2_OUT, res_id_loc,P_FUEL_SUPPLY_ID, &
       P_NOX_BK2_OUT, SEC_EMISS_PR,EMISS_FUEL_COST,EMISS_FUEL_ESCln, &
       EMISS_FUEL_EMISS_PR,EMISS_BLENDING_RATE_ord, PRIM_FUEL_EMISS_PR, &
       PRIM_Ft,FUEL_TYPES,TIE_GROUP, MONTHLY_CAP_ptr, &
       ANNUAL_CL_FIXED_COST_ord, ANNUAL_CL_FIXED_COST_ESC_ord, &
       MAINT_DAYS_ord,dispatch_mult_ord, EXCESS_ENERGY_SALES_cla, AI_TAX_LIFE, &
       AI_ADR_TAX_LIFE, ASSET_CLASS_NUM_ord, ASSET_CLASS_Vctr, &
       INTRA_COMPANY_CLsid, INTRA_COMPANY_xACTION, &
       spl_UNIT_ID, MINIMUM_CAPACITY_FACTOR_ord, &
       MAXIMUM_CAPACITY_FACTOR_ord, TRANSACTION_GROUP_ID, DAY_TYP_ID, &
       UNIT_ACTIVE_ord, BASECASE_PLNT_ID, BASECASE_UNITid, &
       BASECASE_MARKET_AREAid, BASECASE_TRANS_AREAid, &
       BASECASE_PLANT_NERC_SUBid, BASECASE_PLANT_OWNERid, &
       UTILITY_pWNED, MONTHLY_FUEL_idx, START_UP_COSTS_ord, &
       START_LOGIC_ord, ramp_rate_ord, MIN_DOWN_TIME_ord, REPORT_UNIT_ord, &
       p_fuel_dlv, p_fuel_dlv_2_ord, p_fuel_dlv_3_ord, &
       MIN_UP_TIME_ord, HESI_UNIT_ID_NUM, FOR_FREQ, &
       FOR_DURATN, WINTER_ttL_CAPACITY, CONTRIBs_TO_SPIN, &
       H2_UNIT_ID_NUM, POWERDAT_PLID, APPLY_NOX_SSN_DT_ord, &
       NOX_SEASON_DATE_ord, RAMP_DOWN_RATE_ord, NOX_CONTROL_PERCENT_ord, &
       NOX_CONTROL_DATE_ord, EMERGENCY_CAPACITY_ord, EMERGENCY_HEATRATE_ord, &
       MrkT_RESOURCE, pm_str_loc, COUNTY_ord, STATE_or_PROVINCE, &
       NOX_VOM_ord, NOX_FOM_ord, S_FUEL_DELIVERY_ord, S_FUEL_DELIVERY_2_ord, &
       S_FUEL_DELIVERY_3_ord, const_poly, fst_poly, &
       scnd_poly, &
       thrd_poly, USE_POLY_HT_RATES, NEWGEN_UNIT_STATiS, &
       MONTHLY_MUST_RUN_VECTOR_ord, EMISSION_MARKET_LINK, &
       WVPA_RT_TRACKER, ALLOW_DECOMMIT_ord, MIN_SPIN_CAP_ord, MAX_SPIN_CAP_ord, &
       WVPA_rs_TRACKER, WVPA_FL_TRACKER, MONTE_OR_FREQ_ord, &
       WVPA_MEM_TKER_ord, MONTHLY_DECOMMIT_VECTOR_ord, TRANS_BUSid, &
       SOX_CONTROL_PERCENT_ord, SOX_CONTROL_DATE_ord, SOX_VOM_ord, SOX_FOM_ord, &
       LATITUDE_ord, LONGITUDE_ord, MW_INSIDE_FENCE_ord, &
       INTER_BLOCKS,  & ! 152-154
       ns_screen_data%FIRST_YEAR_DECOM_AVAIL, &
       ns_screen_data%DECOM_BASE_YR_COST_ord, & !156
       ns_screen_data%DECOMMISSIONING_COST_ESCALATION, &
       ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR, AGGREGT_THIS_UNIT, & !  159
       name_plate_cap_loc, FUEL_BLENDNG_IS, ns_screen_data%FuelRatio, &
       ns_screen_data%BlendableFuelsPtr, & !  167
       fuel_trans_cost, BlendedEnthUp, &
       BlendedEnthLo, BlendSo2Up, & !  178
       BettermentProjId, decom_cont_cost, &
       ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
        ns_screen_data%EnrgPatternPointer, & !181
       EMISSN_DATA_UNITS, & !  182
       ns_screen_data%EmissRedRate, & !  183-187
       ns_screen_data%EmissMaxRate, ns_screen_data%MaxEnergyLimit, &
       tTYPE,LINKED_BTRMNT_OPTION, &
       CO2_CONTROL_PERCENT_ord,HG_CONTROL_PERCENT_ord, &
        OTHER3_CONTROL_PERCENT_ord, &
       CO2_CONTROL_DATE_ord, HG_CONTROL_DATE_ord, OTHER3_CONTROL_DATE_ord, &
     CO2_VOM_ord, CO2_FOM_ord, HG_VOM_ord, HG_FOM_ord, OTHER3_VOM_ord, &
        OTHER3_FOM_ord, & !  209
     mkt_floor, mkt_ceiling, START_UP_COSTS_ESCLN,  & !  212
       CAP_MARKET_TYPE, CAP_mkt_MONTH, &
       cap_mkt_ptr, CAPACITY_MKT_COST_ASSIGN, &
       cap_mkt_exp_collect, CAP_MARKET_COIN_ADJ_FACT, &
       PRIMARY_FUEL_CAT, SECONDARY_FUEL_CAT, &
       EMISSIONS_FUEL_CAT, RPS_CONTRIBUTION_PERCENT, &
       RETIRMENT_CANDIDATE, rFIT_CANDIDATE, RETROFIT_proj_ID, &
       CO2_BASIN_nm, CO2_PIPELINE_DISTANCE_ord, STATE_Pvnc_NAME, & !  228
       CO2_RETRO_HEAT_MULT_ord, CO2_RETRO_CAP_MULT_ord, thrm_GUID, &
       Therm_parent_id, & !  232
       THRML_AGGREGATED_UNIT, FIRST_RETIREMENT_Yr, &
       UNIT_TYPE_ord, UNIT_TYPE_cat, RPS_PROGRAM_NUMBER

       

               call check_string_corrupt("cla_objt:1000", unit_name, &
                IREC, "Corruption detected!")

              call check_string_corrupt("cla_objt:1001", cl_load_type, &
                IREC, "Corruption detected!")

       call check_string_corrupt("cla_objt:1002", EXPENSE_ASSGMENT, &
                IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1003", EXPENSE_cln, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1004", PHASE_I_STR, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1005", PRIM_Ft, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1006", fuel_types(1), &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1007", fuel_types(2), &
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1008", &
        INTRA_COMPANY_xACTION, IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1009", spl_UNIT_ID, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1010", UNIT_ACTIVE_ord, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1011", BASECASE_PLNT_ID, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1012", BASECASE_UNITid,  &
      IREC, "Corruption detected!")
    call check_string_corrupt("cla_objt:1013", BASECASE_TRANS_AREAid, &
        IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1014", &
        BASECASE_PLANT_NERC_SUBid, IREC, "Corruption detected!")

    call check_string_corrupt("cla_objt:1015",BASECASE_PLANT_OWNERid, &
     IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1016",UTILITY_pWNED,  &
      IREC, "Corruption detected!")
     call check_string_corrupt( "cla_objt:1017",START_LOGIC_ord, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1018",REPORT_UNIT_ord,   &
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1019",CONTRIBs_TO_SPIN ,  &
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1020", APPLY_NOX_SSN_DT_ord, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1021", MrkT_RESOURCE, &
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1022", pm_str_loc, &
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1023", COUNTY_ord, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1024", STATE_or_PROVINCE , &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1025", USE_POLY_HT_RATES, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1026", NEWGEN_UNIT_STATiS , &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1027", WVPA_RT_TRACKER , &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1028", ALLOW_DECOMMIT_ord , &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1029", WVPA_rs_TRACKER, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1030", WVPA_FL_TRACKER, &
      IREC, "Corruption detected!")

     call check_string_corrupt( "cla_objt:1031", MONTE_OR_FREQ_ord ,&
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1032", WVPA_MEM_TKER_ord , &
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1033", &
        LINKED_BTRMNT_OPTION , IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1034", CAP_MARKET_TYPE , &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1035", CAP_mkt_MONTH, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1036", &
        CAPACITY_MKT_COST_ASSIGN , IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1037", &
        cap_mkt_exp_collect, IREC, "Corruption detected!")

    call check_string_corrupt("cla_objt:1038", PRIMARY_FUEL_CAT , &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1039", &
        SECONDARY_FUEL_CAT , IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1040", &
        EMISSIONS_FUEL_CAT , IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1041",RETIRMENT_CANDIDATE ,  &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1042", rFIT_CANDIDATE , &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1043",CO2_BASIN_nm, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1044",STATE_Pvnc_NAME ,  &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1045", thrm_GUID, &
      IREC, "Corruption detected!")

     call check_string_corrupt("cla_objt:1046", &
        THRML_AGGREGATED_UNIT , int(IREC,2), "Corruption detected!")


     ! Fails here debugmod
     call check_string_corrupt("cla_objt:1047", UNIT_TYPE_ord, &
      IREC, "Corruption detected!")
     call check_string_corrupt("cla_objt:1048",UNIT_TYPE_cat ,  &
      IREC, "Corruption detected!")
    call check_string_corrupt("cla_objt:1049",BASECASE_MARKET_AREAid, &
         IREC, "Corruption detected!")



               ENDDO

           ! Todo - fix this assumption. Read is 200 lines back.
           ! Set something meaningful to boolean and use boolean
           ! here (if it's found this statement is actually needed
           ! after attention for correctness). That process of
           ! setting something meaningful should error out for other
           ! conditions if appropriate.
               IF(IOS /= 0) then
                      EXIT !debugstop
                end if
            ENDDO
            ACTIVE_CL_RECORDS(FILE_ID) = IREC
            CLOSE(10)
            CLOSE(11)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "cl_object_ext:0001")
         ENDIF
      ENDDO ! FILE_ID FIRST PASS
      IF(NUCLEAR_UNITS_ACTIVE > 0) THEN
         CALL SETUP_GENERATING_INFORMATION(NUCLEAR_UNITS_ACTIVE)
         DO FILE_ID = 0, MAX_CL_FILES-1
            IF(NUC_UNITS_IN_FILE(FILE_ID) > 0) THEN
               BINARY_FILE_NAME = CL_FILE_BINARY_NAMES(FILE_ID)
               other_filename=trim(OUTPUT_DIRECTORY())// &
                           "BC"//trim(BINARY_FILE_NAME)//".BIN"

               OPEN(11,FILE=other_filename, &
                           ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
           file_name=get_filename_from_unit(11)
            ! call write_log_entry("cla_objt:0027", "Reading from " // &
                ! trim(file_name) // " (unit 11).")

               DO I = 1, NUC_UNITS_IN_FILE(FILE_ID)
                  IREC = NUC_RECORD_POSITION(I,FILE_ID)


          ! Johncheck: Check for _ord and _loc variables.
                  ! second read
                  READ(11,REC=IREC) DELETE,UNIT_NAME,CL_LOAD_TYPE, &
           EXPENSE_ASSGMENT,EXPENSE_cln, &
          GENGRP_ord,FRAOWN,ONLIMO_ord,ONLIYR,OFLIMO_ord,OFLIYR, &
          FUELMX_ord, &
           PRFCST,PRESCR,SEFCST,SDESCR,FUELADJ_ord,EFOR_loc,MAINRT, &
           VARCST,VRESCR,FIXED_COST,FIXED_COST_ESCALATOR, &
           DISPADJ_ord,HRFACT,MW,PLANNING_FACTOR,COEFF, &
           AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
           AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
           AI_REMAINING_LIFE, &
           P_SO2_loc,P_NOX_BK1,P_CO2,POOL_FRAC_OWN, &
           P_OTHER2,P_OTHER3,PHASE_I_STR,DISP_ADJ2_OUT, &
           res_id_loc,P_FUEL_SUPPLY_ID,P_NOX_BK2, &
           SEC_EMISS_PR,EMISS_FUEL_COST,EMISS_FUEL_ESCln, &
           EMISS_FUEL_EMISS_PR,EMISS_BLND_RT_ORD, &
           PRIM_FUEL_EMISS_PR, &
           PRIM_Ft,FUEL_TYPES,TIE_GROUP, &
           MONTHLY_CAP_ptr, &
           ANNUAL_CL_FIXED_COST_ord, &
           ANNUAL_CL_FIXED_COST_ESC_ord, &
           MAINT_DAYS_ord,dispatch_mult_ord, &
           EXCESS_ENERGY_SALES_cla, &
           AI_TAX_LIFE, &
           AI_ADR_TAX_LIFE, &
           ASSET_CLASS_NUM_ord, &
           ASSET_CLASS_Vctr, &
           INTRA_COMPANY_CLsid, &
           INTRA_COMPANY_xACTION, &
           spl_UNIT_ID, &
           MINIMUM_CAPACITY_FACTOR_ord, &
           MAXIMUM_CAPACITY_FACTOR_ord, &
           TRANSACTION_GROUP_ID, &
           DAY_TYP_ID, &
           UNIT_ACTIVE_ord, &
           BASECASE_PLNT_ID, &
           BASECASE_UNITid, &
           BASECASE_MARKET_AREAid, &
           BASECASE_TRANS_AREAid, &
           BASECASE_PLANT_NERC_SUBid, &
           BASECASE_PLANT_OWNERid, &
           UTILITY_pWNED, &
           MONTHLY_FUEL_idx, &
           START_UP_COSTS_ord, &
           START_LOGIC_ord, &
           ramp_rate_ord, &
           MIN_DOWN_TIME_ord, &
           REPORT_UNIT_ord, &
           p_fuel_dlv, &
           p_fuel_dlv_2_ord, &
           p_fuel_dlv_3_ord, &
           MIN_UP_TIME_ord, &
           HESI_UNIT_ID_NUM, &
           FOR_FREQ, &
           FOR_DURATN, &
           WINTER_ttL_CAPACITY, &
           CONTRIBs_TO_SPIN, &
           H2_UNIT_ID_NUM, &
           POWERDAT_PLID, &
           APPLY_NOX_SSN_DT_ord, &
           NOX_SEASON_DATE_ord, &
           RAMP_DOWN_RATE_ord, &
           NOX_CONTROL_PERCENT_ord, &
           NOX_CONTROL_DATE_ord, &
           EMERGENCY_CAPACITY_ord, &
           EMERGENCY_HEATRATE_ord, &
           MrkT_RESOURCE, &
           pm_str_loc, &
           COUNTY_ord, &
           STATE_or_PROVINCE, &
           NOX_VOM_ord, &
           NOX_FOM_ord, &
           S_FUEL_DELIVERY_ord, &
           S_FUEL_DELIVERY_2_ord, &
           S_FUEL_DELIVERY_3_ord, &
           const_poly, &
           fst_poly, &
           scnd_poly, &
           thrd_poly, &
           USE_POLY_HT_RATES, &
           NEWGEN_UNIT_STATiS, &
           MONTHLY_MUST_RUN_VECTOR_ord, &
           EMISSION_MARKET_LINK, &
           WVPA_RT_TRACKER, &
           ALLOW_DECOMMIT_ord, &
           MIN_SPIN_CAP_ord, &
           MAX_SPIN_CAP_ord, &
           WVPA_rs_TRACKER, &
           WVPA_FL_TRACKER, &
           MONTE_OR_FREQ_ord, &
           WVPA_MEM_TKER_ord, &
           MONTHLY_DECOMMIT_VECTOR_ord, &
           TRANS_BUSid, &
           SOX_CONTROL_PERCENT_ord, &
           SOX_CONTROL_DATE_ord, &
           SOX_VOM_ord, &
           SOX_FOM_ord, &
           LATITUDE_ord, &
           LONGITUDE_ord, &
           MW_INSIDE_FENCE_ord, &
           INTER_BLOCKS,  &   ! 152-153
! SP CapEx Variables added 11/17/05 MSG &
                     ns_screen_data%FIRST_YEAR_DECOM_AVAIL, &
                     ns_screen_data%DECOM_BASE_YR_COST_ord,  & !  155
                     ns_screen_data%DECOMMISSIONING_COST_ESCALATION, &
                     ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGT_THIS_UNIT, & !  158
                     name_plate_cap_loc, &
                     FUEL_BLENDNG_IS, &
                     ns_screen_data%FuelRatio, &
                     ns_screen_data%BlendableFuelsPtr, & !  167
                     fuel_trans_cost, &
                     BlendedEnthUp, &
                     BlendedEnthLo, &
                     BlendSo2Up, & !  177
                     BettermentProjId, &
                     decom_cont_cost, &
                     ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
                     ns_screen_data%EnrgPatternPointer, & !  181
                     EMISSN_DATA_UNITS, &
                     ns_screen_data%EmissRedRate, &
                     ns_screen_data%EmissMaxRate, &
                     ns_screen_data%MaxEnergyLimit, &
                     tTYPE, &
                     LINKED_BTRMNT_OPTION, &
                     CO2_CONTROL_PERCENT_ord, &
                     HG_CONTROL_PERCENT_ord, &
                     OTHER3_CONTROL_PERCENT_ord, &
                     CO2_CONTROL_DATE_ord, &
                     HG_CONTROL_DATE_ord, &
                     OTHER3_CONTROL_DATE_ord, &
                     CO2_VOM_ord, &
                     CO2_FOM_ord, &
                     HG_VOM_ord, &
                     HG_FOM_ord, &
                     OTHER3_VOM_ord, &
                     OTHER3_FOM_ord, & !  209
                     mkt_floor, &
                     mkt_ceiling, &
                     START_UP_COSTS_ESCLN, & !  212
                     CAP_MARKET_TYPE, &
                     CAP_mkt_MONTH, &
                     cap_mkt_ptr, &
                     CAPACITY_MKT_COST_ASSIGN, &
                     cap_mkt_exp_collect, &
                     CAP_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CAT, &
                     SECONDARY_FUEL_CAT, &
                     EMISSIONS_FUEL_CAT, &
                     RPS_CONTRIBUTION_PERCENT, &
                     RETIRMENT_CANDIDATE, &
                     rFIT_CANDIDATE, &
                     RETROFIT_proj_ID, &
                     CO2_BASIN_nm, &
                     CO2_PIPELINE_DISTANCE_ord, &
                     STATE_Pvnc_NAME, & !  228
                     CO2_RETRO_HEAT_MULT_ord, &
                     CO2_RETRO_CAP_MULT_ord, &
                     thrm_GUID, &
                     Therm_parent_id, & !  232
                     THRML_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_Yr, &
                     UNIT_TYPE_ord, &
                     UNIT_TYPE_cat, &
                     RPS_PROGRAM_NUMBER

        
        ! After second read

                  ONLINE_loc = 100*(ONLIYR - 1900) + ONLIMO_ord
                  OFLINE_loc = 100*(OFLIYR - 1900) + OFLIMO_ord
                  
        call write_offline_trace("cl_object_ext:0002",ofline_loc)
        
                  CALL CAL_GENERATING_INFORMATION(res_id_loc, &
                                               FRAOWN, &
                                               MW(2), &
                                            MONTHLY_CAP_ptr, &
                                               EFOR_loc, &
                                               MAINRT, &
                                             ONLINE_loc, &
                                             OFLINE_loc, &
                                               UNIT_NAME, &
                                               PRIM_Ft)
               ENDDO
               CLOSE(11)
            ENDIF
         ENDDO ! FILE_ID FOR NUC UNITS
      ENDIF ! NUC UNITS IN FILES
      RETURN



!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.



! OVERLAY THE (FOSSIL-FUELED) CAPACITY-LIMITED-UNITS FILE
! ***************************************************************
      ENTRY CL_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NUMBER)
! ***************************************************************
      IF(.NOT. ACTIVE_BASE_CL_FILES(FILE_NUMBER)) RETURN
      FILE_CODE = CL_FILE_CODES(FILE_NUMBER)
      BINARY_FILE_NAME = CL_FILE_BINARY_NAMES(FILE_NUMBER)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//FILE_CODE//"O"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      ! call write_log_entry("cla_objt:0027", "Reading from " // &
          ! trim(file_name) // " (unit 10).")

      READ(10,*) DELETE
      INUNIT = 12
      NUCLEAR_UNITS_ACTIVE = 0
      IF(FOSSILOL(FILE_NUMBER) == 'BC') THEN
         other_filename=trim(OUTPUT_DIRECTORY())//"BC"// &
                            trim(BINARY_FILE_NAME)//".BIN"

         OPEN(11,FILE=other_filename, &
                           ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      other_filename=trim(OUTPUT_DIRECTORY())//"OL"// &
                            trim(BINARY_FILE_NAME)//".BIN"

      OPEN(12,FILE=other_filename, &
                           ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
            file_name=get_filename_from_unit(10)
            ! call write_log_entry("cla_objt:0100", "Reading from " // &
                ! trim(file_name) // " (unit 11).")

      DO
         DO
            !third read
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,UNIT_NAME, &
               CL_LOAD_TYPE,EXPENSE_ASSGMENT,EXPENSE_cln, &
             GENGRP_ord,FRAOWN,ONLIMO_ord,ONLIYR,OFLIMO_ord,OFLIYR, &
             FUELMX_ord,PRFCST, &
           PRESCR,SEFCST,SDESCR,FUELADJ_ord,EFOR_loc,MAINRT,VARCST, &
           VRESCR, &
               FIXED_COST,FIXED_COST_ESCALATOR, &
               DISPADJ_ord,HRFACT,MW,PLANNING_FACTOR,COEFF, &
               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,AI_ENERGY_RATE, &
               AI_ENERGY_ESCALATOR,AI_REMAINING_LIFE, &
               P_SO2_loc,ns_screen_data%P_NOX,P_PARTICULATES, &
                POOL_FRAC_OWN, &
               P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2_ord, &
               res_id_loc,P_FUEL_SUPPLY_ID, &
            P_NOX_BK2,SEC_EMISS_PR,EMISS_FUEL_COST,EMISS_FUEL_ESCln, &
               EMISS_FUEL_EMISS_PR,EMISS_BLND_RT_ORD, &
               PRIM_FUEL_EMISS_PR, &
               PRIM_Ft,FUEL_TYPES,TIE_GROUP, &
               MONTHLY_CAP_ptr, &
               ANNUAL_CL_FIXED_COST_ord, &
               ANNUAL_CL_FIXED_COST_ESC_ord, &
               MAINT_DAYS_ord,dispatch_mult_ord, &
               EXCESS_ENERGY_SALES_cla, &
               AI_TAX_LIFE, &
               AI_ADR_TAX_LIFE, &
               ASSET_CLASS_NUM_ord, &
               ASSET_CLASS_Vctr, &
               INTRA_COMPANY_CLsid, &
               INTRA_COMPANY_xACTION, &
               spl_UNIT_ID, &
               MINIMUM_CAPACITY_FACTOR_ord, &
               MAXIMUM_CAPACITY_FACTOR_ord, &
               TRANSACTION_GROUP_ID, &
               DAY_TYP_ID, &
               UNIT_ACTIVE_ord, &
               BASECASE_PLNT_ID, &
               BASECASE_UNITid, &
               BASECASE_MARKET_AREAid, &
               BASECASE_TRANS_AREAid, &
               BASECASE_PLANT_NERC_SUBid, &
               BASECASE_PLANT_OWNERid, &
               UTILITY_pWNED, &
               MONTHLY_FUEL_idx, &
               START_UP_COSTS_ord, &
               START_LOGIC_ord, &
               ramp_rate_ord, &
               MIN_DOWN_TIME_ord, &
               REPORT_UNIT_ord, &
               p_fuel_dlv, &
               p_fuel_dlv_2_ord, &
               p_fuel_dlv_3_ord, &
               MIN_UP_TIME_ord, &
               HESI_UNIT_ID_NUM, &
               FOR_FREQ, &
               FOR_DURATN, &
               WINTER_ttL_CAPACITY, &
               CONTRIBs_TO_SPIN, &
               H2_UNIT_ID_NUM, &
               POWERDAT_PLID, &
               APPLY_NOX_SSN_DT_ord, &
               NOX_SEASON_DATE_ord, &
               RAMP_DOWN_RATE_ord, &
               NOX_CONTROL_PERCENT_ord, &
               NOX_CONTROL_DATE_ord, &
               EMERGENCY_CAPACITY_ord, &
               EMERGENCY_HEATRATE_ord, &
               MrkT_RESOURCE, &
               pm_str_loc, &
               COUNTY_ord, &
               STATE_or_PROVINCE, &
               NOX_VOM_ord, &
               NOX_FOM_ord, &
               S_FUEL_DELIVERY_ord, &
               S_FUEL_DELIVERY_2_ord, &
               S_FUEL_DELIVERY_3_ord, &
               const_poly, &
               fst_poly, &
               scnd_poly, &
               thrd_poly, &
               USE_POLY_HT_RATES, &
               NEWGEN_UNIT_STATiS, &
               MONTHLY_MUST_RUN_VECTOR_ord, &
               EMISSION_MARKET_LINK, &
               WVPA_RT_TRACKER, &
               ALLOW_DECOMMIT_ord, &
               MIN_SPIN_CAP_ord, &
               MAX_SPIN_CAP_ord, &
               WVPA_rs_TRACKER, &
               WVPA_FL_TRACKER, &
               MONTE_OR_FREQ_ord, &
               WVPA_MEM_TKER_ord, &
               MONTHLY_DECOMMIT_VECTOR_ord, &
               TRANS_BUSid, &
               SOX_CONTROL_PERCENT_ord, &
               SOX_CONTROL_DATE_ord, &
               SOX_VOM_ord, &
               SOX_FOM_ord, &
               LATITUDE_ord, &
               LONGITUDE_ord, &
               MW_INSIDE_FENCE_ord, &
               INTER_BLOCKS,   &  ! 152-153
               ns_screen_data%FIRST_YEAR_DECOM_AVAIL, &
               ns_screen_data%DECOM_BASE_YR_COST_ord, & !  155
               ns_screen_data%DECOMMISSIONING_COST_ESCALATION, &
               ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR, &
               AGGREGT_THIS_UNIT,& !  158
               name_plate_cap_loc, &
               FUEL_BLENDNG_IS, &
               ns_screen_data%FuelRatio, &
               ns_screen_data%BlendableFuelsPtr, & !  167
               fuel_trans_cost, &
               BlendedEnthUp, &
               BlendedEnthLo, &
               BlendSo2Up,                  & !  177
               BettermentProjId, &
               decom_cont_cost, &
               ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
               ns_screen_data%EnrgPatternPointer, & !  181
               EMISSN_DATA_UNITS, &
               ns_screen_data%EmissRedRate, &
               ns_screen_data%EmissMaxRate, &
               ns_screen_data%MaxEnergyLimit, &
               tTYPE, &
               LINKED_BTRMNT_OPTION, &
               CO2_CONTROL_PERCENT_ord, &
               HG_CONTROL_PERCENT_ord, &
               OTHER3_CONTROL_PERCENT_ord, &
               CO2_CONTROL_DATE_ord, &
               HG_CONTROL_DATE_ord, &
               OTHER3_CONTROL_DATE_ord, &
               CO2_VOM_ord, &
               CO2_FOM_ord, &
               HG_VOM_ord, &
               HG_FOM_ord, &
               OTHER3_VOM_ord, &
               OTHER3_FOM_ord, & !  209
               mkt_floor, &
               mkt_ceiling, &
               START_UP_COSTS_ESCLN,  & !  212
               CAP_MARKET_TYPE, &
               CAP_mkt_MONTH, &
               cap_mkt_ptr, &
               CAPACITY_MKT_COST_ASSIGN, &
               cap_mkt_exp_collect, &
               CAP_MARKET_COIN_ADJ_FACT, &
               PRIMARY_FUEL_CAT, &
               SECONDARY_FUEL_CAT, &
               EMISSIONS_FUEL_CAT, &
               RPS_CONTRIBUTION_PERCENT, &
               RETIRMENT_CANDIDATE, &
               rFIT_CANDIDATE, &
               RETROFIT_proj_ID, &
               CO2_BASIN_nm, &
               CO2_PIPELINE_DISTANCE_ord, &
               STATE_Pvnc_NAME, & !  228
               CO2_RETRO_HEAT_MULT_ord, &
               CO2_RETRO_CAP_MULT_ord, &
               thrm_GUID, &
               Therm_parent_id, & !  232
               THRML_AGGREGATED_UNIT, &
               FIRST_RETIREMENT_Yr, &
               UNIT_TYPE_ord, &
               UNIT_TYPE_cat, &
               RPS_PROGRAM_NUMBER
       ! Third read -

            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,'// &
                           ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                           ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                             ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               TEMP_EXPENSE_COLLECTION = EXPENSE_cln
               ! third read
               READ(RECLN,*,ERR=200) DELETE,UNIT_NAME,CL_LOAD_TYPE, &
                  EXPENSE_ASSGMENT,TEMP_EXPENSE_COLLECTION,GENGRP_ord, &
            FRAOWN,ONLIMO_ord,ONLIYR,OFLIMO_ord,OFLIYR,FUELMX_ord, &
            PRFCST, &
                  PRESCR,SEFCST,SDESCR,FUELADJ_ord,EFOR_loc,MAINRT, &
                  VARCST, &
                     VRESCR,FIXED_COST,FIXED_COST_ESCALATOR, &
                     DISPADJ_ord,HRFACT,MW,PLANNING_FACTOR,COEFF,DESC, &
        P_SO2_loc,ns_screen_data%P_NOX,P_PARTICULATES,POOL_FRAC_OWN, &
                     P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2_ord, &
                     res_id_loc,P_FUEL_SUPPLY_ID, &
                     P_NOX_BK2,SEC_EMISS_PR,EMISS_FUEL_COST, &
                     EMISS_FUEL_ESCln,EMISS_FUEL_EMISS_PR, &
                     EMISS_BLND_RT_ORD,PRIM_FUEL_EMISS_PR, &
                     PRIM_Ft,FUEL_TYPES,TIE_GROUP, &
                     MONTHLY_CAP_ptr, &
                     AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR, &
                     AI_ENERGY_RATE,AI_ENERGY_ESCALATOR, &
                     AI_REMAINING_LIFE, &
                     ANNUAL_CL_FIXED_COST_ord, &
                     ANNUAL_CL_FIXED_COST_ESC_ord, &
                     MAINT_DAYS_ord,dispatch_mult_ord, &
                     EXCESS_ENERGY_SALES_cla, &
                     AI_TAX_LIFE, &
                     AI_ADR_TAX_LIFE, &
                     ASSET_CLASS_NUM_ord, &
                     ASSET_CLASS_Vctr, &
                     INTRA_COMPANY_CLsid, &
                     INTRA_COMPANY_xACTION, &
                     spl_UNIT_ID, &
                     MINIMUM_CAPACITY_FACTOR_ord, &
                     MAXIMUM_CAPACITY_FACTOR_ord, &
                     TRANSACTION_GROUP_ID, &
                     DAY_TYP_ID, &
                     UNIT_ACTIVE_ord, &
                     BASECASE_PLNT_ID, &
                     BASECASE_UNITid, &
                     BASECASE_MARKET_AREAid, &
                     BASECASE_TRANS_AREAid, &
                     BASECASE_PLANT_NERC_SUBid, &
                     BASECASE_PLANT_OWNERid, &
                     UTILITY_pWNED, &
                     MONTHLY_FUEL_idx, &
                     START_UP_COSTS_ord, &
                     START_LOGIC_ord, &
                     ramp_rate_ord, &
                     MIN_DOWN_TIME_ord, &
                     REPORT_UNIT_ord, &
                     p_fuel_dlv, &
                     p_fuel_dlv_2_ord, &
                     p_fuel_dlv_3_ord, &
                     MIN_UP_TIME_ord, &
                     HESI_UNIT_ID_NUM, &
                     FOR_FREQ, &
                     FOR_DURATN, &
                     WINTER_ttL_CAPACITY, &
                     CONTRIBs_TO_SPIN, &
                     H2_UNIT_ID_NUM, &
                     POWERDAT_PLID, &
                     APPLY_NOX_SSN_DT_ord, &
                     NOX_SEASON_DATE_ord, &
                     RAMP_DOWN_RATE_ord, &
                     NOX_CONTROL_PERCENT_ord, &
                     NOX_CONTROL_DATE_ord, &
                     EMERGENCY_CAPACITY_ord, &
                     EMERGENCY_HEATRATE_ord, &
                     MrkT_RESOURCE, &
                     pm_str_loc, &
                     COUNTY_ord, &
                     STATE_or_PROVINCE, &
                     NOX_VOM_ord, &
                     NOX_FOM_ord, &
                     S_FUEL_DELIVERY_ord, &
                     S_FUEL_DELIVERY_2_ord, &
                     S_FUEL_DELIVERY_3_ord, &
                     const_poly, &
                     fst_poly, &
                     scnd_poly, &
                     thrd_poly, &
                     USE_POLY_HT_RATES, &
                     NEWGEN_UNIT_STATiS, &
                     MONTHLY_MUST_RUN_VECTOR_ord, &
                     EMISSION_MARKET_LINK, &
                     WVPA_RT_TRACKER, &
                     ALLOW_DECOMMIT_ord, &
                     MIN_SPIN_CAP_ord, &
                     MAX_SPIN_CAP_ord, &
                     WVPA_rs_TRACKER, &
                     WVPA_FL_TRACKER, &
                     MONTE_OR_FREQ_ord, &
                     WVPA_MEM_TKER_ord, &
                     MONTHLY_DECOMMIT_VECTOR_ord, &
                     TRANS_BUSid, &
                     SOX_CONTROL_PERCENT_ord, &
                     SOX_CONTROL_DATE_ord, &
                     SOX_VOM_ord, &
                     SOX_FOM_ord, &
                     LATITUDE_ord, &
                     LONGITUDE_ord, &
                     MW_INSIDE_FENCE_ord, &
                     INTER_BLOCKS,  &   ! 152-153
                     & ! SP CapEx Variables added 11/17/05 MSG &
                     ns_screen_data%FIRST_YEAR_DECOM_AVAIL, &
                     ns_screen_data%DECOM_BASE_YR_COST_ord,            & !  155
                     ns_screen_data%DECOMMISSIONING_COST_ESCALATION, &
                     ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR, &
                     AGGREGT_THIS_UNIT,               & !  158
                     name_plate_cap_loc, &
                     FUEL_BLENDNG_IS, &
                     ns_screen_data%FuelRatio, &
                     ns_screen_data%BlendableFuelsPtr, & !  167
                     fuel_trans_cost, &
                     BlendedEnthUp, &
                     BlendedEnthLo, &
                     BlendSo2Up, & !  177
                     BettermentProjId, &
                     decom_cont_cost, &
                     ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
                     ns_screen_data%EnrgPatternPointer, & !  181
                     EMISSN_DATA_UNITS, &
                     ns_screen_data%EmissRedRate, &
                     ns_screen_data%EmissMaxRate, &
                     ns_screen_data%MaxEnergyLimit, &
                     tTYPE, &
                     LINKED_BTRMNT_OPTION, &
                     CO2_CONTROL_PERCENT_ord, &
                     HG_CONTROL_PERCENT_ord, &
                     OTHER3_CONTROL_PERCENT_ord, &
                     CO2_CONTROL_DATE_ord, &
                     HG_CONTROL_DATE_ord, &
                     OTHER3_CONTROL_DATE_ord, &
                     CO2_VOM_ord, &
                     CO2_FOM_ord, &
                     HG_VOM_ord, &
                     HG_FOM_ord, &
                     OTHER3_VOM_ord, &
                     OTHER3_FOM_ord, & !  209
                     mkt_floor, &
                     mkt_ceiling, &
                     START_UP_COSTS_ESCLN,  & !  212
                     CAP_MARKET_TYPE, &
                     CAP_mkt_MONTH, &
                     cap_mkt_ptr, &
                     CAPACITY_MKT_COST_ASSIGN, &
                     cap_mkt_exp_collect, &
                     CAP_MARKET_COIN_ADJ_FACT, &
                     PRIMARY_FUEL_CAT, &
                     SECONDARY_FUEL_CAT, &
                     EMISSIONS_FUEL_CAT, &
                     RPS_CONTRIBUTION_PERCENT, &
                     RETIRMENT_CANDIDATE, &
                     rFIT_CANDIDATE, &
                     RETROFIT_proj_ID, &
                     CO2_BASIN_nm, &
                     CO2_PIPELINE_DISTANCE_ord, &
                     STATE_Pvnc_NAME, & !  228
                     CO2_RETRO_HEAT_MULT_ord, &
                     CO2_RETRO_CAP_MULT_ord, &
                     thrm_GUID, &
                     Therm_parent_id, & !  232
                     THRML_AGGREGATED_UNIT, &
                     FIRST_RETIREMENT_Yr, &
                     UNIT_TYPE_ord, &
                     UNIT_TYPE_cat, &
                     RPS_PROGRAM_NUMBER

      ! after third read
      RPS_PROGRAM_NUMBER=RPS_PROGRAM_NUMBER ! debugstop
               IF(INDEX(TEMP_EXPENSE_COLLECTION,'BTL') /= 0) THEN
                  EXPENSE_cln = 'X'
               ELSE
                  EXPENSE_cln = TEMP_EXPENSE_COLLECTION(1:1)
               ENDIF
            ENDIF
            IF(CL_LOAD_TYPE(1:1) == NUCLEAR .AND. DELETE < 8 .AND. &
                     (UNIT_ACTIVE_ord == 'T' .OR. &
                     UNIT_ACTIVE_ord == 't')) THEN
                NUCLEAR_UNITS_ACTIVE = NUCLEAR_UNITS_ACTIVE + 1
            ENDIF
            file_name=get_filename_from_unit(12)
            ! call write_log_entry("cla_objt:0127", "Writing to " // &
                ! trim(file_name) // " (unit 12).")

            ! First write
            WRITE(12,REC=IREC) DELETE,UNIT_NAME, &
               CL_LOAD_TYPE,EXPENSE_ASSGMENT,EXPENSE_cln, &
             GENGRP_ord,FRAOWN,ONLIMO_ord,ONLIYR,OFLIMO_ord,OFLIYR, &
             FUELMX_ord,PRFCST, &
          PRESCR,SEFCST,SDESCR,FUELADJ_ord,EFOR_loc,MAINRT,VARCST, &
          VRESCR, &
               FIXED_COST,FIXED_COST_ESCALATOR, &
               DISPADJ_ord,HRFACT,MW,PLANNING_FACTOR,COEFF, &
               AI_CAPACITY_RATE,AI_CAPACITY_ESCALATOR,AI_ENERGY_RATE, &
               AI_ENERGY_ESCALATOR,AI_REMAINING_LIFE, &
               P_SO2_loc,ns_screen_data%P_NOX,P_PARTICULATES, &
               POOL_FRAC_OWN, &
               P_OTHER2,P_OTHER3,PHASE_I_STR,DISPADJ2_ord, &
               res_id_loc,P_FUEL_SUPPLY_ID, &
            P_NOX_BK2,SEC_EMISS_PR,EMISS_FUEL_COST,EMISS_FUEL_ESCln, &
               EMISS_FUEL_EMISS_PR,EMISS_BLND_RT_ORD, &
               PRIM_FUEL_EMISS_PR, &
               PRIM_Ft,FUEL_TYPES,TIE_GROUP, &
               MONTHLY_CAP_ptr, &
               ANNUAL_CL_FIXED_COST_ord, &
               ANNUAL_CL_FIXED_COST_ESC_ord, &
               MAINT_DAYS_ord,dispatch_mult_ord, &
               EXCESS_ENERGY_SALES_cla, &
               AI_TAX_LIFE, &
               AI_ADR_TAX_LIFE, &
               ASSET_CLASS_NUM_ord, &
               ASSET_CLASS_Vctr, &
               INTRA_COMPANY_CLsid, &
               INTRA_COMPANY_xACTION, &
               spl_UNIT_ID, &
               MINIMUM_CAPACITY_FACTOR_ord, &
               MAXIMUM_CAPACITY_FACTOR_ord, &
               TRANSACTION_GROUP_ID, &
               DAY_TYP_ID, &
               UNIT_ACTIVE_ord, &
               BASECASE_PLNT_ID, &
               BASECASE_UNITid, &
               BASECASE_MARKET_AREAid, &
               BASECASE_TRANS_AREAid, &
               BASECASE_PLANT_NERC_SUBid, &
               BASECASE_PLANT_OWNERid, &
               UTILITY_pWNED, &
               MONTHLY_FUEL_idx, &
               START_UP_COSTS_ord, &
               START_LOGIC_ord, &
               ramp_rate_ord, &
               MIN_DOWN_TIME_ord, &
               REPORT_UNIT_ord, &
               p_fuel_dlv, &
               p_fuel_dlv_2_ord, &
               p_fuel_dlv_3_ord, &
               MIN_UP_TIME_ord, &
               HESI_UNIT_ID_NUM, &
               FOR_FREQ, &
               FOR_DURATN, &
               WINTER_ttL_CAPACITY, &
               CONTRIBs_TO_SPIN, &
               H2_UNIT_ID_NUM, &
               POWERDAT_PLID, &
               APPLY_NOX_SSN_DT_ord, &
               NOX_SEASON_DATE_ord, &
               RAMP_DOWN_RATE_ord, &
               NOX_CONTROL_PERCENT_ord, &
               NOX_CONTROL_DATE_ord, &
               EMERGENCY_CAPACITY_ord, &
               EMERGENCY_HEATRATE_ord, &
               MrkT_RESOURCE, &
               pm_str_loc, &
               COUNTY_ord, &
               STATE_or_PROVINCE, &
               NOX_VOM_ord, &
               NOX_FOM_ord, &
               S_FUEL_DELIVERY_ord, &
               S_FUEL_DELIVERY_2_ord, &
               S_FUEL_DELIVERY_3_ord, &
               const_poly, &
               fst_poly, &
               scnd_poly, &
               thrd_poly, &
               USE_POLY_HT_RATES, &
               NEWGEN_UNIT_STATiS, &
               MONTHLY_MUST_RUN_VECTOR_ord, &
               EMISSION_MARKET_LINK, &
               WVPA_RT_TRACKER, &
               ALLOW_DECOMMIT_ord, &
               MIN_SPIN_CAP_ord, &
               MAX_SPIN_CAP_ord, &
               WVPA_rs_TRACKER, &
               WVPA_FL_TRACKER, &
               MONTE_OR_FREQ_ord, &
               WVPA_MEM_TKER_ord, &
               MONTHLY_DECOMMIT_VECTOR_ord, &
               TRANS_BUSid, &
               SOX_CONTROL_PERCENT_ord, &
               SOX_CONTROL_DATE_ord, &
               SOX_VOM_ord, &
               SOX_FOM_ord, &
               LATITUDE_ord, &
               LONGITUDE_ord, &
               MW_INSIDE_FENCE_ord, &
               INTER_BLOCKS, &     ! 152-153
               & ! SP CapEx Variables added 11/17/05 MSG &
               ns_screen_data%FIRST_YEAR_DECOM_AVAIL, &
               ns_screen_data%DECOM_BASE_YR_COST_ord, & !  155
               ns_screen_data%DECOMMISSIONING_COST_ESCALATION, &
               ns_screen_data%ANNUAL_ENERGY_PLANNING_FACTOR, &
               AGGREGT_THIS_UNIT, & !  158
               name_plate_cap_loc, &
               FUEL_BLENDNG_IS, &
               ns_screen_data%FuelRatio, &
               ns_screen_data%BlendableFuelsPtr, & !  167
               fuel_trans_cost, &
               BlendedEnthUp, &
               BlendedEnthLo, &
               BlendSo2Up, & !  177
               BettermentProjId, &
               decom_cont_cost, &
               ns_screen_data%DECOM_CONTINUING_COST_ESCALATION, &
               ns_screen_data%EnrgPatternPointer, & !  181
               EMISSN_DATA_UNITS, &
               ns_screen_data%EmissRedRate, &
               ns_screen_data%EmissMaxRate, &
               ns_screen_data%MaxEnergyLimit, &
               tTYPE, &
               LINKED_BTRMNT_OPTION, &
               CO2_CONTROL_PERCENT_ord, &
               HG_CONTROL_PERCENT_ord, &
               OTHER3_CONTROL_PERCENT_ord, &
               CO2_CONTROL_DATE_ord, &
               HG_CONTROL_DATE_ord, &
               OTHER3_CONTROL_DATE_ord, &
               CO2_VOM_ord, &
               CO2_FOM_ord, &
               HG_VOM_ord, &
               HG_FOM_ord, &
               OTHER3_VOM_ord, &
               OTHER3_FOM_ord, & !  209
               mkt_floor, &
               mkt_ceiling, &
               START_UP_COSTS_ESCLN,  & !  212
               CAP_MARKET_TYPE, &
               CAP_mkt_MONTH, &
               cap_mkt_ptr, &
               CAPACITY_MKT_COST_ASSIGN, &
               cap_mkt_exp_collect, &
               CAP_MARKET_COIN_ADJ_FACT, &
               PRIMARY_FUEL_CAT, &
               SECONDARY_FUEL_CAT, &
               EMISSIONS_FUEL_CAT, &
               RPS_CONTRIBUTION_PERCENT, &
               RETIRMENT_CANDIDATE, &
               rFIT_CANDIDATE, &
               RETROFIT_proj_ID, &
               CO2_BASIN_nm, &
               CO2_PIPELINE_DISTANCE_ord, &
               STATE_Pvnc_NAME, & !  228
               CO2_RETRO_HEAT_MULT_ord, &
               CO2_RETRO_CAP_MULT_ord, &
               thrm_GUID, &
               Therm_parent_id, & !  232
               THRML_AGGREGATED_UNIT, &
               FIRST_RETIREMENT_Yr, &
               UNIT_TYPE_ord, &
               UNIT_TYPE_cat, &
               RPS_PROGRAM_NUMBER
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      FILE_NAME=get_filename_from_unit(10)
      call write_log_entry("Cla_objt:0023", "Closing " // &
       trim(file_name) // " (unit 10)")

      CLOSE(10)
            call write_log_entry("Cla_objt:0029", "Closing " // &
       trim(file_name) // " (unit 10)")

      CLOSE(12)
      NUC_UNITS_IN_OL_FILE(FILE_NUMBER) = NUCLEAR_UNITS_ACTIVE
      IF(FOSSILOL(FILE_NUMBER) == 'BC') CLOSE(11)
      FOSSILOL(FILE_NUMBER) = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Cla_objt SIID20'
      call end_program(er_message)

! ***************************************************************
      ENTRY SET_FOSSILOL_TO_OL
! ***************************************************************
         DO FILE_ID = 0, MAX_CL_FILES-1
            FOSSILOL(FILE_ID) = 'OL'

            COMMAND1 = trim(OUTPUT_DIRECTORY())//'BC'// &
                           trim(CL_FILE_BINARY_NAMES(FILE_ID))//'.BIN'
            COMMAND2 = trim(OUTPUT_DIRECTORY())//'OL'// &
                           trim(CL_FILE_BINARY_NAMES(FILE_ID))//'.BIN'
            call write_log_entry("cla_objt:2001", &
       "Copying " // trim(Command1) // " to " // trim(command2))
            if(trim(CL_FILE_BINARY_NAMES(FILE_ID))=="FOSIL") then
                COMMAND1=COMMAND1 ! Debugstop
            end if
            CALL COPY_FILE_2_FILE(COMMAND1,COMMAND2) ! debugstop
            NUC_UNITS_IN_OL_FILE(FILE_ID) = NUC_UNITS_IN_FILE(FILE_ID)
         ENDDO
      RETURN
! ***************************************************************
      ENTRY RESET_FOSSILOL
! ***************************************************************
         DO FILE_ID = 0, MAX_CL_FILES-1
            FOSSILOL(FILE_ID) = 'BC'
         ENDDO
      RETURN

! ***************************************************************
      ENTRY OPEN_CL_FILE(UNIT_NO,R_ACTIVE_CL_RECORDS,FILE_NUMBER)
! ***************************************************************
         IF(ACTIVE_CL_RECORDS(FILE_NUMBER) > 0) THEN
            other_filename=trim(OUTPUT_DIRECTORY())// &
                      FOSSILOL(FILE_NUMBER)// &
                      trim(CL_FILE_BINARY_NAMES(FILE_NUMBER))//".BIN"

            OPEN(UNIT_NO,FILE=other_filename, &
                           ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         ENDIF
         R_ACTIVE_CL_RECORDS = MAX(ACTIVE_CL_RECORDS(FILE_NUMBER),0)
      RETURN

! ***************************************************************
      ENTRY RETURN_NUCLEAR_UNITS_ACTIVE(R_NUM_OF_NUCLEAR_UNITS)
! ***************************************************************
         NUCLEAR_UNITS_ACTIVE = 0
         DO FILE_ID = 0, MAX_CL_FILES-1
            NUCLEAR_UNITS_ACTIVE = NUCLEAR_UNITS_ACTIVE &
                                   + NUC_UNITS_IN_OL_FILE(FILE_ID)
         ENDDO
         R_NUM_OF_NUCLEAR_UNITS = NUCLEAR_UNITS_ACTIVE
      RETURN
! ***************************************************************
      ENTRY DOES_CL_FILE_EXIST(R_CL_FILES_ARE_ACTIVE)
! ***************************************************************
         R_CL_FILES_ARE_ACTIVE = CL_FILES_ARE_ACTIVE
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END subroutine cl_object