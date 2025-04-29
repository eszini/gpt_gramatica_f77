
!     MSGOUTPT.FOR
!     Copyright(c) M.S.Gerber & Associates 2000

!     Created: 7/23/2003 10:50:52 AM
!     Author : GREG TURK
!     Last change: msg 12/21/2020 6:44:21 P M



!                                                                     C
!                                                                     C
!                    Reduced Output Set                               C
!                                                                     C
!           Copyright (c) 1991 M.S. Gerber & Associates, Inc.         C
!                       All rights reserved                           C
!                                                                     C
!                                                                     C

module msgoutpt
    use mod_base_year
    use enrglimit
    use msgoutpt_decs
    use abb_capmarketrptdata
    use arrays1
    
    use cap_prod_costs
    use capacity_arrays
    use class_assets_results
    use class_fin_results
    use class_master
    use class_tva_ai_financial
    use ctaxes
    use dsmfrcom
    use end_routine
    use endpoint
    use enrg_prod_costs
    use enrg_prod_costsi4
    use enrg_prod_costsr8
    use envircom
    use fin_results
    use finaclp
    use financial_switches_common
    use fyregycom
    use globecom
    use group_prod
    use grx_planning_routines
    use irec_endpoint_control
    use kepcocom
    use logging
    use mastr
    use midasmod_decs
    use miscmod
    use msgoutpt_data
    use palo_alto_mod
    use params
    use parms
    use procsave_common
    use prod2com
    use prod_arrays_dimensions
    use rptreccontrol
    use servcom
    use sizecom
    use spindriftlib
    use srp_common_stuff
    use tf_decs
    use tva_ai_financial
    use kootenay
    use pp_objt_interfaces
    use logging_decs
    

implicit none
      integer :: reserve_margin_row=0
contains
      subroutine get_variable_value(variable_number,return_value)
         integer (kind=2), intent(in) :: VARIABLE_NUMBER
         real, intent (inout) :: return_value
         
         return_value =  variable_value(variable_number)

      end subroutine get_variable_value
      
      SUBROUTINE OUTPUT(RUN_YEAR,WRITE_THE_RESULTS,ASSET_ANALYST_ONLY)
      use debugtrace
      implicit none

! TEMP HOME UNTIL ADDED TO FINANCIAL OUTPUT

      integer (kind=2) :: plan_number_arg
      

      REAL*8 PRODUCTION_ENRG,CL_GENERATION,CL_TOTAL_MMBTUS, &
             PRODUCTION_FUEL_COST,PRODUCTION_VAR_COST
      REAL FORECAST_YEARLY_ENRG,GET_LAST_THIS_YR_ENERGY
      REAL EMISS_PURCH_CRED_PRICE,EMISS_SELL_CRED_PRICE
      REAL  SRP_TOTAL_SALES,SRP_AVERAGE_SALES_COST,CAWCD_ENERGY, &
            SRP_OPERATING_MARGIN,SRP_AVERAGE_MARGIN,SRP_SALES_COST
      
      
      INTEGER*2 INT2_ZERO/0/
      REAL RETURN_VALUE
      LOGICAL*4 WRITE_THE_RESULTS
      LOGICAL*1 ECON_SALES,ECON_SWITCH,ASSET_ANALYST_ONLY, &
                TRANSACT_ANALYST_ONLY,INIT_FILE_IS_ACTIVE
      CHARACTER*1 WHICH_RESULTS,R_WHICH_RESULTS
      CHARACTER*2 CAPACITY_PLANNING_METHOD,PLANNING_METHOD
      INTEGER*2 VARIABLE_NUMBER
      INTEGER*2 CURRENT_PLANNING_YEAR
      INTEGER*2 R_CURRENT_PLANNING_YEAR,R_RUN_YEAR
      REAL ZERO,NOT_AVAIL,NEAR_ZERO
      PARAMETER(ZERO=0.,NOT_AVAIL=-999999.,NEAR_ZERO=.0000001)
      INTEGER*2 ITEM,COVERAGE_RATIO_TYPE
      INTEGER*2 I,RUN_YEAR,CURRENT_YEAR,J
      REAL  GET_SYSTEM_CAPACITY,GET_INPUT_SYSTEM_PEAK, &
            GET_NUMBER_OF_CL_ADDITIONS,GET_NUMBER_OF_EL_ADDITIONS
      REAL PLANNING_CAPACITY, &
           EL_PLANNING_CAPACITY, &
           LM_PLANNING_CAPACITY, &
           CL_PLANNING_CAPACITY, &
           CT_PLANNING_CAPACITY, &
           DERIV_CAPACITY_PLANNING, &
           PLANNING_CAPACITY_BEING_TESTED, &
           CAPACITY_BEING_TESTED
      REAL GET_ANNUAL_PEAK,ENRG_LIMITED_COST,PLANNING_RESERVE_MARGIN, &
        CLASS_RATES(SYSTEM_CLASS_NUM),AVERAGE_COST,AVERAGE_HEAT_RATE, &
           GET_ENERGY_EXCHANGE_ADJUSTMENT, &
           AI_AFUDC_RATE,AFUDC_BORROWED_RATE, &
           CUSTOMER_DEPOSITS_BAL1, &
           CUSTOMER_DEPOSITS_BAL2, &
           PLANNING_RM_BEFORE_ADDITIONS, &
           FA_AFUDC_RATE,NF_AFUDC_RATE, &
           PLANNING_PEAK_BEFORE_ADDITIONS, &
           GROUP_ECO_SALES(MAX_REPORTING_GROUPS), &
           GROUP_ECO_PURCHASES(MAX_REPORTING_GROUPS)


! ST JOE RATIOS

      REAL TOTAL_INCOME_TAXES, &
           TIMES_INT_B4_INC_TAXES_W_AFDC, &
           TIMES_INT_B4_INC_TAXES_WO_AFDC, &
           INTEREST_PS_DIV, &
           TIMES_INT_PS_B4_INC_TAX_W_AFDC, &
           TIMES_INT_PS_B4_INC_TAX_WO_AFDC, &
           MORTAGE_INDENTURE, &
           ARTICLES_OF_INCORPORATION, &
           DIVIDEND_YIELD, &
           EFFECTIVE_TAX_RATE
      REAL EMBED_COST_CAPITAL_W_STD,EMBED_COST_CAPITAL_WO_STD, &
           SPECIFIED_RETURN_ON_EQUITY


      REAL TRB,RATEBASE_GOPINC



! INCOME ITEMS

      REAL EARNINGS_PER_SHARE,DIVIDENDS_PER_SHARE, &
           DIVIDEND_PAYOUT,INTEREST_COVERAGE_WITH_AFDC,RTEMP, &
           INTEREST_COVERAGE_WITHOUT_AFDC,COMMON_EARNINGS,INTEREST, &
        BOOK_VALUE,MARKET_VALUE,TOTAL_CAPITAL,FUNDS_FOR_CONSTRUCTION, &
           SUM_OPERATIONS,AFDC1_TO_COMMON_EARNINGS,AVERAGE_SHARES, &
           COVERAGE_RATIO_VALUE,NUMERATOR,RETURN_ON_RATEBASE, &
           COVERAGE_DRIVER_RATIO

! FUNDS FLOW ITEMS

      REAL EXTERNAL_FINANCING,SUM_AFUDC,SUM_DEFERRED_ITEMS, &
           EXTERNAL_FINANCING_REQUIRED , &
           SUM_OTHER_PAYMENTS,SUM_CAPITAL_SERVICE

! PRODUCTION ITEMS

      REAL LOAD_MODIFICATION,UNSERVED_ENERGY
      REAL*8 CLASS_MWH(SYSTEM_CLASS_NUM),GET_TOTAL_SALES_ENERGY, &
             GET_TOTAL_SALES_REVENUE
      REAL TOTAL_SALES_REVENUE
      REAL*8 SALES_ENERGY_NOT_IN_FORECAST, &
          ENRG_LIMITED_PURCHASE_ENERGY, &
             GET_SALES_ENRGY_NOT_IN_FORECAST, &
             GET_ENRG_LIMITED_PURCHASE_ENRGY


! SALT RIVER PROJECT VARIABLES

      REAL SRP_FUNDS_FOR_DEBT_SERVICE, &
           SRP_TOTAL_DEBT_SERVICE, &
           SRP_DEBT_SERVICE_COVERAGE_RATIO, &
           SRP_FUNDS_AFTER_DEBT_SERVICE, &
           SRP_FUNDS_FOR_CORP_USE, &
           SRP_NON_DISPATCH_REV

! INCLUDE COMMON BLOCK OF YEARLY SYSTEM FORECAST STATISTICS


      REAL  CAPACITY_PLANNING_PEAK,UPDATE_NET_PLANNING_PEAK, &
            NET_PLANNING_PEAK,AN_DECOMP_PEAK/0./, &
            AN_DECOMP_PLANNING_CAPACITY/0./, &
            TEMP_PEAK
      LOGICAL*1 VOID_LOGOICAL,CURRENT_COVERAGE_RATIO

! RESERVE MARGIN CAPACITY SALES AND PURCHASES




      LOGICAL*1 HOLD_AN_DECOMP_VALUES, &
                  PRICE_FEEDBACK_ACTIVE,RETURN_CONSUMER_SURPLUS_VALUES

!     DECLARATIONS FOR RESERVE MARGIN REPORT
      real (kind=4) :: ep_result, LM_PLANNED_LOAD_CREDIT_dbg
      real (kind=4) :: ADJUSTMENT_PEAK_dbg, ADJUSTMENT_CAPACITY_dbg
      real (kind=4) :: LM_RESOURCE_LOAD_CREDIT_dbg
      real (kind=4) :: PEAK_ADJ_OFF_SYSTEM_SALES_dbg
      real :: OLD_CL_PLANNING_CAPACITY_dbg
      real :: NEW_CL_PLANNING_CAPACITY_dbg, CL_PLANNING_CAPACITY_dbg
      real :: OLD_EL_PLANNING_CAPACITY_dbg, NEW_EL_PLANNING_CAPACITY_dbg

      real :: EL_PLANNING_CAPACITY_dbg, OLD_LM_PLANNING_CAPACITY_dbg
      real :: NEW_LM_PLANNING_CAPACITY_dbg, LM_PLANNING_CAPACITY_dbg
      real :: CAPACITY_PEAKS_dbg, DERIV_CAPACITY_PLANNING_dbg
      real :: CONTRACT_PEAK_ADJUSTMENT_dbg
      real :: CL_PLANNING_LOAD_REDUCTION_dbg, PC_CPP
        
     
      integer (kind=2) :: write_year
      LOGICAL*1 RESERVE_MARGIN_REPORT_NOT_OPEN/.TRUE./,EXPANSION_REPORT
      INTEGER*2 RESERVE_MARGIN_NO,RESERVE_MARGIN_HEADER,REPORT_YEAR
      INTEGER RESERVE_MARGIN_REC
      real :: clpc ! Debug
      REAL*4   LM_RESOURCE_LOAD_CREDIT,LM_PLANNED_LOAD_CREDIT, &
               OLD_CL_PLANNING_CAPACITY,NEW_CL_PLANNING_CAPACITY, &
               OLD_EL_PLANNING_CAPACITY,NEW_EL_PLANNING_CAPACITY, &
               OLD_LM_PLANNING_CAPACITY,NEW_LM_PLANNING_CAPACITY, &
             CAPACITY_PEAKS,ADJUSTMENT_PEAK,CONTRACT_PEAK_ADJUSTMENT, &
               PEAK_ADJ_OFF_SYSTEM_SALES,ADJUSTMENT_CAPACITY, &
               RETURN_RESERVE_MARGIN_RATIO
      real :: CL_PLANNING_LOAD_REDUCTION
      REAL*8 TOTAL_PRODUCTION_COST
      SAVE  RESERVE_MARGIN_NO,CAPACITY_BEING_TESTED,RESERVE_MARGIN_REC
      LOGICAL*1 CAPACITY_PLANNING_IS_ACTIVE/.FALSE./, &
                CAPACITY_PLANNING_ACTIVE
      SAVE DIVIDENDS_PER_SHARE,MARKET_VALUE,COMMON_EARNINGS
      REAL*4 TEMP_SRP_TOTAL_SALES,SRP_OP_FIN_TARGET_DOLLARS, &
             SRP_OP_FIN_MARGIN_DOLLARS,SRP_OP_FIN_TARGET_$_MWH
      CHARACTER (LEN=1) UTILITY_TYPE
      integer :: file_trace_output=0

! DUKE STUFF

      REAL*4 CATAWBA_TOTAL_LEVEL_CAP_PAYMTS
! END OF DATA DECLARATIONS

! !!! WHAT IS THIS? 111507 REMOVED !!!

! 110508. Commented out the RETURN so as to allow routine to work.

!      RETURN
      
      if(file_trace_output==0) then
        file_trace_output=open_trace("output.trace", rq_fto)
        
      end if
      
      IF(.NOT. ASSET_ANALYST_ONLY) THEN
         IF(RUN_YEAR == 1) CAPACITY_PLANNING_IS_ACTIVE = &
                                             CAPACITY_PLANNING_ACTIVE()
         IF(.NOT. CAPACITY_PLANNING_IS_ACTIVE) &
            CAPACITY_PLANNING_PEAK = UPDATE_NET_PLANNING_PEAK(RUN_YEAR)

!     RESERVE MARGIN REPORT

         IF(EXPANSION_REPORT() .AND. WRITE_THE_RESULTS) THEN
            IF(RESERVE_MARGIN_REPORT_NOT_OPEN) THEN
               RESERVE_MARGIN_NO = RESERVE_MARGIN_HEADER( &
                                                    RESERVE_MARGIN_REC)
               RESERVE_MARGIN_REPORT_NOT_OPEN = .FALSE.
            ENDIF
            REPORT_YEAR = MIN(RUN_YEAR,get_globecom_study_period())
            PLANNING_RESERVE_MARGIN = ZERO
            PLANNING_RM_BEFORE_ADDITIONS = ZERO
            PLANNING_CAPACITY = CL_PLANNING_CAPACITY(3,REPORT_YEAR) + &
                                EL_PLANNING_CAPACITY(3,REPORT_YEAR) + &
                                LM_PLANNING_CAPACITY(REPORT_YEAR) + &
                               DERIV_CAPACITY_PLANNING(REPORT_YEAR) + &
                                ADJUSTMENT_CAPACITY(REPORT_YEAR)
            PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
            IF(PLANNING_METHOD == 'SO') THEN ! 053113
                CALL GET_SO_PEAK(RUN_YEAR,TEMP_PEAK)
                CAPACITY_PLANNING_PEAK = TEMP_PEAK
            ELSE
                CAPACITY_PLANNING_PEAK = NET_PLANNING_PEAK(RUN_YEAR)
            ENDIF
            IF(CAPACITY_PLANNING_PEAK /= ZERO)  THEN
               PLANNING_RESERVE_MARGIN = &
            100.*(RETURN_RESERVE_MARGIN_RATIO(CAPACITY_PLANNING_PEAK, &
                                   PLANNING_CAPACITY,REPORT_YEAR) - 1.)
               PLANNING_PEAK_BEFORE_ADDITIONS = &
                           CAPACITY_PEAKS(REPORT_YEAR) - &
                           LM_RESOURCE_LOAD_CREDIT(REPORT_YEAR) + &
                           PEAK_ADJ_OFF_SYSTEM_SALES(REPORT_YEAR)
            PLANNING_RM_BEFORE_ADDITIONS = 100. * (PLANNING_CAPACITY- &
                           NEW_CL_PLANNING_CAPACITY(3,REPORT_YEAR) - &
                           NEW_EL_PLANNING_CAPACITY(3,REPORT_YEAR) - &
                           NEW_LM_PLANNING_CAPACITY(REPORT_YEAR) - &
                           PLANNING_PEAK_BEFORE_ADDITIONS) / &
                           PLANNING_PEAK_BEFORE_ADDITIONS
            ENDIF
            RESERVE_MARGIN_REC = rptrec(RESERVE_MARGIN_NO)
            ! MSGTGRES
            ep_result=prt_endpoint()
            write_year=RUN_YEAR+get_BASE_YEAR()
            OLD_CL_PLANNING_CAPACITY_dbg=OLD_CL_PLANNING_CAPACITY(3,REPORT_YEAR)
            NEW_CL_PLANNING_CAPACITY_dbg=NEW_CL_PLANNING_CAPACITY(3,REPORT_YEAR)
            CL_PLANNING_CAPACITY_dbg=CL_PLANNING_CAPACITY(3,REPORT_YEAR)
            OLD_EL_PLANNING_CAPACITY_dbg=OLD_EL_PLANNING_CAPACITY(3,REPORT_YEAR)
            NEW_EL_PLANNING_CAPACITY_dbg=NEW_EL_PLANNING_CAPACITY(3,REPORT_YEAR)
            EL_PLANNING_CAPACITY_dbg=EL_PLANNING_CAPACITY(3,REPORT_YEAR)
            OLD_LM_PLANNING_CAPACITY_dbg=OLD_LM_PLANNING_CAPACITY(REPORT_YEAR)
            NEW_LM_PLANNING_CAPACITY_dbg= NEW_LM_PLANNING_CAPACITY(REPORT_YEAR)
            LM_PLANNING_CAPACITY_dbg=LM_PLANNING_CAPACITY(REPORT_YEAR)
            DERIV_CAPACITY_PLANNING_dbg=DERIV_CAPACITY_PLANNING(REPORT_YEAR)
            ADJUSTMENT_CAPACITY_dbg=ADJUSTMENT_CAPACITY(REPORT_YEAR)
            CAPACITY_PEAKS_dbg=CAPACITY_PEAKS(REPORT_YEAR)
            LM_PLANNED_LOAD_CREDIT_dbg=LM_PLANNED_LOAD_CREDIT(REPORT_YEAR)
            LM_RESOURCE_LOAD_CREDIT_dbg=LM_RESOURCE_LOAD_CREDIT(REPORT_YEAR)
            ADJUSTMENT_PEAK_dbg=ADJUSTMENT_PEAK(REPORT_YEAR)
            CONTRACT_PEAK_ADJUSTMENT_dbg=CONTRACT_PEAK_ADJUSTMENT(REPORT_YEAR)
            CL_PLANNING_LOAD_REDUCTION_dbg= &
                CL_PLANNING_LOAD_REDUCTION(int(REPORT_YEAR,2))
            PEAK_ADJ_OFF_SYSTEM_SALES_dbg=PEAK_ADJ_OFF_SYSTEM_SALES(REPORT_YEAR)
            PC_CPP=PLANNING_CAPACITY-CAPACITY_PLANNING_PEAK
            
            
            

            reserve_margin_row=reserve_margin_row+1
            
            clpc=NEW_CL_PLANNING_CAPACITY(3,REPORT_YEAR) ! Debugger
            if(report_year==5) then
                report_year=report_year ! Debugstop
            end if

            
            call write_trace_message(file_trace_output, "Writing...")
            if(file_trace_output/=BAD_TRACE_HANDLE) then
                write(file_trace_output, *) &
               "ep_result", ep_result, &
      "year", FLOAT(write_year), &
      "OLD_CL_PLANNING_CAPACITY", &
      OLD_CL_PLANNING_CAPACITY(3,REPORT_YEAR), &
      "NEW_CL_PLANNING_CAPACITY", &
      NEW_CL_PLANNING_CAPACITY(3,REPORT_YEAR), &
      "CL_PLANNING_CAPACITY", CL_PLANNING_CAPACITY(3,REPORT_YEAR), &
      "OLD_EL_PLANNING_CAPACITY", &
      OLD_EL_PLANNING_CAPACITY(3,REPORT_YEAR), &
      "NEW_EL_PLANNING_CAPACITY", &
      NEW_EL_PLANNING_CAPACITY(3,REPORT_YEAR), &
      "EL_PLANNING_CAPACITY", EL_PLANNING_CAPACITY(3,REPORT_YEAR), &
      "OLD_LM_PLANNING_CAPACITY", OLD_LM_PLANNING_CAPACITY(REPORT_YEAR), &
      "NEW_LM_PLANNING_CAPACITY", NEW_LM_PLANNING_CAPACITY(REPORT_YEAR), &
      "LM_PLANNING_CAPACITY", LM_PLANNING_CAPACITY(REPORT_YEAR), &
      "DERIV_CAPACITY_PLANNING", DERIV_CAPACITY_PLANNING(REPORT_YEAR), &
      "ADJUSTMENT_CAPACITY", ADJUSTMENT_CAPACITY(REPORT_YEAR), &
      "PLANNING_CAPACITY", PLANNING_CAPACITY, &
      "CAPACITY_PEAKS", CAPACITY_PEAKS(REPORT_YEAR), &
      "LM_PLANNED_LOAD_CREDIT", LM_PLANNED_LOAD_CREDIT(REPORT_YEAR), &
      "LM_RESOURCE_LOAD_CREDIT", LM_RESOURCE_LOAD_CREDIT(REPORT_YEAR), &
      "LM_PLANNED_LOAD_CREDIT++", &
      LM_PLANNED_LOAD_CREDIT(REPORT_YEAR) + &
      LM_RESOURCE_LOAD_CREDIT(REPORT_YEAR), &
      "ADJUSTMENT_PEAK", ADJUSTMENT_PEAK(REPORT_YEAR), &
      "CONTRACT_PEAK_ADJUSTMENT", CONTRACT_PEAK_ADJUSTMENT(REPORT_YEAR), &
      "CL_PLANNING_LOAD_REDUCTION",  &
      CL_PLANNING_LOAD_REDUCTION(int(REPORT_YEAR,2)), &
      "PEAK_ADJ_OFF_SYSTEM_SALES", &
        PEAK_ADJ_OFF_SYSTEM_SALES(REPORT_YEAR), &
       "CAPACITY_PLANNING_PEAK", CAPACITY_PLANNING_PEAK, &
      "PLANNING_CAPACITY++", PLANNING_CAPACITY - CAPACITY_PLANNING_PEAK, &
       "PLANNING_RM_BEFORE_ADDITIONS", PLANNING_RM_BEFORE_ADDITIONS, &
       "PLANNING_RESERVE_MARGIN", PLANNING_RESERVE_MARGIN
            end if
            
            
            WRITE(RESERVE_MARGIN_NO,REC=RESERVE_MARGIN_REC) &
                           ep_result, &
                           FLOAT(write_year), &
                           OLD_CL_PLANNING_CAPACITY(3,REPORT_YEAR), &
                           NEW_CL_PLANNING_CAPACITY(3,REPORT_YEAR), &
                           CL_PLANNING_CAPACITY(3,REPORT_YEAR), &
                           OLD_EL_PLANNING_CAPACITY(3,REPORT_YEAR), &
                           NEW_EL_PLANNING_CAPACITY(3,REPORT_YEAR), &
                           EL_PLANNING_CAPACITY(3,REPORT_YEAR), &
                           OLD_LM_PLANNING_CAPACITY(REPORT_YEAR), &
                           NEW_LM_PLANNING_CAPACITY(REPORT_YEAR), &
                           LM_PLANNING_CAPACITY(REPORT_YEAR), &
                           DERIV_CAPACITY_PLANNING(REPORT_YEAR), &
                           ADJUSTMENT_CAPACITY(REPORT_YEAR), &
                           PLANNING_CAPACITY, &
                           CAPACITY_PEAKS(REPORT_YEAR), &
                           LM_PLANNED_LOAD_CREDIT(REPORT_YEAR), &
                           LM_RESOURCE_LOAD_CREDIT(REPORT_YEAR), &
                           LM_PLANNED_LOAD_CREDIT(REPORT_YEAR)+ &
                           LM_RESOURCE_LOAD_CREDIT(REPORT_YEAR), &
                           ADJUSTMENT_PEAK(REPORT_YEAR), &
                           CONTRACT_PEAK_ADJUSTMENT(REPORT_YEAR), &
                      CL_PLANNING_LOAD_REDUCTION(int(REPORT_YEAR,2)), &
                           PEAK_ADJ_OFF_SYSTEM_SALES(REPORT_YEAR), &
                           CAPACITY_PLANNING_PEAK, &
                          PLANNING_CAPACITY - CAPACITY_PLANNING_PEAK, &
                           PLANNING_RM_BEFORE_ADDITIONS, &
                           PLANNING_RESERVE_MARGIN

            RESERVE_MARGIN_REC = RESERVE_MARGIN_REC + 1
         ENDIF
      ENDIF

      WHICH_RESULTS = 'X'
      CURRENT_PLANNING_YEAR = RUN_YEAR
      CAPACITY_BEING_TESTED = 0.

      GOTO 10

      ENTRY OUTPUT_AN_DECOMP(RUN_YEAR, &
       R_WHICH_RESULTS,WRITE_THE_RESULTS, &
       R_CURRENT_PLANNING_YEAR,plan_number_arg)

         msgoutpt_elements%PLAN_NUMBER=plan_number_arg
         WHICH_RESULTS = R_WHICH_RESULTS
         CURRENT_PLANNING_YEAR = R_CURRENT_PLANNING_YEAR - &
            get_BASE_YEAR()
         IF(RUN_YEAR < CURRENT_PLANNING_YEAR) THEN
            CAPACITY_BEING_TESTED = 0.
         ELSEIF(RUN_YEAR == CURRENT_PLANNING_YEAR) THEN
            CAPACITY_BEING_TESTED = PLANNING_CAPACITY_BEING_TESTED()
         ENDIF

   10 CURRENT_YEAR = get_BASE_YEAR() + RUN_YEAR
      HOLD_AN_DECOMP_VALUES =  RUN_YEAR > CURRENT_PLANNING_YEAR
      VOID_LOGOICAL = CURRENT_COVERAGE_RATIO(COVERAGE_RATIO_TYPE)
      TOTAL_SALES_REVENUE = SNGL(GET_TOTAL_SALES_REVENUE())
      CALL GET_CLASS_MWH_RATES(CLASS_MWH,CLASS_RATES)
      ENRG_LIMITED_COST = ENRG_LIMITED_FIXED_COST(1) + &
                          ENRG_LIMITED_FIXED_COST(2) + &
                          ENRG_LIMITED_FIXED_COST(3) + &
                          ENRG_LIMITED_VAR_COST(1) + &
                          ENRG_LIMITED_VAR_COST(2) + &
                          ENRG_LIMITED_VAR_COST(3)

      IF(.NOT. ALLOCATED(VARIABLE_VALUE)) then
        ALLOCATE(VARIABLE_VALUE(0:get_LAST_VARIABLE()))
      endif
      VARIABLE_VALUE = 0.
      IF(UTILITY_TYPE() == 'P') THEN

! ENTRY FOR MUNICIPAL UTILITY--PALO ALTO IS THE MODEL
         CALL MUNICIPAL_OUTPUT(VARIABLE_VALUE, &
                 ns_fin_results%INC_FUEL_EXPENSE, &
                 ns_fin_results%INC_VAROM_EXPENSE, &
               ns_fin_results%INC_FIXEDOM_EXPENSE,&
               ns_fin_results%INC_SALES_REVENUES, &
                               MMBTU_FEDERAL_TAX)
      ELSE

! INCOME STATEMENT ITEMS

      ns_mastr%ni = 0.
      COMMON_EARNINGS = ns_mastr%ni - ns_mastr%PSDIV(RUN_YEAR)

      AVERAGE_SHARES = (ns_arrays1%CSSO(2)+ns_arrays1%CSSO(1))/2.
      IF(AVERAGE_SHARES .NE. ZERO) THEN
         EARNINGS_PER_SHARE = COMMON_EARNINGS/AVERAGE_SHARES
         DIVIDENDS_PER_SHARE = ns_mastr%CSDIV/AVERAGE_SHARES
         BOOK_VALUE = ns_arrays1%BKVCS(2)
      ELSE
         EARNINGS_PER_SHARE = ZERO
         DIVIDENDS_PER_SHARE = ZERO
         BOOK_VALUE = ZERO
      ENDIF
      IF(COMMON_EARNINGS .NE. ZERO) THEN

         DIVIDEND_PAYOUT = ns_parms%CSDPR
       AFDC1_TO_COMMON_EARNINGS = 100.*ns_mastr%AFDC1 / COMMON_EARNINGS
      ELSE
         DIVIDEND_PAYOUT = ZERO
         AFDC1_TO_COMMON_EARNINGS = ZERO
      ENDIF

! STANDARD CALCULATION OF INTEREST COVERAGE WITH AND WITHOUT AFUDC

      INTEREST_COVERAGE_WITHOUT_AFDC = NOT_AVAIL
      INTEREST_COVERAGE_WITH_AFDC = NOT_AVAIL
      RTEMP = (ns_mastr%GOPINC+ns_mastr%ITC- &
        ns_mastr%ITCAMT+ns_ctaxes%FDTAXPAID+ns_mastr%AFDC1)
      INTEREST = ns_mastr%LTDINT(RUN_YEAR)+ns_arrays1%STDINT(2)
      IF(INTEREST .NE. ZERO) THEN
         INTEREST_COVERAGE_WITHOUT_AFDC = RTEMP/INTEREST
         INTEREST_COVERAGE_WITH_AFDC = (RTEMP+ns_mastr%AFDC1)/INTEREST
      ENDIF
      COVERAGE_DRIVER_RATIO = NOT_AVAIL
      IF(COVERAGE_RATIO_TYPE == 1) THEN
         INTEREST = ns_mastr%LTDINT(RUN_YEAR) + ns_arrays1%STDINT(2)
         IF(INTEREST /= ZERO) COVERAGE_DRIVER_RATIO = &
            ns_mastr%GOPINC/INTEREST
      ELSEIF(COVERAGE_RATIO_TYPE == 2 .OR. &
                                       COVERAGE_RATIO_TYPE == 3) THEN
         RTEMP = ns_mastr%GOPINC+ns_mastr%ITC-ns_mastr%ITCAMT+ &
         ns_ctaxes%FDTAXPAID+ns_mastr%AFDC1+ns_parms%INTINC+ &
         ns_parms%OTHINC- &
       ns_srp_common_stuff%BTL_FEDERAL_TAXES - &
       ns_srp_common_stuff%BTL_STATE_TAXES
         INTEREST = ns_mastr%LTDINT(RUN_YEAR) + ns_arrays1%STDINT(2)
         IF(COVERAGE_RATIO_TYPE == 2) INTEREST = INTEREST - &
         ns_mastr%AFDC1
         IF(INTEREST /= ZERO) COVERAGE_DRIVER_RATIO=RTEMP/INTEREST
      ELSEIF(COVERAGE_RATIO_TYPE == 8 .OR. &
                                     COVERAGE_RATIO_TYPE == 9) THEN
         RTEMP = ns_mastr%GOPINC+ns_mastr%ITC-ns_mastr%ITCAMT+ &
         ns_ctaxes%FDTAXPAID+ns_mastr%AFDC1+&
         ns_parms%INTINC+ns_parms%OTHINC- &
            ns_srp_common_stuff%BTL_FEDERAL_TAXES - &
            ns_srp_common_stuff%BTL_STATE_TAXES + &
              MAX(0.,ns_parms%LEASE_PAYMENTS_IN_TIER - &
              .02*ns_arrays1%COMEQU(2))/3.
         INTEREST = ns_mastr%LTDINT(RUN_YEAR) + ns_arrays1%STDINT(2) &
         - ns_mastr%AFDC1 + &
              MAX(0.,ns_parms%LEASE_PAYMENTS_IN_TIER - &
              .02*ns_arrays1%COMEQU(2))/3.
       IF(COVERAGE_RATIO_TYPE == 9) INTEREST = INTEREST + ns_mastr%AFDC1
         IF(INTEREST /= ZERO) then
            COVERAGE_DRIVER_RATIO=RTEMP/INTEREST
         endif

        IF(INTEREST+ns_mastr%LTDRET(RUN_YEAR)+ &
        ns_mastr%PSRED(RUN_YEAR) /= ZERO) &
        THEN
            INTEREST_COVERAGE_WITH_AFDC = (RTEMP + ns_mastr%DEPAMT)/ &
           (INTEREST+ns_mastr%LTDRET(RUN_YEAR)+ns_mastr%PSRED(RUN_YEAR))
         INTEREST_COVERAGE_WITHOUT_AFDC = &
            (RTEMP + ns_mastr%DEPAMT -ns_mastr%AFDC1)/ &
          (INTEREST+ns_mastr%LTDRET(RUN_YEAR)+ns_mastr%PSRED(RUN_YEAR))
         ENDIF
      ELSEIF(COVERAGE_RATIO_TYPE == 4 .OR. &
                                         COVERAGE_RATIO_TYPE == 5) THEN
         INTEREST = ns_mastr%LTDINT(RUN_YEAR) + ns_arrays1%STDINT(2) + &
                   ns_mastr%LTDRET(RUN_YEAR) + ns_mastr%PSRED(RUN_YEAR)
         RTEMP = ns_mastr%GOPINC + ns_parms%OTHINC + ns_parms%INTINC + &
         ns_mastr%DEPAMT + &
                        ns_mastr%ITC + ns_mastr%TXDEFP - &
                        ns_mastr%ITCAMT + ns_ctaxes%TXNORM
         IF(INTEREST /= ZERO) COVERAGE_DRIVER_RATIO=RTEMP/INTEREST
      ELSEIF(COVERAGE_RATIO_TYPE == 6 .OR. &
                                         COVERAGE_RATIO_TYPE == 7) THEN
            INTEREST = ns_mastr%LTDINT(RUN_YEAR) + &
                    ns_mastr%LTDRET(RUN_YEAR) + ns_mastr%PSRED(RUN_YEAR)
! NOTE OTNINC IS NET OF BLT_EXPENSES
        RTEMP = ns_mastr%GOPINC + ns_parms%OTHINC + ns_parms%INTINC + & 
                    ns_mastr%TXPROP - ns_arrays1%STDINT(2) + &
                    ns_mastr%BKDPPT + ns_mastr%BDPAFC + ns_mastr%AMTEXP
            IF(INTEREST /= ZERO) COVERAGE_DRIVER_RATIO=RTEMP/INTEREST
            IF(OPMETH == 'C') COVERAGE_DRIVER_RATIO = &
            ns_srp_common_stuff%SRP_RATIO
      ENDIF
      MARKET_VALUE = BOOK_VALUE*ns_parms%MBRATO
      IF(ABS(ns_arrays1%CAPITL(2) + ns_arrays1%STDCUM(2)) &
        > NEAR_ZERO) THEN
      TOTAL_CAPITAL = 100./(ns_arrays1%CAPITL(2) + ns_arrays1%STDCUM(2))
      ELSE
         TOTAL_CAPITAL = ZERO
      ENDIF
      SUM_OPERATIONS = ns_mastr%NOPINC + ns_mastr%DEPAMT + &
      ns_mastr%ITC + &
        ns_mastr%TXDEFP - ns_mastr%ITCAMT - ns_mastr%AFDC1
      IF(ABS(ns_mastr%FPCE + ns_mastr%NFCE) > NEAR_ZERO) THEN
         FUNDS_FOR_CONSTRUCTION = 100.*(SUM_OPERATIONS - &
           ns_mastr%LTDINT(RUN_YEAR) - ns_arrays1%STDINT(2) - &
                          ns_mastr%CSDIV - ns_mastr%PSDIV(RUN_YEAR)) / &
                                         (ns_mastr%FPCE + ns_mastr%NFCE)
      ELSE
         FUNDS_FOR_CONSTRUCTION = ZERO
      ENDIF
      COVERAGE_RATIO_VALUE = ns_mastr%GOPINC + &
      ns_parms%OTHINC + ns_parms%INTINC  + ns_mastr%DEPAMT + &
                       ns_mastr%ITC + ns_mastr%TXDEFP - &
                       ns_mastr%ITCAMT + ns_ctaxes%TXNORM
      NUMERATOR = ns_mastr%LTDINT(RUN_YEAR) + ns_arrays1%STDINT(2) + &
                 ns_mastr%LTDRET(RUN_YEAR) + ns_mastr%PSRED(RUN_YEAR)
      IF(NUMERATOR == ZERO) THEN
         COVERAGE_RATIO_VALUE = ZERO
      ELSE
         COVERAGE_RATIO_VALUE = COVERAGE_RATIO_VALUE/NUMERATOR
      ENDIF

      ENDIF ! IF MUNI ELSE OTHER TYPE OF UTILITY

! COMMON FINANCIAL VARIABLES

      VARIABLE_VALUE(131) = TOTAL_PARTICIPANT_COSTS
      VARIABLE_VALUE(132) = ns_fin_results%INC_DSM_EXPENSE
      VARIABLE_VALUE(133) = ns_fin_results%INC_DSM_REBATE
      VARIABLE_VALUE(324) = EL_SO2_ANNUAL

! PRODUCTION OUTPUT

      IF(ALLOCATED(ENERGY)) THEN
         IF(RUN_YEAR > MAX_SIMULATION_YEARS) THEN
            PRODUCTION_ENRG = ENERGY(MAX_SIMULATION_YEARS)
            CL_GENERATION = TENRG(MAX_SIMULATION_YEARS)
            CL_TOTAL_MMBTUS = TMMBTUS(MAX_SIMULATION_YEARS)
            PRODUCTION_FUEL_COST = FUELCOST(MAX_SIMULATION_YEARS)
            PRODUCTION_VAR_COST= VARCOST(MAX_SIMULATION_YEARS)
         ELSE
            PRODUCTION_ENRG = ENERGY(RUN_YEAR)
            CL_GENERATION = TENRG(RUN_YEAR)
            CL_TOTAL_MMBTUS = TMMBTUS(RUN_YEAR)
            PRODUCTION_FUEL_COST = FUELCOST(RUN_YEAR)
            PRODUCTION_VAR_COST= VARCOST(RUN_YEAR)
         ENDIF
      ELSE
         PRODUCTION_ENRG = 0.
         CL_GENERATION = 0.
         CL_TOTAL_MMBTUS = 0.
         PRODUCTION_FUEL_COST = 0.
         PRODUCTION_VAR_COST= 0.
      ENDIF
      IF(YES_RUN_TRANSACT()) THEN
         FORECAST_YEARLY_ENRG = GET_LAST_THIS_YR_ENERGY(INT2_ZERO)
      ELSEIF(HOLD_AN_DECOMP_VALUES) THEN
         FORECAST_YEARLY_ENRG = FYREGY(CURRENT_PLANNING_YEAR)
      ELSE
         IF(RUN_YEAR > get_globecom_study_period()) THEN
            FORECAST_YEARLY_ENRG = FYREGY(get_globecom_study_period())
         ELSE
            FORECAST_YEARLY_ENRG = FYREGY(RUN_YEAR)
         ENDIF
      ENDIF
      LOAD_MODIFICATION=(FORECAST_YEARLY_ENRG-SNGL(PRODUCTION_ENRG))/ &
                                               1000.
      IF(ABS(LOAD_MODIFICATION) < .01) LOAD_MODIFICATION = ZERO
      SALES_ENERGY_NOT_IN_FORECAST = GET_SALES_ENRGY_NOT_IN_FORECAST()
      ENRG_LIMITED_PURCHASE_ENERGY = GET_ENRG_LIMITED_PURCHASE_ENRGY()
      UNSERVED_ENERGY = SNGL((PRODUCTION_ENRG + &
                              SALES_ENERGY_NOT_IN_FORECAST + &
                              ANNUAL_ECONOMY_SOLD + &
                              STORAGE_ENRG - &
                              ANNUAL_ECONOMY_BOUGHT - &
                              CL_GENERATION - &
                              ENRG_LIMITED_ENRG - &
                              ENRG_LIMITED_PURCHASE_ENERGY - &
                              PURCHASE_ENERGY))/1000.
      IF(ABS(UNSERVED_ENERGY) .LT. .01) UNSERVED_ENERGY = ZERO
      IF(CL_GENERATION + ENRG_LIMITED_ENRG .NE. ZERO) THEN
         AVERAGE_COST = (PRODUCTION_VAR_COST + PRODUCTION_FUEL_COST + &
                    TOTAL_NUCLEAR_FUEL_EXPENSE + ENRG_LIMITED_COST) / &
                          (CL_GENERATION + ENRG_LIMITED_ENRG)
      ELSE
         AVERAGE_COST = ZERO
      ENDIF
      IF(CL_GENERATION .NE. ZERO) THEN
         AVERAGE_HEAT_RATE = CL_TOTAL_MMBTUS/CL_GENERATION*1000.
      ELSE
         AVERAGE_HEAT_RATE = ZERO
      ENDIF

! AVERAGE HEAT RATES

      DO I = 0, MAX_REPORTING_GROUPS
         IF(GROUP_ENERGY(I) .NE. ZERO) THEN
            GROUP_MMBTUS(I) = 1000.*GROUP_MMBTUS(I) / GROUP_ENERGY(I)
         ELSE
            GROUP_MMBTUS(I) = ZERO
         ENDIF
      ENDDO
      IF(ECON_SWITCH() .AND. .NOT. ECON_SALES()) THEN
         CALL GET_GROUP_ECO_SALES_AND_PURCH(GROUP_ECO_SALES, &
                                                   GROUP_ECO_PURCHASES)
      ELSE
         GROUP_ECO_SALES = 0.
         GROUP_ECO_PURCHASES = 0.
      ENDIF

! PLANNING RESERVE MARGIN

      PLANNING_RESERVE_MARGIN = ZERO
      PLANNING_CAPACITY =  CL_PLANNING_CAPACITY(3,RUN_YEAR) + &
                           EL_PLANNING_CAPACITY(3,RUN_YEAR) + &
                           LM_PLANNING_CAPACITY(RUN_YEAR)   + &
                           CT_PLANNING_CAPACITY(3,RUN_YEAR) + &
                           ADJUSTMENT_CAPACITY(RUN_YEAR) + &
                           CAPACITY_BEING_TESTED
      CAPACITY_PLANNING_PEAK = NET_PLANNING_PEAK(RUN_YEAR)
      IF(CAPACITY_PLANNING_PEAK /= ZERO) PLANNING_RESERVE_MARGIN = &
         100. * (PLANNING_CAPACITY - CAPACITY_PLANNING_PEAK)/ &
                            CAPACITY_PLANNING_PEAK

! RESERVE MARGIN COSTS

      msgoutpt_elements%CAPACITY_SALES_TO_LEVEL_RM = 0.
      msgoutpt_elements%CAPACITY_PURCHASES_TO_LEVEL_RM = 0.

      TOTAL_PRODUCTION_COST = (PURCHASE_COSTS + ANNUAL_ECONOMY_COST - &
         ANNUAL_ECONOMY_REVENUE - TOTAL_SALES_REVENUE + &
         PRODUCTION_VAR_COST + PRODUCTION_FUEL_COST + &
         TOTAL_ENRG_LIMITED_VAR_COST + &
         FIXED_OM_COSTS + ENRG_LIMITED_FIXED_COST(1) + &
                          ENRG_LIMITED_FIXED_COST(2) + &
                          ENRG_LIMITED_FIXED_COST(3) + &
         LEASED_NUCLEAR_FUEL_EXPENSE(1) + &
                               LEASED_NUCLEAR_FUEL_EXPENSE(2) + &
                               LEASED_NUCLEAR_FUEL_EXPENSE(3) + &
         OWNED_NUCLEAR_FUEL_EXPENSE(1) + &
                             OWNED_NUCLEAR_FUEL_EXPENSE(2) + &
                             OWNED_NUCLEAR_FUEL_EXPENSE(3))/1000000.

      IF(WABASH_VALLEY) then
        CALL RETURN_ANN_WVPA_VALUES(PURCHASE_COSTS, &
                                    FIXED_OM_COSTS, &
                                    PRODUCTION_FUEL_COST, &
                                    PRODUCTION_VAR_COST, &
                                    TOTAL_PRODUCTION_COST, &
                                    AVERAGE_COST)
      endif

      VARIABLE_VALUE(134) = FORECAST_YEARLY_ENRG/1000
      VARIABLE_VALUE(135) = SNGL(STORAGE_ENRG/1000.)
      VARIABLE_VALUE(136) = SNGL(CL_GENERATION/1000.)
      VARIABLE_VALUE(137) = SNGL(ENRG_LIMITED_ENRG/1000.)
      VARIABLE_VALUE(138) = LOAD_MODIFICATION
      VARIABLE_VALUE(139) = UNSERVED_ENERGY
      VARIABLE_VALUE(140) = SNGL(PRODUCTION_FUEL_COST)/1000000.
      VARIABLE_VALUE(141) = TOTAL_NUCLEAR_FUEL_EXPENSE/1000000.
      VARIABLE_VALUE(142) = ENRG_LIMITED_COST/1000000.
      VARIABLE_VALUE(143) = PURCHASE_COSTS/1000000.
      VARIABLE_VALUE(145) = AVERAGE_COST
      VARIABLE_VALUE(146) = AVERAGE_HEAT_RATE
      VARIABLE_VALUE(147) = SNGL(GET_TOTAL_SALES_ENERGY()/1000.)
      VARIABLE_VALUE(148) = TOTAL_SALES_REVENUE/1000000.
      IF(.NOT. HOLD_AN_DECOMP_VALUES) THEN
! BECAUSE VARIABLE_VALUE IS SAVED IT HOLDS THE FOLLOWING VALUES
!                                        FROM THE CURRENT PLANNING YEAR
         VARIABLE_VALUE(149) = CAPACITY_PLANNING_PEAK
         VARIABLE_VALUE(150) = GET_ANNUAL_PEAK() ! LOAD ADJUSTED PEAK
         VARIABLE_VALUE(151) = GET_SYSTEM_CAPACITY()
         VARIABLE_VALUE(152) = FLOAT(ENRG_LIMITED_CAPACITY)
         VARIABLE_VALUE(153) = PLANNING_RESERVE_MARGIN
         VARIABLE_VALUE(196) = PLANNING_CAPACITY
! ADDED FOR KCPL DSM: FORECAST INPUT PEAK
         VARIABLE_VALUE(328) = GET_INPUT_SYSTEM_PEAK(RUN_YEAR)
         VARIABLE_VALUE(499) = GET_NUMBER_OF_CL_ADDITIONS(RUN_YEAR)
         VARIABLE_VALUE(500) = GET_NUMBER_OF_EL_ADDITIONS(RUN_YEAR)
      ENDIF

! GROUP VARIABLES 154-189

      I = 0
      DO ITEM = 154,184,6
         I = I + 1
         VARIABLE_VALUE(ITEM) = GROUP_CAPACITY(I)
         VARIABLE_VALUE(ITEM+1) = SNGL(GROUP_ENERGY(I))/1000.
         VARIABLE_VALUE(ITEM+2) = SNGL(GROUP_MMBTUS(I))
         VARIABLE_VALUE(ITEM+3) = GROUP_FUEL(I)/1000000.
         VARIABLE_VALUE(ITEM+4) = GROUP_VAROM(I)/1000000.
         VARIABLE_VALUE(ITEM+5) = GROUP_FIXED_OM(I)/1000000.
      ENDDO
      VARIABLE_VALUE(190) = GROUP_CAPACITY(0)
      VARIABLE_VALUE(191) = SNGL(GROUP_ENERGY(0))/1000.
      VARIABLE_VALUE(192) = SNGL(GROUP_MMBTUS(0))
      VARIABLE_VALUE(193) = GROUP_FUEL(0)/1000000.
      VARIABLE_VALUE(194) = GROUP_VAROM(0)/1000000.
      VARIABLE_VALUE(195) = GROUP_FIXED_OM(0)/1000000.

      VARIABLE_VALUE(197) = SNGL(SALES_ENERGY_NOT_IN_FORECAST)/1000.
      VARIABLE_VALUE(198) = SNGL(PURCHASE_ENERGY + &
                                  ENRG_LIMITED_PURCHASE_ENERGY)/1000.
! CLASS_MWH ITEMS 200-206
! CLASS_RATES ITEMS 207-213
      DO I = 1, 7
         VARIABLE_VALUE(199+I) = SNGL(CLASS_MWH(I))/1000.
         VARIABLE_VALUE(206+I) = CLASS_RATES(I)
      ENDDO
      VARIABLE_VALUE(214) = GET_ENERGY_EXCHANGE_ADJUSTMENT()/1000000.

! EMISSION ITEMS 6/20/91

! ITEMS 218-220
      DO I = 1, 3
         VARIABLE_VALUE(217+I) = ANNUAL_EMIS(I)
      ENDDO
! ITEMS 221-241
      DO J = 0, 6
         ITEM = 220 + J * 3
         DO I = 1, 3
            VARIABLE_VALUE(ITEM+I) = GROUP_EMISSIONS(I,J)
         ENDDO
      ENDDO
! ITEMS 242-268
      DO I = 1, 3
         ITEM = 9 * (I-1)
         VARIABLE_VALUE(242+ITEM) = EMIS_CAP(I)
         VARIABLE_VALUE(243+ITEM) = NET_ANN_EMIS(I)
         VARIABLE_VALUE(244+ITEM) = EMIS_PURCH(I)
         VARIABLE_VALUE(245+ITEM) = EMISS_PURCH_CRED_PRICE(I)
         VARIABLE_VALUE(246+ITEM) = EMIS_EXPENSE(I)/1000000.
         VARIABLE_VALUE(247+ITEM) = EMIS_SELL(I)
         VARIABLE_VALUE(248+ITEM) = EMISS_SELL_CRED_PRICE(I)
         VARIABLE_VALUE(249+ITEM) = EMIS_REVENUE(I)/1000000.
         VARIABLE_VALUE(250+ITEM) = CUM_ANN_EMIS(I)
      ENDDO
      VARIABLE_VALUE(269) = INC_NET_EMIS_EXPENSE

! EXTERNALITY PRICES 8/15/90

! ECONOMY INTERCHANGE VARIABLES 1/11/91. 12/3/93 CHANGED UNITS.

      VARIABLE_VALUE(273) = ANNUAL_ECONOMY_COST/1000000.
      VARIABLE_VALUE(274) = ANNUAL_ECONOMY_REVENUE/1000000.
      VARIABLE_VALUE(275) = ANNUAL_ECONOMY_BOUGHT/1000.
      VARIABLE_VALUE(276) = ANNUAL_ECONOMY_SOLD/1000.
! ITEMS 277-290
      DO J = 0, 6
         ITEM = 273 + J * 2
         DO I = 4, 5
            VARIABLE_VALUE(ITEM+I) = GROUP_EMISSIONS(I,J)
         ENDDO
      ENDDO
! ITEMS 291-310
      DO I = 4, 5
         ITEM = 10 * (I-4)
         VARIABLE_VALUE(291+ITEM) = ANNUAL_EMIS(I)
         VARIABLE_VALUE(292+ITEM) = EMIS_CAP(I)
         VARIABLE_VALUE(293+ITEM) = NET_ANN_EMIS(I)
         VARIABLE_VALUE(294+ITEM) = EMIS_PURCH(I)
         VARIABLE_VALUE(295+ITEM) = EMISS_PURCH_CRED_PRICE(I)
         VARIABLE_VALUE(296+ITEM) = EMIS_EXPENSE(I)
         VARIABLE_VALUE(297+ITEM) = EMIS_SELL(I)
         VARIABLE_VALUE(298+ITEM) = EMISS_SELL_CRED_PRICE(I)
         VARIABLE_VALUE(299+ITEM) = EMIS_REVENUE(I)
         VARIABLE_VALUE(300+ITEM) = CUM_ANN_EMIS(I)
      ENDDO
! ADDED FOR FP&L 5/14/92
      VARIABLE_VALUE(331) = (ENRG_LIMITED_VAR_COST(1) + &
                             ENRG_LIMITED_VAR_COST(2) + &
                             ENRG_LIMITED_VAR_COST(3))/1000000.
      VARIABLE_VALUE(332) = (ENRG_LIMITED_FIXED_COST(1) + &
                             ENRG_LIMITED_FIXED_COST(2) + &
                             ENRG_LIMITED_FIXED_COST(3))/1000000.
      VARIABLE_VALUE(333) = FIXED_OM_COSTS/1000000.
      VARIABLE_VALUE(144) = SNGL(PRODUCTION_VAR_COST/1000000.) + &
                            VARIABLE_VALUE(331)
      VARIABLE_VALUE(334) = SNGL(PRODUCTION_VAR_COST/1000000.)
      VARIABLE_VALUE(335) = TOTAL_PRODUCTION_COST
      IF(MAX_REPORTING_GROUPS > 6) THEN
         ITEM = 336
         DO I = 7, MIN(10,MAX_REPORTING_GROUPS)
            VARIABLE_VALUE(ITEM) =  GROUP_CAPACITY(I)
            VARIABLE_VALUE(ITEM+1) = SNGL(GROUP_ENERGY(I))/1000.
            VARIABLE_VALUE(ITEM+2) = SNGL(GROUP_MMBTUS(I))
            VARIABLE_VALUE(ITEM+3) = GROUP_FUEL(I)/1000000.
            VARIABLE_VALUE(ITEM+4) = GROUP_VAROM(I)/1000000.
            ITEM = ITEM + 5
            VARIABLE_VALUE(ITEM) = GROUP_FIXED_OM(I)/1000000.
            DO J = 1, 5
               ITEM = ITEM + 1
               VARIABLE_VALUE(ITEM) = GROUP_EMISSIONS(J,I)
            ENDDO
            ITEM = ITEM + 1
         ENDDO
         ITEM = 402
         DO I = 11, MAX_REPORTING_GROUPS
            VARIABLE_VALUE(ITEM) =  GROUP_CAPACITY(I)
            VARIABLE_VALUE(ITEM+1) = SNGL(GROUP_ENERGY(I))/1000.
            VARIABLE_VALUE(ITEM+2) = SNGL(GROUP_MMBTUS(I))
            VARIABLE_VALUE(ITEM+3) = GROUP_FUEL(I)/1000000.
            VARIABLE_VALUE(ITEM+4) = GROUP_VAROM(I)/1000000.
            ITEM = ITEM + 5
            VARIABLE_VALUE(ITEM) = GROUP_FIXED_OM(I)/1000000.
            DO J = 1, 5
               ITEM = ITEM + 1
               VARIABLE_VALUE(ITEM) = GROUP_EMISSIONS(J,I)
            ENDDO
            ITEM = ITEM + 1
         ENDDO
! LAST VARIABLE IS 456
         VARIABLE_VALUE(380) = &
            ns_mastr%AFDC1-ns_mastr%BTL_STATE_EXEXP_TAX_BENEFIT - &
                                     ns_srp_common_stuff%BTL_STATE_TAXES
        VARIABLE_VALUE(381)=ns_ctaxes%FDTAXPAID- &
            ns_mastr%BTL_FEDERAL_EXEXP_TAX_BENEFIT - &
                                   ns_srp_common_stuff%BTL_FEDERAL_TAXES
         VARIABLE_VALUE(382) = ns_mastr%BTL_STATE_EXEXP_TAX_BENEFIT + &
                                     ns_srp_common_stuff%BTL_STATE_TAXES
        VARIABLE_VALUE(383) = ns_mastr%BTL_FEDERAL_EXEXP_TAX_BENEFIT + &
                                   ns_srp_common_stuff%BTL_FEDERAL_TAXES
         VARIABLE_VALUE(384) = 100.*ns_parms%STTXR
         VARIABLE_VALUE(385) = 100.*ns_parms%FDTXR
        VARIABLE_VALUE(386) = ns_srp_common_stuff%BTL_FEDERAL_TAXES + &
            ns_srp_common_stuff%BTL_STATE_TAXES
      ENDIF

! ADDED 10/25/FOR KEPCO TO REPORT ANNUAL TRANSMISSION, ECT IN THE GROUP
! REPORT

      IF(REALLY_KEPCO) THEN

! GROUP 5 CONTROL AREA 1 WESTPLAINS

            VARIABLE_VALUE(180) = SERVICE_GROUP_COSTS(1,1)
            VARIABLE_VALUE(181) = SERVICE_GROUP_COSTS(2,1)
            VARIABLE_VALUE(236) = SERVICE_GROUP_COSTS(3,1)
            VARIABLE_VALUE(237) = SERVICE_GROUP_COSTS(4,1)
            VARIABLE_VALUE(238) = SERVICE_GROUP_COSTS(5,1)
            VARIABLE_VALUE(287) = SERVICE_GROUP_COSTS(0,1)/1000. + &
                                  GROUP_VAROM(5)/1000000. + &
                                  GROUP_FIXED_OM(5)/1000000.

! GROUP 6 AREA 2 KP&L

            VARIABLE_VALUE(186) = SERVICE_GROUP_COSTS(1,2)
            VARIABLE_VALUE(187) = SERVICE_GROUP_COSTS(2,2)
            VARIABLE_VALUE(239) = SERVICE_GROUP_COSTS(3,2)
            VARIABLE_VALUE(240) = SERVICE_GROUP_COSTS(4,2)
            VARIABLE_VALUE(241) = SERVICE_GROUP_COSTS(5,2)
            VARIABLE_VALUE(289) = SERVICE_GROUP_COSTS(0,2)/1000. + &
                                  GROUP_VAROM(6)/1000000. + &
                                  GROUP_FIXED_OM(6)/1000000.

! GROUP 7 AREA 4 KCP&L

            VARIABLE_VALUE(338) = SERVICE_GROUP_COSTS(1,4)
            VARIABLE_VALUE(339) = SERVICE_GROUP_COSTS(2,4)
            VARIABLE_VALUE(342) = SERVICE_GROUP_COSTS(3,4)
            VARIABLE_VALUE(343) = SERVICE_GROUP_COSTS(4,4)
            VARIABLE_VALUE(344) = SERVICE_GROUP_COSTS(5,4)
            VARIABLE_VALUE(345) = SERVICE_GROUP_COSTS(0,4)/1000. + &
                                  GROUP_VAROM(7)/1000000. + &
                                  GROUP_FIXED_OM(7)/1000000.

! GROUP 8 AREA 3 KG&E

            VARIABLE_VALUE(349) = SERVICE_GROUP_COSTS(1,3)
            VARIABLE_VALUE(350) = SERVICE_GROUP_COSTS(2,3)
            VARIABLE_VALUE(353) = SERVICE_GROUP_COSTS(3,3)
            VARIABLE_VALUE(354) = SERVICE_GROUP_COSTS(4,3)
            VARIABLE_VALUE(355) = SERVICE_GROUP_COSTS(5,3)
            VARIABLE_VALUE(356) = SERVICE_GROUP_COSTS(0,3)/1000. + &
                                  GROUP_VAROM(8)/1000000. + &
                                  GROUP_FIXED_OM(8)/1000000.

! GROUP 9 AREA 5 EMPIRE

            VARIABLE_VALUE(360) = SERVICE_GROUP_COSTS(1,5)
            VARIABLE_VALUE(361) = SERVICE_GROUP_COSTS(2,5)
            VARIABLE_VALUE(357) = SERVICE_GROUP_COSTS(3,5)
            VARIABLE_VALUE(365) = SERVICE_GROUP_COSTS(4,5)
            VARIABLE_VALUE(366) = SERVICE_GROUP_COSTS(5,5)
            VARIABLE_VALUE(367) = SERVICE_GROUP_COSTS(0,5)/1000. + &
                                  GROUP_VAROM(9)/1000000. + &
                                  GROUP_FIXED_OM(9)/1000000.
            VARIABLE_VALUE(368) = GROUP_VAROM(9)/1000000.
            VARIABLE_VALUE(290) = GROUP_FIXED_OM(9)/1000000.

! GROUP 11 AREA 6 SUN FLOWER

            VARIABLE_VALUE(404) = SERVICE_GROUP_COSTS(1,6)
            VARIABLE_VALUE(405) = SERVICE_GROUP_COSTS(2,6)
            VARIABLE_VALUE(406) = SERVICE_GROUP_COSTS(3,6)
            VARIABLE_VALUE(407) = SERVICE_GROUP_COSTS(4,6)
            VARIABLE_VALUE(408) = SERVICE_GROUP_COSTS(5,6)
            VARIABLE_VALUE(409) = SERVICE_GROUP_COSTS(0,6)/1000. + &
                                  GROUP_VAROM(11)/1000000. + &
                                  GROUP_FIXED_OM(11)/1000000.
            VARIABLE_VALUE(410) = GROUP_VAROM(11)/1000000.
            VARIABLE_VALUE(411) = GROUP_FIXED_OM(11)/1000000.

! WOLF CREEK MAINTENANCE EXPENSE 12/23/92 USING UNUSED GROUP VARIALBES

            VARIABLE_VALUE(362) = KEPCO_WC_CURENT_MAINT_ENRG_COST + &
                                          KEPCO_WC_DEF_MAINT_ENRG_AMORT
            VARIABLE_VALUE(363) = KEPCO_WC_CURENT_MAINT_ENRG_COST
            VARIABLE_VALUE(364) = KEPCO_WC_DEF_MAINT_ENRG_AMORT
      ENDIF

! TVA A&I OUTPUT ADDED 9/8/92 MSG FOR DOUG WALTERS

      VARIABLE_VALUE(387) = ns_tva_ai_financial%AI_GPV
      VARIABLE_VALUE(388) = ns_tva_ai_financial%AI_CASH
      VARIABLE_VALUE(389) = ns_tva_ai_financial%AI_DEPRECIATION
      VARIABLE_VALUE(390) = ns_tva_ai_financial%AI_AFUDC1
      VARIABLE_VALUE(391) = ns_tva_ai_financial%AI_EXPENSE
      VARIABLE_VALUE(392) = FA_AFUDC_RATE()*100.
      VARIABLE_VALUE(497) = NF_AFUDC_RATE()*100.
      VARIABLE_VALUE(498) = AI_AFUDC_RATE()*100.
      VARIABLE_VALUE(393) = AFUDC_BORROWED_RATE()*100.
      VARIABLE_VALUE(394) = ns_mastr%TXPROP
      VARIABLE_VALUE(395)=  ns_mastr%TXSS+ &
        ns_mastr%TXOTH+ENVIRONMENTAL_TAX
      VARIABLE_VALUE(396) = COVERAGE_DRIVER_RATIO
      VARIABLE_VALUE(397) = SERVICE_EXPENSES/1000.
! ADDED 10/28/92 FOR KEPCO
      VARIABLE_VALUE(398) = -ns_parms%DEFERRED_EXPENSE_CASH
      VARIABLE_VALUE(399) = ns_parms%DEFERRED_EXPENSE_AMORT - &
                                          KEPCO_WC_DEF_MAINT_ENRG_AMORT
! ADDED 3/16/93 FOR BILL CLINTON
      VARIABLE_VALUE(400) = MMBTU_FEDERAL_TAX
! ADDED 4/3/93 FOR FUN
      VARIABLE_VALUE(401) = ns_parms%BTL_EXPENSES
! ADDED 5/5/93 FOR CUSTOMER DEPOSITS
!      VARIABLE_VALUE(457)=CUSTOMER_DEPOSITS_BAL2
!    VARIABLE_VALUE(458)=CUSTOMER_DEPOSITS_BAL2-CUSTOMER_DEPOSITS_BAL1
! ADDED 9/3/93 FOR KEPCO
      VARIABLE_VALUE(463) = ns_arrays1%INVESTMENTS(1)



! NOTE: 7 ADDITIONAL PASS THROUGH VARIABLE WHERE ADDED FOR TVA 10/6/93

!$ifdefined(remove_4_monthly_midas)
!      CALL RETURN_PASS_THROUGH_VALUES(VARIABLE_VALUE(464))
!$endif

!  LAST PASS THROUGH VARIABLE IS 470

      PRICE_FEEDBACK_ACTIVE = &
                RETURN_CONSUMER_SURPLUS_VALUES(VARIABLE_VALUE(471), &
                      VARIABLE_VALUE(472),VARIABLE_VALUE(473),RUN_YEAR)

!  LAST VARIABLE IS 473
      SRP_FUNDS_FOR_DEBT_SERVICE = ns_mastr%GOPINC + &
                                  ns_parms%INTINC - &
                                  ns_arrays1%STDINT(2) + &
                                  ns_parms%OTHINC + &
                                  ns_mastr%BKDPPT + ns_mastr%BDPAFC + &
                                  ns_mastr%TXPROP + &
                                  ns_mastr%AMTEXP
      SRP_TOTAL_DEBT_SERVICE = ns_mastr%LTDINT(RUN_YEAR) + &
                    ns_mastr%LTDRET(RUN_YEAR) + ns_mastr%PSRED(RUN_YEAR)
      SRP_FUNDS_AFTER_DEBT_SERVICE = SRP_FUNDS_FOR_DEBT_SERVICE - &
                                    SRP_TOTAL_DEBT_SERVICE
      SRP_FUNDS_FOR_CORP_USE = SRP_FUNDS_AFTER_DEBT_SERVICE - &
         ns_mastr%CSDIV - ns_mastr%TXPROP
      IF(SRP_TOTAL_DEBT_SERVICE /= 0.) THEN
        SRP_DEBT_SERVICE_COVERAGE_RATIO = SRP_FUNDS_FOR_DEBT_SERVICE/ &
                                             SRP_TOTAL_DEBT_SERVICE
      ELSE
         SRP_DEBT_SERVICE_COVERAGE_RATIO = NOT_AVAIL
      ENDIF
      VARIABLE_VALUE(474) = SRP_FUNDS_FOR_DEBT_SERVICE
     VARIABLE_VALUE(475) = ns_mastr%LTDRET(RUN_YEAR) + &
     ns_mastr%PSRED(RUN_YEAR)
      VARIABLE_VALUE(476) = SRP_TOTAL_DEBT_SERVICE
      VARIABLE_VALUE(477) = SRP_DEBT_SERVICE_COVERAGE_RATIO
      VARIABLE_VALUE(478) = SRP_FUNDS_AFTER_DEBT_SERVICE
      VARIABLE_VALUE(479) = SRP_FUNDS_FOR_CORP_USE
      VARIABLE_VALUE(480) = (ns_arrays1%LTDCUM(2) + &
        ns_arrays1%STDCUM(2))*TOTAL_CAPITAL
      CALL RETURN_CAWCD_ENERGY(CAWCD_ENERGY)
      SRP_TOTAL_SALES = &
      (SNGL(CLASS_MWH(7)) +GET_TOTAL_SALES_ENERGY() + &
                             ANNUAL_ECONOMY_SOLD + CAWCD_ENERGY)/1000.
      CALL RETURN_SRP_TOTAL_SALES(RUN_YEAR,TEMP_SRP_TOTAL_SALES)

      IF(TEMP_SRP_TOTAL_SALES > 0.) then
          SRP_TOTAL_SALES=TEMP_SRP_TOTAL_SALES
      endif
      SRP_SALES_COST = (ns_mastr%OPEXTX + ns_mastr%CSDIV + &
        ns_parms%BTL_EXPENSES)
      SRP_OPERATING_MARGIN = ns_arrays1%OPREV(2) - &
        ns_mastr%OPEXTX - ns_parms%BTL_EXPENSES
      VARIABLE_VALUE(556) = CAWCD_ENERGY/1000.
      VARIABLE_VALUE(557) = SRP_OPERATING_MARGIN
      VARIABLE_VALUE(559) = SRP_SALES_COST
      IF(SRP_TOTAL_SALES /= 0.) THEN
         SRP_AVERAGE_SALES_COST = 1000. *SRP_SALES_COST/SRP_TOTAL_SALES
         SRP_AVERAGE_MARGIN = 1000. * &
                                 SRP_OPERATING_MARGIN / SRP_TOTAL_SALES
      ELSE
         SRP_AVERAGE_SALES_COST = NOT_AVAIL
         SRP_AVERAGE_MARGIN = NOT_AVAIL
      ENDIF
      VARIABLE_VALUE(558) = SRP_AVERAGE_MARGIN
      VARIABLE_VALUE(551) = SRP_TOTAL_SALES
      VARIABLE_VALUE(552) = SRP_AVERAGE_SALES_COST
      IF(ECON_SWITCH()) THEN
         IF(ECON_SALES()) THEN
            CALL GET_SRP_NON_DISPATCH_REV(SRP_NON_DISPATCH_REV)
         ELSE
            CALL GET_ECON_NON_DISPATCH_REV(SRP_NON_DISPATCH_REV)
         ENDIF
      ELSE
         SRP_NON_DISPATCH_REV = 0.
      ENDIF
      VARIABLE_VALUE(555) = SRP_NON_DISPATCH_REV

      VARIABLE_VALUE(561) = GROUP_ECO_PURCHASES(1)
      VARIABLE_VALUE(562) = GROUP_ECO_PURCHASES(2)
      VARIABLE_VALUE(563) = GROUP_ECO_PURCHASES(3)
      VARIABLE_VALUE(564) = GROUP_ECO_PURCHASES(4)
      VARIABLE_VALUE(565) = GROUP_ECO_PURCHASES(5)
      VARIABLE_VALUE(566) = GROUP_ECO_PURCHASES(6)
      VARIABLE_VALUE(567) = GROUP_ECO_PURCHASES(7)
      VARIABLE_VALUE(568) = GROUP_ECO_PURCHASES(8)
      VARIABLE_VALUE(569) = GROUP_ECO_PURCHASES(9)
      VARIABLE_VALUE(570) = GROUP_ECO_PURCHASES(10)
      VARIABLE_VALUE(571) = GROUP_ECO_PURCHASES(11)
      VARIABLE_VALUE(572) = GROUP_ECO_PURCHASES(12)
      VARIABLE_VALUE(573) = GROUP_ECO_PURCHASES(13)
      VARIABLE_VALUE(574) = GROUP_ECO_PURCHASES(14)
      VARIABLE_VALUE(575) = GROUP_ECO_PURCHASES(15)


! DSM VALUE TEST VARIABLES

      VARIABLE_VALUE(481) = TOTAL_PARTICIPANT_COSTS
      VARIABLE_VALUE(482) = TOTAL_UTIL_NON_ELEC_COSTS
      VARIABLE_VALUE(483) = TOTAL_THIRD_PARTY_COSTS
      VARIABLE_VALUE(484) = TOTAL_OTH_PARTICIPANT_COSTS

! RESERVE MARGIN LEVELIZING COST VARIABLES

      VARIABLE_VALUE(485) = &
        msgoutpt_elements%CAPACITY_PURCHASES_TO_LEVEL_RM
      VARIABLE_VALUE(486) = &
        msgoutpt_elements%CAPACITY_SALES_TO_LEVEL_RM


! 6/24/94. GAT. ANNUAL EMISSIONS REPORTING VARIABLES

      CALL RETURN_ANN_EMIS_RPT_VARIABLES(VARIABLE_VALUE(487))

      IF(INIT_FILE_IS_ACTIVE()) THEN
         CALL RETURN_ASSET_CLASS_VALUES(VARIABLE_VALUE, &
                                        COVERAGE_RATIO_TYPE, &
                                        OPMETH, &
                                      ns_parms%LEASE_PAYMENTS_IN_TIER, &
                                        ns_srp_common_stuff%SRP_RATIO)
         CALL SRP_REPORTING_VARIABLES(RUN_YEAR, &
                                      VARIABLE_VALUE, &
                                      SNGL(CLASS_MWH(7)), &
                                      ANNUAL_ECONOMY_SOLD)
         CALL SEC_RATIOS(VARIABLE_VALUE)
      ENDIF
      call set_west_kootenay_power_ACTIVE(WKP_OBJECT(VARIABLE_VALUE))
      IF(WRITE_THE_RESULTS .AND. WHICH_RESULTS /= 'A') THEN
         CALL OUTPUT_RESULTS(VARIABLE_VALUE,CURRENT_YEAR)
      ELSEIF(WRITE_THE_RESULTS .AND. WHICH_RESULTS == 'A') THEN
         CALL OUTPUT_AN_DECOMP_RESULTS(VARIABLE_VALUE,CURRENT_YEAR, &
          R_CURRENT_PLANNING_YEAR)
      ENDIF

      IF(.NOT. ASSET_ANALYST_ONLY) &
                  CALL GET_TRANSACT_VARIABLES(VARIABLE_VALUE(650))

      RETURN



    end subroutine output
    SUBROUTINE MUNICIPAL_OUTPUT(VARIABLE_VALUE, &
                    INC_FUEL_EXPENSE_ord,INC_VAROM_EXPENSE_ord, &
                               INC_FIXEDOM_EXPENSE_ord, &
                                  INC_SALES_REVENUES_ord, &
                                  MMBTU_FEDERAL_TAX_ord)
      

      implicit none
      
      INTEGER (kind=2) :: I
      REAL :: VARIABLE_VALUE(0:*)
      REAL :: INC_FUEL_EXPENSE_ord,INC_VAROM_EXPENSE_ord, &
        INC_FIXEDOM_EXPENSE_ord, &
          INC_SALES_REVENUES_ord,MMBTU_FEDERAL_TAX_ord
! BUDGET REPORT
! REVENUES
      VARIABLE_VALUE( 0) = SALES_REVENUES + ADDITIONAL_REVENUES
      VARIABLE_VALUE( 1) = ADDENDUM_TO_SALES_REVENUE
      VARIABLE_VALUE(82) = SERVICE_REVENUES/1000.
      VARIABLE_VALUE( 2) = OTHER_REVENUE
      VARIABLE_VALUE( 3) = REVENUE_CONTRACTING_AGENCIES
      VARIABLE_VALUE( 4) = BOND_REVENUE
      VARIABLE_VALUE( 5) = INTEREST_REVENUE
      VARIABLE_VALUE( 6) = REVENUES
      VARIABLE_VALUE( 7) = FROM_RESERVES
      VARIABLE_VALUE( 8) = REVENUES + FROM_RESERVES + BONDS_ISSUED
! USES
      VARIABLE_VALUE( 9) = PURCHASED_EXPENSES
      VARIABLE_VALUE(10) = OPERATING_EXPENSES
      VARIABLE_VALUE(81) = SERVICE_EXPENSES/1000
      VARIABLE_VALUE(11) = CONTINGENCY_EXPENSES
      VARIABLE_VALUE(12) = ADMINISTRATION_EXPENSES
      VARIABLE_VALUE(13) = ALLOCATED_EXPENSES
      VARIABLE_VALUE(14) = OTHER_EXPENSE
      VARIABLE_VALUE(15) = TOTAL_CIP_EXPENDITURES
      VARIABLE_VALUE(16) = DSM_EXPENDITURES
      VARIABLE_VALUE(17) = LOAD_MANAGEMENT_EXPENDITURES
      VARIABLE_VALUE(18) =TOTAL_CIP_EXPENDITURES+TOTAL_DSM_EXPENDITURES

! DEBT SERVICE CALCULATION

      VARIABLE_VALUE(19) = CPA_SHARE_OF_NCPA_DS
      VARIABLE_VALUE(20) = CPA_SHARE_OF_TANC_DS
      VARIABLE_VALUE(21) = OTHER_DEBT_SERVICE
      VARIABLE_VALUE(22) = CPA_SHARE_OF_NCPA_DS + CPA_SHARE_OF_TANC_DS
      VARIABLE_VALUE(23) = DEBT_SERVICE

! CASH TRANSFERS

      VARIABLE_VALUE(24) = RENT_EXPENSE
      VARIABLE_VALUE(25) = CIP_TO_GENERAL_FUND
      VARIABLE_VALUE(26) = ALLOCATED_CHARGES
      VARIABLE_VALUE(27) = OTHER_TRANSFER
      VARIABLE_VALUE(28) = TRANSFER_TO_GENERAL_FUND
      VARIABLE_VALUE(29) = CASH_TRANSFERS + TRANSFER_TO_GENERAL_FUND
      VARIABLE_VALUE(30) = TO_RESERVES
      VARIABLE_VALUE(31) = TO_SPECIAL_RESERVE
      VARIABLE_VALUE(32)= TOTAL_USES + TO_RESERVES + TO_SPECIAL_RESERVE

! RATE BASE ITEMS

      VARIABLE_VALUE(33) = FIXED_ASSETS(1)
      VARIABLE_VALUE(34) = CONTRIBUTED_CIP_IN_RB
      VARIABLE_VALUE(35) = DEPRECIATION
      VARIABLE_VALUE(36) = FIXED_ASSETS(2)
      VARIABLE_VALUE(37) = AVERAGE_FIXED_ASSETS
    VARIABLE_VALUE(38) = (INC_FUEL_EXPENSE_ord + PURCHASED_EXPENSES) * &
                                           PURCHASE_EXPENSES_IN_WC/100.
      VARIABLE_VALUE(39) = &
         (OPERATING_EXPENSES+ &
         ns_fin_results%INC_VAROM_EXPENSE+INC_FIXEDOM_EXPENSE_ord) * &
                                          OPERATING_EXPENSES_IN_WC/100.
      VARIABLE_VALUE(40) = CONTINGENCY_EXPENSES * &
                                        CONTINGENCY_EXPENSES_IN_WC/100.
      VARIABLE_VALUE(41) = ADMINISTRATION_EXPENSES * &
                                     ADMINISTRATION_EXPENSES_IN_WC/100.
      VARIABLE_VALUE(42) = ALLOCATED_EXPENSES * &
                                          ALLOCATED_EXPENSES_IN_WC/100.
      VARIABLE_VALUE(43) = OTHER_EXPENSE * OTHER_EXPENSE_IN_WC/100.
      VARIABLE_VALUE(44) = RENT_EXPENSE * RENT_IN_WORKING_CAPITAL/100.
      VARIABLE_VALUE(45) = ALLOCATED_CHARGES * &
                                           ALLOCATED_CHARGES_IN_WC/100.
      VARIABLE_VALUE(46) = OTHER_TRANSFER * OTHER_TRANSFER_IN_WC/100.
      VARIABLE_VALUE(47) = TOTAL_CIP_EXPENDITURES * &
                                               CIP_ALLOCATED_TO_WC/100.
      VARIABLE_VALUE(48) = DSM_EXPENDITURES * DSM_IN_WC/100.
      VARIABLE_VALUE(49) = LOAD_MANAGEMENT_EXPENDITURES * &
                                             LOAD_MANAGEMENT_IN_WC/100.
      VARIABLE_VALUE(50) = ADDENDUM_TO_WORKING_CAPITAL
      VARIABLE_VALUE(51) = WORKING_CAPITAL_IN_RB
      VARIABLE_VALUE(52) = RATE_BASE
      VARIABLE_VALUE(53) = TRANSFER_TO_GENERAL_FUND_RATE
      VARIABLE_VALUE(54) = RATE_BASE*TRANSFER_TO_GENERAL_FUND_RATE/100.

! RESERVE BALANCES

      VARIABLE_VALUE(55) = MINIMUM_RESERVE_BALANCE
      VARIABLE_VALUE(56) = MAXIMUM_RESERVE_BALANCE
      VARIABLE_VALUE(57) = TARGET_RESERVE_BALANCE
      VARIABLE_VALUE(58) = TOTAL_RESERVE_BALANCE(1)
      VARIABLE_VALUE(59) = TOTAL_RESERVE_BALANCE(2)
      VARIABLE_VALUE(60) = TOTAL_RESERVE_BALANCE(2) - &
                                               TOTAL_RESERVE_BALANCE(1)
      VARIABLE_VALUE(61) = (TOTAL_RESERVE_BALANCE(2) + &
                                           TOTAL_RESERVE_BALANCE(1))/2.
      VARIABLE_VALUE(62) = SPECIAL_RESERVE_BALANCE(1)
      VARIABLE_VALUE(63) = SPECIAL_RESERVE_BALANCE(2)
      VARIABLE_VALUE(64) = SPECIAL_RESERVE_BALANCE(2) - &
                                            SPECIAL_RESERVE_BALANCE(1)
      VARIABLE_VALUE(65) = SYSTEM_AVERAGE_RATE(2)
      VARIABLE_VALUE(66) = SALES_REVENUES
      VARIABLE_VALUE(67) = ADDITIONAL_REVENUES
      VARIABLE_VALUE(68) = RATE_CHANGE
      VARIABLE_VALUE(69) = MINIMUM_ANNUAL_RATE_ADJUSTMENT
      VARIABLE_VALUE(70) = MAXIMUM_ANNUAL_RATE_ADJUSTMENT
      VARIABLE_VALUE(71) = BONDS_ISSUED
      VARIABLE_VALUE(72) = DSM_CASH
      VARIABLE_VALUE(73) = INC_FUEL_EXPENSE_ord
      VARIABLE_VALUE(74) = INC_VAROM_EXPENSE_ord
      VARIABLE_VALUE(75) = INC_FIXEDOM_EXPENSE_ord
      VARIABLE_VALUE(76) = &
        OPERATING_EXPENSES + INC_VAROM_EXPENSE_ord + &
                           INC_FIXEDOM_EXPENSE_ord
      VARIABLE_VALUE(77) = CALAVERAS_RESERVE_BALANCE
      VARIABLE_VALUE(78) = INC_SALES_REVENUES_ord
      VARIABLE_VALUE(79) = MMBTU_FEDERAL_TAX_ord
      VARIABLE_VALUE(80) = INC_FUEL_EXPENSE_ord - MMBTU_FEDERAL_TAX_ord
      DO I = 83, get_last_variable()
         VARIABLE_VALUE(I) = 0.
      ENDDO
      VARIABLE_VALUE(199) = SYSTEM_AVERAGE_RATE(2)
      VARIABLE_VALUE(270) = X_PRICE_MUNI_1
      VARIABLE_VALUE(271) = X_PRICE_MUNI_2
      VARIABLE_VALUE(272) = X_PRICE_MUNI_3

! FLIP TIME DEPENDED VARIABLES

      TOTAL_RESERVE_BALANCE(1) = TOTAL_RESERVE_BALANCE(2)
      SPECIAL_RESERVE_BALANCE(1) = SPECIAL_RESERVE_BALANCE(2)
      FIXED_ASSETS(1) = FIXED_ASSETS(2)
      SYSTEM_AVERAGE_RATE(1) = SYSTEM_AVERAGE_RATE(2)
      RETURN
      END subroutine MUNICIPAL_OUTPUT


! WKP OBJECT ADDED 7/28/93 M.S.G. ALL RIGHTS OWNED BY M.S.G.

      FUNCTION WKP_OBJECT(VARIABLE_VALUE)
      use kootenay
      
      REAL VARIABLE_VALUE(0:*),PSRED,LTDRET
      CHARACTER*1 UTILITY_TYPE
      LOGICAL*1 WKP_OBJECT
                

      WKP_OBJECT = get_west_kootenay_power_active()
      IF(.NOT. WKP_OBJECT) RETURN
         PSRED = VARIABLE_VALUE(115)
         LTDRET = VARIABLE_VALUE(116)
! WKP NEW BALANCE SHEET VARIABLES
! OLD CURRENT ASSETS
         VARIABLE_VALUE(10) = &
! ASSETS - &
                              VARIABLE_VALUE(11) - &
! NPV(2) - &
                              VARIABLE_VALUE(5) - &
! (DDCUM - DDAMTC(2)) &
                              VARIABLE_VALUE(7)
! OLD COMMON EQUITY NOW SHAREHOLDER'S EQUITY
         VARIABLE_VALUE(14) = &
! CSCUM(2) + &
                              VARIABLE_VALUE(12) + &
! REARNC(2) + &
                              VARIABLE_VALUE(13) + &
! PSCUM(2) &
                              VARIABLE_VALUE(15)
! OLD OTHER LT LIABS NOW TOTAL DEFERRED CREDITS
       VARIABLE_VALUE(18) = VARIABLE_VALUE(20) + VARIABLE_VALUE(21) + &
                                                    VARIABLE_VALUE(459)
! OLD LIABS NEC NOW CURRENT LIABILITIES
       VARIABLE_VALUE(22) = VARIABLE_VALUE(23) - VARIABLE_VALUE(18) - &
                                                    VARIABLE_VALUE(17)
! WKP INCOME STATEMENT VARIABLES
! OLD TOTAL OPERATING EXPENSES/NEW TOTAL EXPENSES
         VARIABLE_VALUE(41) = VARIABLE_VALUE(41) + &
! Property Tax,V394 &
                              VARIABLE_VALUE(394) + &
! Provincial Capital Tax,V329 &
                              VARIABLE_VALUE(329)
! OLD OPERATING INCOME/NEW EARNINGS FROM OPERATIONS
! TOTAL REVENUES,V30
         VARIABLE_VALUE(50) = VARIABLE_VALUE(30) - &
! Total Expenses,V41 New variable &
                              VARIABLE_VALUE(41)
! OLD INCOME BEFORE INTEREST/NEW EARNINGS FROM OPERATIONS PLUS AFUDC
! OLD INCOME BEFORE INTEREST,V54

         VARIABLE_VALUE(112) = VARIABLE_VALUE(112) !TOTAL AFUDC
! EARNINGS FROM OPTS,V50
         VARIABLE_VALUE(54) = VARIABLE_VALUE(50) + &
! AFUDC,V112 &
                              VARIABLE_VALUE(112)
! TOTAL INTEREST,V58
! INTEREST ON STD,V56
! INTEREST ON LTD,V55 &
  VARIABLE_VALUE(58) = VARIABLE_VALUE(56) + VARIABLE_VALUE(55)
  
! EARNINGS BEFORE INCOME TAXES/OLD INTEREST IN FUNDS FLOW
  VARIABLE_VALUE(114) = VARIABLE_VALUE(54) - VARIABLE_VALUE(58)
! TOTAL TAXES/OLD FUNDS FLOW V109
         VARIABLE_VALUE(109) = VARIABLE_VALUE(330) + &
                               VARIABLE_VALUE(381) + &
                      VARIABLE_VALUE(48)  + & !I & !  TAX REFUND CREDIT
                               VARIABLE_VALUE(47) - VARIABLE_VALUE(112)
! WKP FUNDS FLOW STATEMENT


! CHANGE THE WORKING CAPITOL SIGN
         ! HOLD OVER FROM - OUTFLOW 9/22/97
         VARIABLE_VALUE(327) = -VARIABLE_VALUE(119) 
! NEW TOTAL ITEMS NOT AFFECTING CASH/TOTAL FUNDS PROVIDED BY 
! OPERATIONS,V113

         VARIABLE_VALUE(111) = VARIABLE_VALUE(47) - VARIABLE_VALUE(112)
         VARIABLE_VALUE(113) = VARIABLE_VALUE(39) + &
                               VARIABLE_VALUE(40) + &
                               VARIABLE_VALUE(327) + &
                               VARIABLE_VALUE(111)
! TOTAL CASH AND ITEMS NOT AFFECTING CASH/OLD NEG PREFERRED DIVIDENDS
         VARIABLE_VALUE(115) = VARIABLE_VALUE(61) + &
                               VARIABLE_VALUE(113)
! TOTAL DIVIDENDS/OLD TOTAL CAPITAL SERVICE PAYMENTS,V117
! Preferred Dividends,V62 &
    VARIABLE_VALUE(117) = VARIABLE_VALUE(62) + VARIABLE_VALUE(64)

! CASH AFTER DIVIDENDS/OLD NEG COMMON DIVIDNEDS,V116
         VARIABLE_VALUE(116) = &
                               VARIABLE_VALUE(115) - &
                               VARIABLE_VALUE(117)
! PREFERRED STOCK NET OF RETIREMENTS
         VARIABLE_VALUE(126) = &
                               VARIABLE_VALUE(126) - &
                               PSRED
! LTD NET OF RETIREMENTS
         VARIABLE_VALUE(127) = &
                               VARIABLE_VALUE(127) - &
                               LTDRET
! SUM OF NET CASH FROM FINANCING
         VARIABLE_VALUE(123) = VARIABLE_VALUE(124) + &
                               VARIABLE_VALUE(125) + &
                               VARIABLE_VALUE(126) + &
                               VARIABLE_VALUE(127) + &
                               VARIABLE_VALUE(128)
! PLANT & EQUITPMENT LESS CIAC
         VARIABLE_VALUE(326) = VARIABLE_VALUE(326) - &
                                   VARIABLE_VALUE(460)
         VARIABLE_VALUE(129) = -VARIABLE_VALUE(129) !Current Assets
         VARIABLE_VALUE(398) = VARIABLE_VALUE(398)  !Deferred Cash
! TOTAL CASH USED OF INVESTMENT/OLD External Financing Required,V122
         VARIABLE_VALUE(122) = VARIABLE_VALUE(326) + &
                               VARIABLE_VALUE(325) + &
                               VARIABLE_VALUE(398) + &
                               VARIABLE_VALUE(129)
      RETURN



      END FUNCTION WKP_OBJECT
      



! BASE YEAR FINANCIAL OUTPUT MOVED FROM MUNIREV AND FINANBSE 7/29/93
! AND COMBINED WITH THE NORMAL OUTPUT

      SUBROUTINE OUTPUT_RESULTS(var_val,CURRENT_YEAR)
      use msgoutpt_decs
      use tf_objt_interfaces
      implicit none
      REAL var_val(:)
      INTEGER*2 CURRENT_YEAR

      INTEGER*2 CURRENT_PLANNING_YEAR,PLAN_NUMBER
      INTEGER*2 GET_AN_DECOMP_VARIABLES
      INTEGER*4 ERROR
      
      
      
      
         IF(msgoutpt_elements%SHORT_FORM_ACTIVE) THEN
            ALLOCATE( &
                msgoutpt_elements%OUTPUT_VARIABLE( &
                    msgoutpt_elements%SHFRM_VARIABLES_USED),STAT=ERROR)
                    
      msgoutpt_elements%SHFRM_VARIABLES_USED= &
        GET_SHORT_FORM_VARIABLES(var_val)
            CALL WRITE_RESULTS(int(CURRENT_YEAR,2), &
                int(msgoutpt_elements%SHFRM_VARIABLES_USED,2), &
                 msgoutpt_elements%OUTPUT_VARIABLE)
            DEALLOCATE(msgoutpt_elements%OUTPUT_VARIABLE,STAT=ERROR)
         ELSE
            CALL WRITE_RESULTS(CURRENT_YEAR, &
             msgoutpt_elements%BIFP_VARIABLES_USED,var_val)
         ENDIF
      END SUBROUTINE OUTPUT_RESULTS

      subroutine OUTPUT_AN_DECOMP_RESULTS(var_val,CURRENT_YEAR, &
         CURRENT_PLANNING_YEAR)
         REAL :: var_val(0:*)
         integer (kind=2) :: current_year, CURRENT_PLANNING_YEAR
         integer :: error

         ALLOCATE(msgoutpt_elements%OUTPUT_VARIABLE( &
            msgoutpt_elements%AN_DECOMP_VARIABLES_USED),STAT=ERROR &
         )
         msgoutpt_elements%AN_DECOMP_VARIABLES_USED = &
         GET_AN_DECOMP_VARIABLES( &
          msgoutpt_elements%OUTPUT_VARIABLE, &
          var_val)
         CALL WRITE_DECOM_RESULTS(CURRENT_YEAR,CURRENT_PLANNING_YEAR, &
                  msgoutpt_elements%PLAN_NUMBER, &
                  msgoutpt_elements%AN_DECOMP_VARIABLES_USED, &
                  msgoutpt_elements%OUTPUT_VARIABLE)
         DEALLOCATE(msgoutpt_elements%OUTPUT_VARIABLE,STAT=ERROR)
      end subroutine OUTPUT_AN_DECOMP_RESULTS

      subroutine SAVE_SHORT_FORM_ACTIVE(SF)
         logical (kind=1) :: SF
         msgoutpt_elements%short_form=sf
         msgoutpt_elements%SHORT_FORM_ACTIVE = &
            msgoutpt_elements%SHORT_FORM
         msgoutpt_elements%BIFP_VARIABLES_USED = get_LAST_VARIABLE()+1
         msgoutpt_elements%SHFRM_VARIABLES_USED = &
            msgoutpt_elements%BIFP_VARIABLES_USED
         msgoutpt_elements%AN_DECOMP_VARIABLES_USED = &
            msgoutpt_elements%BIFP_VARIABLES_USED
      end subroutine SAVE_SHORT_FORM_ACTIVE
      
      


      FUNCTION SET_SHORT_FORM_VARIABLES(SHORT_FORM_FILE_NAME)

      
      implicit none
      
      LOGICAL*1 VARIABLE_ACTIVE(0:1023) ! CHG 8/22/94. GAT.
      INTEGER*2 SHFRM_LAST_VARIABLE,ACTIVE_VARIABLES,I
      
      INTEGER*2 SET_SHORT_FORM_VARIABLES


      CHARACTER*256 SHORT_FORM_FILE_NAME


         OPEN(10,FILE=SHORT_FORM_FILE_NAME,ACCESS="TRANSPARENT", &
                                                          STATUS="OLD")
         READ(10,REC=1) &
                   SHFRM_LAST_VARIABLE,ACTIVE_VARIABLES,VARIABLE_ACTIVE
         CLOSE(10)

         ALLOCATE(VARIABLE_LIST(0:ACTIVE_VARIABLES-1))
         VARIABLES_USED = 0
         DO I = 0, SHFRM_LAST_VARIABLE
            IF(VARIABLE_ACTIVE(I)) THEN
               VARIABLE_LIST(VARIABLES_USED) = I
               VARIABLES_USED = VARIABLES_USED + 1
            ENDIF
         ENDDO
         VARIABLES_USED = ACTIVE_VARIABLES

!         DEALLOCATE(VARIABLE_ACTIVE)
         SET_SHORT_FORM_VARIABLES  = ACTIVE_VARIABLES


      END FUNCTION SET_SHORT_FORM_VARIABLES
      

      

      FUNCTION SET_AN_DECOMP_VARIABLES( &
       SHORT_FORM_NAME,SHORT_FORM_TITLE)


      
      LOGICAL*1 VARIABLE_ACTIVE(:)
      LOGICAL*4 FILE_EXISTS
      INTEGER*2 AN_DECOMP_LAST_VARIABLE,ACTIVE_VARIABLES
      integer (kind=2) :: I
      INTEGER*2 VARIABLE_LIST(:)
      INTEGER*2 VARIABLES_USED,GET_AN_DECOMP_VARIABLES, &
                SET_AN_DECOMP_VARIABLES
      ALLOCATABLE :: VARIABLE_ACTIVE,VARIABLE_LIST
      REAL OUTPUT_VARIABLE(*),VARIABLE_VALUE(0:*)
      CHARACTER*256 DES_FILE_NAME,OUTPUT_DIRECTORY,SHORT_FORM_NAME*8
      CHARACTER*(*) SHORT_FORM_TITLE
      SAVE VARIABLE_LIST,VARIABLES_USED

         ALLOCATE(VARIABLE_ACTIVE(0:get_LAST_VARIABLE()))
         DES_FILE_NAME = trim(OUTPUT_DIRECTORY())// &
                                         trim(SHORT_FORM_NAME)//'.VRS'
         call write_log_entry("msgoutpt:0001",  &
            "Opening " // trim(des_file_name) // "....")
         INQUIRE(FILE=DES_FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(10,FILE=DES_FILE_NAME, &
                                 ACCESS="TRANSPARENT",STATUS="OLD")
            READ(10,REC=1) &
               AN_DECOMP_LAST_VARIABLE,ACTIVE_VARIABLES,VARIABLE_ACTIVE
            CLOSE(10)
         ELSEIF(trim(SHORT_FORM_NAME) == ' ') THEN

            CALL MG_LOCATE_WRITE(20,1, &
                         'For debug Annual Decomp reports to work '// &
                                'a short form', &
                                 ALL_VERSIONS,0)

            CALL MG_LOCATE_WRITE(21,1, &
                    'must be selected in Set Simulation Parameters.', &
                                 ALL_VERSIONS,0)
            er_message='stop requested from MSGOUTPT SIID245'
            call end_program(er_message)
         ELSE

            CALL MG_LOCATE_WRITE(20,0, &
                          'The short form described as "'// &
                        trim(SHORT_FORM_TITLE)//'" does not exist.', &
                                 ALL_VERSIONS,0)
            er_message='stop requested from MSGOUTPT SIID246'
            call end_program(er_message)
         ENDIF

         IF(.NOT. ALLOCATED(VARIABLE_LIST)) &
                          ALLOCATE(VARIABLE_LIST(0:ACTIVE_VARIABLES-1))
         VARIABLES_USED = 0
         DO I = 0, AN_DECOMP_LAST_VARIABLE
            IF(VARIABLE_ACTIVE(I)) THEN
               VARIABLE_LIST(VARIABLES_USED) = I
               VARIABLES_USED = VARIABLES_USED + 1
            ENDIF
         ENDDO

         VARIABLES_USED = ACTIVE_VARIABLES
         DEALLOCATE(VARIABLE_ACTIVE)
         SET_AN_DECOMP_VARIABLES  = ACTIVE_VARIABLES
      RETURN

      ENTRY GET_AN_DECOMP_VARIABLES(OUTPUT_VARIABLE,VARIABLE_VALUE)


         DO I = 0, VARIABLES_USED-1
            OUTPUT_VARIABLE(I+1) = VARIABLE_VALUE(VARIABLE_LIST(I))
         ENDDO
         GET_AN_DECOMP_VARIABLES = VARIABLES_USED
      RETURN
 1000 FORMAT('&',10A)
      END FUNCTION SET_AN_DECOMP_VARIABLES


!  ROUTINE TO SPEED OUTPUT



      SUBROUTINE WRITE_RESULTS(CURRENT_YEAR,LENGHT,VARIABLE_VALUE)
      INTEGER*2 CURRENT_YEAR,S_END_POINT
      REAL END_POINT/1./
      INTEGER*2 LENGHT,CURRENT_PLANNING_YEAR,PLAN_NUMBER
      INTEGER*4 NEXT_REC_1500,R_NEXT_REC_1500, &
                UNIT_38_OUTPUT_RECORD
      SAVE NEXT_REC_1500
      REAL :: VARIABLE_VALUE(LENGHT)
      LOGICAL*1 AN_DECOMP_RPT_OPEN/.FALSE./
      INTEGER*2 VOID_INT2,AN_DECOMP_REPORT_HEADER

         UNIT_38_OUTPUT_RECORD = OUTPUT_RECORD()
         WRITE(38,REC=UNIT_38_OUTPUT_RECORD) END_POINT, &
                                             FLOAT(CURRENT_YEAR), &
                                             VARIABLE_VALUE
      RETURN
      ENTRY WRITE_BASE_YR_RESULTS()
         UNIT_38_OUTPUT_RECORD = OUTPUT_RECORD()
         WRITE(38,REC=UNIT_38_OUTPUT_RECORD) END_POINT, &
                                             FLOAT(get_BASE_YEAR())
      RETURN
      ENTRY WRITE_DECOM_RESULTS(CURRENT_YEAR,CURRENT_PLANNING_YEAR, &
                                     PLAN_NUMBER,LENGHT,VARIABLE_VALUE)
         IF(.NOT. AN_DECOMP_RPT_OPEN) THEN
            VOID_INT2 = AN_DECOMP_REPORT_HEADER(NEXT_REC_1500)
            AN_DECOMP_RPT_OPEN = .TRUE.
         ENDIF
         WRITE(1500,REC=NEXT_REC_1500) &
                   END_POINT,FLOAT(CURRENT_PLANNING_YEAR), &
                  FLOAT(PLAN_NUMBER),FLOAT(CURRENT_YEAR),VARIABLE_VALUE
         NEXT_REC_1500 = NEXT_REC_1500 + 1
      RETURN
      ENTRY STORE_END_POINT(S_END_POINT)
         END_POINT = FLOAT(S_END_POINT)
      RETURN
      END subroutine write_results
      
      function SET_OUTPUT_RECORD(TEMP_OUTPUT_RECORD)
        
        implicit none
        integer(kind=4) :: temp_output_record
        integer(kind=4) :: set_output_record
         CURRENT_OUTPUT_RECORD = TEMP_OUTPUT_RECORD
         SET_OUTPUT_RECORD = CURRENT_OUTPUT_RECORD
      end function set_output_record
	  
      subroutine SET_STARTING_RECORD_4_UNIT38()
	     implicit none
		 logical (kind=1) :: LAHEY_LF95
		 
         IF(LAHEY_LF95()) THEN
            CURRENT_OUTPUT_RECORD = 1
         ELSE
            CURRENT_OUTPUT_RECORD = 0
         ENDIF
         
      end subroutine SET_STARTING_RECORD_4_UNIT38
	  
	  
      FUNCTION OUTPUT_RECORD()
      
      implicit none
      INTEGER*4 OUTPUT_RECORD,SET_OUTPUT_RECORD
      INTEGER*4 TEMP_OUTPUT_RECORD
      
      INTEGER*4 SAVE_END_POINT_STARTING_RECORD, &
                SET_OUT_REC_TO_ENDPNT_START_REC, &
                END_STARTING_POINT_RECORD
      LOGICAL*1 LAHEY_LF95
      SAVE END_STARTING_POINT_RECORD

         CURRENT_OUTPUT_RECORD = CURRENT_OUTPUT_RECORD + 1
         OUTPUT_RECORD = CURRENT_OUTPUT_RECORD
      RETURN

      ENTRY SAVE_END_POINT_STARTING_RECORD
         END_STARTING_POINT_RECORD = CURRENT_OUTPUT_RECORD
         SAVE_END_POINT_STARTING_RECORD = END_STARTING_POINT_RECORD
      RETURN
      ENTRY SET_OUT_REC_TO_ENDPNT_START_REC
         CURRENT_OUTPUT_RECORD = END_STARTING_POINT_RECORD
         SET_OUT_REC_TO_ENDPNT_START_REC = END_STARTING_POINT_RECORD
      RETURN

      END function output_record
      subroutine RETURN_LEVEL_SALES_PURCHASES(R_RM_ASSET_CLASS_ID, &
                                       R_CAPACITY_SALES_TO_LEVEL_RM, &
                                    R_CAPACITY_PURCHASES_TO_LEVEL_RM, &
                                       R_ADD_2_INCOME_STATEMENT)
         implicit none
         ! Arguments
         integer (kind=2) :: R_RM_ASSET_CLASS_ID
         real :: R_CAPACITY_SALES_TO_LEVEL_RM
         real :: R_CAPACITY_PURCHASES_TO_LEVEL_RM
         character (len=1) :: R_ADD_2_INCOME_STATEMENT
         
         !Locals
         
         integer (kind=2) :: RM_ASSET_CLASS_ID=0
         real :: CAPACITY_SALES_TO_LEVEL_RM=0
         real :: CAPACITY_PURCHASES_TO_LEVEL_RM=0

         
         R_ADD_2_INCOME_STATEMENT = &
            msgoutpt_elements%ADD_2_INCOME_STATEMENT
         R_ADD_2_INCOME_STATEMENT = 'T'
         IF(R_RM_ASSET_CLASS_ID == RM_ASSET_CLASS_ID+1) THEN
            R_CAPACITY_SALES_TO_LEVEL_RM = CAPACITY_SALES_TO_LEVEL_RM
            R_CAPACITY_PURCHASES_TO_LEVEL_RM = &
                                         CAPACITY_PURCHASES_TO_LEVEL_RM

         ELSE
            R_CAPACITY_SALES_TO_LEVEL_RM = 0.
            R_CAPACITY_PURCHASES_TO_LEVEL_RM = 0.
         ENDIF
      end subroutine return_level_sales_purchases
      
      SUBROUTINE TRANSACT_GROUP_RESERVE( &
                        RUN_YEAR,WRITE_THE_RESULTS,ASSET_ANALYST_ONLY)
      use tf_objt_interfaces
      use tf_decs
      use debugtrace
      implicit none

      
      real :: DCPA_result
      INTEGER (KIND=2) ::I2_1,I2_0,I2_2
      REAL (KIND=4) :: YR_ITER(2)
      INTEGER (KIND=2) :: DIM_LOOP
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
      REAL ZERO,NOT_AVAIL,NEAR_ZERO
      PARAMETER(ZERO=0.,NOT_AVAIL=-999999.,NEAR_ZERO=.0000001)
      LOGICAL*4 WRITE_THE_RESULTS
      LOGICAL*1 ASSET_ANALYST_ONLY, &
                VOID_LOGICAL, &
                YES_USE_MINIMUM_RM, &
                USE_REGIONAL_MINIMUM_RM_LOGIC
      CHARACTER*1 USE_MINIMUM_RM
                  
                  

      CHARACTER*3 PA_CHAR_NUM
      integer (kind=2) :: GET_PA_FROM_TG !External
      INTEGER*2 I,RUN_YEAR,CURRENT_YEAR,J,TG, &
                  MAX_ACTIVE_GROUPS, &
                  TG_ID,GET_TRANS_GROUP_INDEX, &
                  TG_2_PLANNING_AREA(:),PA_loc, &
                  PA_INDEX,GET_PA_VALUE_FROM_TG
      REAL  GET_SYSTEM_CAPACITY,GET_INPUT_SYSTEM_PEAK, &
            GET_NUMBER_OF_CL_ADDITIONS,GET_NUMBER_OF_EL_ADDITIONS
      REAL, save :: PLANNING_CAPACITY, &
!  Increased EXISTING_PLANNING_CAPACITY array size from 10-16
! because those subscripts are all used. Adds support for two
! preexisting, along with OW, HB, Hydro and CS
           EXISTING_PLANNING_CAPACITY(0:16), &
! Increased NEW_PLANNING_CAPACITY size from the 10
! provided by legacy (actually used 13), to the current 16 used.
           NEW_PLANNING_CAPACITY(0:16), &
           NEWGEN_CAPACITY(10), &
           AFTER_PEAK_CAPACITY, &
           PA_EXISTING_PLANNING_CAPACITY(:,:), &
           PA_NEW_PLANNING_CAPACITY(:,:), &
           PA_NEWGEN_CAPACITY(:,:), &
           SYS_NEWGEN_CAPACITY(10), &
           PA_CAPACITY_PLANNING_PEAK(:), &
           PA_AFTER_PEAK_CAPACITY(:), &
           PA_RETIREMENT_CAPACITY(:), &
           PA_INTERRUPTIBLE_LOAD(:), &
           PA_TRANSFER_CAPACITY(:), &
           SYS_EXISTING_PLANNING_CAPACITY(0:10), &
           SYS_NEW_PLANNING_CAPACITY(0:10), &
           SYS_AFTER_PEAK_CAPACITY, &
           RETIREMENT_CAPACITY, &
           SYS_RETIREMENT_CAPACITY, &
           TG_TRANSFER_CAPACITY, &
           GET_TG_TRANSFER_CAPACITY
      ALLOCATABLE :: &
           PA_EXISTING_PLANNING_CAPACITY, &
           PA_NEW_PLANNING_CAPACITY, &
           PA_NEWGEN_CAPACITY, &
           PA_AFTER_PEAK_CAPACITY, &
           PA_RETIREMENT_CAPACITY, &
           PA_INTERRUPTIBLE_LOAD, &
           PA_TRANSFER_CAPACITY, &
           PA_CAPACITY_PLANNING_PEAK, &
           TG_2_PLANNING_AREA
      REAL GET_ANNUAL_PEAK,ENRG_LIMITED_COST,PLANNING_RESERVE_MARGIN, &
           PLANNING_RM_BEFORE_ADDITIONS, &
           PLANNING_PEAK_BEFORE_ADDITIONS

! INCLUDE COMMON BLOCK OF YEARLY SYSTEM FORECAST STATISTICS

      REAL  CAPACITY_PLANNING_PEAK,UPDATE_NET_PLANNING_PEAK, &
            PEAK_AFTER_INTERRUPTIBLE, &
            INTERRUPTIBLE_LOAD, &
            SYS_INTERRUPTIBLE_LOAD, &
            GET_ANNUAL_INTER_CAPACITY, &
            NET_PLANNING_PEAK,AN_DECOMP_PEAK/0./, &
            AN_DECOMP_PLANNING_CAPACITY/0./

! RESERVE MARGIN CAPACITY SALES AND PURCHASES

      INTEGER*2 RM_ASSET_CLASS_ID,R_RM_ASSET_CLASS_ID
      REAL CAPACITY_SALES_TO_LEVEL_RM, &
           CAPACITY_PURCHASES_TO_LEVEL_RM, &
           R_CAPACITY_SALES_TO_LEVEL_RM, &
           R_CAPACITY_PURCHASES_TO_LEVEL_RM

      REAL LEVELIZING_RESERVE_MARGIN, &
           RM_PURCHASE_FIXED_COST, &
           RM_PURCHASE_VARIABLE_COST, &
           MAX_SELL_RESERVE_MARGIN, &
           RM_SALE_FIXED_AMOUNT, &
           RM_SALE_VARIABLE_AMOUNT, &
           RETURN_RESERVE_MARGIN_COSTS, &
           CAPACITY_TO_LEVELIZE_RM

      

!     DECLARATIONS FOR RESERVE MARGIN REPORT

      LOGICAL*1 RESERVE_MARGIN_REPORT_NOT_OPEN/.TRUE./,EXPANSION_REPORT
      INTEGER*2 RESERVE_MARGIN_NO,TG_RESERVE_MARGIN_HEADER, &
       REPORT_YEAR, &
                TEMP_I2,GET_REGIONAL_PA_NAME
      INTEGER RESERVE_MARGIN_REC
      REAL*4   LM_RESOURCE_LOAD_CREDIT,LM_PLANNED_LOAD_CREDIT, &
               OLD_CL_PLANNING_CAPACITY,NEW_CL_PLANNING_CAPACITY, &
               OLD_EL_PLANNING_CAPACITY,NEW_EL_PLANNING_CAPACITY, &
               OLD_LM_PLANNING_CAPACITY,NEW_LM_PLANNING_CAPACITY, &
             CAPACITY_PEAKS,ADJUSTMENT_PEAK,CONTRACT_PEAK_ADJUSTMENT, &
               PEAK_ADJ_OFF_SYSTEM_SALES,ADJUSTMENT_CAPACITY, &
             RETURN_RESERVE_MARGIN_RATIO, &
               GET_CL_TG_CAP,GET_GROUP_PEAK_ON_PEAK_MONTH, &
               DERIV_CAPACITY_PLANNING_BY_TG, &
               DERIV_NEW_CAP_PLAN_BY_TG, &
               DERIV_CAPACITY_PLANNING_BY_ALL, &
               DERIV_NEW_CAP_PLAN_BY_ALL, &
               DERIV_CAPACITY_PLANNING, &
               GET_NEWGEN_CAP_BY_INDEX, &
               GET_PEAK_ON_PA_PEAK_MONTH, &
               GET_EL_TG_CAP,GET_CL_AFTER_PEAK,GET_CL_TG_RETIRE, &
               EV_DATA_SOURCE/2./
      REAL*8 TOTAL_PRODUCTION_COST
      SAVE  RESERVE_MARGIN_NO,RESERVE_MARGIN_REC, &
               CAPACITY_PURCHASES_TO_LEVEL_RM, &
               CAPACITY_SALES_TO_LEVEL_RM
      LOGICAL*1 CAPACITY_PLANNING_IS_ACTIVE/.FALSE./, &
                CAPACITY_PLANNING_ACTIVE
      CHARACTER*35 PA_NAME
      CHARACTER*35 GET_GROUP_NAME
      
      logical :: first_call=.true.
      integer :: file_trace_tgr=0
      

! END OF DATA DECLARATIONS
      if(file_trace_tgr==0) then
        file_trace_tgr=open_trace("transact_group_reserve.trace", &
            rq_tgr)

      end if
      
      IF(.NOT. ASSET_ANALYST_ONLY) THEN
         IF(RUN_YEAR == 1) CAPACITY_PLANNING_IS_ACTIVE = &
                                            CAPACITY_PLANNING_ACTIVE()
         IF(.NOT. CAPACITY_PLANNING_IS_ACTIVE) &
            CAPACITY_PLANNING_PEAK = UPDATE_NET_PLANNING_PEAK(RUN_YEAR)

!     RESERVE MARGIN REPORT

         IF(EXPANSION_REPORT() .AND. WRITE_THE_RESULTS) THEN
            IF(RESERVE_MARGIN_REPORT_NOT_OPEN) THEN
               RESERVE_MARGIN_NO = TG_RESERVE_MARGIN_HEADER( &
                                                   RESERVE_MARGIN_REC)
               RESERVE_MARGIN_REPORT_NOT_OPEN = .FALSE.
            ENDIF
            MAX_ACTIVE_GROUPS = GET_NUMBER_OF_ACTIVE_GROUPS()
            REPORT_YEAR = MIN(RUN_YEAR,get_globecom_study_period())

            SYS_EXISTING_PLANNING_CAPACITY = 0.
            SYS_NEW_PLANNING_CAPACITY = 0.


            SYS_AFTER_PEAK_CAPACITY = 0.
            SYS_RETIREMENT_CAPACITY = 0.
            SYS_INTERRUPTIBLE_LOAD = 0.

            IF(ALLOCATED(PA_EXISTING_PLANNING_CAPACITY)) &
                  DEALLOCATE(PA_EXISTING_PLANNING_CAPACITY, &
                                       PA_NEW_PLANNING_CAPACITY, &
                                       PA_NEWGEN_CAPACITY, &
                                       PA_AFTER_PEAK_CAPACITY, &
                                       PA_RETIREMENT_CAPACITY, &
                                       PA_INTERRUPTIBLE_LOAD, &
                                       PA_TRANSFER_CAPACITY, &
                                       PA_CAPACITY_PLANNING_PEAK, &
                                       TG_2_PLANNING_AREA)
            ALLOCATE(PA_EXISTING_PLANNING_CAPACITY( &
                                               0:10,MAX_ACTIVE_GROUPS))
            ALLOCATE(PA_NEW_PLANNING_CAPACITY(0:10,MAX_ACTIVE_GROUPS))
            ALLOCATE(PA_NEWGEN_CAPACITY(10,MAX_ACTIVE_GROUPS))
            ALLOCATE(PA_AFTER_PEAK_CAPACITY(MAX_ACTIVE_GROUPS))
            ALLOCATE(PA_RETIREMENT_CAPACITY(MAX_ACTIVE_GROUPS))
            ALLOCATE(PA_INTERRUPTIBLE_LOAD(MAX_ACTIVE_GROUPS))
            ALLOCATE(PA_TRANSFER_CAPACITY(MAX_ACTIVE_GROUPS))
            ALLOCATE(PA_CAPACITY_PLANNING_PEAK(MAX_ACTIVE_GROUPS))
            ALLOCATE(TG_2_PLANNING_AREA(MAX_ACTIVE_GROUPS))

            PA_EXISTING_PLANNING_CAPACITY = 0.
            PA_NEW_PLANNING_CAPACITY = 0.
            PA_NEWGEN_CAPACITY = 0.
            PA_AFTER_PEAK_CAPACITY = 0.
            PA_RETIREMENT_CAPACITY = 0.
            PA_INTERRUPTIBLE_LOAD = 0.
            PA_TRANSFER_CAPACITY = 0.
            PA_CAPACITY_PLANNING_PEAK = 0.
            TG_2_PLANNING_AREA = 0
            SYS_NEWGEN_CAPACITY = 0.

            VOID_LOGICAL = YES_USE_MINIMUM_RM(USE_MINIMUM_RM)
            IF( USE_MINIMUM_RM == 'R') &
                                USE_REGIONAL_MINIMUM_RM_LOGIC = .TRUE.

            DO TG = 1, MAX_ACTIVE_GROUPS

               IF( .NOT. GET_REPORT_TRANS_GROUP(TG)) CYCLE
               
               TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)
               pa_loc=TG_2_PLANNING_AREA(TG)

               IF(pa_loc == 0) then
                CYCLE
               end if

               PLANNING_RESERVE_MARGIN = ZERO
               PLANNING_RM_BEFORE_ADDITIONS = ZERO

               I2_1 = 1
               I2_0 = 0
               I2_2 = 2
               EXISTING_PLANNING_CAPACITY(0) = &
                           GET_CL_TG_CAP(I2_0,TG,RUN_YEAR,I2_1) &
                           + GET_EL_TG_CAP(TG,RUN_YEAR,I2_1) &
                           + DERIV_CAPACITY_PLANNING_BY_TG(TG,RUN_YEAR)

               
               
      call write_trace_int2(file_trace_tgr, "1. TG", TG)
      call write_trace_int2(file_trace_tgr, "1. RUN_YEAR", RUN_YEAR)
      
               ! NEW_PLANNING_CAPACITY(0) not being added up correctly here. -John
               NEW_PLANNING_CAPACITY(0) = &
                        GET_CL_TG_CAP(0_2,TG,RUN_YEAR,2_2) &
                           + GET_EL_TG_CAP(TG,RUN_YEAR,2_2) &
                           + DERIV_NEW_CAP_PLAN_BY_ALL(0_2, &
                TG,RUN_YEAR)
      call write_trace_real(file_trace_tgr, "ECP(0)", &
        EXISTING_PLANNING_CAPACITY(0))
      call write_trace_real(file_trace_tgr, &
      "NCP(0)", NEW_PLANNING_CAPACITY(0))
      
               PLANNING_CAPACITY = EXISTING_PLANNING_CAPACITY(0) + &
                                              NEW_PLANNING_CAPACITY(0)

               EXISTING_PLANNING_CAPACITY(1) = &
                           GET_CL_TG_CAP( &
                                    1_2,TG,RUN_YEAR,1_2) + &
                     DERIV_CAPACITY_PLANNING_BY_ALL(1_2, &
                        TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(1) = &
                              GET_CL_TG_CAP( &
                                    1_2,TG,RUN_YEAR,2_2) + &
                     DERIV_NEW_CAP_PLAN_BY_ALL(1_2,TG,RUN_YEAR)
                     
               EXISTING_PLANNING_CAPACITY(2) = &
                           GET_CL_TG_CAP( &
                                    2_2,TG,RUN_YEAR,1_2) + &
                     DERIV_CAPACITY_PLANNING_BY_ALL(2_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(2) =  &
                              GET_CL_TG_CAP( &
                                    2_2,TG,RUN_YEAR,2_2) + &
                             DERIV_NEW_CAP_PLAN_BY_ALL(2_2,TG,RUN_YEAR)

               EXISTING_PLANNING_CAPACITY(3) = &
                           GET_CL_TG_CAP( &
                                    3_2,TG,RUN_YEAR,1_2) + &
                   DERIV_CAPACITY_PLANNING_BY_ALL(3_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(3) = &
                              GET_CL_TG_CAP( &
                                    3_2,TG,RUN_YEAR,2_2) + &
                        DERIV_NEW_CAP_PLAN_BY_ALL(3_2,TG,RUN_YEAR)

               EXISTING_PLANNING_CAPACITY(4) = &
                           GET_CL_TG_CAP( &
                                    4_2,TG,RUN_YEAR,1_2) + &
                     DERIV_CAPACITY_PLANNING_BY_ALL(4_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(4) = &
                              GET_CL_TG_CAP( &
                                    4_2,TG,RUN_YEAR,2_2) + &
                        DERIV_NEW_CAP_PLAN_BY_ALL(4_2,TG,RUN_YEAR)

               EXISTING_PLANNING_CAPACITY(5) = &
                           GET_CL_TG_CAP( &
                            5_2,TG,RUN_YEAR,1_2) + &
                           GET_EL_TG_CAP(TG,RUN_YEAR,1_2) + &
                 DERIV_CAPACITY_PLANNING_BY_ALL(5_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(5) = &
                              GET_CL_TG_CAP( &
                             5_2,TG,RUN_YEAR,2_2) + &
                            GET_EL_TG_CAP(TG,RUN_YEAR,2_2) + &
                        DERIV_NEW_CAP_PLAN_BY_ALL(5_2,TG,RUN_YEAR)

               EXISTING_PLANNING_CAPACITY(6) = &
                           GET_CL_TG_CAP( &
                                    6_2,TG,RUN_YEAR,1_2) + &
                    DERIV_CAPACITY_PLANNING_BY_ALL(6_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(6) = &
                              GET_CL_TG_CAP( &
                                    6_2,TG,RUN_YEAR,2_2) + &
                        DERIV_NEW_CAP_PLAN_BY_ALL(6_2,TG,RUN_YEAR)
! 7
               EXISTING_PLANNING_CAPACITY(7) = &
                    DERIV_CAPACITY_PLANNING_BY_ALL(7_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(7) = &
                        DERIV_NEW_CAP_PLAN_BY_ALL(7_2,TG,RUN_YEAR)
! 8
               EXISTING_PLANNING_CAPACITY(8) = &
                    DERIV_CAPACITY_PLANNING_BY_ALL(8_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(8) = &
                        DERIV_NEW_CAP_PLAN_BY_ALL(8_2,TG,RUN_YEAR)
! 9
               EXISTING_PLANNING_CAPACITY(9) = &
                    DERIV_CAPACITY_PLANNING_BY_ALL(9_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(9) = &
                        DERIV_NEW_CAP_PLAN_BY_ALL(9_2,TG,RUN_YEAR)
! 10
               EXISTING_PLANNING_CAPACITY(10) = &
                    DERIV_CAPACITY_PLANNING_BY_ALL(10_2,TG,RUN_YEAR)

               NEW_PLANNING_CAPACITY(10) = &
                        DERIV_NEW_CAP_PLAN_BY_ALL(10_2,TG,RUN_YEAR)


!! Todo: support new PMS by uncommenting


               ! EXISTING_PLANNING_CAPACITY(11) = &
                    ! get_cl_tg_cap(11_2, tg, run_year, 1_2) + &
                    ! DERIV_CAPACITY_PLANNING_BY_ALL(11_2, TG, RUN_YEAR)

                ! NEW_PLANNING_CAPACITY(11_2) = &
                   ! DERIV_NEW_CAP_PLAN_BY_ALL(11_2, TG, RUN_YEAR)


            ! DG
                ! EXISTING_PLANNING_CAPACITY(12) = &
                   ! get_cl_tg_cap(12_2, tg, run_year, 1_2) + &
                   ! DERIV_CAPACITY_PLANNING_BY_ALL(12_2, TG, &
        ! RUN_YEAR)

                ! NEW_PLANNING_CAPACITY(12_2) = &
                   ! DERIV_NEW_CAP_PLAN_BY_ALL(12_2, TG, RUN_YEAR)



!             ! 12-15 Getcl & deriv new cap - jtr 03132023
!             ! Offshore Wind
               ! EXISTING_PLANNING_CAPACITY(13) = &
                   ! get_cl_tg_cap(13_2,tg,run_year,1_2)
                ! DCPA_result=DERIV_CAPACITY_PLANNING_BY_ALL(13_2, &
                 ! TG, RUN_YEAR)

       ! existing_planning_capacity(13)=existing_planning_capacity(13) &
        ! +DCPA_result


             ! !OSW
               ! NEW_PLANNING_CAPACITY(13) = GET_CL_TG_CAP( &
                  ! 13_2,TG,RUN_YEAR,2_2)
       ! dcpa_result=DERIV_NEW_CAP_PLAN_BY_ALL(13_2, TG, RUN_YEAR)
       ! new_planning_capacity(13)=new_planning_capacity(13) + &
        ! dcpa_result


             !Hybrid
                    ! EXISTING_PLANNING_CAPACITY(14) = &
                      ! get_cl_tg_cap(14_2,tg,run_year,1_2)
         ! dcpa_result=DERIV_CAPACITY_PLANNING_BY_ALL(14_2, &
           ! TG, RUN_YEAR)
        ! existing_planning_capacity(14)= &
         ! existing_planning_capacity(14) + dcpa_result


             !Hybrid
               ! NEW_PLANNING_CAPACITY(14) = &
                 ! get_cl_tg_cap(14_2,tg,run_year, 2_2)
       ! dcpa_result=DERIV_NEW_CAP_PLAN_BY_ALL(14_2, TG, RUN_YEAR)
       ! new_planning_capacity(14)=new_planning_capacity(14) + &
        ! dcpa_result


             !Hydrogen
               ! EXISTING_PLANNING_CAPACITY(15) = &
                 ! get_cl_tg_cap(15_2,tg,run_year,1_2)
        ! dcpa_result=DERIV_CAPACITY_PLANNING_BY_ALL(15_2, TG, &
         ! RUN_YEAR)
       ! existing_planning_capacity(15)=existing_planning_capacity(15) &
        ! + dcpa_result

!             !Hydrogen
               ! NEW_PLANNING_CAPACITY(15) = &
                  ! get_cl_tg_cap(15_2,tg,run_year,2_2)
       ! dcpa_result=DERIV_NEW_CAP_PLAN_BY_ALL(15_2, TG, RUN_YEAR)
       ! new_planning_capacity(15)= &
        ! new_planning_capacity(15)+dcpa_result


!                !CS(CCS)
                  ! EXISTING_PLANNING_CAPACITY(16) = &
                   ! get_cl_tg_cap(16_2,tg,run_year,1_2)
       ! dcpa_result=DERIV_CAPACITY_PLANNING_BY_ALL(16_2, &
                   ! TG, RUN_YEAR)
       ! existing_planning_capacity(16) = &
        ! existing_planning_capacity(16) + dcpa_result

               ! NEW_PLANNING_CAPACITY(16) = &
               ! get_cl_tg_cap(16_2,tg,run_year,2_2)

        ! dcpa_result=DERIV_NEW_CAP_PLAN_BY_ALL(16_2, TG, RUN_YEAR)
        ! existing_planning_capacity(16)= &
         ! existing_planning_capacity(16) + &
         ! dcpa_result

! Todo: support new PMS by uncommenting (end)

               DO I = 1, 10
                  IF(I == 1) THEN
                     NEWGEN_CAPACITY(I) = &
                             GET_NEWGEN_CAP_BY_INDEX(I,TG,RUN_YEAR) + &
                                    GET_EL_TG_CAP(TG,RUN_YEAR,1_2) + &
                                    GET_EL_TG_CAP(TG,RUN_YEAR,2_2)
                   SYS_NEWGEN_CAPACITY(I) = SYS_NEWGEN_CAPACITY(I) + &
                                                     NEWGEN_CAPACITY(I)
                  ELSE
                     NEWGEN_CAPACITY(I) = &
                                 GET_NEWGEN_CAP_BY_INDEX(I,TG,RUN_YEAR)
                    SYS_NEWGEN_CAPACITY(I) = SYS_NEWGEN_CAPACITY(I) + &
                                                     NEWGEN_CAPACITY(I)
                  ENDIF
               ENDDO


! TODO: Newgen_capsize
               DO I = 0, 10

                  SYS_EXISTING_PLANNING_CAPACITY(I) = &
                           SYS_EXISTING_PLANNING_CAPACITY(I) + &
                                          EXISTING_PLANNING_CAPACITY(I)

                  SYS_NEW_PLANNING_CAPACITY(I) = &
                           SYS_NEW_PLANNING_CAPACITY(I) + &
                                           NEW_PLANNING_CAPACITY(I)

               ENDDO

               AFTER_PEAK_CAPACITY = GET_CL_AFTER_PEAK(TG,RUN_YEAR)
               SYS_AFTER_PEAK_CAPACITY = SYS_AFTER_PEAK_CAPACITY + &
                                                   AFTER_PEAK_CAPACITY

               RETIREMENT_CAPACITY = GET_CL_TG_RETIRE(TG,RUN_YEAR)
               SYS_RETIREMENT_CAPACITY = SYS_RETIREMENT_CAPACITY + &
                                                    RETIREMENT_CAPACITY
! 09/12/02.
               IF(USE_REGIONAL_MINIMUM_RM_LOGIC) THEN
                  CAPACITY_PLANNING_PEAK = &
                    GET_PEAK_ON_PA_PEAK_MONTH(TG,pa_loc)
               ELSE
                  CAPACITY_PLANNING_PEAK = &
                                       GET_GROUP_PEAK_ON_PEAK_MONTH(TG)
               ENDIF


      call write_trace_int2(file_trace_tgr, "TG", TG)
               INTERRUPTIBLE_LOAD = GET_ANNUAL_INTER_CAPACITY(TG)
      call write_trace_real(file_trace_tgr, "1. INTERRUPTIBLE_LOAD", &
      INTERRUPTIBLE_LOAD)
               SYS_INTERRUPTIBLE_LOAD = SYS_INTERRUPTIBLE_LOAD + &
                                                     INTERRUPTIBLE_LOAD
               PEAK_AFTER_INTERRUPTIBLE = MAX(0., &
                           CAPACITY_PLANNING_PEAK - INTERRUPTIBLE_LOAD)

               CALL GET_TG_TRANSFER_CAPACITY(TG,TG_TRANSFER_CAPACITY)

               IF(PEAK_AFTER_INTERRUPTIBLE > NEAR_ZERO)  THEN
               
! After executing the following line of code for the
! first time, these values should be seen:
!
! PLANNING_RESERVE_MARGIN = -1.48587465E+001/-1.48587465E+001
! PLANNING_CAPACITY =  1.77868359E+004/1.77868359E+004
! TG_TRANSFER_CAPACITY = 0.00000000E+000/0
! PEAK_AFTER_INTERRUPTIBLE = 2.08909727E+004/2.08909727E+004
! (checks good)


                  PLANNING_RESERVE_MARGIN = &
                  100.*( (PLANNING_CAPACITY + &
                                 TG_TRANSFER_CAPACITY - &
                                    PEAK_AFTER_INTERRUPTIBLE) / &
                                         PEAK_AFTER_INTERRUPTIBLE )

                  PLANNING_RM_BEFORE_ADDITIONS = &
                        100. * (EXISTING_PLANNING_CAPACITY(0) + &
                                    TG_TRANSFER_CAPACITY - &
                           PEAK_AFTER_INTERRUPTIBLE) / &
                                             PEAK_AFTER_INTERRUPTIBLE
               ELSE
                  PLANNING_RESERVE_MARGIN = ZERO
                  PLANNING_RM_BEFORE_ADDITIONS = ZERO
               ENDIF
               YR_ITER(1) = FLOAT(RUN_YEAR+get_BASE_YEAR())
               DIM_LOOP = GRX_HORIZON_REPORTING(YR_ITER)

                RESERVE_MARGIN_REC = rptrec(RESERVE_MARGIN_NO)
               PA_NAME = GET_GROUP_NAME(TG)
               RXVars(1:13,TG,27)= PEAK_AFTER_INTERRUPTIBLE
               RXVars(1:13,TG,28)= PLANNING_CAPACITY
 
             ! Checking planning reserve margin
               if(index(pa_name, "ERCOT-S")>0) then
                pa_name=pa_name ! Debugstop
               end if
!            !MSGTGRES
               reserve_margin_row=reserve_margin_row+1

               WRITE(RESERVE_MARGIN_NO,REC=RESERVE_MARGIN_REC) &
                            PRT_ENDPOINT(), &
                           (YR_ITER(I),I=1,DIM_LOOP), &
                            PA_NAME, &
                           (EXISTING_PLANNING_CAPACITY(I),I=0,10), &
                           (NEW_PLANNING_CAPACITY(I),I=0,10), &
                           PLANNING_CAPACITY, &
                AFTER_PEAK_CAPACITY, & !  CAPACITY INSTALLED AFTER PEAK
                           PLANNING_CAPACITY,   & !  16
                           CAPACITY_PLANNING_PEAK, &
                           INTERRUPTIBLE_LOAD, &
                           TG_TRANSFER_CAPACITY, & 
                           PEAK_AFTER_INTERRUPTIBLE,         & !  20
                           PLANNING_RM_BEFORE_ADDITIONS, &
                           PLANNING_RESERVE_MARGIN, &
                           RETIREMENT_CAPACITY, &
                           (NEWGEN_CAPACITY(I),I=1,10), &
                           EV_DATA_SOURCE
        call write_trace_message(file_trace_tgr,"Reserve Margin 2")
        if(file_trace_tgr/=BAD_TRACE_HANDLE) then
            write(file_trace_tgr,*) &
       "PRT_ENDPOINT", PRT_ENDPOINT(), &
      "(YR_ITER", (YR_ITER(I),I=1,DIM_LOOP), &
       "PA_NAME", PA_NAME, &
      "(EXISTING_PLANNING_CAPy", (EXISTING_PLANNING_CAPACITY(I),I=0,10), &
      "(NEW_PLANNING_CAPACITY", (NEW_PLANNING_CAPACITY(I),I=0,10), &
      "PLANNING_CAPACITY", PLANNING_CAPACITY, &
      "AFTER_PEAK_CAPACITY", AFTER_PEAK_CAPACITY, & 
      "PLANNING_CAPACITY", PLANNING_CAPACITY,   & !  16
      "CAPACITY_PLANNING_PEAK", CAPACITY_PLANNING_PEAK, &
      "INTERRUPTIBLE_LOAD", INTERRUPTIBLE_LOAD, &
      "TG_TRANSFER_CAPACITY", TG_TRANSFER_CAPACITY, & 
      "PEAK_AFTER_INTERRUPTIBLE", PEAK_AFTER_INTERRUPTIBLE, & !  20
      "PLANNING_RM_BEFORE_ADDITIONS", PLANNING_RM_BEFORE_ADDITIONS, &
      "PLANNING_RESERVE_MARGIN", PLANNING_RESERVE_MARGIN, &
      "RETIREMENT_CAPACITY", RETIREMENT_CAPACITY, &
      "(NEWGEN_CAPACITY", (NEWGEN_CAPACITY(I),I=1,10), &
      "EV_DATA_SOURCE", EV_DATA_SOURCE
        end if
               call write_trace_int(file_trace_tgr, "RESERVE_MARGIN_REC", &
                RESERVE_MARGIN_REC)
                
               RESERVE_MARGIN_REC = RESERVE_MARGIN_REC + 1

               IF(pa_loc> 0) THEN
                  DO I = 0, 10
                     PA_EXISTING_PLANNING_CAPACITY(I,pa_loc) = &
                           PA_EXISTING_PLANNING_CAPACITY(I,pa_loc) + &
                                          EXISTING_PLANNING_CAPACITY(I)
                  PA_NEW_PLANNING_CAPACITY(I,pa_loc) = &
                           PA_NEW_PLANNING_CAPACITY(I,pa_loc) + &
                                           NEW_PLANNING_CAPACITY(I)
                  ENDDO

                  DO I = 1, 10
                     PA_NEWGEN_CAPACITY(I,pa_loc) = &
                                  PA_NEWGEN_CAPACITY(I,pa_loc) + &
                                                     NEWGEN_CAPACITY(I)
                  ENDDO

                  PA_AFTER_PEAK_CAPACITY(pa_loc) = &
                                         PA_AFTER_PEAK_CAPACITY(pa_loc) + &
                                                    AFTER_PEAK_CAPACITY
                  PA_RETIREMENT_CAPACITY(pa_loc) = &
                                         PA_RETIREMENT_CAPACITY(pa_loc) + &
                                                    RETIREMENT_CAPACITY
                  PA_CAPACITY_PLANNING_PEAK(pa_loc) = &
                                 PA_CAPACITY_PLANNING_PEAK(pa_loc) + &
                                                 CAPACITY_PLANNING_PEAK
                  PA_INTERRUPTIBLE_LOAD(pa_loc) = &
                                          PA_INTERRUPTIBLE_LOAD(pa_loc) + &
                                                     INTERRUPTIBLE_LOAD
                  PA_TRANSFER_CAPACITY(pa_loc) = &
                                           PA_TRANSFER_CAPACITY(pa_loc) + &
                                                   TG_TRANSFER_CAPACITY
               ENDIF

            ENDDO ! TRANSACTION GROUPS

            DO pa_loc = 1, MAX_ACTIVE_GROUPS

               
               PA_INDEX = GET_PA_VALUE_FROM_TG(PA_loc)


               IF(PA_INDEX == 0) CYCLE

               PLANNING_CAPACITY = &
                           PA_EXISTING_PLANNING_CAPACITY(1,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(1,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(2,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(2,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(3,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(3,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(4,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(4,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(5,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(5,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(6,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(6,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(7,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(7,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(8,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(8,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(9,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(9,pa_loc) + &
                           PA_EXISTING_PLANNING_CAPACITY(10,pa_loc) + &
                           PA_NEW_PLANNING_CAPACITY(10,pa_loc)
               PEAK_AFTER_INTERRUPTIBLE = MAX(0., &
                         PA_CAPACITY_PLANNING_PEAK(pa_loc) - &
                                             PA_INTERRUPTIBLE_LOAD(pa_loc))
               IF(PEAK_AFTER_INTERRUPTIBLE > NEAR_ZERO)  THEN
                  PLANNING_RESERVE_MARGIN = &
                    100.*( (PLANNING_CAPACITY + &
                                PA_TRANSFER_CAPACITY(pa_loc) - &
                                    PEAK_AFTER_INTERRUPTIBLE) / &
                                         PEAK_AFTER_INTERRUPTIBLE )
! Todo: shorten this statement. Specifying indices is not needed.
                  PLANNING_RM_BEFORE_ADDITIONS = &
                        100. * (PA_EXISTING_PLANNING_CAPACITY(1,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(2,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(3,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(4,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(5,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(6,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(7,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(8,pa_loc) + &
                                PA_EXISTING_PLANNING_CAPACITY(9,pa_loc) + &
                               PA_EXISTING_PLANNING_CAPACITY(10,pa_loc) + &
                                PA_TRANSFER_CAPACITY(pa_loc) - &
                           PEAK_AFTER_INTERRUPTIBLE) / &
                                             PEAK_AFTER_INTERRUPTIBLE
               ELSE
                  PLANNING_RESERVE_MARGIN = ZERO
                  PLANNING_RM_BEFORE_ADDITIONS = ZERO
               ENDIF


               WRITE(PA_CHAR_NUM,"(I3)") PA_INDEX

! 03/16/05. FOR BURESH/ONDEMAND ASP.
               temp_i2=pa_loc ! Debugstop
               
               TEMP_I2 = GET_REGIONAL_PA_NAME(PA_loc,PA_NAME)
               IF(TEMP_I2 <= 0) THEN
                  PA_NAME = "AREA "//PA_CHAR_NUM
               ENDIF


               YR_ITER(1) = FLOAT(RUN_YEAR+get_BASE_YEAR())
               DIM_LOOP = GRX_HORIZON_REPORTING(YR_ITER)

                RESERVE_MARGIN_REC = rptrec(RESERVE_MARGIN_NO)
                !MSGTGRES

                reserve_margin_row=reserve_margin_row+1
                
               WRITE(RESERVE_MARGIN_NO,REC=RESERVE_MARGIN_REC) &
                           PRT_ENDPOINT(), &
                           (YR_ITER(I),I=1,DIM_LOOP), &
                           PA_NAME, &
                        (PA_EXISTING_PLANNING_CAPACITY(I,pa_loc),I=0,10), &
                           (PA_NEW_PLANNING_CAPACITY(I,pa_loc),I=0,10), &
                           PLANNING_CAPACITY, &
         PA_AFTER_PEAK_CAPACITY(pa_loc), & !  CAPACITY INSTALLED AFTER PEAK
                           PLANNING_CAPACITY, &
                           PA_CAPACITY_PLANNING_PEAK(pa_loc), &
                           PA_INTERRUPTIBLE_LOAD(pa_loc), &
                           PA_TRANSFER_CAPACITY(pa_loc), &
                           PEAK_AFTER_INTERRUPTIBLE, &
                           PLANNING_RM_BEFORE_ADDITIONS, &
                           PLANNING_RESERVE_MARGIN, &
                           PA_RETIREMENT_CAPACITY(pa_loc), &
                           (PA_NEWGEN_CAPACITY(I,pa_loc),I=1,10), &
                           EV_DATA_SOURCE
       call write_trace_message(file_trace_tgr,"Reserve Margin 3")
       if(file_trace_tgr/=BAD_TRACE_HANDLE) then
              write(file_trace_tgr, *) &
              "PRT_ENDPOINT", PRT_ENDPOINT(), &
       "(YR_ITER", (YR_ITER(I),I=1,DIM_LOOP), &
       "PA_NAME", PA_NAME, &
       "(PA_EXIS_PLANNG_CAPy", (PA_EXISTING_PLANNING_CAPACITY(I,pa_loc),I=0,10), &
       "(PA_NEW_PLANNING_CAPACITY", (PA_NEW_PLANNING_CAPACITY(I,pa_loc),I=0,10), &
       "PLANNING_CAPACITY", PLANNING_CAPACITY, &
         "PA_AFTER_PEAK_CAPACITY", PA_AFTER_PEAK_CAPACITY(pa_loc), &
         "PLANNING_CAPACITY", PLANNING_CAPACITY, &
        "PA_CAPACITY_PLANNING_PEAK", PA_CAPACITY_PLANNING_PEAK(pa_loc), &
        "PA_INTERRUPTIBLE_LOAD", PA_INTERRUPTIBLE_LOAD(pa_loc), &
        "PA_TRANSFER_CAPACITY", PA_TRANSFER_CAPACITY(pa_loc), &
        "PEAK_AFTER_INTERRUPTIBLE", PEAK_AFTER_INTERRUPTIBLE, &
        "PLANNING_RM_BEFORE_ADDITIONS", PLANNING_RM_BEFORE_ADDITIONS, &
        "PLANNING_RESERVE_MARGIN", PLANNING_RESERVE_MARGIN, &
        "PA_RETIREMENT_CAPACITY", PA_RETIREMENT_CAPACITY(pa_loc), &
        "(PA_NEWGEN_CAPACITY", (PA_NEWGEN_CAPACITY(I,pa_loc),I=1,10), &
        "EV_DATA_SOURCE", EV_DATA_SOURCE
       end if
            call write_trace_int(file_trace_tgr, "RESERVE_MARGIN_REC", &
                RESERVE_MARGIN_REC)
                
               RESERVE_MARGIN_REC = RESERVE_MARGIN_REC + 1
            ENDDO



            PLANNING_CAPACITY = SYS_EXISTING_PLANNING_CAPACITY(0) + &
            SYS_NEW_PLANNING_CAPACITY(0)

           planning_capacity=planning_capacity ! Debugstop                                         
           

            CAPACITY_PLANNING_PEAK = &
                                  GET_GROUP_PEAK_ON_PEAK_MONTH(INT2(0))
          PEAK_AFTER_INTERRUPTIBLE = MAX(0.,CAPACITY_PLANNING_PEAK - &
                                               SYS_INTERRUPTIBLE_LOAD)

            IF(PEAK_AFTER_INTERRUPTIBLE > NEAR_ZERO)  THEN

               PLANNING_RESERVE_MARGIN = &
                  100.*( (PLANNING_CAPACITY - &
                                    PEAK_AFTER_INTERRUPTIBLE) / &
                                         PEAK_AFTER_INTERRUPTIBLE )
               PLANNING_RM_BEFORE_ADDITIONS = &
                        100. * (SYS_EXISTING_PLANNING_CAPACITY(0) - &
                           PEAK_AFTER_INTERRUPTIBLE) / &
                                             PEAK_AFTER_INTERRUPTIBLE
            ELSE
               PLANNING_RM_BEFORE_ADDITIONS = ZERO
            ENDIF


! RESERVE MARGIN COSTS
! 09/27/03. MOVED.
!            IF(.NOT. HOLD_AN_DECOMP_VALUES) THEN
               AN_DECOMP_PEAK = PEAK_AFTER_INTERRUPTIBLE
               AN_DECOMP_PLANNING_CAPACITY = PLANNING_CAPACITY
!            ENDIF

! 09/27/03. MOVED INTO TRANSACT_GROUP_RESERVE

            LEVELIZING_RESERVE_MARGIN = RETURN_RESERVE_MARGIN_COSTS( &
                                              RM_PURCHASE_FIXED_COST, &
                                           RM_PURCHASE_VARIABLE_COST, &
                                             MAX_SELL_RESERVE_MARGIN, &
                                              RM_SALE_FIXED_AMOUNT, &
                                             RM_SALE_VARIABLE_AMOUNT, &
                                              RM_ASSET_CLASS_ID, &
                               msgoutpt_elements%ADD_2_INCOME_STATEMENT)
            CAPACITY_TO_LEVELIZE_RM = AN_DECOMP_PEAK * &
                                      (1. + LEVELIZING_RESERVE_MARGIN)
            CAPACITY_SALES_TO_LEVEL_RM = 0.
            CAPACITY_PURCHASES_TO_LEVEL_RM = 0.
            IF(CAPACITY_TO_LEVELIZE_RM > &
                                      AN_DECOMP_PLANNING_CAPACITY)THEN
! THE BUY CASE
           CAPACITY_PURCHASES_TO_LEVEL_RM = RM_PURCHASE_FIXED_COST + &
                  (ABS(CAPACITY_TO_LEVELIZE_RM - &
                           AN_DECOMP_PLANNING_CAPACITY) * &
                                    RM_PURCHASE_VARIABLE_COST)/1000000.
            ELSEIF(CAPACITY_TO_LEVELIZE_RM < &
                                      AN_DECOMP_PLANNING_CAPACITY) THEN
! THE SELL CASE
               CAPACITY_SALES_TO_LEVEL_RM = RM_SALE_FIXED_AMOUNT + &
                  (RM_SALE_VARIABLE_AMOUNT * &
                    (MIN(AN_DECOMP_PLANNING_CAPACITY, &
                     AN_DECOMP_PEAK * (1.+MAX_SELL_RESERVE_MARGIN)) - &
                                     CAPACITY_TO_LEVELIZE_RM))/1000000.
            ENDIF

             YR_ITER(1) = FLOAT(RUN_YEAR+get_BASE_YEAR())
             DIM_LOOP = GRX_HORIZON_REPORTING(YR_ITER)

             RESERVE_MARGIN_REC = rptrec(RESERVE_MARGIN_NO)
             PA_NAME = 'System'
             !MSGTGRES
             reserve_margin_row=reserve_margin_row+1
             
             WRITE(RESERVE_MARGIN_NO,REC=RESERVE_MARGIN_REC) &
                           PRT_ENDPOINT(), & !  PRT_ENDPOINT(),
                           (YR_ITER(I),I=1,DIM_LOOP), &
                           PA_NAME,  & !  'System              ',
                         (SYS_EXISTING_PLANNING_CAPACITY(I),I=0,10), &
                           (SYS_NEW_PLANNING_CAPACITY(I),I=0,10), &
                           PLANNING_CAPACITY, &
                           SYS_AFTER_PEAK_CAPACITY, &
                           PLANNING_CAPACITY, &
                           CAPACITY_PLANNING_PEAK, &
                           SYS_INTERRUPTIBLE_LOAD, &
                           0., & !  RATIOS
                           PEAK_AFTER_INTERRUPTIBLE, &
                           PLANNING_RM_BEFORE_ADDITIONS, &
                           PLANNING_RESERVE_MARGIN, &
                           SYS_RETIREMENT_CAPACITY, &
                           (SYS_NEWGEN_CAPACITY(I),I=1,10), &
                           EV_DATA_SOURCE
        call write_trace_message(file_trace_tgr,"Reserve Margin 4")
        if(file_trace_tgr/=BAD_TRACE_HANDLE) then
          write(file_trace_tgr,*) &
        "PRT_ENDPOINT", PRT_ENDPOINT(), & 
         "(YR_ITER", (YR_ITER(I),I=1,DIM_LOOP), &
         "PA_NAME", PA_NAME,  & !  'System              ',
       "(SY_EXIG_PLAN_CAP", (SYS_EXISTING_PLANNING_CAPACITY(I),I=0,10), &
         "(SYS_NEW_PLANNING_CAPACITY", (SYS_NEW_PLANNING_CAPACITY(I),I=0,10), &
         "PLANNING_CAPACITY", PLANNING_CAPACITY, &
         "SYS_AFTER_PEAK_CAPACITY", SYS_AFTER_PEAK_CAPACITY, &
         "PLANNING_CAPACITY", PLANNING_CAPACITY, &
         "CAPACITY_PLANNING_PEAK", CAPACITY_PLANNING_PEAK, &
         "SYS_INTERRUPTIBLE_LOAD", SYS_INTERRUPTIBLE_LOAD, &
         "0.,", 0., & !  RATIOS
         "PEAK_AFTER_INTERRUPTIBLE", PEAK_AFTER_INTERRUPTIBLE, &
         "PLANNING_RM_BEFORE_ADDITIONS", PLANNING_RM_BEFORE_ADDITIONS, &
         "PLANNING_RESERVE_MARGIN", PLANNING_RESERVE_MARGIN, &
         "SYS_RETIREMENT_CAPACITY", SYS_RETIREMENT_CAPACITY, &
         "(SYS_NEWGEN_CAPACITY", (SYS_NEWGEN_CAPACITY(I),I=1,10), &
         "EV_DATA_SOURCE", EV_DATA_SOURCE
        end if
            call write_trace_int(file_trace_tgr, "RESERVE_MARGIN_REC", &
                RESERVE_MARGIN_REC)
                
            RESERVE_MARGIN_REC = RESERVE_MARGIN_REC + 1
         ENDIF ! WRITE REPORT
      ENDIF ! NOT JUST ASSET ANALYST

      RETURN



  END subroutine TRANSACT_GROUP_RESERVE
  
  subroutine RETURN_MONTHLY_LEVEL_SALES_PURCHASES(R_RM_ASSET_CLASS_ID, &
                               R_MTHLY_CAP_SALES_TO_LEVEL_RM, &
                            R_MTHLY_CAP_PURCH_TO_LEVEL_RM, &
                               R_ADD_2_INCOME_STATEMENT)
         use msgoutpt_data
         implicit none                      
         integer (kind=2), intent(in) :: R_RM_ASSET_CLASS_ID
         real :: R_MTHLY_CAP_SALES_TO_LEVEL_RM(:)
         real :: R_MTHLY_CAP_PURCH_TO_LEVEL_RM(:)
         character(len=1) :: R_ADD_2_INCOME_STATEMENT
         
         
         R_ADD_2_INCOME_STATEMENT = &
            msgoutpt_elements%ADD_2_INCOME_STATEMENT
         IF(R_RM_ASSET_CLASS_ID == &
           msgoutpt_elements%RM_ASSET_CLASS_ID+1) THEN
            R_MTHLY_CAP_SALES_TO_LEVEL_RM(1:) = &
                        msgoutpt_elements%CAPACITY_SALES_TO_LEVEL_RM/12.

            R_MTHLY_CAP_SALES_TO_LEVEL_RM(0) = &
                          SUM(R_MTHLY_CAP_SALES_TO_LEVEL_RM(1:))

            R_MTHLY_CAP_PURCH_TO_LEVEL_RM(1:) = &
                    msgoutpt_elements%CAPACITY_PURCHASES_TO_LEVEL_RM/12.
            R_MTHLY_CAP_PURCH_TO_LEVEL_RM(0) = &
                      SUM(R_MTHLY_CAP_PURCH_TO_LEVEL_RM(1:))
         ELSE
            R_MTHLY_CAP_SALES_TO_LEVEL_RM = 0.
            R_MTHLY_CAP_PURCH_TO_LEVEL_RM = 0.
         ENDIF
    end subroutine return_monthly_level_sales_purchases
end module msgoutpt
