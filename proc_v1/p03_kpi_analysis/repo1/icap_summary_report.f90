! From clreport
module icap_summary_report
implicit none
    LOGICAL (kind=1) :: MX_ICAP_SUMMARY_REPORT=.FALSE.
    INTEGER (kind=2) :: CG=0
    INTEGER (kind=2) ::  PG=0
contains
subroutine prepare_icap_summary_report(yr, iseas, ICAP_TOTAL_CAPACITY, &
ICAP_TOTAL_MWH,ICAP_TOTAL_REVENUE,ICAP_LEVEL_CAPITAL_COST, &
ICAP_FUEL_COST,ICAP_START_COST,ICAP_VOM_COST,ICAP_EMISSION_COST, &
ANNUAL_ICAP_TOTAL_CAPACITY,MONTHLY_ICAP_TOTAL_CAPACITY, &
ANNUAL_ICAP_TOTAL_MWH,ANNUAL_ICAP_TOTAL_REVENUE, &
ANNUAL_ICAP_LEVEL_CAPITAL_COST,ICAP_VAR_COST,ICAP_MRX_EAS, &
ICAP_TOTAL_COST,ICAP_NET_MARGIN,ICAP_NET_MARGIN_PER_KW, &
ICAP_NET_MARGIN_PER_MWH,ANNUAL_ICAP_CONE_VALUE, &
ANNUAL_ICAP_RESERVE_MARGIN,ANNUAL_ICAP_INSTL_CAPACITY_VALUE, &
ANNUAL_ICAP_DEMAND_POINT,ANNUAL_ICAP_VALUE_POINT,ANNUAL_ICAP_MRX_CONE, &
ANNUAL_ICAP_MRX_EAS,ANNUAL_ICAP_VAR_COST,ANNUAL_ICAP_FUEL_COST, &
ANNUAL_ICAP_START_COST,ANNUAL_ICAP_VOM_COST,ANNUAL_ICAP_EMISSION_COST, &
ANNUAL_ICAP_EAS_REVENUE_OFFSET,MONTHLY_ICAP_VALUE, &
ANNUAL_ICAP_TOTAL_COST,ANNUAL_ICAP_NET_MARGIN, &
ANNUAL_ICAP_NET_MARGIN_PER_KW,ANNUAL_ICAP_NET_MARGIN_PER_MWH, &
ANNUAL_ICAP_MAX_MRX_OR_CONE,ANNUAL_ICAP_ADJ_CAPACITY_VALUE, &
CL_UNIT_MONTHLY_FIXED_COST,ENERGY,BLOCK_FUEL_COST,TONS_CONVERSION, &
MONTH_UNIT_START_COSTS,CL_MONTH_NAME,SEAS_HOURS)

use monthly_icap
use prodcom
use rptreccontrol
use eco
use grx_planning_routines
use foshydi2com
use globecom
use dr_booth_modules
use irec_endpoint_control
use mod_base_year
use program_state
implicit none
integer (kind=2) :: imm_i, imm_year ! for passing where immutable arguments required
REAL (kind=4) :: UNIT_EMISSIONS(5)
REAL (kind=4) :: UNIT_EMISSIONS_COST(5)
REAL (kind=4) :: ICAP_TOTAL_CAPACITY(:)
REAL (kind=4) :: ICAP_TOTAL_MWH(:)
REAL (kind=4) :: ICAP_TOTAL_REVENUE(:)
REAL (kind=4) :: ICAP_LEVEL_CAPITAL_COST(:)
REAL (kind=4) :: ICAP_FUEL_COST(:)
REAL (kind=4) :: ICAP_START_COST(:)
REAL (kind=4) :: ICAP_VOM_COST(:)
REAL (kind=4) :: ICAP_EMISSION_COST(:)
REAL (kind=4) :: ANNUAL_ICAP_TOTAL_CAPACITY(:)
REAL (kind=4) :: MONTHLY_ICAP_TOTAL_CAPACITY(:,:)
REAL (kind=4) :: ANNUAL_ICAP_TOTAL_MWH(:)
REAL (kind=4) :: ANNUAL_ICAP_TOTAL_REVENUE(:)
REAL (kind=4) :: ANNUAL_ICAP_LEVEL_CAPITAL_COST(:)
REAL (kind=4) :: ICAP_VAR_COST(:)
REAL (kind=4) :: ICAP_MRX_EAS(:)
REAL (kind=4) :: ICAP_TOTAL_COST(:)
REAL (kind=4) :: ICAP_NET_MARGIN(:)
REAL (kind=4) :: ICAP_NET_MARGIN_PER_KW(:)
REAL (kind=4) :: ICAP_NET_MARGIN_PER_MWH(:)
REAL (kind=4) :: ANNUAL_ICAP_CONE_VALUE(:)
REAL (kind=4) :: ANNUAL_ICAP_RESERVE_MARGIN(:)
REAL (kind=4) :: ANNUAL_ICAP_INSTL_CAPACITY_VALUE(:)
REAL (kind=4) :: ANNUAL_ICAP_DEMAND_POINT(:)
REAL (kind=4) :: ANNUAL_ICAP_VALUE_POINT(:)
REAL (kind=4) :: ANNUAL_ICAP_MRX_CONE(:)
REAL (kind=4) :: ANNUAL_ICAP_MRX_EAS(:)
REAL (kind=4) :: ANNUAL_ICAP_VAR_COST(:)
REAL (kind=4) :: ANNUAL_ICAP_FUEL_COST(:)
REAL (kind=4) :: ANNUAL_ICAP_START_COST(:)
REAL (kind=4) :: ANNUAL_ICAP_VOM_COST(:)
REAL (kind=4) :: ANNUAL_ICAP_EMISSION_COST(:)
REAL (kind=4) :: ANNUAL_ICAP_EAS_REVENUE_OFFSET(:)
REAL (kind=4) :: MONTHLY_ICAP_VALUE(:)
REAL (kind=4) :: ANNUAL_ICAP_TOTAL_COST(:)
REAL (kind=4) :: ANNUAL_ICAP_NET_MARGIN(:)
REAL (kind=4) :: ANNUAL_ICAP_NET_MARGIN_PER_KW(:)
REAL (kind=4) :: ANNUAL_ICAP_NET_MARGIN_PER_MWH(:)
REAL (kind=4) :: ANNUAL_ICAP_MAX_MRX_OR_CONE(:)
REAL (kind=4) :: ANNUAL_ICAP_ADJ_CAPACITY_VALUE(:)
REAL (kind=4) :: MRX_EAS_REV_PER_KWMO
REAL (kind=4) :: MRX_ICAP_VALUE_POINT
REAL (kind=4) :: MRX_CONE_PER_KWMO
REAL (kind=4) :: MRX_NET_MARGIN_PER_KWMO
REAL (kind=4) :: MRX_NET_MARGIN_PER_MWH
REAL (kind=4) :: MRX_NET_MARGIN_MM
REAL (kind=4) :: MRX_ANN_CAP_COST
REAL (kind=4) :: MRX_TOTAL_COST
REAL (kind=4) :: MRX_EMISSIONS
REAL (kind=4) :: MRX_VOM
REAL (kind=4) :: MRX_FUEL
REAL (kind=4) :: MRX_TOTAL_VARIABLE
REAL (kind=4) :: MRX_ENERGY
REAL (kind=4) :: MRX_CAPACITY
INTEGER (kind=2) :: ICAP_MRX_USE_MARGINAL
REAL (kind=4) :: ICAP_ADJ_CAPACITY_VALUE
REAL (kind=4) :: MRX_TOTAL_REVENUE
REAL (kind=4) :: ICAP_EAS_REVENUE_OFFSET
REAL (kind=4) :: ICAP_INSTL_CAPACITY_VALUE
REAL (kind=4) :: ICAP_VALUE_POINT
REAL (kind=4) :: ICAP_DEMAND_POINT
REAL (kind=4) :: ICAP_RESERVE_MARGIN
REAL (kind=4) :: ICAP_CONE_VALUE
REAL (kind=4) :: ICAP_MRX_EAS_PER_KW
REAL (kind=4) :: ICAP_MRX_CONE
CHARACTER (len=20) :: CM_NAME*35
INTEGER (kind=2) :: TEMP_I2
LOGICAL (kind=1) :: TEMP_L1
integer (kind=2) :: uba

INTEGER (kind=2) :: CM_INDEX
CHARACTER (len=3) :: CM_CHAR_NUM
INTEGER (kind=2) :: MX_ICAP_REPORT_VARIABLES
INTEGER (kind=2) :: MX_ICAP_ANNUAL_HEADER
INTEGER (kind=2) :: MX_ICAP_ANNUAL_ALT_NO=0
LOGICAL (kind=1) :: MX_ICAP_REPORT_NOT_OPEN=.TRUE.
INTEGER :: MX_ICAP_ANNUAL_ALT_REC
INTEGER (kind=2) :: TG_POSITION
INTEGER (kind=2) :: TP
REAL (kind=4) :: TOTAL_EMISSION_COSTS
INTEGER (kind=2) :: TG
REAL (kind=4) ::  EMIS_DISPATCH_1
REAL (kind=4) ::  EMIS_DISPATCH_2
REAL (kind=4) ::  EMIS_DISPATCH_3
REAL (kind=4) ::  EMIS_DISPATCH_4
REAL (kind=4) ::  EMIS_DISPATCH_5
INTEGER (kind=2), intent(in) :: YR
INTEGER (kind=2) , intent(in) :: ISEAS
REAL :: R_SOX,R_NOX,R_CO2,R_OTH2,R_OTH3
REAL :: TONS_CONVERSION
REAL :: SEAS_HOURS
REAL (kind=8) :: FUEL_COST
INTEGER (kind=2) :: TEMP_POINTER
REAL (kind=4) :: ENRG
INTEGER (kind=2) :: UNITNO
INTEGER (kind=2) :: I
! INTEGER (kind=2) :: gc_end_point
INTEGER (kind=2) :: FIRST_MONTHLY_TRANSACT=0
INTEGER (kind=2) :: LPM
REAL (kind=4) :: ANNUAL_ICAP_PEAK_MONTH_CAP
LOGICAL (kind=4) :: FILE_OPENED
LOGICAL (kind=1) :: GET_ICAP_REPORT_VALUES
INTEGER (kind=2) :: GET_CM_INDEX_FROM_TG
INTEGER (kind=2) :: GET_REGIONAL_CM_NAME
CHARACTER (len=9) :: CL_MONTH_NAME(14), cl_month_name_value
INTEGER (kind=2) :: GET_CM_PEAK_MONTH
LOGICAL (kind=1) :: MARGINAL_ICAP
INTEGER (kind=2) :: GET_CM_FROM_TG
REAL :: CL_UNIT_MONTHLY_FIXED_COST(*)
INTEGER (kind=2) :: get_TRANSACTION_GROUP
INTEGER (kind=2) :: GET_TRANS_GROUP_POSITION
REAL (kind=4) :: RETURN_SCREEN_CAP_COST
REAL (kind=4) :: MONTH_UNIT_START_COSTS(MAX_CL_UNITS)
INTEGER (kind=2) :: GET_POINTER_FOR_NEW_CL_UNIT
REAL :: ENERGY(2,MAX_CL_UNITS)
REAL :: BLOCK_FUEL_COST(2,MAX_CL_UNITS)
REAL (kind=4) :: TRANS_DISPATCH_EMIS_ADDER





     if(yr==0) then
        call end_program("icap_summary_report:0005 - YR is zero.")
     end if

   
IF(MX_ICAP_SUMMARY_REPORT) THEN ! .true./.true. tests good
   ! NUNITS=asdf/980
   uba=units_before_additions()+1
   
   DO I = uba, get_nunits()

!              ! Second assignment of UNITNO.
      ! I=adsf/asdf
      ! nunits=asdf/asdf

      UNITNO = I
      TEMP_POINTER = GET_POINTER_FOR_NEW_CL_UNIT(UNITNO)

      ENRG = SEAS_HOURS * (ENERGY(1,UNITNO) + ENERGY(2,UNITNO))
      call write_energy_trace("prepare_icap_summary_report", &
        "prepare_icap_summary_report:0001", ENRG)
      IF(ENRG > 0.45 .AND. TEMP_POINTER > 0) THEN
         FUEL_COST = BLOCK_FUEL_COST(1,UNITNO) + &
                                      BLOCK_FUEL_COST(2,UNITNO)
         FUEL_COST = FUEL_COST + &
                           ENRG * FUEL_ADDER_ADJUSTMENT(UNITNO)
         CALL RETURN_EMISSIONS_BY_BLOCK(UNITNO,INT2(1),R_SOX, &
                                     R_NOX,R_CO2,R_OTH2,R_OTH3)
!                 ! TODO: Unit Emissions 1..5 should be named constants.
         UNIT_EMISSIONS(1) = R_SOX/TONS_CONVERSION
         UNIT_EMISSIONS(2) = R_NOX/TONS_CONVERSION
         UNIT_EMISSIONS(3) = R_CO2/TONS_CONVERSION
         UNIT_EMISSIONS(4) = R_OTH2/TONS_CONVERSION
         UNIT_EMISSIONS(5) = R_OTH3/TONS_CONVERSION
!
         CALL RETURN_EMISSIONS_BY_BLOCK(UNITNO,INT2(2),R_SOX, &
                                     R_NOX,R_CO2,R_OTH2,R_OTH3)
         UNIT_EMISSIONS(1) = UNIT_EMISSIONS(1) + &
                                          R_SOX/TONS_CONVERSION
         UNIT_EMISSIONS(2) = UNIT_EMISSIONS(2) + &
                                          R_NOX/TONS_CONVERSION
         UNIT_EMISSIONS(3) = UNIT_EMISSIONS(3) + &
                                          R_CO2/TONS_CONVERSION
         UNIT_EMISSIONS(4) = UNIT_EMISSIONS(4) + &
                                         R_OTH2/TONS_CONVERSION
         UNIT_EMISSIONS(5) = UNIT_EMISSIONS(5) + &
                                         R_OTH3/TONS_CONVERSION
!
!
         if(yr==0) then
            call end_program("icap_summary_report:0004 - YR is zero.")
         end if
         EMIS_DISPATCH_1 = 0.002* &
                  TRANS_DISPATCH_EMIS_ADDER(1,ISEAS,YR,UNITNO)
         EMIS_DISPATCH_2 = 0.002* &
                  TRANS_DISPATCH_EMIS_ADDER(2,ISEAS,YR,UNITNO)
         EMIS_DISPATCH_3 = 0.002* &
                  TRANS_DISPATCH_EMIS_ADDER(3,ISEAS,YR,UNITNO)
         EMIS_DISPATCH_4 = 0.002* &
                  TRANS_DISPATCH_EMIS_ADDER(4,ISEAS,YR,UNITNO)
         EMIS_DISPATCH_5 = 0.002* &
                  TRANS_DISPATCH_EMIS_ADDER(5,ISEAS,YR,UNITNO)
!
         UNIT_EMISSIONS_COST(1) = &
                            UNIT_EMISSIONS(1) * EMIS_DISPATCH_1
         UNIT_EMISSIONS_COST(2) = &
                            UNIT_EMISSIONS(2) * EMIS_DISPATCH_2
         UNIT_EMISSIONS_COST(3) = &
                            UNIT_EMISSIONS(3) * EMIS_DISPATCH_3
         UNIT_EMISSIONS_COST(4) = &
                            UNIT_EMISSIONS(4) * EMIS_DISPATCH_4
         UNIT_EMISSIONS_COST(5) = &
                            UNIT_EMISSIONS(5) * EMIS_DISPATCH_5
         TOTAL_EMISSION_COSTS = SUM(UNIT_EMISSIONS_COST)
      ELSE
         FUEL_COST = 0.0 D0
         UNIT_EMISSIONS(1:5) = 0.
!
         UNIT_EMISSIONS_COST(1:5) = 0.
         TOTAL_EMISSION_COSTS = 0.
!
      endif
!
!
!
! ALL ICAP VARIABLES NEED TO BE BY TRANSACTION GROUP
!
!
      
      TG = MAX(1,get_TRANSACTION_GROUP(I))
      TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
!
      TP = GET_CM_FROM_TG(TG_POSITION) ! 072716
! 030311. ADDED ON-LINE OFF-LINE MONTH CHECK FOR ODEC.
      IF(TEMP_POINTER > 0 .AND. &
          ONLINE(I) .LE. DATE2 .AND. OFLINE(I) .GE. DATE1) THEN
         ICAP_TOTAL_CAPACITY(TP) = &
                              ICAP_TOTAL_CAPACITY(TP) + MW(2,I)
         ICAP_TOTAL_MWH(TP) = ICAP_TOTAL_MWH(TP) + ENRG/1000.
         ICAP_TOTAL_REVENUE(TP) = ICAP_TOTAL_REVENUE(TP) + &
                        MON_ECO_SALES_REV_FROM(UNITNO)/1000000.
         ICAP_LEVEL_CAPITAL_COST(TP) = &
               ICAP_LEVEL_CAPITAL_COST(TP) + &
               RETURN_SCREEN_CAP_COST(TEMP_POINTER,globecom_YEAR)/12. + &
               CL_UNIT_MONTHLY_FIXED_COST(I)/1000000.
         ICAP_FUEL_COST(TP) = ICAP_FUEL_COST(TP) + &
                                        SNGL(FUEL_COST)*.000001
         ICAP_START_COST(TP) = ICAP_START_COST(TP) + &
                        MONTH_UNIT_START_COSTS(UNITNO)/1000000.
         ICAP_VOM_COST(TP) = ICAP_VOM_COST(TP) + &
                                    VCPMWH(UNITNO)*ENRG*.000001
         ICAP_EMISSION_COST(TP) = ICAP_EMISSION_COST(TP) + &
                                           TOTAL_EMISSION_COSTS
!
         ANNUAL_ICAP_TOTAL_CAPACITY(TP) = &
                       ANNUAL_ICAP_TOTAL_CAPACITY(TP) + MW(2,I)
         MONTHLY_ICAP_TOTAL_CAPACITY(TP,ISEAS) = &
                MONTHLY_ICAP_TOTAL_CAPACITY(TP,ISEAS) + MW(2,I)
         ANNUAL_ICAP_TOTAL_MWH(TP) = &
                         ANNUAL_ICAP_TOTAL_MWH(TP) + ENRG/1000.
         ANNUAL_ICAP_TOTAL_REVENUE(TP) = &
                  ANNUAL_ICAP_TOTAL_REVENUE(TP) + &
                        MON_ECO_SALES_REV_FROM(UNITNO)/1000000.
         ANNUAL_ICAP_LEVEL_CAPITAL_COST(TP) = &
               ANNUAL_ICAP_LEVEL_CAPITAL_COST(TP) + &
             RETURN_SCREEN_CAP_COST(TEMP_POINTER,globecom_YEAR)/12. + &
               CL_UNIT_MONTHLY_FIXED_COST(I)/1000000.
         MONTHLY_ICAP_REVENUES(UNITNO) = &
                     MONTHLY_ICAP_REVENUES(UNITNO) + &
                        MON_ECO_SALES_REV_FROM(UNITNO)/1000000.
      endif
!
   ENDDO ! NUNITS FOR ICAP REPORT
!
!
   ! mx_icap_report_not_open=.true./.true.
   IF(MX_ICAP_REPORT_NOT_OPEN) THEN
      MX_ICAP_REPORT_NOT_OPEN = .FALSE.
      MX_ICAP_REPORT_VARIABLES = 22 ! 22 110408 ! 17 103008 !042308.


      
      MX_ICAP_ANNUAL_ALT_NO = MX_ICAP_ANNUAL_HEADER( &
              MX_ICAP_REPORT_VARIABLES, MX_ICAP_ANNUAL_ALT_REC)
              
      ! Probably not needed...

!
   endif
!
!

   if(cg==0) then
       call end_program("icap_summary_report:0002 - class groups " // &
        "count is zero.")
   end if
   DO I = 1, CG
!
!
      CM_INDEX = GET_CM_INDEX_FROM_TG(I)
!
      WRITE(CM_CHAR_NUM,"(I3)") CM_INDEX
!
!
! 03/16/05. FOR BURESH/ONDEMAND ASP.
!
!
      imm_i=i
      TEMP_I2 = GET_REGIONAL_CM_NAME(imm_i,CM_NAME)
      IF(TEMP_I2 <= 0) THEN
         CM_NAME = "AREA "//CM_CHAR_NUM
      endif
!
      ICAP_VAR_COST(I) = &
             ICAP_FUEL_COST(I)  + &
             ICAP_START_COST(I) + &
             ICAP_VOM_COST(I)   + &
             ICAP_EMISSION_COST(I)
!
      ICAP_MRX_EAS(I) = &
                       ICAP_TOTAL_REVENUE(I) - ICAP_VAR_COST(I)
!
      ICAP_TOTAL_COST(I) = ICAP_TOTAL_COST(I) + &
                               ICAP_VAR_COST(I) + &
                                     ICAP_LEVEL_CAPITAL_COST(I)
!
      ICAP_NET_MARGIN(I) = ICAP_TOTAL_REVENUE(I) - &
                                             ICAP_TOTAL_COST(I)
!
      IF(ICAP_TOTAL_CAPACITY(I) > 0.) THEN
         ICAP_NET_MARGIN_PER_KW(I) = 1000. * &
                                 ICAP_NET_MARGIN(I) / &
                                         ICAP_TOTAL_CAPACITY(I)
         ICAP_MRX_EAS_PER_KW = 1000. * &
                                 ICAP_MRX_EAS(I) / &
                                         ICAP_TOTAL_CAPACITY(I)
!
         ICAP_MRX_CONE = &
                  1000.0 * ICAP_LEVEL_CAPITAL_COST(TP)/ &
                                         ICAP_TOTAL_CAPACITY(I)
      ELSE
         ICAP_NET_MARGIN_PER_KW(I) = 0.
         ICAP_MRX_EAS_PER_KW = 0.
         ICAP_MRX_CONE = 0.0
      endif
      IF(ICAP_TOTAL_MWH(I) > 0.) THEN
         ICAP_NET_MARGIN_PER_MWH(I) = 1000. * &
                                 ICAP_NET_MARGIN(I) / &
                                            ICAP_TOTAL_MWH(I)
      ELSE
         ICAP_NET_MARGIN_PER_MWH(I) = 0.
      endif
      
      if(yr==0) then
        call end_program("icap_summary_report:0003 - Year must be " // &
            "nonzero.")
      end if
      imm_i=1
      TEMP_L1 = GET_ICAP_REPORT_VALUES(imm_I, &
                                yr, &
                                ICAP_CONE_VALUE, &
                                ICAP_MRX_CONE, &
                                ICAP_RESERVE_MARGIN, &
                                ICAP_DEMAND_POINT, &
                                ICAP_VALUE_POINT, &
                                ICAP_INSTL_CAPACITY_VALUE, &
                                ICAP_EAS_REVENUE_OFFSET, &
                                ICAP_ADJ_CAPACITY_VALUE, &
                                ICAP_MRX_EAS_PER_KW, &
                                ICAP_MRX_USE_MARGINAL)
!
! 110709.
!
      IF(ICAP_MRX_USE_MARGINAL > 0) THEN
         CALL GET_MRX_ICAP_REPORT_VALUES( &
                                       I, &
                                       MRX_CAPACITY, &
                                       MRX_ENERGY, &
                                       MRX_TOTAL_REVENUE, &
                                       MRX_TOTAL_VARIABLE, &
                                       MRX_FUEL, &
                                       MRX_VOM, &
                                       MRX_EMISSIONS, &
                                       MRX_TOTAL_COST, &
                                       MRX_ANN_CAP_COST, &
                                       MRX_NET_MARGIN_MM, &
                                       MRX_NET_MARGIN_PER_MWH, &
                                       MRX_NET_MARGIN_PER_KWMO, &
                                       MRX_CONE_PER_KWMO, &
                                       MRX_EAS_REV_PER_KWMO, &
                                       MRX_ICAP_VALUE_POINT)
! 111009. Simple treatment of annual to monthly: divide by 12
         ICAP_TOTAL_CAPACITY(I) = MRX_CAPACITY
         ICAP_TOTAL_MWH(I) = MRX_ENERGY/12.0
         ICAP_TOTAL_REVENUE(I) = MRX_TOTAL_REVENUE/12.0
         ICAP_VAR_COST(I) = MRX_TOTAL_VARIABLE/12.0
         ICAP_FUEL_COST(I) = MRX_FUEL/12.0
         ICAP_VOM_COST(I) = MRX_VOM/12.0
         ICAP_EMISSION_COST(I) = MRX_EMISSIONS/12.0
         ICAP_TOTAL_COST(I) = MRX_TOTAL_COST/12.0
         ICAP_LEVEL_CAPITAL_COST(I) = MRX_ANN_CAP_COST/12.0
         ICAP_NET_MARGIN(I) = MRX_NET_MARGIN_MM/12.0
         ICAP_NET_MARGIN_PER_MWH(I) = MRX_NET_MARGIN_PER_MWH
         ICAP_NET_MARGIN_PER_KW(I) = &
                              MRX_NET_MARGIN_PER_KWMO/12.0
         ICAP_MRX_CONE = MRX_CONE_PER_KWMO/12.0
         ICAP_MRX_EAS_PER_KW = MRX_EAS_REV_PER_KWMO ! /12.0
         ICAP_VALUE_POINT = MRX_ICAP_VALUE_POINT/12.0
      END IF
!
      ANNUAL_ICAP_CONE_VALUE(I) = &
                  ANNUAL_ICAP_CONE_VALUE(I) + ICAP_CONE_VALUE

      ANNUAL_ICAP_RESERVE_MARGIN(I) = &
                  ANNUAL_ICAP_RESERVE_MARGIN(I) + &
                                 ICAP_RESERVE_MARGIN
      ANNUAL_ICAP_INSTL_CAPACITY_VALUE(I) = &
                  ANNUAL_ICAP_INSTL_CAPACITY_VALUE(I) + &
            ICAP_INSTL_CAPACITY_VALUE  ! * ICAP_TOTAL_CAPACITY(I)
      ANNUAL_ICAP_DEMAND_POINT(I) = &
                  ANNUAL_ICAP_DEMAND_POINT(I) + &
                                 ICAP_DEMAND_POINT
      ANNUAL_ICAP_VALUE_POINT(I) = &
                  ANNUAL_ICAP_VALUE_POINT(I) + &
                                 ICAP_VALUE_POINT
      ANNUAL_ICAP_MRX_CONE(I) = ANNUAL_ICAP_MRX_CONE(I) + &
                                                  ICAP_MRX_CONE
      ANNUAL_ICAP_MRX_EAS(I) = ANNUAL_ICAP_MRX_EAS(I) + &
                                               ICAP_MRX_EAS(I)
      ANNUAL_ICAP_VAR_COST(I) = ANNUAL_ICAP_VAR_COST(I) + &
                                               ICAP_VAR_COST(I)
      ANNUAL_ICAP_FUEL_COST(I) =  ANNUAL_ICAP_FUEL_COST(I) + &
                                    ICAP_FUEL_COST(I)
      ANNUAL_ICAP_START_COST(I) = ANNUAL_ICAP_START_COST(I) + &
                                    ICAP_START_COST(I)
      ANNUAL_ICAP_VOM_COST(I) =  ANNUAL_ICAP_VOM_COST(I) + &
                                    ICAP_VOM_COST(I)
      ANNUAL_ICAP_EMISSION_COST(I) = &
           ANNUAL_ICAP_EMISSION_COST(I) + ICAP_EMISSION_COST(I)
      ANNUAL_ICAP_EAS_REVENUE_OFFSET(I) = &
                  ANNUAL_ICAP_EAS_REVENUE_OFFSET(I) + &
                           ICAP_EAS_REVENUE_OFFSET * &
                                    ICAP_TOTAL_CAPACITY(I)
      MONTHLY_ICAP_VALUE(I) = ICAP_VALUE_POINT
 

      MX_ICAP_ANNUAL_ALT_REC = RPTREC(MX_ICAP_ANNUAL_ALT_NO)
      if (iseas==0) then
        call end_program ("icap_summary_report:0006 - iseas of zero " // &
        "is invalid.")
      end if
      
      cl_month_name_value=CL_MONTH_NAME(ISEAS)
      WRITE(MX_ICAP_ANNUAL_ALT_NO,REC=MX_ICAP_ANNUAL_ALT_REC) &
               FLOAT(gc_end_point), &
               FLOAT(globecom_YEAR+get_base_year()), &
               cl_month_name_value, &
               CM_NAME, &
               ICAP_TOTAL_CAPACITY(I), & ! 0
               ICAP_TOTAL_MWH(I), &
               ICAP_TOTAL_COST(I), &
               ICAP_TOTAL_REVENUE(I), &
               ICAP_NET_MARGIN(I), &
               ICAP_NET_MARGIN_PER_MWH(I), &
               ICAP_NET_MARGIN_PER_KW(I), &
               ICAP_LEVEL_CAPITAL_COST(I), &
               ICAP_CONE_VALUE, &     ! 8  CONE VALUE
               ! 9 INSTALLED CAP VALUE
               ICAP_INSTL_CAPACITY_VALUE, & ! 9 INSTALLED CAP VALUE
               ICAP_RESERVE_MARGIN, & ! 10 RESERVE MARGIN
               ICAP_DEMAND_POINT, &   ! 11 DEMAND CURVE MULT
               ICAP_VALUE_POINT, &    ! 12 ICAP VALUE
               ICAP_MRX_CONE, & ! 13 MRX CONE
               ICAP_MRX_EAS_PER_KW, & ! 14 MRX EAS
               ICAP_EAS_REVENUE_OFFSET, & ! 15 EAS OFFSET
               ICAP_ADJ_CAPACITY_VALUE, & ! 16 ADJ CAP VALUE
               ICAP_VAR_COST(I), &
               ICAP_FUEL_COST(I), &
               ICAP_START_COST(I), & !19
               ICAP_VOM_COST(I), & ! 20
               ICAP_EMISSION_COST(I) !21

      MX_ICAP_ANNUAL_ALT_REC = MX_ICAP_ANNUAL_ALT_REC + 1
   ENDDO
!
   IF(ISEAS == 12 .AND. FIRST_MONTHLY_TRANSACT < 13) THEN
!
      DO I = 1, CG
!
         CM_INDEX = GET_CM_INDEX_FROM_TG(I)
!
         WRITE(CM_CHAR_NUM,"(I3)") CM_INDEX
         imm_i=i
         TEMP_I2 = GET_REGIONAL_CM_NAME(imm_I,CM_NAME)
         IF(TEMP_I2 <= 0) THEN
            CM_NAME = "AREA "//CM_CHAR_NUM
         endif
         imm_i=i
         LPM = GET_CM_PEAK_MONTH(imm_I)
         ANNUAL_ICAP_PEAK_MONTH_CAP = &
                    MONTHLY_ICAP_TOTAL_CAPACITY(imm_I,LPM)
         ANNUAL_ICAP_TOTAL_COST(I) = &
                        ANNUAL_ICAP_VAR_COST(imm_I) + &
                              ANNUAL_ICAP_LEVEL_CAPITAL_COST(imm_I)
         imm_i=i
         ANNUAL_ICAP_NET_MARGIN(I) = &
                  ANNUAL_ICAP_TOTAL_REVENUE(imm_I) - &
                                      ANNUAL_ICAP_TOTAL_COST(imm_I)
!
         IF(ANNUAL_ICAP_PEAK_MONTH_CAP > 0.) THEN
!

            ANNUAL_ICAP_MRX_CONE(I) = &
                  1000. * ANNUAL_ICAP_LEVEL_CAPITAL_COST(I)/ &
                              (12.0*ANNUAL_ICAP_PEAK_MONTH_CAP)
            ANNUAL_ICAP_NET_MARGIN_PER_KW(I) = 1000. * &
                                 ANNUAL_ICAP_NET_MARGIN(I) / &
                              (12.0*ANNUAL_ICAP_PEAK_MONTH_CAP)
            ICAP_MRX_EAS_PER_KW = 1000. * &
                              ANNUAL_ICAP_MRX_EAS(I) / &
                          (12.*ANNUAL_ICAP_PEAK_MONTH_CAP)
         ELSE

            ANNUAL_ICAP_MRX_CONE(I) = 0.
            ANNUAL_ICAP_NET_MARGIN_PER_KW(I) = 0.
            ICAP_MRX_EAS_PER_KW = 0.
         endif
            ANNUAL_ICAP_INSTL_CAPACITY_VALUE(I) = &
               ANNUAL_ICAP_INSTL_CAPACITY_VALUE(I)/12.0
         IF(ANNUAL_ICAP_TOTAL_CAPACITY(I) > 0.) THEN
            ICAP_EAS_REVENUE_OFFSET = &
                        ANNUAL_ICAP_EAS_REVENUE_OFFSET(I) / &
                          ANNUAL_ICAP_TOTAL_CAPACITY(I)
         ELSE
            ICAP_EAS_REVENUE_OFFSET = 0.
         ENDIF
         IF(ANNUAL_ICAP_TOTAL_MWH(I) > 0.) THEN
            ANNUAL_ICAP_NET_MARGIN_PER_MWH(I) = 1000. * &
                     ANNUAL_ICAP_NET_MARGIN(I) / &
                                       ANNUAL_ICAP_TOTAL_MWH(I)
         ELSE
            ANNUAL_ICAP_NET_MARGIN_PER_MWH(I) = 0.
         ENDIF

         ANNUAL_ICAP_CONE_VALUE(I) = &
                  ANNUAL_ICAP_CONE_VALUE(I) ! /12.0
         ANNUAL_ICAP_MAX_MRX_OR_CONE(I) = &
               MAX(-ANNUAL_ICAP_NET_MARGIN_PER_KW(I), &
                     ANNUAL_ICAP_CONE_VALUE(I))

         ANNUAL_ICAP_RESERVE_MARGIN(I) = &
                  ANNUAL_ICAP_RESERVE_MARGIN(I)/12.0
         ANNUAL_ICAP_DEMAND_POINT(I) = &
                  ANNUAL_ICAP_DEMAND_POINT(I)/12.0

         ANNUAL_ICAP_ADJ_CAPACITY_VALUE(I) = &
               ANNUAL_ICAP_INSTL_CAPACITY_VALUE(I) - &
                                      ICAP_EAS_REVENUE_OFFSET
            imm_year=globecom_YEAR
            imm_i=i
         IF(MARGINAL_ICAP(imm_year,imm_I)) THEN
! 031210
            imm_i=i

            CALL GET_MRX_ICAP_REPORT_VALUES( &
                                       imm_I, &
                                       MRX_CAPACITY, &
                                       MRX_ENERGY, &
                                       MRX_TOTAL_REVENUE, &
                                       MRX_TOTAL_VARIABLE, &
                                       MRX_FUEL, &
                                       MRX_VOM, &
                                       MRX_EMISSIONS, &
                                       MRX_TOTAL_COST, &
                                       MRX_ANN_CAP_COST, &
                                       MRX_NET_MARGIN_MM, &
                                       MRX_NET_MARGIN_PER_MWH, &
                                       MRX_NET_MARGIN_PER_KWMO, &
                                       MRX_CONE_PER_KWMO, &
                                       MRX_EAS_REV_PER_KWMO, &
                                       MRX_ICAP_VALUE_POINT)
!
            ANNUAL_ICAP_PEAK_MONTH_CAP = MRX_CAPACITY
            ANNUAL_ICAP_TOTAL_MWH(I) = MRX_ENERGY
            ANNUAL_ICAP_TOTAL_COST(I) = MRX_TOTAL_COST
            ANNUAL_ICAP_TOTAL_REVENUE(I) = MRX_TOTAL_REVENUE
            ANNUAL_ICAP_LEVEL_CAPITAL_COST(I) = &
                                               MRX_ANN_CAP_COST
            ANNUAL_ICAP_VALUE_POINT(I) = &
                                       MRX_ICAP_VALUE_POINT ! /12.
            ANNUAL_ICAP_NET_MARGIN(I) = MRX_NET_MARGIN_MM
            ANNUAL_ICAP_NET_MARGIN_PER_MWH(I) = &
                                         MRX_NET_MARGIN_PER_MWH
            ANNUAL_ICAP_NET_MARGIN_PER_KW(I) = &
                                    MRX_NET_MARGIN_PER_KWMO !/12.
            ANNUAL_ICAP_MRX_CONE(I) = MRX_CONE_PER_KWMO / 12.
! 042717.
            ICAP_MRX_EAS_PER_KW = MRX_EAS_REV_PER_KWMO !

         endif
         MX_ICAP_ANNUAL_ALT_REC = RPTREC(MX_ICAP_ANNUAL_ALT_NO)
         INQUIRE(UNIT=1809,OPENED=FILE_OPENED)
         IF(.NOT. FILE_OPENED) THEN
            WRITE(9,*) "ERROR IN MX ICAP REPORT OPEN STATUS"
     er_message='Stop requested from Clreport SIID34'
            call end_program(er_message)
         endif
         WRITE(MX_ICAP_ANNUAL_ALT_NO, &
                                    REC=MX_ICAP_ANNUAL_ALT_REC) &
               FLOAT(gc_end_point), &
               FLOAT(globecom_YEAR+get_base_year()), &
               CL_MONTH_NAME(13), &
               CM_NAME, &
               ANNUAL_ICAP_PEAK_MONTH_CAP, & ! 0
               ANNUAL_ICAP_TOTAL_MWH(I), &
               ANNUAL_ICAP_TOTAL_COST(I), &
               ANNUAL_ICAP_TOTAL_REVENUE(I), &
               ANNUAL_ICAP_NET_MARGIN(I), &
               ANNUAL_ICAP_NET_MARGIN_PER_MWH(I), &
               ANNUAL_ICAP_NET_MARGIN_PER_KW(I), & ! /12.,
               ANNUAL_ICAP_LEVEL_CAPITAL_COST(I), &
               ANNUAL_ICAP_CONE_VALUE(I)/12., & 
               ! ANNUAL_ICAP_MRX_CONE(I)/12.,
               ANNUAL_ICAP_INSTL_CAPACITY_VALUE(I), & ! 9
               ANNUAL_ICAP_RESERVE_MARGIN(I), & !10
               ANNUAL_ICAP_DEMAND_POINT(I), &
               ANNUAL_ICAP_VALUE_POINT(I)/12, & ! 12
               ANNUAL_ICAP_MRX_CONE(I), & ! /12., ! 13 MRX CONE
               ICAP_MRX_EAS_PER_KW, & ! /12.,         ! 14
               ICAP_EAS_REVENUE_OFFSET, & ! 15
               ANNUAL_ICAP_ADJ_CAPACITY_VALUE(I), & ! 16
               ANNUAL_ICAP_VAR_COST(I), &
               ANNUAL_ICAP_FUEL_COST(I), &
               ANNUAL_ICAP_START_COST(I), & !19
               ANNUAL_ICAP_VOM_COST(I), &
               ANNUAL_ICAP_EMISSION_COST(I)  ! 21
         MX_ICAP_ANNUAL_ALT_REC = MX_ICAP_ANNUAL_ALT_REC + 1
      ENDDO ! PG
   ENDIF ! ANNUAL REPORT
!
ENDIF ! ICAP REPORT
! TODO: EXTRACT_METHOD prepare_icap_summary_report (end)
end subroutine prepare_icap_summary_report
end module icap_summary_report