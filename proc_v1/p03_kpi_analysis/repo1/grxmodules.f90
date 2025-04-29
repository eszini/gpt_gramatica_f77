
!     GRXModules.F95
!     Copyright(c)  2000
!
!     Created: 9/2/2009 12:15:27 PM
!     Author : MARK S GERBER

module grxmodules
use GRX_PLANNING_ROUTINES
use prod_arrays_dimensions
use eco
use annual_cl_unit
use flusher
implicit none
contains


SUBROUTINE GRX_RETROFIT_UNIT_LIST(CO2_UNIT,CO2_RETRO_TYPE, &
CO2_DECISION_COST)
USE GRX_PLANNING_ROUTINES
INTEGER (KIND=2) :: CO2_UNIT,CO2_RETRO_TYPE
REAL (KIND=4) :: CO2_DECISION_COST
   IF(UNITS_RETROFITTED == 0) THEN
      RETROFIT_LIST = 0
      RETROFIT_TYPE_FIXED_GRX = 0
      RETROFIT_CO2_DECISION_COST = 0.
   ENDIF
   UNITS_RETROFITTED =  UNITS_RETROFITTED + 1
   RETROFIT_LIST(UNITS_RETROFITTED) = CO2_UNIT
   RETROFIT_TYPE_FIXED_GRX(CO2_UNIT) = CO2_RETRO_TYPE
   RETROFIT_CO2_DECISION_COST(CO2_UNIT) = CO2_DECISION_COST
END SUBROUTINE GRX_RETROFIT_UNIT_LIST
!-------
LOGICAL FUNCTION GRX_RETROFITED_UNIT(CO2_UNIT,RETRO_CAP_CO2_MULT)
USE GRX_PLANNING_ROUTINES
INTEGER (KIND=2) :: CO2_UNIT,RETRO_ID,RETROFIT_ACTIVE_ID
REAL (KIND=4) :: RETRO_CAP_CO2_MULT, &
                 CO2_Min_Capacity_Change, &
                 CO2_Max_Capacity_Change, &
                 CO2_HEATRATE_MULITPLIER, &
                 CO2_REMOVAL_PERCENTAGE

   GRX_RETROFITED_UNIT = .FALSE.
   IF(RETROFIT_TYPE_FIXED_GRX(CO2_UNIT) == 2) THEN
      RETRO_ID = RETROFIT_ACTIVE_ID(CO2_UNIT)
      CALL RETURN_RETROFIT_PROJECT_IMPACTS(RETRO_ID,  &
                                           CO2_Min_Capacity_Change, &
                                           CO2_Max_Capacity_Change, &
                                           CO2_HEATRATE_MULITPLIER, &
                                           CO2_REMOVAL_PERCENTAGE)
      RETRO_CAP_CO2_MULT = CO2_Max_Capacity_Change
      GRX_RETROFITED_UNIT = .TRUE.
   ENDIF
END FUNCTION GRX_RETROFITED_UNIT
!-------
      SUBROUTINE GRX_RETIREMENT_RETROFIT_RPT(base_yr,YR,YEAR_END)
USE CLA_OBJT_ARRAYS
USE CAPACITY_OPTIONS_ALLOC_VARS
USE conversion_routines
USE TRANS_GROUP_VARIABLES
USE GRX_PLANNING_ROUTINES
use prod_arrays_dimensions
use hesi
USE INTERNAL_OPERATION_SWITCHES
use mod_base_year
	  integer (kind=2) :: YR, base_yr
LOGICAL (KIND=1), SAVE :: RPT_FILE_OPEN=.FALSE.,RPT_FILE_OPEN_2=.FALSE.
CHARACTER (LEN=30) :: H2_STR30,EV_STR30,DATE_STR
CHARACTER (LEN=5) :: GET_SCENAME
CHARACTER (LEN=7) :: OFF_LINE_STR
INTEGER (KIND=2) :: UNIT_NO,TG,TG_POSITION,I,GET_TRANS_GROUP_POSITION, &
                    get_TRANSACTION_GROUP,YEAR_END, &
                    RETRO_ID,RETROFIT_ACTIVE_ID, &
                    RETIREMENT_YEAR
CHARACTER (LEN=20) :: RETURN_UNITNM
REAL (KIND=4) :: CO2_Min_Capacity_Change,CO2_Max_Capacity_Change, &
                 CO2_HEATRATE_MULITPLIER,CO2_REMOVAL_PERCENTAGE, &
                 RETRO_VOM,RETRO_FOM,Decision_CO2_Price,TEMP_R4, &
                 CL_CAPACITY_PLANNING_ADJ
LOGICAL (KIND=1) :: GET_CO2_RETIREMENTS_LOGIC,CO2_RETROFIT_LOGIC_ACTIVE
character (len=1024) :: recln
!
	IF(.NOT.(GET_CO2_RETIREMENTS_LOGIC().OR.&
		CO2_RETROFIT_LOGIC_ACTIVE())) THEN
		RETURN
	end if

	IF(.NOT. RPT_FILE_OPEN) THEN
	   OPEN(8337,FILE="GRX2EnterprizeRetireRetro-"// &
		TRIM(GET_SCENAME())//".CSV",STATUS="REPLACE")
	   IF(INSTALLED_POINTER + UNITS_RETROFITTED > 0) THEN
		  WRITE(8337,"(1X,5A)") &
		   '"MktSymArea Names",MktSymAreaAbbrev,"Unit Name",', &
		   '"EV ID","HESI ID",Date,ModType,"VOM Adder","FOM Adder", &
			"Max Capacity Derate Multiplier",', &
		   '"Min Capacity Derate Multiplier","HR Multiplier", &
		   "CO2 Removal Percent","MWs Retired",' , &
		   '"Endpoint","Unit CO2 Cost","CO2 Price","EBITDA1", &
		   "EBITDA2","EBITDA3","EBITDA4","Off-line Date"'
		  RPT_FILE_OPEN = .TRUE.
		  CALL flush_unit(INT(8337,2))
	   ELSE
		  CLOSE(8337,STATUS='DELETE')
	   ENDIF
	ENDIF
	IF(.NOT. RPT_FILE_OPEN_2) THEN
	   OPEN(8339,FILE="GRXMRXRetireRetro-"//TRIM(GET_SCENAME())// &
	   ".CSV",STATUS="REPLACE")
	   IF(INSTALLED_POINTER + UNITS_RETROFITTED > 0) THEN
		  WRITE(8339,"(1X,5A)") '"Transaction Group","Unit Name",', &
		   '"EV ID",Date,ModType,"VOM Adder","FOM Adder", &
		   "Max Capacity Derate Multiplier",', &
		   '"Min Capacity Derate Multiplier","HR Multiplier", &
		   "CO2 Removal Percent","MWs Retired",' , &
		   '"Endpoint","Unit CO2 Cost","CO2 Price","EBITDA1", &
		   "EBITDA2","EBITDA3","EBITDA4",', &
		   '"Regional CO2 Price","Off-Line Date"'
		  RPT_FILE_OPEN_2 = .TRUE.
		  CALL flush_unit(INT(8339,2))
	   ELSE
		  CLOSE(8339,STATUS='DELETE')
	   ENDIF
	ENDIF
	IF(GET_CO2_RETIREMENTS_LOGIC() .AND. INSTALLED_POINTER > 0) THEN
	   RETIREMENT_YEAR = base_yr+YR+1
	   DO I = 1, INSTALLED_POINTER
		  UNIT_NO = TEMP_RETIRED_UNIT_NO(I)
		IF(GRX_RETIRE_THIS_YEAR .AND. &
			RETIREMENT_YEAR <= YEAR_END) THEN
		TEMP_R4 = CL_CAPACITY_PLANNING_ADJ( &
		RETIREMENT_YEAR,UNIT_NO,2,.TRUE.)
      ENDIF
      RETROFIT_CANDIDATE(UNIT_NO) = 'F'
      RETIREMENT_CANDIDATE(UNIT_NO) = 'F'
      FIRST_RETIREMENT_YEAR(UNIT_NO) = 9999 ! RETIRED
      TG = MAX(INT(1,2),get_TRANSACTION_GROUP(UNIT_NO))
      TG_POSITION = MAX(INT(1,2),GET_TRANS_GROUP_POSITION(TG))
      H2_STR30 = CONVERT_2_STR(H2_UNIT_ID_NUM(UNIT_NO))
      EV_STR30 = CONVERT_2_STR(HESI_UNIT_ID_NUMs(UNIT_NO))   ! EV ID
      OFF_LINE_STR = TRIM(CONVERT_2_STR(INT(OFF_LINE_MONTH(UNIT_NO),4)))// &
                  '/'//CONVERT_2_STR(INT(OFF_LINE_YEAR(UNIT_NO),4))
      IF(GRX_RETIRE_THIS_YEAR) THEN
         DATE_STR = "12/31/"//CONVERT_2_STR(INT(base_yr+YR,4))
      ELSE
         DATE_STR = "12/31/"//CONVERT_2_STR(INT(base_yr+YR-1,4))
      ENDIF
      recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'",'
      recln = trim(recln)//'"'// &
		TRIM(TG_BASECASE_MARKET_AREA_ID(TG_POSITION))//'",' 
      recln = trim(recln)//'"'//TRIM(RETURN_UNITNM(UNIT_NO))//'",'    
      recln = trim(recln)//TRIM(EV_STR30)//','     ! EV ID
      recln = trim(recln)//TRIM(H2_STR30)//','     ! HESI ID
      recln = trim(recln)//'"'//TRIM(DATE_STR)//'",Retirement,'// & 
                   '0.,0.,0.,0.,0.,100.,'// &
                   TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
      Decision_CO2_Price = sum(ECON_RETIRE_MARGIN(UNIT_NO,1:4))
      recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// &
	                ! Endpoint
                    TRIM(CONVERT_2_STR(Decision_CO2_Price))//','// &
                    TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','
			recln = trim(recln)// &
				TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
            TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// &
            ! Endpoint
            TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
            TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
            TRIM(OFF_LINE_STR)//','
      WRITE(8337,'(A)') trim(recln)   ! Enterprize Interface

! Doug requested interface to Alan program
      recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'",'
      recln = trim(recln)//'"'//TRIM(RETURN_UNITNM(UNIT_NO))//'",'
      ! Unit Name
      recln = trim(recln)//TRIM(EV_STR30)//','     ! EV ID
      recln = trim(recln)//'"'//TRIM(DATE_STR)//'",Retirement,'// &
                   ! Date 
                   '0.,0.,0.,0.,0.,100.,'// &
                   TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
      Decision_CO2_Price = sum(ECON_RETIRE_MARGIN(UNIT_NO,1:4))
      recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// & 
                    TRIM(CONVERT_2_STR(Decision_CO2_Price))//','// &
                    TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','
      recln = trim(recln)// &
		TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
      TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// & 
      TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
      TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
      TRIM(CONVERT_2_STR(RETIREMENT_CO2_COST_PER_TON(UNIT_NO)))//',"'// &
      TRIM(OFF_LINE_STR)//'",'
      WRITE(8339,'(A)') trim(recln)   ! Alan Interface
   ENDDO
   CALL flush_unit(INT(8337,2))
   CALL flush_unit(INT(8339,2))
ENDIF
IF(.NOT. (CO2_RETROFIT_LOGIC_ACTIVE() .AND.  &
	UNITS_RETROFITTED > 0)) THEN
	RETURN
ENDIF
DO I = 1, UNITS_RETROFITTED
   UNIT_NO = RETROFIT_LIST(I)
   RETIREMENT_CANDIDATE(UNIT_NO) = 'F'
   FIRST_RETIREMENT_YEAR(UNIT_NO) = 9999 ! RETIRED
   RETROFIT_CANDIDATE(UNIT_NO) = 'F'
   TG = MAX(INT(1,2),get_TRANSACTION_GROUP(UNIT_NO))
   TG_POSITION = MAX(INT(1,2),GET_TRANS_GROUP_POSITION(TG))
   RETRO_ID = RETROFIT_ACTIVE_ID(UNIT_NO)
   CALL RETROFIT_VOM_FOM_ADDERS(RETRO_ID,RETRO_VOM,RETRO_FOM)
   CALL RETURN_RETROFIT_PROJECT_IMPACTS(RETRO_ID,  &
                                        CO2_Min_Capacity_Change, &
                                        CO2_Max_Capacity_Change, &
                                        CO2_HEATRATE_MULITPLIER, &
                                        CO2_REMOVAL_PERCENTAGE)
   CO2_Min_Capacity_Change = 1.-CO2_Min_Capacity_Change
   CO2_Max_Capacity_Change = 1.-CO2_Max_Capacity_Change
   H2_STR30 = CONVERT_2_STR(H2_UNIT_ID_NUM(UNIT_NO))
   EV_STR30 = CONVERT_2_STR(HESI_UNIT_ID_NUMs(UNIT_NO))   ! EV ID
   DATE_STR = "1/1/"//CONVERT_2_STR(INT(get_base_year()+YR,4))
   recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'","'//  &
                TRIM(TG_BASECASE_MARKET_AREA_ID(TG_POSITION))//'","'// &
                ! MktSymAreaAbbrev
                TRIM(RETURN_UNITNM(UNIT_NO))//'",'    ! Unit Name
   recln = trim(recln)//TRIM(EV_STR30)//','// & ! EV ID
                        TRIM(H2_STR30)//','     ! HESI ID
   IF(RETROFIT_TYPE_FIXED_GRX(UNIT_NO) == 4) THEN
      recln = trim(recln)//'"'//TRIM(DATE_STR)//'","Fixed Retrofit",'
      ! Date
   ELSE
      recln = trim(recln)//'"'//TRIM(DATE_STR)//'","GRX Retrofit",'
      ! Date
   ENDIF
   recln = trim(recln)//TRIM(CONVERT_2_STR(RETRO_VOM))//','// &
           ! VOM Adder
           TRIM(CONVERT_2_STR(RETRO_FOM))//','// & ! FOM Adder
           TRIM(CONVERT_2_STR(CO2_Min_Capacity_Change))//','// &
           ! Capacity
           TRIM(CONVERT_2_STR(CO2_Max_Capacity_Change))//','
           ! Derate Multipliear
   recln = trim(recln)//TRIM(CONVERT_2_STR(CO2_HEATRATE_MULITPLIER))//','// &
                 TRIM(CONVERT_2_STR(CO2_REMOVAL_PERCENTAGE))//','// & 
                 TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
   recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// &  
           TRIM(CONVERT_2_STR(RETROFIT_CO2_DECISION_COST(UNIT_NO)))//','// &
           TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','// &
           TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
           TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// &
           TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
           TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
           TRIM(OFF_LINE_STR)//','
   WRITE(8337,'(A)') trim(recln)
! Alan interface
       recln = '"'//TRIM(TRANS_GROUP_FULL_NAME(TG_POSITION))//'","'// &
                    TRIM(RETURN_UNITNM(UNIT_NO))//'",'    ! Unit Name
       recln = trim(recln)//TRIM(EV_STR30)//','  ! EV ID
       IF(RETROFIT_TYPE_FIXED_GRX(UNIT_NO) == 4) THEN
        recln = trim(recln)//'"'//TRIM(DATE_STR)//'","Fixed Retrofit",'  ! Date
       ELSE
        recln = trim(recln)//'"'//TRIM(DATE_STR)//'","GRX Retrofit",'  ! Date
       ENDIF
       recln = trim(recln)//TRIM(CONVERT_2_STR(RETRO_VOM))//','// &
            ! VOM Adder
            TRIM(CONVERT_2_STR(RETRO_FOM))//','// & ! FOM Adder
            TRIM(CONVERT_2_STR(CO2_Min_Capacity_Change))//','// &
            ! Capacity
            TRIM(CONVERT_2_STR(CO2_Max_Capacity_Change))//','
            ! Derate Multipliear
       recln = trim(recln)//TRIM(CONVERT_2_STR(CO2_HEATRATE_MULITPLIER))//','// &
                  TRIM(CONVERT_2_STR(CO2_REMOVAL_PERCENTAGE))//','// &
                  ! CO2 Removal Percent
                  TRIM(CONVERT_2_STR(RETIREMENTS_MW_RPT(UNIT_NO)))//','
       recln = trim(recln)//TRIM(CONVERT_2_STR(PRT_ENDPOINT()))//','// &
       TRIM(CONVERT_2_STR(RETROFIT_CO2_DECISION_COST(UNIT_NO)))//','// &
       TRIM(CONVERT_2_STR(CURRENT_CO2_DISPATCH_COST))//','// &
       TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,1)))//','// &
       TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,2)))//','// &
       TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,3)))//','// &
       TRIM(CONVERT_2_STR(ECON_RETIRE_MARGIN(UNIT_NO,4)))//','// &
       TRIM(CONVERT_2_STR(RETIREMENT_CO2_COST_PER_TON(UNIT_NO)))//',"'// &
       TRIM(OFF_LINE_STR)//'",'
       WRITE(8339,'(A)') trim(recln)
    ENDDO
    CALL flush_unit(INT(8337,2))
    CALL flush_unit(INT(8339,2))
    UNITS_RETROFITTED = 0
 1000 FORMAT(3('"',A,'",'),2(A,','),2('"',A,'",'),20(A,','))
      END SUBROUTINE GRX_RETIREMENT_RETROFIT_RPT
!-------
SUBROUTINE CO2_MARKET_REDUCTION(CO2_CREDITS_AVAILABLE, &
                                CO2_CREDIT_PRICE)
!-------
USE conversion_routines
use grx_planning_routines
use globecom
use mod_base_year



REAL (KIND=4) :: CO2_CREDITS_AVAILABLE,CO2_CREDIT_PRICE
!
WRITE(9,1009) gc_end_point,  &
                      globecom_year+get_BASE_YEAR() , &
                      globecom_year+get_BASE_YEAR() , &
              "CO2 Market", &
              "CO2 Mkt Purchase-"//CONVERT_2_STR(CO2_CREDIT_PRICE), & ! 20 CHARACTERS
              0., &
              CO2_CREDITS_AVAILABLE, &
              CO2_CREDIT_PRICE
!
 1009 FORMAT(1X,I4,5X,I4,4X,I4,2X,A,2X,A20,1X,F9.1,F12.0,F7.2)
      END SUBROUTINE CO2_MARKET_REDUCTION
!-------
REAL FUNCTION NEWTON_ABAT_PRICE_SEARCH(MIN_POINT,MAX_POINT, &
                                       MIN_CO2,MAX_CO2,CO2_CAP) &
                                                       RESULT(NEW_PRICE)
   REAL (KIND=4), OPTIONAL :: MIN_POINT,MAX_POINT,MIN_CO2, &
   	MAX_CO2, CO2_CAP
   REAL (KIND=4), SAVE :: MIN_PRICE=0.,MAX_PRICE=0., &
                          MIN_CO2_EMISS=0.,MAX_CO2_EMISS=0., &
                          EMISS_CAP=0.
   IF(PRESENT(MAX_POINT)) MAX_PRICE = MAX_POINT
   IF(PRESENT(MIN_POINT)) MIN_PRICE = MIN_POINT
   IF(PRESENT(MIN_CO2)) MIN_CO2_EMISS = MIN_CO2
   IF(PRESENT(MAX_CO2)) MAX_CO2_EMISS = MAX_CO2
   IF(PRESENT(CO2_CAP)) EMISS_CAP = CO2_CAP
   IF(MIN_CO2_EMISS-MAX_CO2_EMISS == 0.) THEN
      NEW_PRICE = (MAX_PRICE + MIN_PRICE)/2.
   ELSE
      NEW_PRICE = (EMISS_CAP-MIN_CO2_EMISS)*(MIN_PRICE-MAX_PRICE)/ &
                   (MIN_CO2_EMISS-MAX_CO2_EMISS)+ MIN_PRICE
   ENDIF
END FUNCTION NEWTON_ABAT_PRICE_SEARCH
!-------
REAL FUNCTION GRX_NEWTON_CONVERGENCE(TG)
   use grx_data
   use co2_data
   REAL (KIND=4) :: GET_EMISS_CAP_FOR_CLASS, &
                    CO2_EMISSIONS_CAP, CO2_EMISSIONS
   INTEGER (KIND=2), parameter :: const_GRX_ITERATIONS=1
   integer (kind=2) :: TG

   ! This only processes const_GRX_ITERATIONS with value 1
   CO2_EMISSIONS_CAP = GET_EMISS_CAP_FOR_CLASS(3_2,TG)
   CO2_EMISSIONS = GET_CO2_EMISS_FOR_SYSTEM()
   IF(const_GRX_ITERATIONS == 1) THEN ! It's always 1. Why the IF?
      IF(CO2_EMISSIONS < CO2_EMISSIONS_CAP) THEN
         IF(CURRENT_CO2_DISPATCH_COST == 0.) THEN
            GRX_CONVERGED = .TRUE.
         ENDIF
      ENDIF
   ENDIF
   GRX_NEWTON_CONVERGENCE = 1.
END FUNCTION GRX_NEWTON_CONVERGENCE
!-------
REAL FUNCTION CALCULATE_CO2_EMISS_FOR_SYSTEM()
        use nunits_shared
        implicit none
  INTEGER (KIND=2) :: I
   CO2_EMISSIONS_FOR_SYSTEM = 0.
   DO I = 1, GET_NUNITS()
      CO2_EMISSIONS_FOR_SYSTEM = CO2_EMISSIONS_FOR_SYSTEM + &
      ANNUAL_CL_UNIT_EMISSIONS(3,I)
   ENDDO
   CALCULATE_CO2_EMISS_FOR_SYSTEM = CO2_EMISSIONS_FOR_SYSTEM
END function CALCULATE_CO2_EMISS_FOR_SYSTEM
!-------
LOGICAL FUNCTION GRX_WITH_MRX_PLANNING()
   GRX_WITH_MRX_PLANNING = .true. ! .FALSE.
END FUNCTION GRX_WITH_MRX_PLANNING

LOGICAL FUNCTION HardWiredRetrofitProject(R_NUNITS)
 USE CLA_OBJT_ARRAYS
	   use globecom
       use mod_base_year
 INTEGER (KIND=2) :: R_NUNITS,RETROFIT_PROJECT_YEAR
 
 HardWiredRetrofitProject = .FALSE.
 IF(CO2_CONTROL_PERCENT(R_NUNITS) >= 0.) THEN
    RETROFIT_PROJECT_YEAR=(CO2_CONTROL_DATE(R_NUNITS)-10000)/100 &
                             + 2000  ! should be a valid year ie 2028
    HardWiredRetrofitProject = get_base_year() < RETROFIT_PROJECT_YEAR &
                   .AND. RETROFIT_PROJECT_YEAR <= gc_last_study_year
 ENDIF
END FUNCTION HardWiredRetrofitProject
LOGICAL FUNCTION HWRetrofitProjectThisYear(R_NUNITS)
 USE CLA_OBJT_ARRAYS
	   use globecom
       use mod_base_year
 INTEGER (KIND=2) :: R_NUNITS,RETROFIT_PROJECT_YEAR
 
 HWRetrofitProjectThisYear = .FALSE.
 IF(CO2_CONTROL_PERCENT(R_NUNITS) >= 0.) THEN
    RETROFIT_PROJECT_YEAR=(CO2_CONTROL_DATE(R_NUNITS)-10000)/100 &
                           + 2000  ! should be a valid year ie 2028
    HWRetrofitProjectThisYear = RETROFIT_PROJECT_YEAR == get_base_year() + globecom_YEAR
 ENDIF
END FUNCTION HWRetrofitProjectThisYear

end module grxmodules