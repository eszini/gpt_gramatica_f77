!     ******************************************************************
!     Fuelused.for
!     Created: 10/14/02 1:10:21 PM
!     Author : msg
!     Last change: msg 8/14/2016 11:01:57 AM
!     ******************************************************************

C**********************************************************************
C
C      FUEL USAGE MODULE FOR SWITCHING FUEL TYPES BASED ON INVENTORY
C                        COPYRIGHT (C) 1992
C                   M.S. GERBER & ASSOCIATES, INC.
C                       ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE FUEL_INVENTORY(MMBTUS,MMBTUS_USED,MMBTU_FUEL_BALANCE,
     +                          USE_SECONDARY_FUEL,
     +                          FUEL_MIX,
     +                          EMISS_BLENDING_RATE_ord)
      use fuelused_decs
      implicit none
      LOGICAL*1 USE_SECONDARY_FUEL
      REAL*8 MMBTUS,DBLE_ZERO
      PARAMETER (DBLE_ZERO = 0.D0)
      REAL*8 MMBTU_FUEL_BALANCE,MMBTUS_USED
      REAL*4 FUEL_MIX,EMISS_BLENDING_RATE_ord
C
      MMBTUS_USED = ABS(FUEL_MIX)*MMBTUS*(1.-EMISS_BLENDING_RATE_ord)
      IF(MMBTUS_USED >= MMBTU_FUEL_BALANCE) THEN
         MMBTUS_USED = MAX(DBLE_ZERO,MMBTU_FUEL_BALANCE)
         MMBTU_FUEL_BALANCE = DBLE_ZERO
         USE_SECONDARY_FUEL = .TRUE.
      ELSE
         MMBTU_FUEL_BALANCE = MAX(DBLE_ZERO,
     +                                 MMBTU_FUEL_BALANCE - MMBTUS_USED)
         USE_SECONDARY_FUEL = .FALSE.
      ENDIF
      IF(EMISS_BLENDING_RATE_ord /= 1.) THEN
         MMBTUS_USED = MMBTUS_USED/DBLE((1.-EMISS_BLENDING_RATE_ord))
      ELSE
         MMBTUS_USED = DBLE(ABS(FUEL_MIX)) * MMBTUS
      ENDIF
      IF(MMBTUS /= DBLE_ZERO) FUEL_MIX = SNGL(MMBTUS_USED/MMBTUS)
      RETURN
      END
C**********************************************************************
C
C      SETS THE FUEL INVENTORY DATA FOR ANNUAL AND MONTHLY AMOUNTS
C                        COPYRIGHT (C) 1992
C                   M.S. GERBER & ASSOCIATES, INC.
C                       ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE BUILD_FUEL_INVENTORIES(MMBTU_FUEL_BALANCE,
     +                                  FUEL_INVENTORY_ID,
     +                                  MONTHLY_INVENTORY_ACTIVE,
     +                                  FUEL_INVENTORY_ACTIVE)
C

      use spindriftlib
      use prod_arrays_dimensions 
      use sizecom
      use program_state

C
!
      INTEGER*2 MAX_FUEL_INV_SERIES
      PARAMETER (MAX_FUEL_INV_SERIES=1024)
!
      REAL*8   BTU_CONTENT,
     +         LOCAL_BTU_CONTENT,
     +         GET_VAR,
     +         ANNUAL_AMOUNT(:),
     +         PREVIOUS_ANNUAL_AMOUNT(:),
     +         MONTHLY_AMOUNT(:)
      INTEGER*2 YR,IN_REC,DELETE,START_MO,END_MO,YEAR,I,
     +          PSMO,PEMO,FUEL_INVENTORIES_FOUND,INDEX,FUEL_ID_POINTR,
     +          R_FUEL_INVENTORIES_FOUND,R_I
      INTEGER IOS
      REAL TAKE_OR_PAY_PENALTY,R4_BTU_CONTENT
      INTEGER*2 PENALTY_ESCALATION_VECTOR,PERIOD,
     +          MONTH,IREC,LREC,RUN_YEARS,YEARS_TO_RUN
      CHARACTER*1 INVENTORY_TREATMENT
      CHARACTER*64 IOS_MESSAGE
      INTEGER*4 ERROR
      LOGICAL*4 FUEL_INVENTORY_ACTIVE
      INTEGER*2 FUEL_ID,ANNUAL_INDEX,MONTHLY_INDEX,INVENTORY_INDEX
      INTEGER*2 FUEL_GROUP,fu_mo
      INTEGER*2 DAYS_IN
      CHARACTER*30 DESC,UNITS*1,FUEL_STRATEGY*1
!      
      LOGICAL*1 SAVE_FUEL_INVENTORY_ACTIVE/.FALSE./,
     +            R_FUEL_INVENTORY_ACTIVE
      INTEGER*2 SAVE_FUEL_INVENTORY_ID(MAX_FUEL_INV_SERIES),R_FUEL_ID
!      
      REAL*8 MMBTU_FUEL_BALANCE(0:MAXIMUM_FUEL_TYPES),CONVERSION_FACTOR,
     +         SAVE_MMBTU_FUEL_BALANCE(0:MAXIMUM_FUEL_TYPES),
     +         R_MMBTU_FUEL_BALANCE
      REAL*8 PERIOD_AMOUNT
      INTEGER*2 FUEL_INVENTORY_ID(0:1024)
      INTEGER*2 ANNUAL_FILE_POINTR(:),MONTHLY_FILE_POINTR(:),
     +          MMBTU_BALANCE_POINTR(:),R_MMBTU_BALANCE_POINTR
      ALLOCATABLE :: ANNUAL_FILE_POINTR,MONTHLY_FILE_POINTR,
     +               MMBTU_BALANCE_POINTR
      LOGICAL*1 FIRST_TIME_FOR_THIS_MONTHLY(:,:),
     +          FUEL_INVENTORY_IS_MONTHLY(:),
     +          FUEL_INVENTORY_IS_ANNUAL(:),
     +          MONTHLY_INVENTORY_ACTIVE,
     +          TEMP_L
      ALLOCATABLE :: ANNUAL_AMOUNT,PREVIOUS_ANNUAL_AMOUNT,
     +               FIRST_TIME_FOR_THIS_MONTHLY,
     +               FUEL_INVENTORY_IS_MONTHLY,
     +               FUEL_INVENTORY_IS_ANNUAL,
     +               MONTHLY_AMOUNT
      SAVE FUEL_INVENTORY_IS_MONTHLY,
     +     ANNUAL_FILE_POINTR,MONTHLY_FILE_POINTR,
     +     MMBTU_BALANCE_POINTR,FUEL_INVENTORIES_FOUND,
     +     SAVE_MMBTU_FUEL_BALANCE,
     +     SAVE_FUEL_INVENTORY_ID,
     +               ANNUAL_AMOUNT,
     +               PREVIOUS_ANNUAL_AMOUNT,
     +               FIRST_TIME_FOR_THIS_MONTHLY,
     +               FUEL_INVENTORY_IS_ANNUAL,
     +               MONTHLY_AMOUNT
! END DATA DECLARATIONS
!
      CALL OPEN_FUEL_INVENTORY_FILE(10,FUEL_INVENTORY_ACTIVE)
!      
!
      IF(.NOT. FUEL_INVENTORY_ACTIVE) THEN
         SAVE_FUEL_INVENTORY_ACTIVE =  .FALSE.
      ELSE
         SAVE_FUEL_INVENTORY_ACTIVE =  .TRUE.
      ENDIF
!      
      IF(.NOT. FUEL_INVENTORY_ACTIVE) RETURN
C
      IF(ALLOCATED(FUEL_INVENTORY_IS_MONTHLY))
     +               DEALLOCATE(FUEL_INVENTORY_IS_MONTHLY,
     +                          ANNUAL_FILE_POINTR,
     +                          MONTHLY_FILE_POINTR,
     +                          MMBTU_BALANCE_POINTR,STAT=ERROR)
      ALLOCATE(FUEL_INVENTORY_IS_MONTHLY(0:MAX_FUEL_INV_SERIES))
      ALLOCATE(ANNUAL_FILE_POINTR(0:MAX_FUEL_INV_SERIES))
      ALLOCATE(MONTHLY_FILE_POINTR(0:MAX_FUEL_INV_SERIES))
      ALLOCATE(MMBTU_BALANCE_POINTR(MAXIMUM_FUEL_TYPES))
!
      IF(ALLOCATED(PREVIOUS_ANNUAL_AMOUNT)) 
     +           DEALLOCATE(PREVIOUS_ANNUAL_AMOUNT,
     +                      FIRST_TIME_FOR_THIS_MONTHLY,
     +                      FUEL_INVENTORY_IS_ANNUAL,
     +                      ANNUAL_AMOUNT,
     +                      MONTHLY_AMOUNT) ! ,STAT=ERROR)
!
      ALLOCATE(PREVIOUS_ANNUAL_AMOUNT(0:AVAIL_DATA_YEARS))
      ALLOCATE(ANNUAL_AMOUNT(0:AVAIL_DATA_YEARS))
      ALLOCATE(MONTHLY_AMOUNT(0:AVAIL_DATA_YEARS))
      ALLOCATE(FIRST_TIME_FOR_THIS_MONTHLY(12,0:MAX_FUEL_INV_SERIES))
      ALLOCATE(FUEL_INVENTORY_IS_ANNUAL(0:MAX_FUEL_INV_SERIES))
C
      MONTHLY_AMOUNT = 0.0D0
      PREVIOUS_ANNUAL_AMOUNT = 0.0D0
      ANNUAL_AMOUNT = 0.0D0
      MMBTU_FUEL_BALANCE = 0.0D0
!
      SAVE_MMBTU_FUEL_BALANCE = 0.0D0
!
      MMBTU_BALANCE_POINTR = 0
      ANNUAL_FILE_POINTR = 0
      MONTHLY_FILE_POINTR = 0
      TEMP_L = .FALSE.
      FUEL_INVENTORY_IS_MONTHLY = .FALSE.
      FUEL_INVENTORY_IS_ANNUAL = .FALSE.
      TEMP_L = .TRUE.
      FIRST_TIME_FOR_THIS_MONTHLY = .TRUE.
!
C
C OPEN ANNUAL AND MONTHLY OUTPUT FILE
C
      LREC = 248 + 8 * (MAX_SIMULATION_YEARS+1)
!      LREC = 248 + 8 * (MAX_SIMULATION_YEARS+1)
      OPEN(UNIT=111,FILE='BCFLINVA.BIN',ACCESS='DIRECT',RECL=LREC)
      OPEN(UNIT=112,FILE='BCFLINVM.BIN',ACCESS='DIRECT',RECL=LREC)
      IN_REC = 1
      INVENTORY_INDEX = 0
      ANNUAL_INDEX = 0
      MONTHLY_INDEX = 0
      YEARS_TO_RUN = RUN_YEARS()
      DO
         READ(10,REC=IN_REC,IOSTAT=IOS) DELETE,DESC,FUEL_ID,FUEL_GROUP,
     +                           UNITS,
     +                          (ANNUAL_AMOUNT(I),I=0,AVAIL_DATA_YEARS),
     +                           BTU_CONTENT,FUEL_STRATEGY,
     +                           START_MO,END_MO,
     +                           TAKE_OR_PAY_PENALTY,
     +                           PENALTY_ESCALATION_VECTOR,
     +                           INVENTORY_TREATMENT
         IF(IOS /= 0) EXIT
         IN_REC = IN_REC + 1
         IF(DELETE > 7) CYCLE
         DO I = AVAIL_DATA_YEARS+1,MAX_SIMULATION_YEARS
            ANNUAL_AMOUNT(I) = ANNUAL_AMOUNT(AVAIL_DATA_YEARS)
         ENDDO
         FUEL_ID = ABS(FUEL_ID)
         IF(UNITS /= 'Q') THEN
            CALL GET_CONVERSION_FACTOR(UNITS,CONVERSION_FACTOR)
            DO YR = 0, YEARS_TO_RUN
               IF(BTU_CONTENT < -0.1) THEN
                  R4_BTU_CONTENT = ABS(BTU_CONTENT)
                  LOCAL_BTU_CONTENT = 
     +                    GET_VAR(R4_BTU_CONTENT,YR,DESC(1:20))
               ELSE
                  LOCAL_BTU_CONTENT = BTU_CONTENT
               ENDIF
               ANNUAL_AMOUNT(YR) =  ANNUAL_AMOUNT(YR) * 
     +                              LOCAL_BTU_CONTENT *
     +                              CONVERSION_FACTOR
            ENDDO
         ENDIF
         IF(START_MO == 0 .AND. END_MO == 0) THEN
C     +      .OR. (START_MO == 1 .AND. END_MO == 12) ) THEN
            IF(FUEL_INVENTORY_IS_MONTHLY(FUEL_ID)) THEN
C NEED ERROR MESSAGE
            ENDIF
            IF(UNITS == 'D') THEN
               DO YR = 0, YEARS_TO_RUN
                  ANNUAL_AMOUNT(YR) =  ANNUAL_AMOUNT(YR) * 365.D0
               ENDDO
            ENDIF
            IF(.NOT. FUEL_INVENTORY_IS_ANNUAL(FUEL_ID)) THEN
               ANNUAL_INDEX = ANNUAL_INDEX + 1
               ANNUAL_FILE_POINTR(FUEL_ID) = ANNUAL_INDEX
               IREC = ANNUAL_INDEX
               FUEL_INVENTORY_IS_ANNUAL(FUEL_ID) = .TRUE.
               INVENTORY_INDEX = INVENTORY_INDEX + 1
            ELSEIF(INVENTORY_TREATMENT == 'A') THEN
               IREC = ANNUAL_FILE_POINTR(FUEL_ID)
               READ(111,REC=IREC) TAKE_OR_PAY_PENALTY,
     +                            PENALTY_ESCALATION_VECTOR,
     +                            FUEL_STRATEGY,
     +                            PREVIOUS_ANNUAL_AMOUNT
               DO YR = 0, YEARS_TO_RUN
                  ANNUAL_AMOUNT(YR) = ANNUAL_AMOUNT(YR) +
     +                                     PREVIOUS_ANNUAL_AMOUNT(YR)
               ENDDO
            ELSE
               IREC = ANNUAL_FILE_POINTR(FUEL_ID)
            ENDIF
            WRITE(111,REC=IREC) TAKE_OR_PAY_PENALTY,
     +                          PENALTY_ESCALATION_VECTOR,
     +                          FUEL_STRATEGY,
     +                          ANNUAL_AMOUNT
         ELSE
            IF(FUEL_INVENTORY_IS_ANNUAL(FUEL_ID)) THEN
C NEED ERROR MESSAGE FOR PREVIOUS ANNUAL FUEL ID

            ENDIF
            IF(.NOT. FUEL_INVENTORY_IS_MONTHLY(FUEL_ID)) THEN
               MONTHLY_INDEX = MONTHLY_INDEX + 1
               MONTHLY_FILE_POINTR(FUEL_ID) = MONTHLY_INDEX
               IREC = 12*(MONTHLY_INDEX-1)
               FUEL_INVENTORY_IS_MONTHLY(FUEL_ID) = .TRUE.
               MONTHLY_INVENTORY_ACTIVE = .TRUE.
            ELSE
               IREC = 12*(MONTHLY_FILE_POINTR(FUEL_ID) - 1)
            ENDIF
            DO fu_mo = 1, 12
               IF( .NOT. ((fu_mo >= START_MO .AND. fu_mo <= END_MO) .OR.
     +              (START_MO > END_MO .AND. 
     +             (fu_mo >= START_MO .OR. fu_mo <= END_MO))) ) then
                   CYCLE
               end if
               IF(UNITS == 'D') THEN
                  DO YR = 0, YEARS_TO_RUN
                     MONTHLY_AMOUNT(YR) =  ANNUAL_AMOUNT(YR) * 
     +                                       DBLE(DAYS_IN(fu_mo))
                  ENDDO
               ELSE
                  DO YR = 0, YEARS_TO_RUN
                     MONTHLY_AMOUNT(YR) =  ANNUAL_AMOUNT(YR)
                  ENDDO
               ENDIF
               IF(.NOT. FIRST_TIME_FOR_THIS_MONTHLY(fu_mo,FUEL_ID) .AND.
     +                                  INVENTORY_TREATMENT == 'A') THEN
                  READ(112,REC=IREC+fu_mo) TAKE_OR_PAY_PENALTY,
     +                                  PENALTY_ESCALATION_VECTOR,
     +                                  FUEL_STRATEGY,
     +                                  PREVIOUS_ANNUAL_AMOUNT
                  DO YR = 0, YEARS_TO_RUN
                     MONTHLY_AMOUNT(YR) = MONTHLY_AMOUNT(YR) +
     +                                     PREVIOUS_ANNUAL_AMOUNT(YR)
                  ENDDO
               ENDIF
               WRITE(112,REC=IREC+fu_mo) TAKE_OR_PAY_PENALTY,
     +                                PENALTY_ESCALATION_VECTOR,
     +                                FUEL_STRATEGY,
     +                                MONTHLY_AMOUNT
               FIRST_TIME_FOR_THIS_MONTHLY(fu_mo,FUEL_ID) = .FALSE.
            ENDDO
         ENDIF
      ENDDO
!
      RETURN
!      
      INDEX = 0
      DO I = 1, MAX_FUEL_INV_SERIES
         IF(ANNUAL_FILE_POINTR(I) == 0 .AND.
     +                                MONTHLY_FILE_POINTR(I) == 0) CYCLE
         INDEX = INDEX + 1
         MMBTU_BALANCE_POINTR(INDEX) = I
         FUEL_INVENTORY_ID(I) = INDEX
         SAVE_FUEL_INVENTORY_ID(I) = INDEX
      ENDDO
      FUEL_INVENTORIES_FOUND = INDEX
!      DEALLOCATE(PREVIOUS_ANNUAL_AMOUNT,
!     +                      FIRST_TIME_FOR_THIS_MONTHLY,
!     +                      FUEL_INVENTORY_IS_ANNUAL,
!     +                      ANNUAL_AMOUNT,
!     +                      MONTHLY_AMOUNT,STAT=ERROR)
      CALL CLOSE_FUEL_INVENTORY_FILE
      RETURN
C**********************************************************************
      ENTRY RETURN_NUMBER_OF_INVENTORIES(R_FUEL_INVENTORIES_FOUND)
C**********************************************************************
         R_FUEL_INVENTORIES_FOUND = FUEL_INVENTORIES_FOUND
      RETURN
C**********************************************************************
      ENTRY RETURN_MMBTU_BALANCE_POINTR(R_I,R_MMBTU_BALANCE_POINTR)
C**********************************************************************
         I = R_I
         R_MMBTU_BALANCE_POINTR = MMBTU_BALANCE_POINTR(I)
      RETURN
C
C SETS THE BEGINNING YEAR AND MONTHLY BALANCE FOR FUEL INVENTORIES      
C
C**********************************************************************
      ENTRY GET_FUEL_INVENTORIES(MMBTU_FUEL_BALANCE,YEAR,PERIOD,
     +                           PSMO,PEMO)
C**********************************************************************
C
C NEED TO ADD LOGIC TO INPLEMENT SEASONAL AND ANNUAL LIMITS USING THE 
C MONTHLY LIMIT DATA
C
!      ALLOCATE(ANNUAL_AMOUNT(0:MAX_SIMULATION_YEARS),STAT=ERROR)
      ANNUAL_AMOUNT = 0.0D0
C
      
      DO FUEL_ID = 1, FUEL_INVENTORIES_FOUND
         IF(MMBTU_BALANCE_POINTR(FUEL_ID) == 0) CYCLE
         FUEL_ID_POINTR = MMBTU_BALANCE_POINTR(FUEL_ID)
         IF(.NOT. FUEL_INVENTORY_IS_MONTHLY(FUEL_ID_POINTR) .AND.
     +                                                 PERIOD > 1) CYCLE
C
         IF(PERIOD == 1) THEN
            IF(FUEL_INVENTORY_IS_MONTHLY(FUEL_ID_POINTR)) THEN
               PERIOD_AMOUNT = 0.D0
               DO MONTH = PSMO, PEMO
                  IREC=12*(MONTHLY_FILE_POINTR(FUEL_ID_POINTR)-1)+MONTH
                  READ(112,REC=IREC) TAKE_OR_PAY_PENALTY,
     +                               PENALTY_ESCALATION_VECTOR,
     +                               FUEL_STRATEGY,
     +                               ANNUAL_AMOUNT
                  PERIOD_AMOUNT = PERIOD_AMOUNT + ANNUAL_AMOUNT(YEAR)
               ENDDO
            ELSE
               IREC = ANNUAL_FILE_POINTR(FUEL_ID_POINTR)
               READ(111,REC=IREC) TAKE_OR_PAY_PENALTY,
     +                            PENALTY_ESCALATION_VECTOR,
     +                            FUEL_STRATEGY,
     +                            ANNUAL_AMOUNT
               PERIOD_AMOUNT = ANNUAL_AMOUNT(YEAR)
            ENDIF
         ELSE
            PERIOD_AMOUNT = 0.D0
            DO MONTH = PSMO, PEMO
               IREC = 12*(MONTHLY_FILE_POINTR(FUEL_ID_POINTR)-1) + MONTH
               READ(112,REC=IREC) TAKE_OR_PAY_PENALTY,
     +                            PENALTY_ESCALATION_VECTOR,
     +                            FUEL_STRATEGY,
     +                            ANNUAL_AMOUNT
               PERIOD_AMOUNT = PERIOD_AMOUNT + ANNUAL_AMOUNT(YEAR)
            ENDDO
         ENDIF
         IF(FUEL_STRATEGY == 'C') THEN
            MMBTU_FUEL_BALANCE(FUEL_ID) = PERIOD_AMOUNT +
     +                                       MMBTU_FUEL_BALANCE(FUEL_ID)
         ELSE
            MMBTU_FUEL_BALANCE(FUEL_ID) = PERIOD_AMOUNT
         ENDIF
         SAVE_MMBTU_FUEL_BALANCE(FUEL_ID) = MMBTU_FUEL_BALANCE(FUEL_ID)
      ENDDO
!      DEALLOCATE(ANNUAL_AMOUNT,STAT=ERROR)
      RETURN
C**********************************************************************
! CALLED FROM HOURLY COMMITMENT
      ENTRY GET_MMBTU_FUEL_BALANCE(R_FUEL_INVENTORY_ACTIVE,
     +                             R_MMBTU_FUEL_BALANCE,R_FUEL_ID)
C**********************************************************************
         R_FUEL_INVENTORY_ACTIVE = .FALSE.
         IF(SAVE_FUEL_INVENTORY_ACTIVE .AND. R_FUEL_ID /= 0) THEN
            IF(R_FUEL_ID > 0 .AND. R_FUEL_ID < MAX_FUEL_INV_SERIES) THEN
               R_MMBTU_FUEL_BALANCE = 
     +                 SAVE_MMBTU_FUEL_BALANCE(
     +                                SAVE_FUEL_INVENTORY_ID(R_FUEL_ID))
               R_FUEL_INVENTORY_ACTIVE = .TRUE.
            ELSE
               R_MMBTU_FUEL_BALANCE = 0.D0
            ENDIF
         ELSE
         R_MMBTU_FUEL_BALANCE = 1000000000000000.D0 ! 10^15 = UNLIMITED
         ENDIF
      RETURN
C**********************************************************************
! CALLED FROM HOURLY COMMITMENT
      ENTRY REDUCE_LOCAL_MMBTU_FUEL(R_MMBTU_FUEL_BALANCE,R_FUEL_ID)
C**********************************************************************
         IF(SAVE_FUEL_INVENTORY_ACTIVE .AND. R_FUEL_ID /= 0) THEN
            IF(R_FUEL_ID > 0 .AND. R_FUEL_ID < MAX_FUEL_INV_SERIES) THEN
               SAVE_MMBTU_FUEL_BALANCE(
     +                       SAVE_FUEL_INVENTORY_ID(R_FUEL_ID)) = 
     +                                              R_MMBTU_FUEL_BALANCE
            ENDIF
         ENDIF
      RETURN
      END
C**********************************************************************
C
C      FUEL USAGE MODULE FOR SWITCHING FUEL TYPES BASED ON INVENTORY
C                        COPYRIGHT (C) 1992
C                   M.S. GERBER & ASSOCIATES, INC.
C                       ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE GET_CONVERSION_FACTOR(UNITS,CONVERSION_FACTOR)
      use fuelused_decs
      implicit none
      CHARACTER*1 UNITS,COUNTRY
      LOGICAL*1 CANADA
      REAL*8 CONVERSION_FACTOR
C      
      CANADA = COUNTRY() == 'C'
      CONVERSION_FACTOR=0 ! Apparently affects only Canada
      SELECT CASE (UNITS)
      CASE ('T')
                  IF(.not. CANADA) THEN
                     CONVERSION_FACTOR = .002
                  ENDIF
      CASE ('B')
                  IF(.not. CANADA) THEN
                     CONVERSION_FACTOR = .000042
                  ENDIF
      CASE ('5')
                  IF(.not. CANADA) THEN
                     CONVERSION_FACTOR = .000055
                  ENDIF
      CASE ('M', 'Q', 'D')
                  IF(.not. CANADA) THEN
                     CONVERSION_FACTOR = 1.
                  ENDIF
      END SELECT
      RETURN
      END
C**********************************************************************
C
C                    MONTHLY FUEL PRICE MODULE
C                        COPYRIGHT (C) 1992
C                   M.S. GERBER & ASSOCIATES, INC.
C                       ALL RIGHTS RESERVED
C
C**********************************************************************
C
      SUBROUTINE BUILD_FUEL_PRICE_INFO(R_FUEL_PRICE_DATA_AVAILABLE)
      use end_routine, only: end_program, er_message
	  use cla_decs
	  use fuelused_decs
      use p_fuel_annl 
      use mod_base_year
      use spindriftlib
      use prod_arrays_dimensions 
      USE SIZECOM
      use dsex_obj_interfaces
      use cl_units_read_decs
      use program_state
      use fuel_pointers
      integer :: ub, dc_idx
      INTEGER*2   MAX_FUEL_PRICE_SERIES,
     +            MAX_NEW_FUEL_PRICE_SERIES/0/,
     +            TG,get_TRANSACTION_GROUP,
     +            R_YEAR
      PARAMETER (MAX_FUEL_PRICE_SERIES=10999)
!
      REAL*8 CONVERSION_FACTOR
      INTEGER*2 MAX_FUEL_TYPES_FOUND,IN_REC,OUT_REC
      INTEGER IOS
      INTEGER*2 :: BASE_YR
      INTEGER*2 nr_units,I,IREC,MONTH,RUN_YEARS,YEARS_TO_RUN,
     +          EXTENSION_YEARS,R_UNIT
      INTEGER*2 R_YR,YR,YEAR
      INTEGER*4 REC_LEN
      SAVE REC_LEN
C
C FUEL PRICE VARIABLES
C
      CHARACTER*1 RECORD_ACTIVE
      INTEGER*2 CHANGE_YEAR,START_MO,END_MO,DELETE,fu_mo
      INTEGER*2 STARTING_MO,ENDING_MO

      INTEGER*2 UNITS_PASSED

      CHARACTER*1 DATA_TYPE
      REAL*8 BTU_CONTENT,
     +       LOCAL_BTU_CONTENT
      CHARACTER*1 SHIPPING_UNITS,
     +            P_SHIPPING_UNITS(:),
     +            S_SHIPPING_UNITS(:),
     +            E_SHIPPING_UNITS(:)
      CHARACTER*20 RETURN_UNITNM
      CHARACTER*30 DESC
      INTEGER*2 FUEL_GROUP
      INTEGER*2 FUEL_ID,MAX_OUT_REC/0/
      INTEGER*4 ERROR

      integer :: times_sufci=0
      REAL PBTUCT(*),SBTUCT(*),EMISS_FUEL_COST_loc(*),
     +     FUEL_BTU_COST(*),FUELMX(*),DISP_BTU_COST(*),
     +     P_BTU_CONTENT(:),
     +     S_BTU_CONTENT(:),
     +     E_BTU_CONTENT(:),
     +     R_BTU_CONTENT,
     +     GET_MONTHLY_REGIONAL_PARAM

      REAL*8 MMBTU_FUEL_BALANCE(0:*)
      INTEGER*2 SFESCR(*),EMISS_FUEL_ESCAL_loc(*),
     +          FUEL_INVENTORY_ID(0:*),Fuel_SupplyID(*)
      LOGICAL*4 FUEL_INVENTORY_ACTIVE
C
      REAL  FUEL_PRICE(:),
     +      TEMP_FUEL_PRICE(0:30),
     +      DELIVERY_COST_1,
     +      DELIVERY_COST_2,
     +      DELIVERY_COST_3,
     +      R_TOTAL_DELIVERY_COST,
     +      R_DELIVERY_INDEX_1,
     +      R_DELIVERY_INDEX_2,
     +      R_DELIVERY_INDEX_3,
     +      GET_NEW_UNIT_FUEL_DELIVERY,
     +      GET_FUEL_DELIVERY,
     +      TEMP_R,
     +      R_P_DELIVERY_COST,
     +      R_S_DELIVERY_COST,
     +      PRIMARY_DELIVERY_COST(:),
     +      SECONDARY_DELIVERY_COST(:),
     +      R_PRIMARY_FUEL_ID,
     +      R_DELIVERY_ID_1,
     +      R_DELIVERY_ID_2,
     +      R_DELIVERY_ID_3,
     +      R_VALUE_TO_PUT
!      REAL FUEL_PRICE(0:30)
      REAL EMISS_BLENDING_RATE_loc(*),BLENDED_BTU_COST(*)
      INTEGER*2 P_BTU_COST_POINTR(:),S_BTU_COST_POINTR(:),
     +          E_BTU_COST_POINTR(:),
     +          R_VECTOR_TO_PUT,MON,R_MONTH,LOCAL_YEAR
      LOGICAL*1 P_BTU_COST_IS_A_POINTR(:),S_BTU_COST_IS_A_POINTR(:),
     +          E_BTU_COST_IS_A_POINTR(:),
     +          FUEL_POINTERS_USED,
     +          R_FUEL_PRICE_DATA_AVAILABLE,
     +          R_EXISTING_UNIT,
     +          YES_REGIONAL_PARAMETER_ACTIVE/.FALSE./,
     +          REGIONAL_PARAMS_ACTIVE,
     +          DYNAMIC_FUEL_CHOICE/.FALSE./,
     +          YES_DYNAMIC_FUEL_PRICING
      ALLOCATABLE :: P_BTU_COST_POINTR,S_BTU_COST_POINTR,
     +               P_BTU_COST_IS_A_POINTR,S_BTU_COST_IS_A_POINTR,
     +               E_BTU_COST_POINTR,E_BTU_COST_IS_A_POINTR,
     +               FUEL_PRICE,
     +               P_BTU_CONTENT,
     +               S_BTU_CONTENT,
     +               E_BTU_CONTENT,
     +               P_SHIPPING_UNITS,
     +               S_SHIPPING_UNITS,
     +               E_SHIPPING_UNITS,
     +               PRIMARY_DELIVERY_COST,
     +               SECONDARY_DELIVERY_COST
      LOGICAL*1 FIRST_TIME_FOR_THIS(:),LOAD_FUEL_PRICES
      INTEGER*2 THIS_FUEL_POINTER
C
      LOGICAL*1 VOID_LOG
      CHARACTER*1 PERCENT_OR_ESCALATION
      INTEGER*2 LAST_ESCAL_CHANGE_YEAR_PRICE,FUEL_PRICE_YEARS,
     +          PLANNING_YEAR
      REAL ESCALATION_RATES(AVAIL_DATA_YEARS),get_escalated_value,
     +     ESCALATED_FUEL_MONTHLY_VALUE,
     +     R4_BTU_CONTENT,
     +     GET_VAR
      CHARACTER*20 UNITNM(*)
      LOGICAL* 1 YES_SP_CAPEX_ACTIVE
      ALLOCATABLE :: FIRST_TIME_FOR_THIS
C
      SAVE P_BTU_COST_POINTR,S_BTU_COST_POINTR,
     +     P_BTU_COST_IS_A_POINTR,S_BTU_COST_IS_A_POINTR,
     +     E_BTU_COST_POINTR,E_BTU_COST_IS_A_POINTR,
     +     YEARS_TO_RUN,FUEL_PRICE_YEARS,
     +     P_BTU_CONTENT,
     +     S_BTU_CONTENT,
     +     E_BTU_CONTENT,
     +     P_SHIPPING_UNITS,
     +     S_SHIPPING_UNITS,
     +     E_SHIPPING_UNITS,
     +     PRIMARY_DELIVERY_COST,
     +     SECONDARY_DELIVERY_COST,
     +     YES_SP_CAPEX_ACTIVE
      INTEGER FILE_ID,MAX_FUEL_FILES
      integer :: abs_pcost1, int_pcost1
      character (len=50) :: floatstr
      REAL (KIND=4) :: IMPLIED_RATE
      integer :: timeshere=0
!     
!
!     END DATA DECLARATIONS
!
C
!
!
      CALL FUEL_PRICE_FILES_EXIST(LOAD_FUEL_PRICES,MAX_FUEL_FILES)
      R_FUEL_PRICE_DATA_AVAILABLE = LOAD_FUEL_PRICES
      IF(.NOT. LOAD_FUEL_PRICES) RETURN
C
C THERE IS AT LEAST ONE FUEL PRICE FILE TO PROCESS
C
      YEARS_TO_RUN = MIN(RUN_YEARS() + EXTENSION_YEARS(),30)
      ! 11/03/06 needed to get fuel values into extension period
      YEARS_TO_RUN = RUN_YEARS() + EXTENSION_YEARS() 
!
      YES_SP_CAPEX_ACTIVE = SP_CAPEX_ACTIVE()
!
      YES_REGIONAL_PARAMETER_ACTIVE = REGIONAL_PARAMS_ACTIVE()
!      
      ALLOCATE(FIRST_TIME_FOR_THIS(0:MAX_FUEL_PRICE_SERIES),STAT=ERROR)
      FIRST_TIME_FOR_THIS = .TRUE.
      MAX_NEW_FUEL_PRICE_SERIES = 0
      DO FILE_ID = 0, MAX_FUEL_FILES-1
         
         CALL OPEN_FUEL_PRICE_FILE(10,LOAD_FUEL_PRICES,FILE_ID)
         IF(.NOT. LOAD_FUEL_PRICES) CYCLE
         IN_REC = 0
!
         DO
            IN_REC = IN_REC + 1
            READ(10,REC=IN_REC,IOSTAT=IOS) DELETE,DESC,
     +                                     FUEL_ID,FUEL_GROUP,
     +                                     STARTING_MO,ENDING_MO,
     +                                     SHIPPING_UNITS,BTU_CONTENT,
     +                                     DATA_TYPE,CHANGE_YEAR,
     +                                     PERCENT_OR_ESCALATION,
     +                                     TEMP_FUEL_PRICE,
     +                                     RECORD_ACTIVE
            IF(IOS .NE. 0) EXIT
            IF(DELETE > 7 .OR. RECORD_ACTIVE /= 'T') CYCLE
! 
            IF(DATA_TYPE == 'K') THEN
!
               FUEL_ID = FUEL_ID + 10000
!               
            ENDIF
!
            IF(.NOT. FIRST_TIME_FOR_THIS(FUEL_ID)) CYCLE
            MAX_NEW_FUEL_PRICE_SERIES = MAX_NEW_FUEL_PRICE_SERIES + 1
            FIRST_TIME_FOR_THIS(FUEL_ID) = .FALSE.
         
         ENDDO
         CALL CLOSE_FUEL_PRICE_FILE
      ENDDO
C
C THE FUEL PRICE FILES HAVE BEEN POLLED TO FIND THE SIZE
C 
      FIRST_TIME_FOR_THIS = .TRUE.
      IF(ALLOCATED(NEW_FUEL_PRICE)) DEALLOCATE (NEW_FUEL_PRICE)
      MAX_NEW_FUEL_PRICE_SERIES = MAX(1,MAX_NEW_FUEL_PRICE_SERIES)
      ALLOCATE (NEW_FUEL_PRICE
     +               (MAX_NEW_FUEL_PRICE_SERIES,12,0:YEARS_TO_RUN)) 
     
      NEW_FUEL_PRICE = 0.
!      CALL CLOSE_FUEL_PRICE_FILE
!
!      CALL OPEN_FUEL_PRICE_FILE(10,LOAD_FUEL_PRICES)
!      
C
      BASE_YR = get_base_year()
      REC_LEN = MAX(64,4*(YEARS_TO_RUN+3))
      OPEN(UNIT=11,FILE='BCFLCOST.BIN',ACCESS='DIRECT',RECL=REC_LEN)
C
      IF(.NOT. ALLOCATED(FUEL_PRICE_POINTR))
     +   ALLOCATE(FUEL_PRICE_POINTR(0:MAX_FUEL_PRICE_SERIES),STAT=ERROR)
      if(error/=0) then
         call end_program("fuelused:0001 - Allocation failure for  " //
     + "fuel_price_pointr.")
      end if
      ub=ubound(FUEL_PRICE_POINTR,1)
      call check_fuel_price_pointer()
      FUEL_PRICE_POINTR = 0

      FUEL_PRICE_YEARS = YEARS_TO_RUN

      ALLOCATE(FUEL_PRICE(0:FUEL_PRICE_YEARS))
      MAX_FUEL_TYPES_FOUND = 0
      MAX_OUT_REC = 0
C
C RE-READ THE FILE AND PROCESS THE INFORMATION
C
      DO FILE_ID = 0, MAX_FUEL_FILES-1
         
         CALL OPEN_FUEL_PRICE_FILE(10,LOAD_FUEL_PRICES,FILE_ID)
         IF(.NOT. LOAD_FUEL_PRICES) CYCLE
         IN_REC = 0
         DO
            IN_REC = IN_REC + 1
            READ(10,REC=IN_REC,IOSTAT=IOS) DELETE,DESC,
     +                                   FUEL_ID,FUEL_GROUP,
     +                                   STARTING_MO,ENDING_MO,
     +                                   SHIPPING_UNITS,BTU_CONTENT,
     +                                   DATA_TYPE,CHANGE_YEAR,
     +                                   PERCENT_OR_ESCALATION,
     +                                   TEMP_FUEL_PRICE,
     +                                   RECORD_ACTIVE
            IF(IOS .NE. 0) EXIT
            IF(DELETE > 7 .OR. RECORD_ACTIVE /= 'T') CYCLE
            IF(DATA_TYPE == 'K') THEN
!
               FUEL_ID = FUEL_ID + 10000
!               
            ENDIF
!               
C
C 11/03/06 Dr. G
C Extended the FUEL_PRICE variable beyond the 30 year limit
C
            IF(FUEL_PRICE_YEARS > 30) THEN
               FUEL_PRICE(0:30) = TEMP_FUEL_PRICE(0:30)
               IF(CHANGE_YEAR > BASE_YR .AND.
     +                               (CHANGE_YEAR - BASE_YR) <= 30) THEN
                 FUEL_PRICE(31:FUEL_PRICE_YEARS) = TEMP_FUEL_PRICE(30)
               ELSE
                  IF(TEMP_FUEL_PRICE(29) /= 0.) THEN
                     IMPLIED_RATE =
     +                      (TEMP_FUEL_PRICE(30) - TEMP_FUEL_PRICE(29))/
     +                               TEMP_FUEL_PRICE(29)
                     IF(IMPLIED_RATE > 0.2) THEN
                        IMPLIED_RATE = 0.2
                     ELSEIF(IMPLIED_RATE < -.2) THEN
                        IMPLIED_RATE = 0.
                     ENDIF
                     FUEL_PRICE(31:FUEL_PRICE_YEARS) = 100.*IMPLIED_RATE
                  ELSE
                     FUEL_PRICE(31:FUEL_PRICE_YEARS) = 0.
                  ENDIF
                  CHANGE_YEAR = BASE_YR + 31
               ENDIF
            ELSE               
               FUEL_PRICE(0:FUEL_PRICE_YEARS) =
     +                               TEMP_FUEL_PRICE(0:FUEL_PRICE_YEARS)
            ENDIF
C End code added 11/03/06 to extend the fuel vectors Dr. G
            IF(CHANGE_YEAR > BASE_YR .AND.
     +                     (CHANGE_YEAR - BASE_YR) <= YEARS_TO_RUN) THEN
C
C NOTE YEARS_TO_RUN CAN BE 31 FUEL_PRICE IS DIMENSIONED 0:30
C SEE ABOVE DEFINITION OF YEARS_TO_RUN
C
               IF(PERCENT_OR_ESCALATION == 'P') THEN
                  DO YEAR = CHANGE_YEAR-BASE_YR, YEARS_TO_RUN
                     FUEL_PRICE(YEAR) = (1. + FUEL_PRICE(YEAR)/100.) * 
     +                                                FUEL_PRICE(YEAR-1)
                  ENDDO
               ELSE
                  LAST_ESCAL_CHANGE_YEAR_PRICE =
     +                           INT2(FUEL_PRICE(CHANGE_YEAR - BASE_YR))
C
C ASSUME THE THAT THE NEW ESCALATION RATES IS BEING USED.
C
                  DO YEAR = CHANGE_YEAR-BASE_YR, YEARS_TO_RUN
                     FUEL_PRICE(YEAR) =  
     +                     ESCALATED_FUEL_MONTHLY_VALUE(
     +                                     FUEL_PRICE(YEAR-1),
     +                                     LAST_ESCAL_CHANGE_YEAR_PRICE,
     +                                     YEAR,
     +                                     STARTING_MO)
                  ENDDO
               ENDIF
            ENDIF

            IF(FIRST_TIME_FOR_THIS(FUEL_ID)) THEN
               MAX_FUEL_TYPES_FOUND = MAX_FUEL_TYPES_FOUND + 1
               OUT_REC = 13*(MAX_FUEL_TYPES_FOUND - 1) + 1
               MAX_OUT_REC = MAX(MAX_OUT_REC,OUT_REC)
               WRITE(11,REC=OUT_REC) DESC,FUEL_ID,FUEL_GROUP,
     +                               SHIPPING_UNITS,BTU_CONTENT,
     +                               DATA_TYPE,CHANGE_YEAR,
     +                               STARTING_MO,ENDING_MO
               FIRST_TIME_FOR_THIS(FUEL_ID) = .FALSE.
               FUEL_PRICE_POINTR(FUEL_ID) = MAX_FUEL_TYPES_FOUND

            ENDIF
            IF(DATA_TYPE == 'S') THEN
               CALL GET_CONVERSION_FACTOR(SHIPPING_UNITS,
     +                                                CONVERSION_FACTOR)
               DO YEAR = 0, YEARS_TO_RUN
                  IF(BTU_CONTENT < -0.1) THEN
                     R4_BTU_CONTENT = ABS(BTU_CONTENT)
                     LOCAL_BTU_CONTENT = 
     +                    GET_VAR(R4_BTU_CONTENT,YEAR,DESC(1:20))
                  ELSE
                     LOCAL_BTU_CONTENT = BTU_CONTENT
                  ENDIF
                  CONVERSION_FACTOR = CONVERSION_FACTOR * BTU_CONTENT
                  IF(CONVERSION_FACTOR > 0) THEN
                     FUEL_PRICE(YEAR)=FUEL_PRICE(YEAR)/CONVERSION_FACTOR
                  ELSE

                     CALL MG_LOCATE_WRITE(16,9,
     +                        'Inconsistent fuel price information for',
     +                                                   ALL_VERSIONS,0)
                     CALL MG_LOCATE_WRITE(17,9,
     +                        trim(DESC)//'.  Shipping units were',
     +                                                   ALL_VERSIONS,0)
                     WRITE(SCREEN_MESSAGES,*)
     +                        'specified and BTU content is',BTU_CONTENT
                     CALL MG_LOCATE_WRITE(18,9,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
                     CALL MG_LOCATE_WRITE(19,9,
     +                        'which leads to an undefined fuel price.',
     +                                                   ALL_VERSIONS,0)
                  ENDIF
               ENDDO
            ENDIF
            OUT_REC = 13*(FUEL_PRICE_POINTR(FUEL_ID)-1) + STARTING_MO+ 1
            MAX_OUT_REC = MAX(MAX_OUT_REC,OUT_REC)
            IF(STARTING_MO <= ENDING_MO) THEN
               DO fu_mo = STARTING_MO, ENDING_MO
                  WRITE(11,REC=OUT_REC) fu_mo,FUEL_PRICE
                  OUT_REC = OUT_REC + 1
                  DO YR = 0, YEARS_TO_RUN
                  NEW_FUEL_PRICE(FUEL_PRICE_POINTR(FUEL_ID),fu_mo,YR) = 
     +                                                    FUEL_PRICE(YR)
                  ENDDO
               ENDDO
            ELSE
               DO fu_mo = STARTING_MO, 12
                  WRITE(11,REC=OUT_REC) fu_mo,FUEL_PRICE
                  OUT_REC = OUT_REC + 1
                  DO YR = 0, YEARS_TO_RUN
                  NEW_FUEL_PRICE(FUEL_PRICE_POINTR(FUEL_ID),fu_mo,YR) = 
     +                                                    FUEL_PRICE(YR)
                  ENDDO
               ENDDO
               OUT_REC = 13*(MAX_FUEL_TYPES_FOUND - 1) + 2
               MAX_OUT_REC = MAX(MAX_OUT_REC,OUT_REC)
               DO fu_mo = 1, ENDING_MO
                  WRITE(11,REC=OUT_REC) fu_mo,FUEL_PRICE
                  OUT_REC = OUT_REC + 1
                  DO YR = 0, YEARS_TO_RUN
                  NEW_FUEL_PRICE(FUEL_PRICE_POINTR(FUEL_ID),fu_mo,YR) = 
     +                                                    FUEL_PRICE(YR)
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
         CALL CLOSE_FUEL_PRICE_FILE
      ENDDO ! FILE LOOP
      DEALLOCATE(FUEL_PRICE,FIRST_TIME_FOR_THIS)
      CLOSE(11)
      RETURN
C**********************************************************************
C
C                    MONTHLY FUEL PRICE MODULE
C                        COPYRIGHT (C) 1993
C                   M.S. GERBER & ASSOCIATES, INC.
C                       ALL RIGHTS RESERVED
C
C**********************************************************************
C
      ENTRY SET_UP_FUEL_COST_INFO(nr_units,
     +                            SFESCR,
     +                            EMISS_FUEL_ESCAL_loc)
C**********************************************************************
C
!
         YES_SP_CAPEX_ACTIVE = SP_CAPEX_ACTIVE()
         timeshere=timeshere+1
         if(timeshere==14) then
            timeshere=timeshere ! Debugstop
         end if
         times_sufci=times_sufci+1
         if(times_sufci==14) then
            nr_units=nr_units ! Debugstop
         end if
         ! call write_log_entry("fuelused:0028", "timeshere=" // 
       ! +       trim(itos(timeshere)))

         IF(nr_units <= 0) THEN
            WRITE(4,*) "Fuel prices have been specified in the"
            WRITE(4,*) "Fuel Price file, but no CL units are"
            WRITE(4,*) "active."
            WRITE(4,*) " "
            RETURN
         ENDIF
!
         IF(ALLOCATED(P_BTU_COST_POINTR)) then
            DEALLOCATE(P_BTU_COST_POINTR, STAT=ERROR)
            call check_alloc("fuelused:0005", "P_BTU_COST_POINTR", 
     +          error)
         end if
         IF(ALLOCATED(P_BTU_CONTENT)) then
            DEALLOCATE(P_BTU_CONTENT, STAT=ERROR)
            call check_alloc("fuelused:0006", "P_BTU_CONTENT", error)
         end if
         IF(ALLOCATED(S_BTU_CONTENT)) then
            DEALLOCATE(S_BTU_CONTENT, STAT=ERROR)
            call check_alloc("fuelused:0008","S_BTU_CONTENT", error)
         end if
         IF(ALLOCATED(E_BTU_CONTENT)) then
            DEALLOCATE(E_BTU_CONTENT, STAT=ERROR)
            call check_alloc("fuelused:0009","E_BTU_CONTENT", error)
         end if
         
         IF(ALLOCATED(P_SHIPPING_UNITS)) then
            DEALLOCATE(P_SHIPPING_UNITS, STAT=ERROR)
            call check_alloc("fuelused:0010", "P_SHIPPING_UNITS", error)
         end if
         IF(ALLOCATED(S_SHIPPING_UNITS)) then
            DEALLOCATE(S_SHIPPING_UNITS, STAT=ERROR)
            call check_alloc("fuelused:0011","S_SHIPPING_UNITS" , error)
         end if
         IF(ALLOCATED(E_SHIPPING_UNITS)) then
            DEALLOCATE(E_SHIPPING_UNITS, STAT=ERROR)
            call check_alloc("fuelused:0012", "E_SHIPPING_UNITS", error)
         end if
         IF(ALLOCATED(S_BTU_COST_POINTR)) then
            DEALLOCATE(S_BTU_COST_POINTR, STAT=ERROR)
            call check_alloc("fuelused:0013", "S_BTU_COST_POINTR", 
     +          error)
         end if
         IF(ALLOCATED(E_BTU_COST_POINTR)) then
            DEALLOCATE(E_BTU_COST_POINTR, STAT=ERROR)
            call check_alloc("fuelused:0014", "E_BTU_COST_POINTR", 
     +          error)
         end if
         IF(ALLOCATED(P_BTU_COST_IS_A_POINTR)) then  
             DEALLOCATE(P_BTU_COST_IS_A_POINTR,STAT=ERROR)
             call check_alloc("fuelused:0015", 
     +          "P_BTU_COST_IS_A_POINTR", error)
         end if
         

         IF(ALLOCATED(S_BTU_COST_IS_A_POINTR)) then
             DEALLOCATE(S_BTU_COST_IS_A_POINTR,STAT=ERROR)
             call check_alloc("fuelused:0016", 
     +           "S_BTU_COST_IS_A_POINTR", error)
         end if
         
         IF(ALLOCATED(E_BTU_COST_IS_A_POINTR)) then
             DEALLOCATE(E_BTU_COST_IS_A_POINTR,STAT=ERROR)
             call check_alloc("fuelused:0017", 
     +                "E_BTU_COST_IS_A_POINTR", error)
         end if

         ALLOCATE(P_BTU_COST_POINTR(nr_units), stat=error)


         BCP_arraysize=nr_units 
         call check_alloc("fuelused:0018", "P_BTU_COST_POINTR", error)
         p_btu_cost_pointr=0 ! Initialize
         
         ALLOCATE(P_BTU_CONTENT(nr_units))
         call check_alloc("fuelused:0019", "P_BTU_CONTENT", error)
         p_btu_content=0
         ALLOCATE(S_BTU_CONTENT(nr_units))
         call check_alloc("fuelused:0020", "S_BTU_CONTENT", error)
         s_btu_content=0
         ALLOCATE(E_BTU_CONTENT(nr_units))
         call check_alloc("fuelused:0021", "E_BTU_CONTENT", error)
         e_btu_content=0
         ALLOCATE(P_SHIPPING_UNITS(nr_units))
         call check_alloc("fuelused:0022", "S_SHIPPING_UNITS", error)
         p_shipping_units=" "
         ALLOCATE(S_SHIPPING_UNITS(nr_units))
         call check_alloc("fuelused:0022", "E_SHIPPING_UNITS", error)
         S_SHIPPING_UNITS=" "
         ALLOCATE(E_SHIPPING_UNITS(nr_units))
         call check_alloc("fuelused:0023", "E_SHIPPING_UNITS", error)
         E_SHIPPING_UNITS=" "
         ALLOCATE(S_BTU_COST_POINTR(nr_units))
         call check_alloc("fuelused:0024", "S_BTU_COST_POINTR", error)
         S_BTU_COST_POINTR=0
         ALLOCATE(E_BTU_COST_POINTR(nr_units))
         call check_alloc("fuelused:0025", "E_BTU_COST_POINTR", error)
         E_BTU_COST_POINTR=0
         ALLOCATE(P_BTU_COST_IS_A_POINTR(nr_units))
         call check_alloc("fuelused:0026", "P_BTU_COST_IS_A_POINTR", 
     +     error)
         P_BTU_COST_IS_A_POINTR=.false.
         ALLOCATE(S_BTU_COST_IS_A_POINTR(nr_units))
         call check_alloc("fuelused:0027", 
     +              "S_BTU_COST_IS_A_POINTR", error)
         S_BTU_COST_IS_A_POINTR=.false.
         ALLOCATE(E_BTU_COST_IS_A_POINTR(nr_units))
         call check_alloc("fuelused:0029", 
     +                     "E_BTU_COST_IS_A_POINTR", error)
         E_BTU_COST_IS_A_POINTR=.false.
         if(BCP_arraysize>1000) then
            BCP_arraysize=BCP_arraysize ! Debugstop
         end if
         if(ubound(p_btu_cost_pointr,1)>1000) then
           p_btu_cost_pointr(1)=p_btu_cost_pointr(1) ! Debugstop
         end if
         ub=ubound(e_btu_cost_pointr,1)
         UNITS_PASSED = RETURN_CL_FUEL_POINTERS(P_BTU_COST_POINTR,
     +                             S_BTU_COST_POINTR,
     +         E_BTU_COST_POINTR, ub)
!
! MOVED THE OPEN FOR BTU_CONTENT. 1/26/00. GAT.
!
      OPEN(UNIT=666,FILE='BCFLCOST.BIN',ACCESS='DIRECT',RECL=REC_LEN)
!
      P_BTU_CONTENT = 1.0
      S_BTU_CONTENT = 1.0
      E_BTU_CONTENT = 1.0
      P_SHIPPING_UNITS = ' '
      S_SHIPPING_UNITS = ' '
      E_SHIPPING_UNITS = ' '
!      
      DO I = 1, nr_units
         IF(P_BTU_COST_POINTR(I) < 0) THEN
            P_BTU_COST_POINTR(I) = ABS(P_BTU_COST_POINTR(I))
            P_BTU_COST_IS_A_POINTR(I) = .TRUE.
C           PFESCR(I) = 0
!
! ADDED FOR BTU_CONTENT: LOOKS LIKE MONTHLY BTU CONTENT WAS WIPED OUT
! ONLY THE VALUE OF THE FIRST RECORD WAS RETAINED.
! I SHOULD REWRITE ALL THIS POINTER MESS, LOAD INTO MEMORY, 
! WRITE RELATIVE TO BASIS VALUES, REMEMBER MONTHLY STUFF, ...
!
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(P_BTU_COST_POINTR(I))
!
            IF(THIS_FUEL_POINTER == 0) THEN
               WRITE(4,*) "WHEN READING FUEL PRICES, AN INVALID"
               WRITE(4,*) "FUEL VECTOR",P_BTU_COST_POINTR(I)
               WRITE(4,*) "WAS ENCOUNTERED FOR UNIT",RETURN_UNITNM(I)
            ENDIF
!
            OUT_REC = 13*(THIS_FUEL_POINTER - 1) + 1
            IF(OUT_REC > MAX_OUT_REC .OR. OUT_REC <= 0) THEN
               WRITE(4,*) "WHEN READING FUEL PRICES, AN INVALID"
               WRITE(4,*) "FUEL VECTOR",P_BTU_COST_POINTR(I)
               WRITE(4,*) "WAS ENCOUNTERED FOR UNIT",RETURN_UNITNM(I)
               OUT_REC = 1
            ENDIF
            READ(666,REC=OUT_REC) DESC,FUEL_ID,FUEL_GROUP,
     +                            SHIPPING_UNITS,BTU_CONTENT,
     +                            DATA_TYPE,CHANGE_YEAR,
     +                            STARTING_MO,ENDING_MO
!
            P_BTU_CONTENT(I) = BTU_CONTENT

            P_SHIPPING_UNITS(I) = SHIPPING_UNITS
!            
         ELSE
            P_BTU_COST_POINTR(I) = 0
            P_BTU_CONTENT(I) = 1.
            P_BTU_COST_IS_A_POINTR(I) = .FALSE.
         ENDIF
         IF(S_BTU_COST_POINTR(I) < 0) THEN
            S_BTU_COST_POINTR(I) = ABS(S_BTU_COST_POINTR(I))
            S_BTU_COST_IS_A_POINTR(I) = .TRUE.
            SFESCR(I) = 0
!
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(S_BTU_COST_POINTR(I))
            OUT_REC = 13*(THIS_FUEL_POINTER - 1) + 1
            IF(OUT_REC > MAX_OUT_REC .OR. OUT_REC <= 0) THEN
               WRITE(4,*) "WHEN READING FUEL PRICES, AN INVALID"
               WRITE(4,*) "FUEL VECTOR",S_BTU_COST_POINTR(I)
               WRITE(4,*) "WAS ENCOUNTERED FOR UNIT",RETURN_UNITNM(I)
               OUT_REC = 1
            ENDIF

            READ(666,REC=OUT_REC) DESC,FUEL_ID,FUEL_GROUP,
     +                            SHIPPING_UNITS,BTU_CONTENT,
     +                            DATA_TYPE,CHANGE_YEAR,
     +                            STARTING_MO,ENDING_MO
!
            S_BTU_CONTENT(I) = BTU_CONTENT
            S_SHIPPING_UNITS(I) = SHIPPING_UNITS
!            
         ELSE
            S_BTU_COST_POINTR(I) = 0
            S_BTU_COST_IS_A_POINTR(I) = .FALSE.
            S_BTU_CONTENT(I) = 1.
         ENDIF
         IF(E_BTU_COST_POINTR(I) < 0) THEN
            E_BTU_COST_POINTR(I) = ABS(E_BTU_COST_POINTR(I))
            E_BTU_COST_IS_A_POINTR(I) = .TRUE.
            EMISS_FUEL_ESCAL_loc(I) = 0
!
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(E_BTU_COST_POINTR(I))
            OUT_REC = 13*(THIS_FUEL_POINTER - 1) + 1
            IF(OUT_REC > MAX_OUT_REC .OR. OUT_REC <= 0) THEN
               WRITE(4,*) "WHEN READING FUEL PRICES, AN INVALID"
               WRITE(4,*) "FUEL VECTOR",E_BTU_COST_POINTR(I)
               WRITE(4,*) "WAS ENCOUNTERED FOR UNIT",RETURN_UNITNM(I)
               OUT_REC = 1
            ENDIF
            READ(666,REC=OUT_REC) DESC,FUEL_ID,FUEL_GROUP,
     +                            SHIPPING_UNITS,BTU_CONTENT,
     +                            DATA_TYPE,CHANGE_YEAR,
     +                            STARTING_MO,ENDING_MO
!
            E_BTU_CONTENT(I) = BTU_CONTENT

            E_SHIPPING_UNITS(I) = SHIPPING_UNITS
         ELSE
            E_BTU_COST_POINTR(I) = 0
            E_BTU_COST_IS_A_POINTR(I) = .FALSE.
            E_BTU_CONTENT(I) = 1.
         ENDIF
      ENDDO
      RETURN
C**********************************************************************
      ENTRY GET_P_FUEL_PRICE_CONVERSION(R_UNIT,R_BTU_CONTENT,R_YEAR)
C**********************************************************************
         IF(P_BTU_CONTENT(R_UNIT) < -0.1) THEN
            R4_BTU_CONTENT = ABS(P_BTU_CONTENT(R_UNIT))
            LOCAL_BTU_CONTENT = GET_VAR(R4_BTU_CONTENT,
     +                                                R_YEAR,DESC(1:20))
         ELSE
            LOCAL_BTU_CONTENT = P_BTU_CONTENT(R_UNIT)
         ENDIF
!
         R_BTU_CONTENT = 1.0
!
         IF(LOCAL_BTU_CONTENT <= 0. .OR. 
     +                             P_SHIPPING_UNITS(R_UNIT) == "Q") THEN
            R_BTU_CONTENT = 1.0
         ELSEIF(P_SHIPPING_UNITS(R_UNIT) == "T") THEN
            R_BTU_CONTENT = 500.0 / LOCAL_BTU_CONTENT
         ELSEIF(P_SHIPPING_UNITS(R_UNIT) == "M") THEN
            R_BTU_CONTENT = 1000.0/ LOCAL_BTU_CONTENT
         ELSEIF(P_SHIPPING_UNITS(R_UNIT) == "B") THEN
            R_BTU_CONTENT = 1000000. / (LOCAL_BTU_CONTENT*42.)
         ELSEIF(P_SHIPPING_UNITS(R_UNIT) == "5") THEN
            R_BTU_CONTENT = 1000000. / (LOCAL_BTU_CONTENT*55.)
         ENDIF
!            
!         R_BTU_CONTENT = P_BTU_CONTENT(R_UNIT)
!
      RETURN
C**********************************************************************
      ENTRY GET_S_FUEL_PRICE_CONVERSION(R_UNIT,R_BTU_CONTENT,R_YEAR)
C**********************************************************************
         IF(S_BTU_CONTENT(R_UNIT) < -0.1) THEN
            R4_BTU_CONTENT = ABS(S_BTU_CONTENT(R_UNIT))
            LOCAL_BTU_CONTENT = GET_VAR(R4_BTU_CONTENT,
     +                                                R_YEAR,DESC(1:20))
         ELSE
            LOCAL_BTU_CONTENT = S_BTU_CONTENT(R_UNIT)
         ENDIF
!
         R_BTU_CONTENT = 1.0
!
         IF(LOCAL_BTU_CONTENT <= 0. .OR. 
     +                             S_SHIPPING_UNITS(R_UNIT) == "Q") THEN
            R_BTU_CONTENT = 1.0
         ELSEIF(S_SHIPPING_UNITS(R_UNIT) == "T") THEN
            R_BTU_CONTENT = 500.0 / LOCAL_BTU_CONTENT
         ELSEIF(S_SHIPPING_UNITS(R_UNIT) == "M") THEN
            R_BTU_CONTENT = 1000.0/ LOCAL_BTU_CONTENT
         ELSEIF(S_SHIPPING_UNITS(R_UNIT) == "B") THEN
            R_BTU_CONTENT = 1000000. / (LOCAL_BTU_CONTENT*42.)
         ELSEIF(S_SHIPPING_UNITS(R_UNIT) == "5") THEN
            R_BTU_CONTENT = 1000000. / (LOCAL_BTU_CONTENT*55.)
         ENDIF
!         R_BTU_CONTENT = S_BTU_CONTENT(R_UNIT)
      RETURN
C**********************************************************************
      ENTRY GET_E_FUEL_PRICE_CONVERSION(R_UNIT,R_BTU_CONTENT,R_YEAR)
C**********************************************************************
         IF(E_BTU_CONTENT(R_UNIT) < -0.1) THEN
            R4_BTU_CONTENT = ABS(E_BTU_CONTENT(R_UNIT))
            LOCAL_BTU_CONTENT = GET_VAR(R4_BTU_CONTENT,
     +                                                R_YEAR,DESC(1:20))
         ELSE
            LOCAL_BTU_CONTENT = E_BTU_CONTENT(R_UNIT)
         ENDIF
!
         R_BTU_CONTENT = 1.0
!
         IF(LOCAL_BTU_CONTENT <= 0. .OR. 
     +                             E_SHIPPING_UNITS(R_UNIT) == "Q") THEN
            R_BTU_CONTENT = 1.0
         ELSEIF(E_SHIPPING_UNITS(R_UNIT) == "T") THEN
            R_BTU_CONTENT = 500.0 / LOCAL_BTU_CONTENT
         ELSEIF(E_SHIPPING_UNITS(R_UNIT) == "M") THEN
            R_BTU_CONTENT = 1000.0/ LOCAL_BTU_CONTENT
         ELSEIF(E_SHIPPING_UNITS(R_UNIT) == "B") THEN
            R_BTU_CONTENT = 1000000. / (LOCAL_BTU_CONTENT*42.)
         ELSEIF(E_SHIPPING_UNITS(R_UNIT) == "5") THEN
            R_BTU_CONTENT = 1000000. / (LOCAL_BTU_CONTENT*55.)
         ENDIF
!         R_BTU_CONTENT = E_BTU_CONTENT(R_UNIT)
      RETURN
C**********************************************************************
      ENTRY GET_TOTAL_DELIVERY_COST(R_TOTAL_DELIVERY_COST,R_UNIT)
C**********************************************************************
         R_TOTAL_DELIVERY_COST = PRIMARY_DELIVERY_COST(R_UNIT)
      RETURN
C**********************************************************************
      ENTRY GET_TOTAL_S_DELIVERY_COST(R_TOTAL_DELIVERY_COST,R_UNIT)
C**********************************************************************
         R_TOTAL_DELIVERY_COST = SECONDARY_DELIVERY_COST(R_UNIT)
      RETURN
C**********************************************************************
      ENTRY CALC_TOTAL_DELIVERY_COSTS(R_UNIT,START_MO,R_YR,UNITNM)
C**********************************************************************
!
         IF(R_UNIT <= 0) RETURN
!
!
         IF(ALLOCATED(PRIMARY_DELIVERY_COST))
     +         DEALLOCATE(PRIMARY_DELIVERY_COST,SECONDARY_DELIVERY_COST)
         ALLOCATE(PRIMARY_DELIVERY_COST(R_UNIT))
         ALLOCATE(SECONDARY_DELIVERY_COST(R_UNIT))
         PRIMARY_DELIVERY_COST = 0.
         SECONDARY_DELIVERY_COST = 0.
! 080306.
! 080406.
         YES_SP_CAPEX_ACTIVE = SP_CAPEX_ACTIVE()
         IF(YES_SP_CAPEX_ACTIVE) RETURN
!         
         DO I = 1, R_UNIT ! TOTAL UNITS
!
! PRIMARY DELIVERY CALC
!     
            DELIVERY_COST_1 = GET_P_FUEL_DELIVERY(I)
            IF(DELIVERY_COST_1 < 0.) THEN
               THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_1)))
               IF(THIS_FUEL_POINTER <= 0) THEN
                  WRITE(4,*) 'No fuel price vector',DELIVERY_COST_1
                  WRITE(4,*) 'was found in first delivery cost'
                  WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
                  WRITE(4,*) '*** line 954 FUELUSED.FOR ***'
                  er_message='See WARNING MESSAGES -Fuelused.for-1'
                  call end_program(er_message)
               ENDIF
               DELIVERY_COST_1 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
            ENDIF
! ADDED 8/26/02. GAT.
            IF(YES_REGIONAL_PARAMETER_ACTIVE) THEN
               TG = get_TRANSACTION_GROUP(I)
               DELIVERY_COST_1 = DELIVERY_COST_1 * 
     +             GET_MONTHLY_REGIONAL_PARAM(R_YR,START_MO,TG,INT2(10))

            ENDIF
!            
            DELIVERY_COST_2 = GET_P_FUEL_DELIVERY_2(I)
            IF(DELIVERY_COST_2 < 0.) THEN
               THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_2)))
               IF(THIS_FUEL_POINTER <= 0) THEN
                  WRITE(4,*) 'No fuel price vector',DELIVERY_COST_2
                  WRITE(4,*) 'was found in the primary fuel variable'
                  WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
                  WRITE(4,*) '*** line 968 FUELUSED.FOR ***'
                  er_message='See WARNING MESSAGES -Fuelused.for-2'
                  call end_program(er_message)
               ENDIF
               DELIVERY_COST_2 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
            ENDIF
            DELIVERY_COST_3 = GET_P_FUEL_DELIVERY_3(I)
            IF(DELIVERY_COST_3 < 0.) THEN
               THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_3)))
               IF(THIS_FUEL_POINTER <= 0) THEN
                  WRITE(4,*) 'No fuel price vector',DELIVERY_COST_3
                  WRITE(4,*) 'was found in the primary fuel variable'
                  WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
                  WRITE(4,*) '*** line 982 FUELUSED.FOR ***'
                  er_message='See WARNING MESSAGES -Fuelused.for-3'
                  call end_program(er_message)
               ENDIF
               DELIVERY_COST_3 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
            ENDIF
            PRIMARY_DELIVERY_COST(I) = 
     +                           DELIVERY_COST_1 +
     +                           DELIVERY_COST_2 +
     +                           DELIVERY_COST_3 
!
! SECONDARY DELIVERY CALC
            
            
            DELIVERY_COST_1 = GET_S_FUEL_DELIVERY(I)
!        Fuel delivery arrays become "not visible" between return from
!        the previous call, to the following call to 
!        GET_S_FUEL_DELIVERY_2 a few lines down.
       ! TODO: Delete these lines. They're put here for troubleshooting.
            DELIVERY_COST_1 = GET_S_FUEL_DELIVERY(I)
            ! End of delete todo

            IF(DELIVERY_COST_1 < 0.) THEN
               dc_idx=ABS(INT2(DELIVERY_COST_1))
               
               THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(dc_idx)
               IF(THIS_FUEL_POINTER <= 0) THEN
                  WRITE(4,*) 'No fuel price vector',DELIVERY_COST_1
                  WRITE(4,*) 'was found in first delivery cost'
                  WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
                  WRITE(4,*) '*** line 954 FUELUSED.FOR ***'
                  er_message='See WARNING MESSAGES -Fuelused.for-4'
                  call end_program(er_message)
               ENDIF
               DELIVERY_COST_1 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
            ENDIF
! ADDED 8/26/02. GAT.
            IF(YES_REGIONAL_PARAMETER_ACTIVE) THEN
               TG = get_TRANSACTION_GROUP(I)
               DELIVERY_COST_1 = DELIVERY_COST_1 * 
     +             GET_MONTHLY_REGIONAL_PARAM(R_YR,START_MO,TG,INT2(10))

            ENDIF
!           !Fuel arrays became unallocated before this call.  Something
            !in previous code is hinky. Look for allocation and index
            !issues.
            DELIVERY_COST_2 = GET_S_FUEL_DELIVERY_2(I)
            IF(DELIVERY_COST_2 < 0.) THEN
               THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_2)))
               IF(THIS_FUEL_POINTER <= 0) THEN
                  WRITE(4,*) 'No fuel price vector',DELIVERY_COST_2
                  WRITE(4,*) 'was found in the primary fuel variable'
                  WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
                  WRITE(4,*) '*** line 968 FUELUSED.FOR ***'
                  er_message='See WARNING MESSAGES -Fuelused.for-5'
                  call end_program(er_message)
               ENDIF
               DELIVERY_COST_2 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
            ENDIF
            DELIVERY_COST_3 = GET_S_FUEL_DELIVERY_3(I)
            IF(DELIVERY_COST_3 < 0.) THEN
               THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_3)))
               IF(THIS_FUEL_POINTER <= 0) THEN
                  WRITE(4,*) 'No fuel price vector',DELIVERY_COST_3
                  WRITE(4,*) 'was found in the primary fuel variable'
                  WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
                  WRITE(4,*) '*** line 982 FUELUSED.FOR ***'
                  er_message='See WARNING MESSAGES -Fuelused.for-6'
                  call end_program(er_message)
               ENDIF
               DELIVERY_COST_3 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
            ENDIF
!
            SECONDARY_DELIVERY_COST(I) = 
     +                           DELIVERY_COST_1 +
     +                           DELIVERY_COST_2 +
     +                           DELIVERY_COST_3 
!
         ENDDO ! UNITS
      RETURN
C**********************************************************************
      ENTRY GET_MRX_DELIVERY_COST(        R_UNIT,
     +                                    R_TOTAL_DELIVERY_COST,
     +                                    START_MO,R_YR,
     +                                    FUEL_POINTERS_USED,
     +                                    R_EXISTING_UNIT)
C**********************************************************************


         IF(R_EXISTING_UNIT) THEN
            P_COST_1 = GET_FUEL_DELIVERY(R_UNIT,
     +                          DELIVERY_COST_1,
     +                          DELIVERY_COST_2,
     +                          DELIVERY_COST_3)         
            IF(P_BTU_COST_IS_A_POINTR(R_UNIT)) THEN
               P_COST_1 = -P_BTU_COST_POINTR(R_UNIT)

            ENDIF
         ELSE
            P_COST_1 = GET_NEW_UNIT_FUEL_DELIVERY(
     +                          R_UNIT,
     +                          DELIVERY_COST_1,
     +                          DELIVERY_COST_2,
     +                          DELIVERY_COST_3)         

         ENDIF
     

         int_pcost1=int(p_cost_1,2)
         

         
         if(int_pcost1==0 .and. (p_cost_1>1 .or. p_cost_1 
     +    <-1.0)) then
           er_message="fuelused:0002 - p_cost_1 has valid data,"
           er_message=trim(er_message) // " but the data isn't "
           er_message=trim(er_message) // "getting returned "
           er_message=trim(er_message) // "properly " //
     + " via Lahey intrinsics: [=int(x)]."
         
            call end_program(er_message)
         end if
         
         write(floatstr, *) p_cost_1
         write(floatstr, *) int_pcost1
         
         abs_pcost1=abs(int_pcost1)
         ! int_pcost==/
         ! abs_pcost1=/
         IF(P_COST_1 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(abs_pcost1)
         !This_fuel_pointr=1010/0
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',P_COST_1
               WRITE(4,*) 'was found in first delivery cost'
               WRITE(4,*) 'for Capacity Limited unit ',
     +                                             RETURN_UNITNM(R_UNIT)
               WRITE(4,*) '*** line 1011 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-7'
               call end_program(er_message)
            ENDIF
!
            P_COST_1 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
!     
            FUEL_POINTERS_USED = .TRUE.
         ELSE
            FUEL_POINTERS_USED = .FALSE.
         ENDIF
         IF(DELIVERY_COST_1 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_1)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',DELIVERY_COST_1
               WRITE(4,*) 'was found in first delivery cost'
               WRITE(4,*) 'for Capacity Limited unit ',
     +                                             RETURN_UNITNM(R_UNIT)
               WRITE(4,*) '*** line 1030 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-8'
               call end_program(er_message)
            ENDIF
            DELIVERY_COST_1 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ENDIF
!         DELIVERY_COST_2 = R_DELIVERY_INDEX_2
         IF(DELIVERY_COST_2 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_2)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',DELIVERY_COST_2
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit ',
     +                                             RETURN_UNITNM(R_UNIT)
               WRITE(4,*) '*** line 1044 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-9'
               call end_program(er_message)
            ENDIF
            DELIVERY_COST_2 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ENDIF
!         DELIVERY_COST_3 = R_DELIVERY_INDEX_3
         IF(DELIVERY_COST_3 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_3)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',DELIVERY_COST_3
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit ',
     +                                             RETURN_UNITNM(R_UNIT)
               WRITE(4,*) '*** line 1058 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-10'
               call end_program(er_message)
            ENDIF
            DELIVERY_COST_3 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ENDIF
         R_TOTAL_DELIVERY_COST = 
     +                           P_COST_1 +
     +                           DELIVERY_COST_1 +
     +                           DELIVERY_COST_2 +
     +                           DELIVERY_COST_3 
      RETURN
C**********************************************************************
      ENTRY GET_DELIVERED_FUEL_COST(      R_PRIMARY_FUEL_ID,
     +                                    R_DELIVERY_ID_1,
     +                                    R_DELIVERY_ID_2,
     +                                    R_DELIVERY_ID_3,
     +                                    R_TOTAL_DELIVERY_COST,
     +                                    START_MO,R_YR)
!     +                                    FUEL_POINTERS_USED)
C**********************************************************************

         P_COST_1 = R_PRIMARY_FUEL_ID
         DELIVERY_COST_1 = R_DELIVERY_ID_1
         DELIVERY_COST_2 = R_DELIVERY_ID_2
         DELIVERY_COST_3 = R_DELIVERY_ID_3
!
         IF(P_COST_1 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(P_COST_1)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',P_COST_1
               WRITE(4,*) 'was found in first delivery cost'
               WRITE(4,*) 'for Capacity Limited unit '

               WRITE(4,*) '*** line 1011 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-11'
               call end_program(er_message)
            ENDIF
!
            P_COST_1 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
!     
!            FUEL_POINTERS_USED = .TRUE.
         ELSE
!            FUEL_POINTERS_USED = .FALSE.
         ENDIF
         IF(DELIVERY_COST_1 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_1)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',DELIVERY_COST_1
               WRITE(4,*) 'was found in first delivery cost'
               WRITE(4,*) 'for Capacity Limited unit '

               WRITE(4,*) '*** line 1030 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-12'
               call end_program(er_message)
            ENDIF
            DELIVERY_COST_1 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ENDIF
!         DELIVERY_COST_2 = R_DELIVERY_INDEX_2
         IF(DELIVERY_COST_2 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_2)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',DELIVERY_COST_2
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit '

               WRITE(4,*) '*** line 1044 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-13'
               call end_program(er_message)
            ENDIF
            DELIVERY_COST_2 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ENDIF
!         DELIVERY_COST_3 = R_DELIVERY_INDEX_3
         IF(DELIVERY_COST_3 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(DELIVERY_COST_3)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',DELIVERY_COST_3
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit '

               WRITE(4,*) '*** line 1058 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-14'
               call end_program(er_message)
            ENDIF
            DELIVERY_COST_3 = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ENDIF
         R_TOTAL_DELIVERY_COST = 
     +                           P_COST_1 +
     +                           DELIVERY_COST_1 +
     +                           DELIVERY_COST_2 +
     +                           DELIVERY_COST_3 
      RETURN
C**********************************************************************
      ENTRY GET_A_FUEL_PRICE(             R_PRIMARY_FUEL_ID,
     +                                    R_TOTAL_DELIVERY_COST,
     +                                    START_MO,R_YR)
C**********************************************************************
         P_COST_1 = R_PRIMARY_FUEL_ID
!
         IF(P_COST_1 < 0.) THEN
            THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(INT2(P_COST_1)))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',P_COST_1
               WRITE(4,*) 'was found in first delivery cost'
               WRITE(4,*) 'for Capacity Limited unit '
               WRITE(4,*) '*** line 1011 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-15'
               call end_program(er_message)
            ENDIF
!
            P_COST_1 = NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ELSE
! JUST USE THE PRICE           
         ENDIF
         R_TOTAL_DELIVERY_COST =  P_COST_1 
      RETURN
C**********************************************************************
      ENTRY PUT_A_FUEL_PRICE(R_YEAR,
     +                       R_MONTH,
     +                       R_VECTOR_TO_PUT,
     +                       R_VALUE_TO_PUT)
C**********************************************************************
!
         LOCAL_YEAR = R_YEAR
         MON = R_MONTH
!
         THIS_FUEL_POINTER = 
     +                     FUEL_PRICE_POINTR(ABS(R_VECTOR_TO_PUT))
         IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',P_COST_1
               WRITE(4,*) 'was found in first delivery cost'
               WRITE(4,*) 'for Capacity Limited unit '
               WRITE(4,*) '*** line 1011 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-16'
               call end_program(er_message)
         ENDIF
!
! 041510. MARK SHOULD LOOK AT THIS CODE.
!
         IF(MON < 12) THEN
            MON = MON + 1 
            NEW_FUEL_PRICE(THIS_FUEL_POINTER,MON,LOCAL_YEAR) = 
     +                                                    R_VALUE_TO_PUT
         ELSE
            MON = 1
            NEW_FUEL_PRICE(THIS_FUEL_POINTER,MON,LOCAL_YEAR) = 
     +                                                    R_VALUE_TO_PUT

            IF(LOCAL_YEAR < YEARS_TO_RUN) THEN
               LOCAL_YEAR = R_YEAR + 1
               NEW_FUEL_PRICE(THIS_FUEL_POINTER,MON,LOCAL_YEAR) = 
     +                                                    R_VALUE_TO_PUT
            ENDIF
         ENDIF

      RETURN

!      
C**********************************************************************
      ENTRY GET_THOSE_FUEL_PRICES(nr_units,START_MO,END_MO,R_YR,
     +    PBTUCT,SBTUCT,FUEL_BTU_COST,FUELMX,
     +    DISP_BTU_COST,MMBTU_FUEL_BALANCE,
     +    FUEL_INVENTORY_ID,
     +    FUEL_INVENTORY_ACTIVE,
     +    EMISS_FUEL_COST_loc,EMISS_FUEL_ESCAL_loc,
     +    EMISS_BLENDING_RATE_loc,BLENDED_BTU_COST,
     +    Fuel_SupplyID,UNITNM)
C**********************************************************************
      PLANNING_YEAR = R_YR
      YR = MIN(R_YR,YEARS_TO_RUN)

      DO I = 1, nr_units
         IF(.NOT. (P_BTU_COST_IS_A_POINTR(I) .OR. 
     +                    S_BTU_COST_IS_A_POINTR(I) .OR.
     +                                 E_BTU_COST_IS_A_POINTR(I))) CYCLE
         IF(P_BTU_COST_IS_A_POINTR(I)) THEN
            PBTUCT(I) = 0.
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(P_BTU_COST_POINTR(I))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',THIS_FUEL_POINTER
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
               WRITE(4,*) '*** line 1094 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-18'
               call end_program(er_message)
            ENDIF
            IREC = 13*(THIS_FUEL_POINTER - 1) + START_MO + 1
            DO fu_mo = START_MO, END_MO

               IF(PLANNING_YEAR > MAX_SIMULATION_YEARS) THEN

                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) = 
     +                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) * 
     +                      NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,
     +                                               FUEL_PRICE_YEARS)**
     +                            (PLANNING_YEAR - MAX_SIMULATION_YEARS)
               ENDIF
               PBTUCT(I) = PBTUCT(I) + 
     +                        NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR)
               IREC = IREC + 1
            ENDDO
            IF(END_MO > START_MO) PBTUCT(I) = PBTUCT(I)/
     +                                FLOAT(END_MO - START_MO +1)
         ENDIF
         IF(S_BTU_COST_IS_A_POINTR(I)) THEN
            SBTUCT(I) = 0.
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(S_BTU_COST_POINTR(I))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',THIS_FUEL_POINTER
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
               WRITE(4,*) '*** line 1126 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-19'
               call end_program(er_message)
            ENDIF
            IREC = 13*(THIS_FUEL_POINTER - 1) + START_MO + 1
            DO fu_mo = START_MO, END_MO

               IF(PLANNING_YEAR > MAX_SIMULATION_YEARS) THEN

                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) = 
     +                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) * 
     +                      NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,
     +                                               FUEL_PRICE_YEARS)**
     +                            (PLANNING_YEAR - MAX_SIMULATION_YEARS)
               ENDIF
               SBTUCT(I) = SBTUCT(I) + 
     +                        NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR)
               IREC = IREC + 1
            ENDDO
            IF(END_MO > START_MO) SBTUCT(I) = SBTUCT(I)/
     +                                FLOAT(END_MO - START_MO +1)
         ENDIF
         IF(E_BTU_COST_IS_A_POINTR(I)) THEN
            EMISS_FUEL_COST_loc(I) = 0.
            IREC = 13*(FUEL_PRICE_POINTR(E_BTU_COST_POINTR(I)) - 1) + 
     +                                                      START_MO + 1
            DO fu_mo = START_MO, END_MO

               IF(PLANNING_YEAR > MAX_SIMULATION_YEARS) THEN

                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) = 
     +                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) * 
     +                      NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,
     +                                               FUEL_PRICE_YEARS)**
     +                            (PLANNING_YEAR - MAX_SIMULATION_YEARS)
               ENDIF
               EMISS_FUEL_COST_loc(I) = EMISS_FUEL_COST_loc(I) + 
     +                        NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR)
               IREC = IREC + 1
            ENDDO
            IF(END_MO > START_MO) EMISS_FUEL_COST_loc(I)=
     + EMISS_FUEL_COST_loc(I)/
     +                                       FLOAT(END_MO - START_MO +1)
         ENDIF
C
C RE-CALCULATE FUEL PRICES ! TESTING THIS
C
C         CALL RECALCULATE_FUEL_COSTS(I)
C
C EMISSIONS BLENDING ADDED 12/30/92
C
         IF(EMISS_BLENDING_RATE_loc(I) > 0.) THEN
       BLENDED_BTU_COST(I) = (1.-EMISS_BLENDING_RATE_loc(I))*PBTUCT(I)+
     +             EMISS_BLENDING_RATE_loc(I) * EMISS_FUEL_COST_loc(I)
         ELSE
            BLENDED_BTU_COST(I) = PBTUCT(I)
         ENDIF
         IF(ABS(FUELMX(I)) /= 1.) THEN
            FUEL_BTU_COST(I) = ABS(FUELMX(I)) * BLENDED_BTU_COST(I) +
     +                                (1. - ABS(FUELMX(I))) * SBTUCT(I)
         ELSE
            FUEL_BTU_COST(I) = BLENDED_BTU_COST(I)
         ENDIF
         FUEL_ID = Fuel_SupplyID(I)
         IF(FUELMX(I) < 0. .OR.
     +     (FUEL_INVENTORY_ACTIVE .AND. FUEL_ID /= 0 .AND. 
     +        MMBTU_FUEL_BALANCE(FUEL_INVENTORY_ID(FUEL_ID))<=0.D0))THEN
            DISP_BTU_COST(I) = SBTUCT(I)
         ELSE
            DISP_BTU_COST(I) = FUEL_BTU_COST(I)
         ENDIF
      ENDDO
      DEALLOCATE(FUEL_PRICE,STAT=ERROR)
      RETURN
C**********************************************************************
      ENTRY GET_FUEL_PRICES_BY_UNIT(R_UNIT,START_MO,END_MO,R_YR,
     +       PBTUCT,SBTUCT,FUEL_BTU_COST,FUELMX,
     +       DISP_BTU_COST,MMBTU_FUEL_BALANCE,
     +       FUEL_INVENTORY_ID,
     +       FUEL_INVENTORY_ACTIVE,
     +       EMISS_FUEL_COST_loc,EMISS_FUEL_ESCAL_loc,
     +       EMISS_BLENDING_RATE_loc,BLENDED_BTU_COST,
     +       Fuel_SupplyID,UNITNM,
     +       FUEL_POINTERS_USED)

C**********************************************************************
      PLANNING_YEAR = R_YR
      YR = MIN(R_YR,YEARS_TO_RUN)
! SHOULD BE CALLED ONCE PER YEAR
      DYNAMIC_FUEL_CHOICE = YES_DYNAMIC_FUEL_PRICING()

      I = R_UNIT

      IF(YES_SP_CAPEX_ACTIVE) RETURN
!      
      IF(.NOT. (P_BTU_COST_IS_A_POINTR(I) .OR. 
     +                    S_BTU_COST_IS_A_POINTR(I) .OR.
     +                                E_BTU_COST_IS_A_POINTR(I))) THEN
         FUEL_POINTERS_USED = .FALSE.
      ELSE
         IF(P_BTU_COST_IS_A_POINTR(I)) THEN
            PBTUCT(I) = 0.
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(P_BTU_COST_POINTR(I))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',THIS_FUEL_POINTER
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
               WRITE(4,*) 'The input value was ',P_BTU_COST_POINTR(I)
               WRITE(4,*) '*** line 1235 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-20'
               call end_program(er_message)
            ENDIF
            IREC = 13*(THIS_FUEL_POINTER - 1) + START_MO + 1
            DO fu_mo = START_MO, END_MO

               IF(PLANNING_YEAR > MAX_SIMULATION_YEARS) THEN

                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) = 
     +                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) * 
     +                      NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,
     +                                               FUEL_PRICE_YEARS)**
     +                            (PLANNING_YEAR - MAX_SIMULATION_YEARS)
               ENDIF
               PBTUCT(I) = PBTUCT(I) +
     +                        NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR)
               IREC = IREC + 1
            ENDDO
            IF(END_MO > START_MO) PBTUCT(I) = PBTUCT(I)/
     +                                FLOAT(END_MO - START_MO +1)
         ENDIF
         IF(S_BTU_COST_IS_A_POINTR(I)) THEN
            SBTUCT(I) = 0.
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(S_BTU_COST_POINTR(I))
            IF(THIS_FUEL_POINTER <= 0) THEN
               WRITE(4,*) 'No fuel price vector',THIS_FUEL_POINTER
               WRITE(4,*) 'was found in the primary fuel variable'
               WRITE(4,*) 'for Capacity Limited unit ',UNITNM(I)  
               WRITE(4,*) '*** line 1267 FUELUSED.FOR ***'
               er_message='See WARNING MESSAGES -Fuelused.for-21'
               call end_program(er_message)
            ENDIF
            IREC = 13*(THIS_FUEL_POINTER - 1) + START_MO + 1
            DO fu_mo = START_MO, END_MO

               IF(PLANNING_YEAR > MAX_SIMULATION_YEARS) THEN

                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) = 
     +                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) * 
     +                      NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,
     +                                               FUEL_PRICE_YEARS)**
     +                            (PLANNING_YEAR - MAX_SIMULATION_YEARS)
               ENDIF
               SBTUCT(I) = SBTUCT(I) + 
     +                        NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR)
               IREC = IREC + 1
            ENDDO
            IF(END_MO > START_MO) SBTUCT(I) = SBTUCT(I)/
     +                                FLOAT(END_MO - START_MO +1)
         ENDIF
         IF(E_BTU_COST_IS_A_POINTR(I)) THEN
            EMISS_FUEL_COST_loc(I) = 0.
            IREC = 13*(FUEL_PRICE_POINTR(E_BTU_COST_POINTR(I)) - 1) + 
     +                                                      START_MO + 1
            DO fu_mo = START_MO, END_MO

               IF(PLANNING_YEAR > MAX_SIMULATION_YEARS) THEN

                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) = 
     +                  NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR) * 
     +                      NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,
     +                                               FUEL_PRICE_YEARS)**
     +                            (PLANNING_YEAR - MAX_SIMULATION_YEARS)
               ENDIF
               EMISS_FUEL_COST_loc(I) = EMISS_FUEL_COST_loc(I) + 
     +                       NEW_FUEL_PRICE(THIS_FUEL_POINTER,fu_mo,YR)
               IREC = IREC + 1
            ENDDO
            IF(END_MO > START_MO) EMISS_FUEL_COST_loc(I)=
     + EMISS_FUEL_COST_loc(I)/
     +                                       FLOAT(END_MO - START_MO +1)
         ENDIF

! PRIMARY AND SECONDARY DELIVERY COSTS ADDED 08/30/02.

         IF(EMISS_BLENDING_RATE_loc(I) > 0.) THEN
       BLENDED_BTU_COST(I) = (1.-EMISS_BLENDING_RATE_loc(I))*PBTUCT(I)+
     +              EMISS_BLENDING_RATE_loc(I) * EMISS_FUEL_COST_loc(I)
         ELSE
            BLENDED_BTU_COST(I) = PBTUCT(I) 
         ENDIF
!
         BLENDED_BTU_COST(I) = BLENDED_BTU_COST(I) + 
     +                                          PRIMARY_DELIVERY_COST(I)

!
         IF(DYNAMIC_FUEL_CHOICE .AND.  
     +            FUELMX(I) > .999 .AND.
     +                   SBTUCT(I)+SECONDARY_DELIVERY_COST(I) > 0.) THEN
            IF(BLENDED_BTU_COST(I) > 
     +                        SBTUCT(I)+SECONDARY_DELIVERY_COST(I)) THEN
               FUEL_BTU_COST(I) = SBTUCT(I)+SECONDARY_DELIVERY_COST(I)
            ELSE
               FUEL_BTU_COST(I) = BLENDED_BTU_COST(I)
            ENDIF
         ELSEIF(ABS(FUELMX(I)) /= 1.) THEN
            FUEL_BTU_COST(I) = ABS(FUELMX(I)) * 
     +               BLENDED_BTU_COST(I) +
     +                       (1. - ABS(FUELMX(I))) * 
     +                            (SBTUCT(I)+SECONDARY_DELIVERY_COST(I))
         ELSE
            FUEL_BTU_COST(I) = BLENDED_BTU_COST(I)
         ENDIF
         FUEL_ID = Fuel_SupplyID(I)
         IF(FUELMX(I) < 0. .OR.
     +     (FUEL_INVENTORY_ACTIVE .AND. FUEL_ID /= 0 .AND. 
     +        MMBTU_FUEL_BALANCE(FUEL_INVENTORY_ID(FUEL_ID))<=0.D0))THEN
            DISP_BTU_COST(I) = SBTUCT(I) + SECONDARY_DELIVERY_COST(I)
         ELSE
            ! DELIVERY COST ALREADY INCLUDED
            DISP_BTU_COST(I) = FUEL_BTU_COST(I) 

         ENDIF
         FUEL_POINTERS_USED = .TRUE.
      ENDIF ! USE A FUEL POINTER FOR AT LEAST ONE FUEL.

      DEALLOCATE(FUEL_PRICE,STAT=ERROR)
      RETURN
      ENTRY GET_FUEL_PRICE_POINTR(R_PRIMARY_FUEL_ID,  
     +                            START_MO,R_YR)
!
         IF(R_PRIMARY_FUEL_ID < 0.) THEN
            THIS_FUEL_POINTER = FUEL_PRICE_POINTR(
     +                                     ABS(INT2(R_PRIMARY_FUEL_ID)))
            IF(THIS_FUEL_POINTER > 0) R_PRIMARY_FUEL_ID = 
     +                   NEW_FUEL_PRICE(THIS_FUEL_POINTER,START_MO,R_YR)
         ENDIF
      RETURN
      END

