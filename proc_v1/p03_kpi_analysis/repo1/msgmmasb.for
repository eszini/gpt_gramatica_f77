
!     msgmmasb.for
!     Created: 10/2/02 9:16:35 PM
!     Author : msg
!     Last change: MSG 2/12/2007 3:09:39 PM



!          CALCULATE FUTURE ASSET VALUE

      SUBROUTINE FINASSET(R_FIRST_END_POINT)

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use fainpt
      use lcarry
      use tastgp
      use wkarry
      implicit none
      logical :: dbgresult
      LOGICAL (kind=1) ::  FIRST_END_POINT,OVERLAY_EXPENSE_FILES_ACTIVE
      LOGICAL (kind=4) ::  R_FIRST_END_POINT
      LOGICAL (kind=1) ::  VOID_LOGICAL,STORE_PROPERTY_ESCALATION,
     +          STORE_AI_AFUDC_RATES
      LOGICAL (kind=1) ::  FUTURE_ASSETS_REPORT,EXISTING_ASSETS_REPORT,
     +          NUCLEAR_FUEL_REPORT,DEBT_REPORT,DEBIT_REPORT,
     +          EXPENSE_REVENUES_REPORT
      CHARACTER (len=1) ::  UTILITY_TYPE
      CHARACTER (len=256) ::  FILE_NAME,OUTPUT_DIRECTORY
      INTEGER (kind=2) ::  I,J,K,DELETE
      INTEGER (kind=4) ::  IOS
      REAL (kind=4) ::  IARRAY(MAX_FINANCIAL_SIMULATION_YEARS,6),
     +     NON_ZERO_PROP_ESC_RATE
      REAL (kind=4) ::  EATAXDEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +     EAREGTAXDEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +     EAREGDEFTAX(MAX_FINANCIAL_SIMULATION_YEARS),
     +     EADEFTAX(MAX_FINANCIAL_SIMULATION_YEARS),
     +     TOTAL_EA_ACE_DEP(MAX_FINANCIAL_SIMULATION_YEARS)
!
      REAL (kind=4) ::  AFUDC_AI_RATE(MAX_FINANCIAL_SIMULATION_YEARS)
      REAL (kind=4) ::  AFDCRT
      REAL (kind=4) ::  AFDCBR
      REAL (kind=4) ::  INTEREST_CAP_RATE,CURRENT_INTEREST_CAP_RATE,
     +     PROPERTY_ESCALATION,
     +     AFUDC_NF_RATE
      COMMON/AFUDC_STUFF/ AFDCRT(MAX_FINANCIAL_SIMULATION_YEARS),
     +        AFDCBR(MAX_FINANCIAL_SIMULATION_YEARS),
     +        INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +        PROPERTY_ESCALATION(MAX_FINANCIAL_SIMULATION_YEARS),
     +        CURRENT_INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS),
     +        AFUDC_NF_RATE(MAX_FINANCIAL_SIMULATION_YEARS)


      INTEGER (kind=4) ::  DUMMY(64)
      LOGICAL (kind=4) ::  FILEXIST
      CHARACTER (len=2) ::  DEBTOL
      CHARACTER (len=2) ::  PARMASOL,EXASTOL,ASSET_VEC_OL,DEBITOL,

     +            NFUEL_OL
      LOGICAL (KIND=1) :: FUASTOL,ESCAL_OL,GET_ESCAL_OL
!     DECLARATION FOR WORLD VARIABLES FOR FUTURE ASSETS
      CHARACTER (len=2) ::  DATA_DRIVE
      INTEGER (kind=2) ::  BASEYEAR,gc_max_years
      COMMON /WORLD/ BASEYEAR,gc_max_years
      COMMON /DATA_DRIVE_LOCATION/ DATA_DRIVE
!
      DATA DUMMY/64*0/
!
!
! VALUES FROM THE PARAMETER FILE ARE NEEDED READ THEM
!
      NON_ZERO_PROP_ESC_RATE = 0.
      
      ! R_FIRST_END_POINT is true on first call.
      FIRST_END_POINT = R_FIRST_END_POINT
!
      CALL DISPLAY_TIME
      AFDCRT = 0.1
      AFDCBR = 0.5
      AFUDC_AI_RATE = 0.1
      AFUDC_NF_RATE = 0.1
      EATAXDEP = 0.
      EAREGTAXDEP = 0.
      EAREGDEFTAX = 0.
      EADEFTAX = 0.
      TOTAL_EA_ACE_DEP = 0.
      PROPERTY_ESCALATION = 0.
      CURRENT_INTEREST_CAP_RATE = 0.
      INTEREST_CAP_RATE = 1.
      IARRAY = 0.
      IF(UTILITY_TYPE() == 'T') THEN
         AFDCRT = .05
         AFDCBR = 1.0
      ENDIF
!
      CALL DISPLAY_TIME
      CALL GET_PARMAS_OL(PARMASOL)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//PARMASOL//"ASTPRM.BIN"
      INQUIRE(FILE=FILE_NAME,EXIST=FILEXIST)
      IF(FILEXIST) THEN
         CALL OPEN_ASSET_RATES_FILE(10)
         INTEREST_CAP_RATE(1) = 100.
         IOS = 0
         I = 2
         DO
            READ(10,REC=I-1,IOSTAT=IOS) DELETE,DELETE,AFDCRT(I),
     +         AFDCBR(I),(IARRAY(I,J),J=1,6),
     +         EATAXDEP(I),EADEFTAX(I),EAREGTAXDEP(I),
     +         EAREGDEFTAX(I),INTEREST_CAP_RATE(I),
     +         TOTAL_EA_ACE_DEP(I),PROPERTY_ESCALATION(I),
     +         CURRENT_INTEREST_CAP_RATE(I),
     +         AFUDC_NF_RATE(I),
     +         AFUDC_AI_RATE(I)
            IF(IOS /= 0) EXIT
            I = I + 1
         ENDDO
         CALL CLOSE_ASSET_RATES_FILE
         IF(I > 2) THEN
            INTEREST_CAP_RATE(1) = INTEREST_CAP_RATE(2)
            PROPERTY_ESCALATION(1) = PROPERTY_ESCALATION(2)
            CURRENT_INTEREST_CAP_RATE(1) =
     +                                CURRENT_INTEREST_CAP_RATE(2)
            AFUDC_AI_RATE(1) = AFUDC_AI_RATE(2)
            AFDCRT(1) = AFDCRT(2)
            AFUDC_NF_RATE(1) = AFUDC_NF_RATE(2)
            AFDCBR(1) = AFDCBR(2)
         ENDIF
!
            IARRAY(1,:) = IARRAY(2,:)
!
         IF(I < MAX_FINANCIAL_SIMULATION_YEARS) THEN
            DO J = I, MAX_FINANCIAL_SIMULATION_YEARS
               AFDCRT(J) = AFDCRT(J-1)
               AFDCBR(J) = AFDCBR(J-1)
               AFUDC_NF_RATE(J) = AFUDC_NF_RATE(J-1)
               AFUDC_AI_RATE(J) = AFUDC_AI_RATE(J-1)
               EATAXDEP(J) = EATAXDEP(J-1)
               TOTAL_EA_ACE_DEP(J) = TOTAL_EA_ACE_DEP(J-1)
               EAREGTAXDEP(J) = EAREGTAXDEP(J-1)
               EAREGDEFTAX(J) = EAREGDEFTAX(J-1)
               EADEFTAX(J) = EADEFTAX(J-1)
               INTEREST_CAP_RATE(J) = INTEREST_CAP_RATE(J-1)
               CURRENT_INTEREST_CAP_RATE(J) =
     +                              CURRENT_INTEREST_CAP_RATE(J-1)
               PROPERTY_ESCALATION(J) = PROPERTY_ESCALATION(J-1)
               DO K = 1, 6
                  IARRAY(J,K) = IARRAY(J-1,K)
               ENDDO
            ENDDO
         ENDIF
         DO I = 1, MAX_FINANCIAL_SIMULATION_YEARS
            AFDCRT(I) = AFDCRT(I)/100.
            AFUDC_NF_RATE(I) = AFUDC_NF_RATE(I)/100.
            AFUDC_AI_RATE(I) = AFUDC_AI_RATE(I)/100.
            AFDCBR(I) = AFDCBR(I)/100.
            INTEREST_CAP_RATE(I) = INTEREST_CAP_RATE(I)/100.
            CURRENT_INTEREST_CAP_RATE(I) =
     +                              CURRENT_INTEREST_CAP_RATE(I)/100.
            PROPERTY_ESCALATION(I) = PROPERTY_ESCALATION(I)/100.
            NON_ZERO_PROP_ESC_RATE = NON_ZERO_PROP_ESC_RATE +
     +                                         PROPERTY_ESCALATION(I)
            DO K = 1, 6
               IARRAY(I,K) = IARRAY(I,K)/100.
            ENDDO
         ENDDO
      ENDIF
!
      CALL DISPLAY_TIME
      EADEFTAX = 0.
      TOTAL_EA_ACE_DEP = 0.
!
      VOID_LOGICAL = STORE_PROPERTY_ESCALATION(PROPERTY_ESCALATION,
     +                                         NON_ZERO_PROP_ESC_RATE)
      VOID_LOGICAL = STORE_AI_AFUDC_RATES(AFUDC_AI_RATE,
     +                                    AFDCRT,
     +                                    AFUDC_NF_RATE,
     +                                    AFDCBR,
     +                                    CURRENT_INTEREST_CAP_RATE)
!
! GET FILE STATUS INFO
!
      CALL GET_ASSET_VEC_OL(ASSET_VEC_OL)
      ESCAL_OL = GET_ESCAL_OL()
      CALL GET_DEBT_OL(DEBTOL)
      CALL GET_EXAST_OL(EXASTOL)
      CALL GET_DEBIT_OL(DEBITOL)
      CALL GET_EXPENSES_ACTIVE(OVERLAY_EXPENSE_FILES_ACTIVE)
      CALL GET_NFUEL_OL(NFUEL_OL)
      CALL GET_FUAST_OL(FUASTOL)
      IF(FIRST_END_POINT .OR. DEBTOL == 'OL' .OR.
     +             ASSET_VEC_OL == 'OL' .OR. PARMASOL == 'OL' .OR.
     +                                   DEBT_REPORT(VOID_LOGICAL)) THEN
     
         ! FIRST_END_POINT is modified in this call - jtr
         CALL DEBT(IARRAY,FIRST_END_POINT)
      ELSE
         CALL READ_DEBT_BASE_CASE
      ENDIF
      
      ! Right side returns .false. first time. What's in Legacy?
      dbgresult=EXISTING_ASSETS_REPORT(VOID_LOGICAL)
      
      IF(FIRST_END_POINT .OR. EXASTOL == 'OL' .OR.
     +               PARMASOL == 'OL' .OR. ASSET_VEC_OL == 'OL' .OR.
     +                        dbgresult) THEN
         CALL EXASST(EATAXDEP,EAREGTAXDEP,EAREGDEFTAX,EADEFTAX,
     +               TOTAL_EA_ACE_DEP,PROPERTY_ESCALATION,
     +               FIRST_END_POINT)
      ELSE
         CALL READ_EA_BASE_CASE
      ENDIF
      CALL DISPLAY_TIME
! First call:  first_end_point=true, debitol="BC" asset_vec_ol="BC"

      ! First call: dbgresult should be .false.
      dbgresult=DEBIT_REPORT(VOID_LOGICAL)
       
      ! First_end_point is .false. in this call and should be .true.
      ! Why?
      IF(FIRST_END_POINT .OR. DEBITOL == 'OL' .OR.
     +                                        ASSET_VEC_OL == 'OL' .OR.
     +                                  DEBIT_REPORT(VOID_LOGICAL)) THEN
         ! This call does the initial write (that fails inside 
         ! the read_debit_base_case routine).
         CALL DEFERRED_DEBITS(FIRST_END_POINT)
      ELSE
         ! Breaks in this call because deferred_debits hasn't been called.
         CALL READ_DEBIT_BASE_CASE
      ENDIF
      CALL DISPLAY_TIME
!
         CALL EXPENSE(FIRST_END_POINT) ! always run expenses
!
      CALL DISPLAY_TIME
      IF(FIRST_END_POINT .OR. FUASTOL .OR. ESCAL_OL .OR.
     +          PARMASOL == 'OL' .OR. ASSET_VEC_OL == 'OL' .OR.
     +                          FUTURE_ASSETS_REPORT(VOID_LOGICAL)) THEN

         CALL FUASST(R_FIRST_END_POINT,.FALSE.)   ! THE VALUE MUST BE L*4
      ELSE
         CALL READ_FA_BASE_CASE
      ENDIF
      CALL DISPLAY_TIME
      IF(FIRST_END_POINT .OR. NFUEL_OL=='OL' .OR. ESCAL_OL .OR.
     +           PARMASOL=='OL' .OR. ASSET_VEC_OL=='OL' .OR.
     +                           NUCLEAR_FUEL_REPORT(VOID_LOGICAL)) THEN
         CALL NUFUEL(FIRST_END_POINT)
      ELSE
         CALL READ_NUC_FUEL_BASE_CASE
      ENDIF
      CLOSE(12)
      CALL write_scroll_line_RW(' ',2)
      CALL write_scroll_line_RW(' ',0)
      CALL DISPLAY_TIME
      RETURN
      END
!

      FUNCTION STORE_PROPERTY_ESCALATION(R_PROPERTY_ESCALATION,
     +                                   NON_ZERO_PROP_ESC_RATE)

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      
      INTEGER (kind=2) ::  I
      LOGICAL (kind=1) ::  STORE_PROPERTY_ESCALATION
      LOGICAL (kind=1) ::  REAL_PROPERTY_TAX_VALUES_ACTIVE,
     +          REAL_PROPERTY_TAX_VALUES
      REAL (kind=4) ::  NON_ZERO_PROP_ESC_RATE
      INTEGER (kind=2) ::  R_YEAR
      REAL (kind=4) ::  R_PROPERTY_ESCALATION(*),
     +     PROPERTY_ESCALATION_RATES(MAX_FINANCIAL_SIMULATION_YEARS)
      REAL (kind=4) ::  PROPERTY_ESCALATION
      SAVE PROPERTY_ESCALATION_RATES,
     +     REAL_PROPERTY_TAX_VALUES_ACTIVE

         REAL_PROPERTY_TAX_VALUES_ACTIVE = .TRUE. ! NON_ZERO_PROP_ESC_RATE /= 0.
         DO I = 1, MAX_FINANCIAL_SIMULATION_YEARS
            PROPERTY_ESCALATION_RATES(I) = R_PROPERTY_ESCALATION(I)
         ENDDO
         STORE_PROPERTY_ESCALATION = .TRUE.
      RETURN

      ENTRY PROPERTY_ESCALATION(R_YEAR)
         PROPERTY_ESCALATION = PROPERTY_ESCALATION_RATES(R_YEAR)
      RETURN
      ENTRY REAL_PROPERTY_TAX_VALUES
         CALL RETURN_APPRAISED_PROP_SWITCH(
     +                                  REAL_PROPERTY_TAX_VALUES_ACTIVE) ! 7/13/99 ACTIVATED SWITCH IN RUN SWITCHES
         REAL_PROPERTY_TAX_VALUES = REAL_PROPERTY_TAX_VALUES_ACTIVE
      RETURN
      END


      FUNCTION STORE_AI_AFUDC_RATES(R_AFUDC_AI_RATE,
     +                              R_AFUDC_FA_RATE,
     +                              R_AFUDC_NF_RATE,
     +                              R_BORROWED_AFUDC_RATE,
     +                              R_CURRENT_INTEREST_CAP_RATE)

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom

      LOGICAL (kind=1) ::  STORE_AI_AFUDC_RATES
      REAL (kind=4) ::  R_BORROWED_AFUDC_RATE(*)
      REAL (kind=4) ::  R_AFUDC_AI_RATE(*)
      REAL (kind=4) ::  R_AFUDC_FA_RATE(*)
      REAL (kind=4) ::  R_AFUDC_NF_RATE(*)
      REAL (kind=4) ::  R_CURRENT_INTEREST_CAP_RATE(*)
      REAL (kind=4) ::  AI_AFUDC_RATE,AFUDC_BORROWED_RATE,

     +     FA_AFUDC_RATE,NF_AFUDC_RATE,INTEREST_CAP_RATE
      REAL (kind=4) ::  STORED_AFUDC_BORROWED_RATE
     +                               (MAX_FINANCIAL_SIMULATION_YEARS-1),
     +     STORED_AI_AFUDC_RATE(MAX_FINANCIAL_SIMULATION_YEARS-1),
     +     STORED_NF_AFUDC_RATE(MAX_FINANCIAL_SIMULATION_YEARS-1),
     +     STORED_FA_AFUDC_RATE(MAX_FINANCIAL_SIMULATION_YEARS-1),
     +     STORED_INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS-1)
      INTEGER (kind=2) ::  I
      SAVE STORED_AFUDC_BORROWED_RATE,
     +     STORED_AI_AFUDC_RATE,
     +     STORED_FA_AFUDC_RATE,
     +     STORED_NF_AFUDC_RATE,
     +     STORED_INTEREST_CAP_RATE

         DO I = 2, MAX_FINANCIAL_SIMULATION_YEARS
            STORED_AFUDC_BORROWED_RATE(I-1) = R_BORROWED_AFUDC_RATE(I)
            STORED_AI_AFUDC_RATE(I-1) = R_AFUDC_AI_RATE(I)
            STORED_NF_AFUDC_RATE(I-1) = R_AFUDC_NF_RATE(I)
            STORED_FA_AFUDC_RATE(I-1) = R_AFUDC_FA_RATE(I)
            STORED_INTEREST_CAP_RATE(I-1)=R_CURRENT_INTEREST_CAP_RATE(I)
         ENDDO
         STORE_AI_AFUDC_RATES = .TRUE.
      RETURN

      ENTRY AI_AFUDC_RATE
         AI_AFUDC_RATE = STORED_AI_AFUDC_RATE(globecom_year)
      RETURN
      ENTRY AFUDC_BORROWED_RATE
         AFUDC_BORROWED_RATE = STORED_AFUDC_BORROWED_RATE(globecom_year)
      RETURN
      ENTRY FA_AFUDC_RATE
         FA_AFUDC_RATE = STORED_FA_AFUDC_RATE(globecom_year)
      RETURN
      ENTRY NF_AFUDC_RATE
         NF_AFUDC_RATE = STORED_NF_AFUDC_RATE(globecom_year)
      RETURN
      ENTRY INTEREST_CAP_RATE
         INTEREST_CAP_RATE = STORED_INTEREST_CAP_RATE(globecom_year)
      RETURN
      END


      FUNCTION MACRS_TABLE(ADR_TAX_LIFE,DB_RATE,TAX_LIFE)


         REAL (kind=4) ::  ADR_TAX_LIFE,DB_RATE,TAX_LIFE,MACRS_TABLE

         DB_RATE = 1.5
         IF(ADR_TAX_LIFE <= 20.) DB_RATE = 2.
         TAX_LIFE = 20.
         IF(ADR_TAX_LIFE <= 4.) TAX_LIFE = 3
         IF(ADR_TAX_LIFE <= 10.) TAX_LIFE = 5
         IF(ADR_TAX_LIFE <= 16.) TAX_LIFE = 7
         IF(ADR_TAX_LIFE <= 20.) TAX_LIFE = 10
         IF(ADR_TAX_LIFE <= 25.) TAX_LIFE = 15
         MACRS_TABLE = .5
         RETURN
      END
