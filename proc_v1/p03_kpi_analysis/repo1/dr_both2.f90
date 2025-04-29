!     DR_BOTH2.FOR
!     Copyright(c)  2000

!     Created: 11/15/2006 2:03:00 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 2:56:36 PM



!
! ROUTINE: CONVOL
!
! PURPOSE:
!    TO CONVOLVE THE EQUIVALENT LOAD DURATION CURVE
!
!  INPUT VARIABLES
!    CAPON    THE AMOUNT OF CAPACITY TO BE CONVOLVED
!    P        THE AVAILABILITY OF THE UNIT
!    CURRENT  THE POINT SHOWING THE POSITION OF THE INCOMING LPROB
!    DELTA    THE CALCULATION GRID SPACING
!    LPROB    THE LPROBS
!
!  OUTPUT VARIABLES
!    LPROB    THE NEW LPROB
!  INTERNAL VARIABLES
!    ITEMP    TEMPORARY INTEGER STORAGE LOCATION
!    SHIFT    THE NUMBER OF INDEX POSITIONS THE LPROB IS SHIFTED
!    INDEX    THE LAST GRID POSITION FOR CALCULATION
!    Q        THE UNAVAILABILITY OF THE UNIT
!  ALGORITHM
!    SAME AS PCS MODEL.  BASED ON THE BOOTH - BALERIAUX METHOD
!    SPECIALIZED FOR MIDAS
!
!  AUTHOR:
!    M. S. GERBER & ASSOCIATES, INC.
!    COPYRIGHT (C) 1979-1987   ALL RIGHTS RESERVED

!
      SUBROUTINE CONVOL(CAPON,EA,CURRENT,LIMR,LPROB,LODDUR,LAST_POINT)
      use production
      implicit none
      INTEGER (kind=2) ::  BOX,CURRENT,NEXT,I,LAST_POINT
      REAL ::  EA
      REAL ::  Q
      REAL ::  CAPON
      REAL ::  XTEMP
      REAL ::  YTEMP
      REAL ::  LPROB(1000,2)
      REAL ::  LODDUR(1000)
      REAL ::  SLOPE
      REAL, parameter ::  NEAR_ZERO=10000000 ! 10^10
      REAL ::  LIMR
      LOGICAL (kind=1) ::  BOTH_NOT_ZERO

      IF(EA .GT. 0.999 .OR. EA .LT. .001) RETURN
      Q = 1. - EA
!
      NEXT = MOD(CURRENT,INT(2,2)) + 1
      LPROB(1,NEXT) = 1.
      BOX = 1
      I = 2
      BOTH_NOT_ZERO = .TRUE.
      DO WHILE (BOTH_NOT_ZERO)
         XTEMP = LODDUR(I) - CAPON
         IF(XTEMP .LE. LODDUR(1)) THEN
            LPROB(I,NEXT) = EA * LPROB(I,CURRENT) + Q
         ELSE
            DO WHILE (.NOT. (LODDUR(BOX) .LE. XTEMP .AND. &
                                              LODDUR(BOX+1) .GT. XTEMP))
               IF(LODDUR(BOX) .GT. XTEMP) THEN
                  BOX = BOX - 1
               ELSE
                  BOX = BOX + 1
               ENDIF
            ENDDO
            IF(LODDUR(BOX) .EQ. XTEMP) THEN
               YTEMP = LPROB(BOX,CURRENT)
            ELSE
               SLOPE = (XTEMP-LODDUR(BOX))
               YTEMP = (LODDUR(BOX+1)-LODDUR(BOX))
               SLOPE = SLOPE/YTEMP
               YTEMP = LPROB(BOX,CURRENT)*(1. - SLOPE) + &
                       LPROB(BOX+1,CURRENT) * SLOPE
            ENDIF
            IF(YTEMP .LT. NEAR_ZERO) YTEMP = 0.
!
            LPROB(I,NEXT) = EA * LPROB(I,CURRENT) + Q * YTEMP
         ENDIF
         BOTH_NOT_ZERO = (LPROB(I,NEXT) .NE. 0.) .AND. &
                            (I + 1 .LE. LAST_POINT) .AND. &
                                 (LODDUR(I) .LT. LIMR)
         I = I + 1
      ENDDO
      CURRENT = NEXT
      RETURN
      END

!
!            RETURNS THE LOLP (Y-AXIS) VALUE FOR A LOAD VALUE
!                   COPYRIGHT (C) 1996
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED
!

!
      FUNCTION ELDC_Y_AXIS_VALUE(LOAD_POINT,LPROB,LODDUR,BOX)
!
      REAL ::  LOAD_POINT,LPROB(*),LODDUR(*),ELDC_Y_AXIS_VALUE
      REAL ::  SLOPE,LOLP_VALUE,DELTA_X
      INTEGER (kind=2) ::  BOX
!
         IF(LOAD_POINT <= LODDUR(1)) THEN
            LOLP_VALUE = 1.
         ELSE
            DO WHILE (.NOT. (LODDUR(BOX) <= LOAD_POINT .AND. &
                                            LODDUR(BOX+1) > LOAD_POINT))
               IF(LODDUR(BOX) > LOAD_POINT) THEN
                  BOX = BOX - 1
               ELSE
                  BOX = BOX + 1
               ENDIF
            ENDDO
            IF(LODDUR(BOX) == LOAD_POINT) THEN
               LOLP_VALUE = LPROB(BOX)
            ELSE
               SLOPE = (LOAD_POINT-LODDUR(BOX))
               DELTA_X = (LODDUR(BOX+1)-LODDUR(BOX))
               SLOPE = SLOPE/DELTA_X
               LOLP_VALUE = LPROB(BOX)*(1.-SLOPE) + LPROB(BOX+1) * SLOPE
            ENDIF
         ENDIF
         ELDC_Y_AXIS_VALUE = LOLP_VALUE
      RETURN
      END

!
!            CALCULATES CAPCITY DEPENDENT LOLP
!                   COPYRIGHT (C) 1996
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED
!

!
      SUBROUTINE LOLP_ANALYSIS(CAPACITY,LPROB,LODDUR)
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      use endpoint
      implicit none

      REAL ::  CAPACITY,LPROB(*),LODDUR(*),ELDC_Y_AXIS_VALUE
      INTEGER (kind=2) ::  LOLP_POS=-1,BOX
      SAVE BOX
      REAL ::  LOLP(1000),X_AXIS(1000),LOLP_VALUE
      SAVE LOLP,X_AXIS
      LOGICAL (kind=1) ::  WRITE_LOLP_CURVE_NOT_OPEN=.TRUE.
      INTEGER (kind=2) ::  I,ISEAS,EP_loc,YEAR,START_CURVE
      INTEGER (kind=2) ::  LOLP_CURVE_NO,LOLP_CURVE_HEADER,PEAK_MONTH
      SAVE LOLP_CURVE_NO
      REAL ::  HOURS
      REAL ::  MONTHLY_LOLP(12),ANNUAL_LOLP=0.
      SAVE MONTHLY_LOLP
      LOGICAL (kind=1) ::  LOLP_MONTHLY_REPORT_NOT_OPEN=.TRUE.
      INTEGER (kind=2) ::  LOLP_MONTHLY_REPORT_NO,LOLP_MONTHLY_HEADER
      INTEGER ::  LOLP_MONTHLY_REPORT_REC
      SAVE LOLP_MONTHLY_REPORT_NO,LOLP_MONTHLY_REPORT_REC
      REAL ::  NEAR_ZERO
      PARAMETER (NEAR_ZERO = 10.**(-10))
!
         IF(LOLP_POS == -1) THEN
            LOLP = 0.
            X_AXIS = 0.
            LOLP_POS = 1
            BOX = 1
         ENDIF
         IF(CAPACITY <= LODDUR(1)) THEN
            LOLP(1) = 1.
            X_AXIS(1) = CAPACITY
         ELSE
            LOLP_VALUE = ELDC_Y_AXIS_VALUE(CAPACITY,LPROB,LODDUR,BOX)
            IF(LOLP_VALUE < NEAR_ZERO) LOLP_VALUE = 0.
            LOLP_POS = LOLP_POS + 1
            LOLP(LOLP_POS) = LOLP_VALUE
            X_AXIS(LOLP_POS) = CAPACITY
         ENDIF
      RETURN
!

      ENTRY CALCULATE_MONTHLY_LOLP(ISEAS,HOURS,CAPACITY,LPROB,LODDUR)

!
         IF(CAPACITY <= LODDUR(1)) THEN
            MONTHLY_LOLP(ISEAS) = 1.
         ELSE
            BOX = 1
            MONTHLY_LOLP(ISEAS) = &
                            ELDC_Y_AXIS_VALUE(CAPACITY,LPROB,LODDUR,BOX)
            IF(MONTHLY_LOLP(ISEAS) < 0.) MONTHLY_LOLP(ISEAS) = 0.
         ENDIF
         ANNUAL_LOLP = ANNUAL_LOLP + MONTHLY_LOLP(ISEAS) * HOURS
      RETURN

      ENTRY LOLP_ANALYSIS_REPORT(ISEAS,EP_loc,PEAK_MONTH,YEAR)

!
         IF(ISEAS == PEAK_MONTH) THEN
            IF(WRITE_LOLP_CURVE_NOT_OPEN) THEN
               LOLP_CURVE_NO = LOLP_CURVE_HEADER()
               WRITE_LOLP_CURVE_NOT_OPEN = .FALSE.
            ENDIF
            START_CURVE = MAX(1,LOLP_POS-40)
            WRITE(LOLP_CURVE_NO,1010) EP_loc,YEAR,ISEAS, &
                                      (X_AXIS(I),I=START_CURVE,LOLP_POS)
            WRITE(LOLP_CURVE_NO,1000) EP_loc,YEAR,ISEAS, &
                                        (LOLP(I),I=START_CURVE,LOLP_POS)
!
! CHANGE IN LOLP WITH REDUCED CAPACITY
!
            WRITE(LOLP_CURVE_NO,1010) EP_loc,YEAR,ISEAS, &
                ((X_AXIS(LOLP_POS)-X_AXIS(I)),I=LOLP_POS,START_CURVE,-1)
            WRITE(LOLP_CURVE_NO,1000) EP_loc,YEAR,ISEAS, &
                                     (LOLP(I),I=LOLP_POS,START_CURVE,-1)
         ENDIF
!
         LOLP_POS = -1

      RETURN
!

      ENTRY LOLP_MONTHLY_REPORT(EP_loc,YEAR)

!
         IF(LOLP_MONTHLY_REPORT_NOT_OPEN) THEN
            LOLP_MONTHLY_REPORT_NO = LOLP_MONTHLY_HEADER( &
                                                LOLP_MONTHLY_REPORT_REC)
            LOLP_MONTHLY_REPORT_NOT_OPEN = .FALSE.
         ENDIF
         WRITE(LOLP_MONTHLY_REPORT_NO,REC=LOLP_MONTHLY_REPORT_REC) &
                                          PRT_ENDPOINT(),FLOAT(YEAR), &
                                          MONTHLY_LOLP,ANNUAL_LOLP/8760.
         LOLP_MONTHLY_REPORT_REC = LOLP_MONTHLY_REPORT_REC + 1
         ANNUAL_LOLP = 0.
      RETURN
!
 1000 FORMAT(1X,2(I4,','),I2,',',1000(F8.6,','))
 1010 FORMAT(1X,2(I4,','),I2,',',1000(F8.1,','))
!
      END

!
!   RE-ORDERS THE UNITS UNDER BASE LOAD FOR PROPER CONVOLUTION
!                   COPYRIGHT (C) 1989-91
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED
!

!
      SUBROUTINE COLAPSE(BASE,MAINTENANCE_RATE)
!
      use SpinDriftLib
      use prod_arrays_dimensions
      use mwunih
      USE SIZECOM
      use prodcom


      INTEGER (kind=2) ::  I,UNITNO
      REAL ::  BASE,CAPBLK,CHKVAL
      REAL ::  MAINTENANCE_RATE(MAX_CL_UNITS)
!
      CHKVAL = MWBLOK(1) * (1. - MAINTENANCE_RATE(UNIT(1)))
      DO I = 2, NBLOCK
         IF(CHKVAL .GT. BASE) EXIT
         UNITNO = UNIT(I)
         CAPBLK = MWBLOK(I) * (1. - MAINTENANCE_RATE(UNITNO))
         CHKVAL = CHKVAL + CAPBLK
         IF(BLKNO(I).EQ.2 .AND. UNIT(I-1) .NE. UNITNO) CALL MOVE_UNIT(I)
      ENDDO
      RETURN
      END


      SUBROUTINE MOVE_UNIT(I)
      use SpinDriftLib
      use prod_arrays_dimensions
      use mwunih
      USE SIZECOM
      use prodcom


      INTEGER (kind=2) ::  I,J,L
      INTEGER (kind=2) ::  ITEMPUN,ITEMPBK
      REAL ::  RTEMPIHR,RTEMPMW
      DO J = 1, I-2
         IF(UNIT(J) .EQ. UNIT(I)) THEN
            RTEMPMW = MWBLOK(J)
            ITEMPUN = UNIT(J)
            ITEMPBK = BLKNO(J)
            RTEMPIHR = INHEAT(J)
            DO L = J, I-2
               MWBLOK(L) = MWBLOK(L+1)
               UNIT(L) = UNIT(L+1)
               BLKNO(L) = BLKNO(L+1)
               INHEAT(L) = INHEAT(L+1)
            ENDDO
            MWBLOK(I-1) = RTEMPMW
            UNIT(I-1) = ITEMPUN
            BLKNO(I-1) = ITEMPBK
            INHEAT(I-1) = RTEMPIHR
            RETURN
         ENDIF
      ENDDO
      RETURN
      END

!                                                                      C
!     A SUBROUTINE TO INITIALIZE CONTRACTS                             C
!                                                                      C

      SUBROUTINE INITIALIZE_CONTRACTS_WKP(LPROB,LODDUR,HOURS,DX, &
                           A,B,REMAINING_ENERGY,ISEAS, &
                           CONTRACTS_ACTIVE, &
                           CONTRACT_DISPATCH_COST, &
                           CONTRACT_ENERGY,CONTRACT_CAPACITY, &
                           date_1,date_2,ISTART,LAST_POINT, &
                           AVAILABLE_CONTRACT,AVAILABLE_CONTRACT_NO, &
                           MAXIMUM_ENERGY,MAXIMUM_CAPACITY, &
                           CUM_CONTRACT_CAP,SEASONAL_CONTRACT_CAPACITY, &
                           MINIMUM_ENERGY,MINIMUM_CAPACITY, &
                           PEAK_MONTH,YR, &
                           ENRG_FROM_ENRG_ONLY_CONTRACTS, &
                           REMAINING_ENRG_ONLY_ENRG, &
                           MIN_ENRG_FROM_ENRG_ONLY_ENRG, &
                           MAX_RATCHET_CAPACITY,REMAIN_ANN_ENRG, &
                           MIN_RATCHET_CAPACITY,MAXIMUM_ANNUAL_CAPACITY, &
                           TOTAL_MIN_RATCHET_ENERGY,CNTR_ENRG, &
                           MIN_BC_FIXED_COST,BC_BLK1_COST,BCM_NO, &
                           PERIOD_COUNTER,TOTAL_CONTRACT_ENRG, &
                           CONTRACTS_IN_PERIOD)
      use end_routine, only: end_program, er_message
	  use production
      use enrglimit
! DECLARE VARIABLES
!
! VALUES USED IN OTHER INITIAL_CONTRACTS ROUTINE PLACED HERE TO MAKE THE
!   CALLING ARGUMENTS THE SAME.

      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      use contracts_data

!
      INTEGER (kind=2) ::  CONTRACT
      INTEGER (kind=2) ::  date_1
      INTEGER (kind=2) ::  date_2
      INTEGER (kind=2) ::  ISTART
      INTEGER (kind=2) ::  LAST_POINT
      INTEGER (kind=2) ::  AVAILABLE_CONTRACT_NO
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  PEAK_MONTH
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  BCM_NO
      INTEGER (kind=2) ::  LOWEST_COST_CONTRACT
      INTEGER (kind=2) ::  PERIOD_COUNTER
      INTEGER (kind=2) ::  LAST_SURPLUS_CONTRACT
      INTEGER (kind=2) ::  NOM_YEARS_3808=0
!
      REAL ::  PRODUCTION_COST_PERIODS
      REAL ::  REMAINING_ENRG_ONLY_ENRG
      REAL ::  ENRG_FROM_ENRG_ONLY_CONTRACTS
      REAL ::  MIN_ENRG_FROM_ENRG_ONLY_ENRG
      REAL ::  HOURS
      REAL ::  ENRG_FROM_SURPLUS_CONTRACTS
      REAL ::  MIN_ENRG_FROM_SURPLUS_ENRG
      REAL ::  REMAINING_SURPLUS_ENRG
      REAL ::  SURPLUS_ENRG_USED
      REAL ::  A
      REAL ::  B
      REAL ::  REMAINING_ENERGY
      REAL ::  LPROB(LOAD_CURVE_POINTS)
      REAL ::  LODDUR(LOAD_CURVE_POINTS)
      REAL ::  DX
      REAL ::  LEFT
      REAL ::  RIGHT
      REAL ::  TEMP_CONTRACT_COST
      REAL ::  CURRENT_CONTRACT_ENERGY
      REAL ::  CONTRACT_ENERGY(MAX_CONTRACTS)
      REAL ::  CONTRACT_CAPACITY(MAX_CONTRACTS)
      REAL ::  CONTRACT_DISPATCH_COST
      REAL ::  MAXIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  GET_VAR
      REAL ::  CURRENT_CONTRACT_CAPACITY
      REAL ::  SEASONAL_CONTRACT_CAPACITY
      REAL ::  CUM_CONTRACT_CAP
      REAL ::  MINIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  MINIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  RATCHET_PERCENT
      REAL ::  ENRG_ONLY_ENRG_USED
      REAL ::  MAX_RATCHET_CAPACITY(MAX_CONTRACTS)
      REAL ::  REMAIN_ANN_ENRG(MAX_CONTRACTS)
      REAL ::  MIN_RATCHET_CAPACITY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_ANNUAL_CAPACITY(MAX_CONTRACTS)
      REAL ::  TOTAL_MIN_RATCHET_ENERGY
      REAL ::  CNTR_ENRG
      REAL ::  MIN_BC_FIXED_COST
      REAL ::  BC_BLK1_COST(3)
      REAL ::  SURPLUS_ENERGY
      REAL ::  MAX_MUST_ENERGY
      REAL ::  TOTAL_CONTRACT_ENRG
      REAL ::  CAP_3808=0.
      REAL ::  SAVE_CAP_3808
      REAL ::  INIT_CAP_3808
      REAL ::  Y_CAPACITY=0.
      REAL ::  TEMP_Y_CAPACITY
      REAL ::  CONTRACT_CAPACITY_3808(*)
      REAL ::  ENERGY_REMOVED_FROM_LDC
      REAL ::  TOTAL_SURPLUS_ENERGY
      REAL ::  TOTAL_SURPLUS_CAPACITY
      LOGICAL (kind=1) ::  AVAILABLE_CONTRACT(MAX_CONTRACTS)
      LOGICAL (kind=1) ::  CONTRACTS_ACTIVE
      LOGICAL (kind=1) ::  CONTRACTS_IN_PERIOD
      LOGICAL (kind=1) ::  ACTIVE_3808
      LOGICAL (kind=1) ::  SAVE_ACTIVE_3808
      LOGICAL (kind=1) ::  VOID_LOGICAL
!
      INTEGER (kind=2) ::  X_CONTRACT=0
      INTEGER (kind=2) ::  Y_CONTRACT=0
      INTEGER (kind=2) ::  Z_CONTRACT=0
      INTEGER (kind=2) ::  F_CONTRACT=0
      INTEGER (kind=2) ::  G_CONTRACT=0
      INTEGER (kind=2) ::  H_CONTRACT=0
      INTEGER (kind=2) ::  J_CONTRACT=0
      INTEGER (kind=2) ::  MONTHS_SINCE_Y_UPDATED=0
      INTEGER (kind=2) ::  R_PEAK_MONTH
      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  R_X_CONTRACT
      INTEGER (kind=2) ::  R_Y_CONTRACT
      INTEGER (kind=2) ::  R_Z_CONTRACT
      INTEGER (kind=2) ::  COUNT
!
!
! END DATA DECLARATIONS
!
!
! INITIALIZE VARIABLES
!
      PRODUCTION_COST_PERIODS = get_PRODUCTION_PERIODS_in()
      IF(YR .EQ. 1 .AND. PERIOD_COUNTER .EQ. 1) THEN
         TOTAL_MIN_RATCHET_ENERGY = 0.
         CNTR_ENRG = 0.
         MIN_BC_FIXED_COST = 0.
         BC_BLK1_COST(1) = 0.
         BC_BLK1_COST(2) = 0.
         BC_BLK1_COST(3) = 0.
         BCM_NO = 0.
         ACTIVE_3808 = SAVE_ACTIVE_3808(.FALSE.)
         DO CONTRACT = 1 , NUMBER_OF_CONTRACTS
            IF(MAX_RATCHET_PATTERN(CONTRACT) /= 0) THEN
               MAX_RATCHET_CAPACITY(CONTRACT) = MAX(0., &
                             GET_VAR(MAX_RATCHET_PATTERN(CONTRACT), &
                                                   13,CNTRNM(CONTRACT)))
            ELSE
               MAX_RATCHET_CAPACITY(CONTRACT) = 0.
            ENDIF

         ENDDO
      ENDIF

      AVAILABLE_CONTRACT_NO = 0
      SEASONAL_CONTRACT_CAPACITY = 0.
      LOWEST_COST_CONTRACT = 0
      SURPLUS_ENERGY = 0.
!
      ENRG_FROM_SURPLUS_CONTRACTS = 0.
      MIN_ENRG_FROM_SURPLUS_ENRG = 0.
      LAST_SURPLUS_CONTRACT = 0
!
      F_CONTRACT = 0
      G_CONTRACT = 0
      H_CONTRACT = 0
      J_CONTRACT = 0
!
      X_CONTRACT = 0
      Y_CONTRACT = 0
      Z_CONTRACT = 0
!
      TOTAL_SURPLUS_ENERGY = 0.
      TOTAL_SURPLUS_CAPACITY = 0.
!
      DO CONTRACT = 1 , NUMBER_OF_CONTRACTS
         CONTRACT_ENERGY(CONTRACT) = 0.
         CONTRACT_CAPACITY(CONTRACT) = 0.
         MAXIMUM_CAPACITY(CONTRACT) = 0.
         MINIMUM_CAPACITY(CONTRACT) = 0.
         IF (CNTR_ON_LI(CONTRACT) .LE. date_2 .AND. &
                                  CNTR_OFF_LI(CONTRACT) .GE. date_1) THEN
            IF(CNTRTYPE(CONTRACT) == 'F') THEN
               F_CONTRACT = CONTRACT
               CNTR_ENERGY_SWITCH(CONTRACT) = 'M'
               CNTR_CAPACITY_SWITCH(CONTRACT) = 'M'
!               CYCLE
            ELSEIF(CNTRTYPE(CONTRACT) == 'G') THEN
               G_CONTRACT = CONTRACT
               CNTR_ENERGY_SWITCH(CONTRACT) = 'M'
               CNTR_CAPACITY_SWITCH(CONTRACT) = 'M'
!               CYCLE
            ELSEIF(CNTRTYPE(CONTRACT) == 'H') THEN
               H_CONTRACT = CONTRACT
               CNTR_ENERGY_SWITCH(CONTRACT) = 'M'
               CNTR_CAPACITY_SWITCH(CONTRACT) = 'M'
!               CYCLE
            ELSEIF(CNTRTYPE(CONTRACT) == 'J') THEN
               J_CONTRACT = CONTRACT
               CNTR_ENERGY_SWITCH(CONTRACT) = 'M'
               CNTR_CAPACITY_SWITCH(CONTRACT) = 'M'
!               CYCLE
            ELSEIF(CNTRTYPE(CONTRACT) == 'X') THEN
               X_CONTRACT = CONTRACT
               CNTR_ENERGY_SWITCH(CONTRACT) = 'M'
               CNTR_CAPACITY_SWITCH(CONTRACT) = 'M'
               NOM_YEARS_3808 = CNTR_OFF_LI(CONTRACT) - &
                                     100*INT(CNTR_OFF_LI(CONTRACT)/100.)
               CYCLE
            ELSEIF(CNTRTYPE(CONTRACT) == 'Y') THEN
               Y_CONTRACT = CONTRACT
               CNTR_ENERGY_SWITCH(CONTRACT) = 'M'
               CNTR_CAPACITY_SWITCH(CONTRACT) = 'M'
               IF(MIN_RATCHET_PATTERN(CONTRACT) == 0) THEN
                  WRITE(4,*) "EXCESS RATCHET "// &
                                         "MUST HAVE MIN RATCHET PATTERN"
                  WRITE(4,*) '*** line 469 DR_BOTH2.FOR ***'
                  er_message='See WARNING MESSAGES-dr_both2-3'
                  call end_program(er_message)
               ENDIF
               CYCLE
            ELSEIF(CNTRTYPE(CONTRACT) == 'Z') THEN
               Z_CONTRACT = CONTRACT
               CNTR_ENERGY_SWITCH(CONTRACT) = 'M'
               CNTR_CAPACITY_SWITCH(CONTRACT) = 'V'
            ENDIF
            IF(MAX_CAPACITY(CONTRACT) .LT. 0.) THEN
               MAXIMUM_CAPACITY(CONTRACT) = MIN( &
                  MAXIMUM_ANNUAL_CAPACITY(CONTRACT), &
                  GET_VAR(MAX_CAPACITY(CONTRACT), &
                  ISEAS,CNTRNM(CONTRACT)))*CNTR_OWN(CONTRACT)
            ELSE
               MAXIMUM_CAPACITY(CONTRACT) = MIN(MAX_CAPACITY(CONTRACT), &
                  MAXIMUM_ANNUAL_CAPACITY(CONTRACT))*CNTR_OWN(CONTRACT)
            ENDIF
            IF(MAX_ENERGY(CONTRACT) .LT. 0.) THEN
               MAXIMUM_ENERGY(CONTRACT) = MIN(REMAIN_ANN_ENRG(CONTRACT), &
                  GET_VAR(MAX_ENERGY(CONTRACT),ISEAS,CNTRNM(CONTRACT))) &
                                                    *CNTR_OWN(CONTRACT)
            ELSE
               MAXIMUM_ENERGY(CONTRACT) = MIN(REMAIN_ANN_ENRG(CONTRACT), &
                        MAX_ENERGY(CONTRACT))  *CNTR_OWN(CONTRACT)
            ENDIF
            IF(MIN_CAPACITY(CONTRACT) .LT. 0.) THEN
               MINIMUM_CAPACITY(CONTRACT) = CNTR_OWN(CONTRACT) * &
                  GET_VAR(MIN_CAPACITY(CONTRACT),ISEAS,CNTRNM(CONTRACT))
            ELSE
               MINIMUM_CAPACITY(CONTRACT) = CNTR_OWN(CONTRACT)* &
                                                  MIN_CAPACITY(CONTRACT)
            ENDIF
            IF(MIN_ENERGY(CONTRACT) .LT. 0.) THEN
               MINIMUM_ENERGY(CONTRACT) = CNTR_OWN(CONTRACT) * &
                  GET_VAR(MIN_ENERGY(CONTRACT),ISEAS,CNTRNM(CONTRACT))
            ELSE
               MINIMUM_ENERGY(CONTRACT) = MIN_ENERGY(CONTRACT) &
                                                    *CNTR_OWN(CONTRACT)
            ENDIF
!
! LOAD FACTOR TO ENERGY
!
            IF(MINIMUM_ENERGY(CONTRACT) .LT. 2.) THEN
               MINIMUM_ENERGY(CONTRACT) = MINIMUM_ENERGY(CONTRACT) * &
                                      HOURS * MAXIMUM_CAPACITY(CONTRACT)
            ENDIF
            IF(MAXIMUM_ENERGY(CONTRACT) .LT. 2.) THEN
               MAXIMUM_ENERGY(CONTRACT) = MAXIMUM_ENERGY(CONTRACT) * &
                                      HOURS * MAXIMUM_CAPACITY(CONTRACT)
            ENDIF
!
! MAX RATCHET CONTRACT LOGIC
!
            IF(MAX_RATCHET_PATTERN(CONTRACT) .NE. 0) THEN
               RATCHET_PERCENT = GET_VAR(MAX_RATCHET_PATTERN(CONTRACT), &
                                               ISEAS,CNTRNM(CONTRACT))
               IF(RATCHET_PERCENT .LT. 0. .OR. &
                                          RATCHET_PERCENT .GT. 1.) THEN
                  WRITE(4,*) "INVALID RATCHET MULTIPLIER FOR CONTRACT "
                  WRITE(4,*) CNTRNM(CONTRACT)," VALUES RANGE: 0.<x<1."
                  WRITE(4,*) " "
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "V"
               ELSEIF( ISEAS .NE. PEAK_MONTH ) THEN
                  MAXIMUM_CAPACITY(CONTRACT) = &
                     MAX_RATCHET_CAPACITY(CONTRACT) * RATCHET_PERCENT
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "M"
               ELSE
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "V"
               ENDIF
            ENDIF

! MIN RATCHET CONTRACT LOGIC
!
            IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. "E") THEN
               IF(MAXIMUM_ENERGY(CONTRACT)  > 0.) THEN
                  ENRG_FROM_ENRG_ONLY_CONTRACTS = &
                     MAXIMUM_ENERGY(CONTRACT)+ &
                                           ENRG_FROM_ENRG_ONLY_CONTRACTS
                  MIN_ENRG_FROM_ENRG_ONLY_ENRG = &
                         MINIMUM_ENERGY(CONTRACT) + &
                                            MIN_ENRG_FROM_ENRG_ONLY_ENRG
                  AVAILABLE_CONTRACT(CONTRACT) = .TRUE.
               ELSE
                  AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               ENDIF
            ELSEIF(  CNTRTYPE(CONTRACT) == 'F'.OR. &
                     CNTRTYPE(CONTRACT) == 'G'.OR. &
                     CNTRTYPE(CONTRACT) == 'H'.OR. &
                     CNTRTYPE(CONTRACT) == 'J') THEN

               AVAILABLE_CONTRACT(CONTRACT) = .TRUE.
               AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO + 1
            ELSEIF(CNTR_ENERGY_SWITCH(CONTRACT) .EQ. "S" .AND. &
                        MINIMUM_ENERGY(CONTRACT) > 0. .AND. &
                        MINIMUM_CAPACITY(CONTRACT) > 0. .AND. &
                        MAXIMUM_ENERGY(CONTRACT) > 0. .AND. &
                        MAXIMUM_CAPACITY(CONTRACT) > 0.) THEN
               ENRG_FROM_SURPLUS_CONTRACTS = MAXIMUM_ENERGY(CONTRACT) + &
                                             ENRG_FROM_SURPLUS_CONTRACTS
               IF(MINIMUM_ENERGY(CONTRACT) == 0.) THEN
                  WRITE(4,*) "SURPLUS UNIT",CNTRNM(CONTRACT)
                  WRITE(4,*) "DOES NOT HAVE A MINIMUM ENERGY ASSIGNED"
                  WRITE(4,*) "SURPLUS UNITS MUST HAVE MINIMUM ENERGY"
               ENDIF
               MIN_ENRG_FROM_SURPLUS_ENRG = MINIMUM_ENERGY(CONTRACT) + &
                                              MIN_ENRG_FROM_SURPLUS_ENRG
               AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO + 1
               AVAILABLE_CONTRACT(CONTRACT) = .TRUE.
               LAST_SURPLUS_CONTRACT = CONTRACT
! ADDED 5/13/92
            ELSEIF( CNTR_ENERGY_SWITCH(CONTRACT) == "O") THEN
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               MAXIMUM_CAPACITY(CONTRACT) = 0.
               MINIMUM_CAPACITY(CONTRACT) = 0.
            ELSE
               IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. "V" .AND. &
                                 CONTRACT_FIXED_COST(CONTRACT) .EQ. 0.) &
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "M"
               IF(MINIMUM_CAPACITY(CONTRACT) .LE. &
                                 MAXIMUM_CAPACITY(CONTRACT)        .AND. &
                  MINIMUM_ENERGY(CONTRACT) .LE. &
                                  MAXIMUM_ENERGY(CONTRACT)         .AND. &
                  MAXIMUM_ENERGY(CONTRACT) .GT. 0.                 .AND. &
                             MAXIMUM_CAPACITY(CONTRACT) .GT. 0.)    THEN
                  AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO + 1
                  AVAILABLE_CONTRACT(CONTRACT) = .TRUE.
                  SEASONAL_CONTRACT_CAPACITY=SEASONAL_CONTRACT_CAPACITY+ &
                                              MAXIMUM_CAPACITY(CONTRACT)
               ELSE
                  IF(MINIMUM_CAPACITY(CONTRACT) .GT. &
                                        MAXIMUM_CAPACITY(CONTRACT)) THEN
                     MINIMUM_CAPACITY(CONTRACT) = &
                                        MAXIMUM_CAPACITY(CONTRACT)
                  ENDIF
                  IF(MINIMUM_ENERGY(CONTRACT) .GT. &
                                          MAXIMUM_ENERGY(CONTRACT)) THEN
                     WRITE(4,*) "MINIMUM CONTRACT ENERGY > "// &
                           "MAXIMUM FOR CONTRACT ",CNTRNM(CONTRACT)
                     WRITE(4,*) MINIMUM_ENERGY(CONTRACT), &
                                        MAXIMUM_ENERGY(CONTRACT)
                  ENDIF
                  AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               ENDIF
            ENDIF
            IF(CNTR_ENERGY_SWITCH(CONTRACT) == 'S' .AND. &
                        MINIMUM_ENERGY(CONTRACT) > 0. .AND. &
                        MINIMUM_CAPACITY(CONTRACT) > 0. .AND. &
                        MAXIMUM_ENERGY(CONTRACT) > 0. .AND. &
                        MAXIMUM_CAPACITY(CONTRACT) > 0.) THEN
               TOTAL_SURPLUS_CAPACITY = TOTAL_SURPLUS_CAPACITY + &
                                              MINIMUM_CAPACITY(CONTRACT)
               TOTAL_SURPLUS_ENERGY = TOTAL_SURPLUS_ENERGY + &
                                          MINIMUM_ENERGY(CONTRACT)/HOURS
!
               CONTRACT_CAPACITY(CONTRACT) = MINIMUM_CAPACITY(CONTRACT)
               CONTRACT_ENERGY(CONTRACT) = &
                                          MINIMUM_ENERGY(CONTRACT)/HOURS
!
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO - 1
!
            ENDIF
         ELSE
            AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
         ENDIF ! CONTRACT IS ON-LINE.
      ENDDO ! CONTRACTS TO ESTABLISH MIN'S AND MAX'S
      IF(X_CONTRACT > 0 .AND. Y_CONTRACT > 0 .AND. Z_CONTRACT > 0)THEN
         ACTIVE_3808 = SAVE_ACTIVE_3808(.TRUE.)
         AVAILABLE_CONTRACT(X_CONTRACT) = .TRUE.
         AVAILABLE_CONTRACT(Y_CONTRACT) = .TRUE.
         AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO + 2
!
         IF(ISEAS == PEAK_MONTH) THEN
!
            IF(globecom_YEAR > NOM_YEARS_3808) THEN
               IF(Y_CAPACITY > 0. .AND. &
                                       MONTHS_SINCE_Y_UPDATED < 12) THEN
                  MAXIMUM_CAPACITY(X_CONTRACT) = .50 * CAP_3808
                  MAXIMUM_ENERGY(X_CONTRACT) = HOURS * &
                                            MAXIMUM_CAPACITY(X_CONTRACT)
                  IF(Y_CAPACITY + MAXIMUM_CAPACITY(X_CONTRACT) > &
                                      MAXIMUM_CAPACITY(Z_CONTRACT)) THEN
                     WRITE(4,*) "THE MUST AND EXCESS PORTIONS OF 3808"
                     WRITE(4,*) "EXCEED THE AVAILABLE TOTAL CAPACITY."
                     WRITE(4,*) "CHECK THE MAXIMUM CAPACITY OF THE"
                     WRITE(4,*) "DISPATCH CONTRACT."
                     WRITE(4,*) '*** line 682 DR_BOTH2.FOR ***'
                     er_message='See WARNING MESSAGES-dr_both2-2'
                     call end_program(er_message)
                  ENDIF
               ELSE
                  MAXIMUM_CAPACITY(X_CONTRACT) = 0.
                  MAXIMUM_ENERGY(X_CONTRACT) = 0.
               ENDIF
!
               CAP_3808 = MAXIMUM_CAPACITY(Z_CONTRACT)
!
            ELSE
!
               CAP_3808 = MAXIMUM_CAPACITY(Z_CONTRACT)
!
               MAXIMUM_CAPACITY(X_CONTRACT) = .50 * CAP_3808
               MAXIMUM_ENERGY(X_CONTRACT) = HOURS * &
                                            MAXIMUM_CAPACITY(X_CONTRACT)
            ENDIF
!
         ELSE
!
            MAXIMUM_CAPACITY(X_CONTRACT) = .50 * CAP_3808
            MAXIMUM_ENERGY(X_CONTRACT) = HOURS * &
                                            MAXIMUM_CAPACITY(X_CONTRACT)
         ENDIF
!
         IF(Y_CAPACITY > 0. .AND. MONTHS_SINCE_Y_UPDATED < 11) THEN
            MONTHS_SINCE_Y_UPDATED = MONTHS_SINCE_Y_UPDATED + 1
            MAXIMUM_CAPACITY(Y_CONTRACT) = Y_CAPACITY
            MAXIMUM_ENERGY(Y_CONTRACT) = HOURS * &
                                            MAXIMUM_CAPACITY(Y_CONTRACT)
         ELSE
            Y_CAPACITY = 0.
            MONTHS_SINCE_Y_UPDATED = 0
            MAXIMUM_CAPACITY(Y_CONTRACT) = 0.
            MAXIMUM_ENERGY(Y_CONTRACT) = 0.
         ENDIF
!
         MAXIMUM_CAPACITY(Z_CONTRACT) = MAX(0., &
                           CAP_3808 - MAXIMUM_CAPACITY(X_CONTRACT) - &
                                           MAXIMUM_CAPACITY(Y_CONTRACT))
         MAXIMUM_ENERGY(Z_CONTRACT) = HOURS * &
                                            MAXIMUM_CAPACITY(Z_CONTRACT)
!
      ELSEIF(X_CONTRACT + Y_CONTRACT + Z_CONTRACT > 0) THEN
         ACTIVE_3808 = SAVE_ACTIVE_3808(.TRUE.)
         WRITE(4,*) "DID NOT FIND THREE CONTRACT PIECES FOR WKP 3808"
         WRITE(4,*) "CONTRACT IN YEAR ",globecom_YEAR," AND SEASON ",ISEAS
      ELSE
         ACTIVE_3808 = SAVE_ACTIVE_3808(.FALSE.)
      ENDIF
      IF(AVAILABLE_CONTRACT_NO .GT. 0) THEN
         CONTRACTS_ACTIVE = .TRUE.
         CONTRACTS_IN_PERIOD = .TRUE.
      ELSE
         RETURN
      ENDIF
      LEFT = 1.
      RIGHT = 1.

      CONTRACT = 0.
!
      REMAINING_ENRG_ONLY_ENRG = ENRG_FROM_ENRG_ONLY_CONTRACTS/HOURS
      REMAINING_SURPLUS_ENRG = ENRG_FROM_SURPLUS_CONTRACTS/HOURS
      SURPLUS_ENRG_USED = 0.
!
      IF(   F_CONTRACT > 0 .OR. &
            G_CONTRACT > 0 .OR. &
            H_CONTRACT > 0 .OR. &
            J_CONTRACT > 0 ) THEN
         DO COUNT = 1, 4
            IF(COUNT == 1) THEN
               CONTRACT = J_CONTRACT
            ELSEIF(COUNT == 2) THEN
               CONTRACT = G_CONTRACT
            ELSEIF(COUNT == 3) THEN
               CONTRACT = H_CONTRACT
            ELSEIF(COUNT == 4) THEN
               CONTRACT = F_CONTRACT
            ENDIF
            IF(CONTRACT == 0) CYCLE
!
            A = B
            CALL WKP_BFP_PURCHASE(  ISEAS, &
                                    CNTRTYPE(CONTRACT), &
                                    MINIMUM_ENERGY(CONTRACT), &
                                    MINIMUM_CAPACITY(CONTRACT), &
                                    LPROB,LODDUR,HOURS,LAST_POINT, &
                                    CURRENT_CONTRACT_ENERGY, &
                                    CURRENT_CONTRACT_CAPACITY, &
                                    MAXIMUM_CAPACITY(CONTRACT))
!
            AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
            AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO - 1
            CONTRACT_ENERGY(CONTRACT) = CURRENT_CONTRACT_ENERGY
            CONTRACT_CAPACITY(CONTRACT) = CURRENT_CONTRACT_CAPACITY
            ENERGY_REMOVED_FROM_LDC = CURRENT_CONTRACT_ENERGY
!
            REMAINING_ENERGY = REMAINING_ENERGY - &
                                                 ENERGY_REMOVED_FROM_LDC
            TOTAL_CONTRACT_ENRG = TOTAL_CONTRACT_ENRG + &
                                                 CURRENT_CONTRACT_ENERGY
!
         ENDDO
      ENDIF
!
      IF(TOTAL_SURPLUS_ENERGY > 0. .AND. &
                                       TOTAL_SURPLUS_CAPACITY > 0.) THEN
         A = 0.
         B = TOTAL_SURPLUS_CAPACITY
         CALL CALC_CONTRACT_ENERGY(LODDUR,LPROB,DX,A,B, &
                        CURRENT_CONTRACT_ENERGY,LEFT,RIGHT, &
                        MIN(TOTAL_SURPLUS_ENERGY,REMAINING_ENERGY), &
                        ISTART,LAST_POINT, &
                        REMAINING_ENRG_ONLY_ENRG, &
                        ENRG_ONLY_ENRG_USED)
!
         REMAINING_ENERGY = REMAINING_ENERGY - CURRENT_CONTRACT_ENERGY - &
                                                     ENRG_ONLY_ENRG_USED
!
         CUM_CONTRACT_CAP = CUM_CONTRACT_CAP + B - A
         TOTAL_CONTRACT_ENRG = TOTAL_CONTRACT_ENRG + &
                                                    TOTAL_SURPLUS_ENERGY
!
         IF(REMAINING_ENERGY < .0001) THEN
            REMAINING_ENERGY = 0.0
            CONTRACTS_ACTIVE = .FALSE.
         ENDIF
!
      ENDIF
!
      DO WHILE(CONTRACT .LT. NUMBER_OF_CONTRACTS .AND. &
            (CONTRACTS_ACTIVE .OR. CONTRACT .LE. LAST_SURPLUS_CONTRACT))
         CONTRACT = CONTRACT + 1
         ENRG_ONLY_ENRG_USED = 0.

         IF(AVAILABLE_CONTRACT(CONTRACT) .AND. &
                      CNTR_CAPACITY_SWITCH(CONTRACT) .NE. "E") THEN
            CURRENT_CONTRACT_ENERGY = 0.
            CURRENT_CONTRACT_CAPACITY = 0.
            ENERGY_REMOVED_FROM_LDC = 0.

            MAX_MUST_ENERGY = MINIMUM_ENERGY(CONTRACT)/HOURS
! REWRITTEN. 7/31/96. GAT.
! ACCUMULATED PER EGOLF. 9/19/96.
!
            IF(CNTR_ENERGY_SWITCH(CONTRACT) == 'S' .AND. &
                        MINIMUM_ENERGY(CONTRACT) > 0. .AND. &
                        MINIMUM_CAPACITY(CONTRACT) > 0. .AND. &
                        MAXIMUM_ENERGY(CONTRACT) > 0. .AND. &
                        MAXIMUM_CAPACITY(CONTRACT) > 0.) THEN
!
               CYCLE

            ELSEIF(MINIMUM_ENERGY(CONTRACT) .GT. 0. .AND. &
                        MINIMUM_CAPACITY(CONTRACT) .GT. 0 .AND. &
                        MINIMUM_ENERGY(CONTRACT) .LE. &
                        MINIMUM_CAPACITY(CONTRACT)*HOURS) THEN
               A = B
               CALL NEW_PEAK_SHIFT( &
                        LODDUR,LPROB,MINIMUM_CAPACITY(CONTRACT), &
                        MINIMUM_ENERGY(CONTRACT)/HOURS, &
                        LAST_POINT,A,B,CURRENT_CONTRACT_CAPACITY, &
                        CURRENT_CONTRACT_ENERGY,HOURS, &
                        MAX_MUST_ENERGY,CNTRNM(CONTRACT), &
                        ISEAS,DX, &
                        REMAINING_ENRG_ONLY_ENRG, &
                        ENRG_ONLY_ENRG_USED)
               ENERGY_REMOVED_FROM_LDC = CURRENT_CONTRACT_ENERGY &
                                                   - ENRG_ONLY_ENRG_USED
            ELSEIF(MINIMUM_ENERGY(CONTRACT) .GT. 0. .OR. &
                        MINIMUM_CAPACITY(CONTRACT) .GT. 0) THEN
               CURRENT_CONTRACT_CAPACITY = &
                     MAX(MINIMUM_CAPACITY(CONTRACT), &
                     MINIMUM_ENERGY(CONTRACT)/HOURS)
!
!              LOAD TOTAL MINIMUM CAPACITY INTO THE
!              ELDC AND DETERMINE ENERGY.
!
               A = B
!
               IF(MINIMUM_CAPACITY(CONTRACT) .GE. &
                                 MINIMUM_ENERGY(CONTRACT)/HOURS) THEN
                  B = MIN(A + &
                          MINIMUM_CAPACITY(CONTRACT),LODDUR(LAST_POINT))
                  CALL CALC_CONTRACT_ENERGY(LODDUR,LPROB,DX,A,B, &
                        CURRENT_CONTRACT_ENERGY, &
                        LEFT,RIGHT,REMAINING_ENERGY,ISTART,LAST_POINT, &
                        REMAINING_ENRG_ONLY_ENRG, &
                        ENRG_ONLY_ENRG_USED)

               ELSE
                  B = MIN(A+MAXIMUM_CAPACITY(CONTRACT), &
                                                   LODDUR(LAST_POINT))
                  CALL CALC_CONTRACT_ENERGY(LODDUR,LPROB,DX,A,B, &
                        CURRENT_CONTRACT_ENERGY,LEFT,RIGHT, &
                        MIN(MINIMUM_ENERGY(CONTRACT)/HOURS, &
                                                     REMAINING_ENERGY), &
                        ISTART,LAST_POINT, &
                        REMAINING_ENRG_ONLY_ENRG, &
                        ENRG_ONLY_ENRG_USED)



!                 CURRENT_CONTRACT_CAPACITY MAY BE RESET BY
!                           CALC_CONTRACT_ENERGY

                  CURRENT_CONTRACT_CAPACITY = B - A
               ENDIF
               ENERGY_REMOVED_FROM_LDC = CURRENT_CONTRACT_ENERGY &
                                                   - ENRG_ONLY_ENRG_USED
            ENDIF
            IF(MINIMUM_ENERGY(CONTRACT) .GT. 0. .OR. &
                        MINIMUM_CAPACITY(CONTRACT) .GT. 0)THEN

               CUM_CONTRACT_CAP = CUM_CONTRACT_CAP + B - A
               CONTRACT_CAPACITY(CONTRACT) = CURRENT_CONTRACT_CAPACITY
               REMAINING_ENERGY = REMAINING_ENERGY - &
                                                 ENERGY_REMOVED_FROM_LDC

               CONTRACT_ENERGY(CONTRACT) = CURRENT_CONTRACT_ENERGY
               TOTAL_CONTRACT_ENRG = TOTAL_CONTRACT_ENRG + &
                                                 CURRENT_CONTRACT_ENERGY
               IF(CONTRACT_CAPACITY(CONTRACT)*1.000001 .GE. &
                        MAXIMUM_CAPACITY(CONTRACT) .OR. &
                        CONTRACT_ENERGY(CONTRACT)*1.000001 .GE. &
                        MAXIMUM_ENERGY(CONTRACT)/HOURS) THEN
                  AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
                  AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO - 1
               ENDIF
            ENDIF
            IF(REMAINING_ENERGY >= .001) THEN

               IF(RIGHT == 0.0) THEN
                  WRITE(4,*) "INCONSISTENT CALCULATIONS"
                  WRITE(4,*) "BETWEEN CAPACITY CONTRACT "
                  WRITE(4,*) "AND THE LOAD CURVE"
                  WRITE(4,*) '*** line 1026 DR_BOTH2.FOR ***'
                  er_message='See WARNING MESSAGES-dr_both2-1'
                  call end_program(er_message)
               ENDIF
               IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. 'V') THEN
                  TEMP_CONTRACT_COST = &
                           (CONTRACT_VARIABLE_COST(CONTRACT) + &
                            CT_ENERGY_COST_ADDER(CONTRACT) + &
                           12*1000*CONTRACT_FIXED_COST(CONTRACT)/(HOURS* &
                           RIGHT*PRODUCTION_COST_PERIODS))*10.
               ELSE
                  TEMP_CONTRACT_COST=10. * &
                                      (CT_ENERGY_COST_ADDER(CONTRACT) + &
                                      CONTRACT_VARIABLE_COST(CONTRACT) )
               ENDIF
               IF(TEMP_CONTRACT_COST < CONTRACT_DISPATCH_COST .AND. &
                                     AVAILABLE_CONTRACT(CONTRACT)) THEN
                  CONTRACT_DISPATCH_COST = TEMP_CONTRACT_COST
                  LOWEST_COST_CONTRACT = CONTRACT
               ENDIF
            ELSE
               REMAINING_ENERGY = 0.0
               CONTRACTS_ACTIVE = .FALSE.
            ENDIF
         ENDIF
      ENDDO
!
!
      IF(AVAILABLE_CONTRACT_NO .EQ. 0) CONTRACTS_ACTIVE = .FALSE.
!
      RETURN
!
      ENTRY CALC_3808_CAPACITY(CONTRACT_CAPACITY_3808, &
                                                   R_PEAK_MONTH,R_ISEAS)
!
         IF(globecom_YEAR > NOM_YEARS_3808 .AND. R_ISEAS == R_PEAK_MONTH) THEN
            CAP_3808 = &
               CONTRACT_CAPACITY_3808(X_CONTRACT) + &
               CONTRACT_CAPACITY_3808(Y_CONTRACT) + &
               CONTRACT_CAPACITY_3808(Z_CONTRACT)
         ENDIF
!
         TEMP_Y_CAPACITY = GET_VAR(MIN_RATCHET_PATTERN(Y_CONTRACT), &
                                   globecom_YEAR,CNTRNM(Y_CONTRACT))/100.
         IF(TEMP_Y_CAPACITY <= 0. .OR. TEMP_Y_CAPACITY > 1.0) THEN
            WRITE(4,*) "DID NOT FIND MIN RATCHET FOR YEAR", &
                           globecom_year
            WRITE(4,*) "A VALUE OF 75% WAS ASSUMED"
            TEMP_Y_CAPACITY = .75
         ENDIF

         TEMP_Y_CAPACITY = MAX(0., &
               (CONTRACT_CAPACITY_3808(X_CONTRACT) + &
               CONTRACT_CAPACITY_3808(Y_CONTRACT)  + &
               CONTRACT_CAPACITY_3808(Z_CONTRACT))*TEMP_Y_CAPACITY - &
                                                            .5*CAP_3808)
         IF(TEMP_Y_CAPACITY > Y_CAPACITY) THEN
            MONTHS_SINCE_Y_UPDATED = 0
            Y_CAPACITY = TEMP_Y_CAPACITY
         ENDIF
!
      RETURN
      ENTRY GET_X_Y_Z_CONTRACT(R_X_CONTRACT,R_Y_CONTRACT,R_Z_CONTRACT)
         R_X_CONTRACT = X_CONTRACT
         R_Y_CONTRACT = Y_CONTRACT
         R_Z_CONTRACT = Z_CONTRACT
      RETURN
!
      END

!                                                                      C
!     A SUBROUTINE TO INITIALIZE CONTRACTS                             C
!                                                                      C

      SUBROUTINE INITIALIZE_CONTRACTS(LPROB,LODDUR,HOURS,DX, &
                           A,B,REMAINING_ENERGY,ISEAS,CONTRACTS_ACTIVE, &
                           CONTRACT_DISPATCH_COST, &
                           CONTRACT_ENERGY,CONTRACT_CAPACITY, &
                           date_1,date_2,ISTART,LAST_POINT, &
                           AVAILABLE_CONTRACT,AVAILABLE_CONTRACT_NO, &
                           MAXIMUM_ENERGY,MAXIMUM_CAPACITY, &
                           CUM_CONTRACT_CAP,SEASONAL_CONTRACT_CAPACITY, &
                           MINIMUM_ENERGY,MINIMUM_CAPACITY, &
                           PEAK_MONTH,YR,ENRG_FROM_ENRG_ONLY_CONTRACTS, &
                           REMAINING_ENRG_ONLY_ENRG, &
                           MIN_ENRG_FROM_ENRG_ONLY_ENRG, &
                           MAX_RATCHET_CAPACITY,REMAIN_ANN_ENRG, &
                           MIN_RATCHET_CAPACITY,MAXIMUM_ANNUAL_CAPACITY, &
                           TOTAL_MIN_RATCHET_ENERGY,CNTR_ENRG, &
                           MIN_BC_FIXED_COST,BC_BLK1_COST,BCM_NO, &
                           PERIOD_COUNTER,TOTAL_CONTRACT_ENRG, &
                           SUR_SALE_NO,SURPLUS_SALE, &
                           RATCHET_CAPACITY_BASIS,CONTRACTS_IN_PERIOD)
!
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      USE SIZECOM
      use contracts_data
	  use production


!
! DECLARE VARIABLES
!
      real :: hours
      INTEGER (kind=2) ::  CONTRACT
      INTEGER (kind=2) ::  date_1
      INTEGER (kind=2) ::  date_2
      INTEGER (kind=2) ::  ISTART
      INTEGER (kind=2) ::  LAST_POINT
      INTEGER (kind=2) ::  AVAILABLE_CONTRACT_NO
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  PEAK_MONTH
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  BCM_NO
      INTEGER (kind=2) ::  LOWEST_COST_CONTRACT
      INTEGER (kind=2) ::  PERIOD_COUNTER
      INTEGER (kind=2) ::  LAST_SURPLUS
      INTEGER (kind=2) ::  SURPLUS_SALE(MAX_CONTRACTS)
      INTEGER (kind=2) ::  SUR_SALE_NO
      INTEGER (kind=2) ::  AREA
!
      REAL ::  PRODUCTION_COST_PERIODS
      REAL ::  TEMP_CAPACITY,TEMP_ENRG
      REAL ::  REMAINING_ENRG_ONLY_ENRG
      REAL ::  ENRG_FROM_ENRG_ONLY_CONTRACTS
      REAL ::  MIN_ENRG_FROM_ENRG_ONLY_ENRG

      REAL ::  A
      REAL ::  B
      REAL ::  REMAINING_ENERGY
      REAL ::  LPROB(1000)
      REAL ::  LODDUR(1000)
      REAL ::  DX
      REAL ::  LEFT
      REAL ::  RIGHT
      REAL ::  TEMP_CONTRACT_COST
      REAL ::  CURRENT_CONTRACT_ENERGY
      REAL ::  CONTRACT_ENERGY(MAX_CONTRACTS)
      REAL ::  CONTRACT_CAPACITY(MAX_CONTRACTS)
      REAL ::  CONTRACT_DISPATCH_COST
      REAL ::  MAXIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  GET_VAR
      REAL ::  CURRENT_CONTRACT_CAPACITY
      REAL ::  SEASONAL_CONTRACT_CAPACITY
      REAL ::  CUM_CONTRACT_CAP
      REAL ::  MINIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  MINIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  RATCHET_PERCENT
      REAL ::  ENRG_ONLY_ENRG_USED
      REAL ::  MAX_RATCHET_CAPACITY(MAX_CONTRACTS)
      REAL ::  REMAIN_ANN_ENRG(MAX_CONTRACTS)
      REAL ::  MIN_RATCHET_CAPACITY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_ANNUAL_CAPACITY(MAX_CONTRACTS)
      REAL ::  TOTAL_MIN_RATCHET_ENERGY
      REAL ::  CNTR_ENRG
      REAL ::  MIN_BC_FIXED_COST
      REAL ::  BC_BLK1_COST(3)
      REAL ::  SURPLUS_ENERGY
      REAL ::  MAX_MUST_ENERGY
      REAL ::  TOTAL_CONTRACT_ENRG
! ADDED 10/19/93 GAT
      REAL ::  INIT_ENERGY_ONLY_PRICE
      REAL ::  REAL_DUMMY
      REAL ::  SAVE_ENERGY_ONLY_PRICE
      REAL ::  LAST_ENERGY_ONLY_PRICE
      REAL ::  ENERGY_ONLY_FOR_DISPATCHING
      REAL ::  GET_LAST_ENERGY_ONLY_PRICE
      LOGICAL (kind=1) ::  PALO_ALTO_ACTIVE=.FALSE.
      CHARACTER (len=1) ::  UTILITY_TYPE
!
! ADDED 6/16/92 MSG
!
      REAL ::  RATCHET_CAPACITY_BASIS(MAX_CONTRACTS)
      LOGICAL (kind=1) ::  AVAILABLE_CONTRACT(MAX_CONTRACTS)
      LOGICAL (kind=1) ::  CONTRACTS_ACTIVE
      LOGICAL (kind=1) ::  CONTRACTS_IN_PERIOD
      LOGICAL (kind=1) ::  FIXED_COST_LOGIC
!
!
! INITIALIZE VARIABLES
!
      REAL_DUMMY = INIT_ENERGY_ONLY_PRICE()
      LAST_ENERGY_ONLY_PRICE = GET_LAST_ENERGY_ONLY_PRICE()
      PRODUCTION_COST_PERIODS = get_PRODUCTION_PERIODS_in()
      IF(YR .EQ. 1 .AND. PERIOD_COUNTER .EQ. 1) THEN
         IF(UTILITY_TYPE() == 'P') THEN
            PALO_ALTO_ACTIVE = .TRUE.
         ELSE
            PALO_ALTO_ACTIVE = .FALSE.
         ENDIF
         CALL UTILITY_TYPE_IN_CONTRACTS(PALO_ALTO_ACTIVE)
         TOTAL_MIN_RATCHET_ENERGY = 0.
         CNTR_ENRG = 0.
         MIN_BC_FIXED_COST = 0.
         BC_BLK1_COST(1) = 0.
         BC_BLK1_COST(2) = 0.
         BC_BLK1_COST(3) = 0.
         BCM_NO = 0.
         DO CONTRACT = 1 , NUMBER_OF_CONTRACTS
!
! 7/1/93 PROBLEM WITH MAXIMUM_CAPACITY(CONTRACT) BEING USED BEFORE IT
! IS DEFINED MSG
!
            MAXIMUM_CAPACITY(CONTRACT) = 0.
            MINIMUM_CAPACITY(CONTRACT) = 0.

            IF(MAX_RATCHET_PATTERN(CONTRACT) /= 0) THEN
               MAX_RATCHET_CAPACITY(CONTRACT) = MAX(0., &
                             GET_VAR(MAX_RATCHET_PATTERN(CONTRACT), &
                                                   13,CNTRNM(CONTRACT)))
            ELSE
               MAX_RATCHET_CAPACITY(CONTRACT) = 0.
            ENDIF
            IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. 'B') &
                                                      BCM_NO = CONTRACT
            IF(MIN_RATCHET_PATTERN(CONTRACT) /= 0) THEN
               MIN_RATCHET_CAPACITY(CONTRACT) = MAX(0., &
                               GET_VAR(MIN_RATCHET_PATTERN(CONTRACT), &
                                                   13,CNTRNM(CONTRACT)))
            ELSE
               MIN_RATCHET_CAPACITY(CONTRACT) = 0.
            ENDIF
         ENDDO
      ENDIF
!
      AVAILABLE_CONTRACT_NO = 0
      SEASONAL_CONTRACT_CAPACITY = 0.
      LOWEST_COST_CONTRACT = 0
      SURPLUS_ENERGY = 0.
      LAST_SURPLUS = 0
      SUR_SALE_NO = 0
      CALL USING_FIXED_LOGIC(FIXED_COST_LOGIC)
      DO CONTRACT = 1 , NUMBER_OF_CONTRACTS
         CONTRACT_ENERGY(CONTRACT) = 0.
         CONTRACT_CAPACITY(CONTRACT) = 0.
         RATCHET_CAPACITY_BASIS(CONTRACT) = 0.
         IF (CNTR_ON_LI(CONTRACT) .LE. date_2 .AND. &
                                  CNTR_OFF_LI(CONTRACT) .GE. date_1) THEN
            IF(MAX_CAPACITY(CONTRACT) < 0.) THEN
               TEMP_CAPACITY = GET_VAR(MAX_CAPACITY(CONTRACT),ISEAS, &
                                                       CNTRNM(CONTRACT))
               IF(TEMP_CAPACITY <= 1.) THEN
                  MAXIMUM_CAPACITY(CONTRACT) = TEMP_CAPACITY * &
                                                 CNTR_OWN(CONTRACT) * &
                                       MAXIMUM_ANNUAL_CAPACITY(CONTRACT)
               ELSE
                  MAXIMUM_CAPACITY(CONTRACT) = MIN(TEMP_CAPACITY, &
                                    MAXIMUM_ANNUAL_CAPACITY(CONTRACT)) * &
                                                      CNTR_OWN(CONTRACT)
               ENDIF
            ELSEIF(MAX_CAPACITY(CONTRACT) <= 1.) THEN
               MAXIMUM_CAPACITY(CONTRACT) = MAX_CAPACITY(CONTRACT) * &
                                                 CNTR_OWN(CONTRACT) * &
                                       MAXIMUM_ANNUAL_CAPACITY(CONTRACT)
            ELSE
               MAXIMUM_CAPACITY(CONTRACT) = CNTR_OWN(CONTRACT) * &
                                 MIN(MAX_CAPACITY(CONTRACT), &
                                      MAXIMUM_ANNUAL_CAPACITY(CONTRACT))

            ENDIF
            IF(MAX_ENERGY(CONTRACT) < 0.) THEN
               TEMP_ENRG = &
                    GET_VAR(MAX_ENERGY(CONTRACT),ISEAS,CNTRNM(CONTRACT))
               IF(TEMP_ENRG < 2) THEN
                  MAXIMUM_ENERGY(CONTRACT) = TEMP_ENRG * HOURS * &
                                              MAXIMUM_CAPACITY(CONTRACT)
               ELSE
                  MAXIMUM_ENERGY(CONTRACT) = CNTR_OWN(CONTRACT) * &
                                MIN(REMAIN_ANN_ENRG(CONTRACT),TEMP_ENRG)
               ENDIF
            ELSEIF(MAX_ENERGY(CONTRACT) < 2.) THEN
               MAXIMUM_ENERGY(CONTRACT) = MAX_ENERGY(CONTRACT) * &
                                      HOURS * MAXIMUM_CAPACITY(CONTRACT)
            ELSE
               MAXIMUM_ENERGY(CONTRACT) = CNTR_OWN(CONTRACT) * &
                     MIN(REMAIN_ANN_ENRG(CONTRACT),MAX_ENERGY(CONTRACT))
            ENDIF
!
            IF(MIN_CAPACITY(CONTRACT) < 0.) THEN
               TEMP_CAPACITY = GET_VAR(MIN_CAPACITY(CONTRACT),ISEAS, &
                                                       CNTRNM(CONTRACT))
               IF(TEMP_CAPACITY <= 1.) THEN
                  MINIMUM_CAPACITY(CONTRACT) = TEMP_CAPACITY * &
                                                 CNTR_OWN(CONTRACT) * &
                                       MAXIMUM_ANNUAL_CAPACITY(CONTRACT)
               ELSE
                  MINIMUM_CAPACITY(CONTRACT) = CNTR_OWN(CONTRACT) * &
                                               TEMP_CAPACITY
               ENDIF
            ELSEIF(MIN_CAPACITY(CONTRACT) <= 1.) THEN
                  MINIMUM_CAPACITY(CONTRACT) = MIN_CAPACITY(CONTRACT) * &
                                                 CNTR_OWN(CONTRACT) * &
                                       MAXIMUM_ANNUAL_CAPACITY(CONTRACT)
            ELSE
               MINIMUM_CAPACITY(CONTRACT) = CNTR_OWN(CONTRACT)* &
                                                  MIN_CAPACITY(CONTRACT)
            ENDIF
!
            IF(MIN_ENERGY(CONTRACT) < 0.) THEN
               TEMP_ENRG = &
                    GET_VAR(MIN_ENERGY(CONTRACT),ISEAS,CNTRNM(CONTRACT))
               IF(TEMP_ENRG < 2) THEN
                  MINIMUM_ENERGY(CONTRACT) = TEMP_ENRG * HOURS * &
                                              MAXIMUM_CAPACITY(CONTRACT)
               ELSE
                  MINIMUM_ENERGY(CONTRACT)=CNTR_OWN(CONTRACT)*TEMP_ENRG
               ENDIF
            ELSEIF(MIN_ENERGY(CONTRACT) < 2.) THEN
               MINIMUM_ENERGY(CONTRACT) = MIN_ENERGY(CONTRACT) * HOURS * &
                                              MAXIMUM_CAPACITY(CONTRACT)
            ELSE
               MINIMUM_ENERGY(CONTRACT) = MIN_ENERGY(CONTRACT) * &
                                                      CNTR_OWN(CONTRACT)
            ENDIF
!
! 7/28/93. GAT. PALO ALTO 2 TIER ENERGY CONTRACT.
!
            CALL INIT_TIER_2
            IF(CNTR_ENERGY_SWITCH(CONTRACT) == 'T') THEN
               CONTRACT_VARIABLE_COST(CONTRACT) = &
                                   CT_MONTHLY_1ST_ENERGY_PRICE(CONTRACT)
            ENDIF
!
! MAX RATCHET CONTRACT LOGIC
!
            IF(MAX_RATCHET_PATTERN(CONTRACT) .NE. 0) THEN
               RATCHET_PERCENT = GET_VAR(MAX_RATCHET_PATTERN(CONTRACT), &
                                               ISEAS,CNTRNM(CONTRACT))
               IF(RATCHET_PERCENT .LT. 0. .OR. &
                                          RATCHET_PERCENT .GT. 1.) THEN
                  WRITE(4,*) "INVALID RATCHET MULTIPLIER FOR CONTRACT "
                  WRITE(4,*) CNTRNM(CONTRACT)," VALUES RANGE: 0.<x<1."
                  WRITE(4,*) " "
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "V"
               ELSEIF( ISEAS .NE. PEAK_MONTH .OR. KEPCO) THEN
                  IF(.NOT. REALLY_KEPCO) THEN
                     MAXIMUM_CAPACITY(CONTRACT) = &
                        MAX_RATCHET_CAPACITY(CONTRACT) * RATCHET_PERCENT
                  ENDIF
                  RATCHET_CAPACITY_BASIS(CONTRACT) = &
                        MAX_RATCHET_CAPACITY(CONTRACT) * RATCHET_PERCENT
                  MAXIMUM_ENERGY(CONTRACT) = &
                              MIN(MAXIMUM_CAPACITY(CONTRACT)*HOURS, &
                                               MAXIMUM_ENERGY(CONTRACT))
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "M"
               ELSE
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "V"
               ENDIF
!
! MIN RATCHET CONTRACT LOGIC
!
            ELSEIF(MIN_RATCHET_PATTERN(CONTRACT) .NE. 0) THEN

               CNTR_CAPACITY_SWITCH(CONTRACT) = "V"

            ENDIF

! ADDED 5/21/92 FOR KEPCO

            AREA = INDEX('123456',CNTRTYPE(CONTRACT))
            IF(CNTRTYPE(CONTRACT) == 'K' .OR. & !  2/12/98. GAT. ADDED FOR WVPA
                                         CNTRTYPE(CONTRACT) == 'L') THEN
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
            ELSEIF(AREA .NE. 0) THEN
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.

               AREA_LAST_RESOURCE(AREA) = CONTRACT

            ELSEIF(CNTRTYPE(CONTRACT) == 'E') THEN
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               EMERGENCY_ENERGY_CONTRACT = CONTRACT
            ELSEIF(CNTRTYPE(CONTRACT) == 'M') THEN
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               MAINTENANCE_ENERGY_CONTRACT = CONTRACT
            ELSEIF(CNTRTYPE(CONTRACT) == 'S') THEN
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
            ELSEIF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. "E") THEN
               ENRG_FROM_ENRG_ONLY_CONTRACTS = MAXIMUM_ENERGY(CONTRACT)+ &
                                           ENRG_FROM_ENRG_ONLY_CONTRACTS
               MIN_ENRG_FROM_ENRG_ONLY_ENRG = MINIMUM_ENERGY(CONTRACT) + &
                                            MIN_ENRG_FROM_ENRG_ONLY_ENRG
               AVAILABLE_CONTRACT(CONTRACT) = .TRUE.
               IF(MAXIMUM_ENERGY(CONTRACT) > 0.0) &
                  REAL_DUMMY = SAVE_ENERGY_ONLY_PRICE( &
                                       CONTRACT_VARIABLE_COST(CONTRACT))
! ADDED 5/13/92
            ELSEIF( CNTR_ENERGY_SWITCH(CONTRACT) == "O") THEN
               SUR_SALE_NO = SUR_SALE_NO + 1
               SURPLUS_SALE(SUR_SALE_NO) = CONTRACT
               AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               MAXIMUM_CAPACITY(CONTRACT) = 0.
               MINIMUM_CAPACITY(CONTRACT) = 0.
            ELSE
               IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. "V" .AND. &
                                 CONTRACT_FIXED_COST(CONTRACT) .EQ. 0.) &
                  CNTR_CAPACITY_SWITCH(CONTRACT) = "M"



               IF( (MINIMUM_CAPACITY(CONTRACT) .LE. &
                                 MAXIMUM_CAPACITY(CONTRACT)) .AND. &
                   (MINIMUM_ENERGY(CONTRACT) .LE. &
                                  MAXIMUM_ENERGY(CONTRACT))  .AND. &
                   (MAXIMUM_ENERGY(CONTRACT) .GT. 0.)        .AND. &
                   (MAXIMUM_CAPACITY(CONTRACT) .GT. 0.) )           THEN
                  AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO + 1
                  AVAILABLE_CONTRACT(CONTRACT) = .TRUE.
                  SEASONAL_CONTRACT_CAPACITY=SEASONAL_CONTRACT_CAPACITY+ &
                                              MAXIMUM_CAPACITY(CONTRACT)
               ELSE
                  IF(MINIMUM_CAPACITY(CONTRACT) .GT. &
                                        MAXIMUM_CAPACITY(CONTRACT)) THEN
                     WRITE(4,*)"MINIMUM CONTRACT CAPACITY > "// &
                     "MAXIMUM FOR CONTRACT ",CNTRNM(CONTRACT)
                     WRITE(4,*) MINIMUM_CAPACITY(CONTRACT), &
                                        MAXIMUM_CAPACITY(CONTRACT)
                  ENDIF
                  IF(MINIMUM_ENERGY(CONTRACT) .GT. &
                                          MAXIMUM_ENERGY(CONTRACT)) THEN
                     WRITE(4,*) "MINIMUM CONTRACT ENERGY > "// &
                           "MAXIMUM FOR CONTRACT ",CNTRNM(CONTRACT)
                     WRITE(4,*) MINIMUM_ENERGY(CONTRACT), &
                                        MAXIMUM_ENERGY(CONTRACT)
                  ENDIF
                  AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
               ENDIF
            ENDIF
!           SURPLUS CONTRACTS
            LAST_SURPLUS = CONTRACT
         ELSE
            AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
         ENDIF
      ENDDO
      IF(AVAILABLE_CONTRACT_NO .GT. 0) THEN
         CONTRACTS_ACTIVE = .TRUE.
         CONTRACTS_IN_PERIOD = .TRUE.
      ELSE
         RETURN
      ENDIF
      LEFT = 1.
      RIGHT = 1.
!
!        DETERMINE TOTAL CONTRACT MINIMUMS.
      CONTRACT = 0.
      REMAINING_ENRG_ONLY_ENRG = ENRG_FROM_ENRG_ONLY_CONTRACTS/HOURS
      DO WHILE(CONTRACT .LT. NUMBER_OF_CONTRACTS .AND. &
                    (CONTRACTS_ACTIVE .OR. CONTRACT .LE. LAST_SURPLUS))
         CONTRACT = CONTRACT + 1
         ENRG_ONLY_ENRG_USED = 0.
!
!
!
!
!
         IF(AVAILABLE_CONTRACT(CONTRACT) .AND. &
                      CNTR_CAPACITY_SWITCH(CONTRACT) .NE. "E") THEN
            CURRENT_CONTRACT_ENERGY = 0.
            CURRENT_CONTRACT_CAPACITY = 0.
! AT SOME POINT, NEW_PEAK_SHIFT SHOULD REPLACE
!
            IF(MINIMUM_ENERGY(CONTRACT) .GT. 0. .AND. &
                        MINIMUM_CAPACITY(CONTRACT) .GT. 0 .AND. &
                        MINIMUM_ENERGY(CONTRACT) .LE. &
                        MINIMUM_CAPACITY(CONTRACT)*HOURS) THEN
               IF(MINIMUM_ENERGY(CONTRACT) .GT. &
                                   MINIMUM_CAPACITY(CONTRACT)*HOURS) &
                     MINIMUM_ENERGY(CONTRACT) = &
                        MIN(MINIMUM_CAPACITY(CONTRACT)*HOURS, &
                        MAXIMUM_ENERGY(CONTRACT))
               A = B
               IF(CNTR_ENERGY_SWITCH(CONTRACT) .EQ. 'S' .AND. &
                     MINIMUM_ENERGY(CONTRACT)/HOURS + &
                                        SURPLUS_ENERGY > 0.) THEN
                  MAX_MUST_ENERGY = MAXIMUM_ENERGY(CONTRACT)/HOURS + &
                                                        SURPLUS_ENERGY
               ELSE
                  MAX_MUST_ENERGY = MAXIMUM_ENERGY(CONTRACT)/HOURS
               ENDIF
               CALL NEW_PEAK_SHIFT( &
                        LODDUR,LPROB,MINIMUM_CAPACITY(CONTRACT), &
                        MINIMUM_ENERGY(CONTRACT)/HOURS, &
                        LAST_POINT,A,B,CURRENT_CONTRACT_CAPACITY, &
                        CURRENT_CONTRACT_ENERGY,HOURS, &
                        MAX_MUST_ENERGY,CNTRNM(CONTRACT), &
                        ISEAS,DX, &
                        REMAINING_ENRG_ONLY_ENRG, &
                        ENRG_ONLY_ENRG_USED)
               IF(CNTR_ENERGY_SWITCH(CONTRACT) .EQ. 'S') THEN
                  SURPLUS_ENERGY = SURPLUS_ENERGY + &
                     MAXIMUM_ENERGY(CONTRACT)/HOURS - &
                     CURRENT_CONTRACT_ENERGY
                  CURRENT_CONTRACT_ENERGY = &
                                    MAXIMUM_ENERGY(CONTRACT)/HOURS
               ENDIF
            ELSEIF(MINIMUM_ENERGY(CONTRACT) .GT. 0. .OR. &
                        MINIMUM_CAPACITY(CONTRACT) .GT. 0) THEN
               IF(MINIMUM_CAPACITY(CONTRACT)*HOURS .GT. &
                                      MAXIMUM_ENERGY(CONTRACT)) THEN
                  MINIMUM_CAPACITY(CONTRACT) = &
                                      MAXIMUM_ENERGY(CONTRACT)/HOURS
               ENDIF
               CURRENT_CONTRACT_CAPACITY = &
                     MAX(MINIMUM_CAPACITY(CONTRACT), &
                     MINIMUM_ENERGY(CONTRACT)/HOURS)

!              LOAD TOTAL MINIMUM CAPACITY INTO THE
!              ELDC AND DETERMINE ENERGY.

               A = B
!
               IF(PALO_ALTO_ACTIVE) THEN
                  IF(LAST_ENERGY_ONLY_PRICE < &
                                 CONTRACT_VARIABLE_COST(CONTRACT)) THEN
                     ENERGY_ONLY_FOR_DISPATCHING = &
                                               REMAINING_ENRG_ONLY_ENRG
                  ELSE
                     ENERGY_ONLY_FOR_DISPATCHING = 0.0
                  ENDIF
               ELSE
                     ENERGY_ONLY_FOR_DISPATCHING = &
                                               REMAINING_ENRG_ONLY_ENRG
               ENDIF
               IF(MINIMUM_CAPACITY(CONTRACT) .GE. &
                                 MINIMUM_ENERGY(CONTRACT)/HOURS) THEN
                  B = MIN(A + &
                          MINIMUM_CAPACITY(CONTRACT),LODDUR(LAST_POINT), &
                          A + MAXIMUM_ENERGY(CONTRACT)/HOURS)
                  CALL CALC_CONTRACT_ENERGY(LODDUR,LPROB,DX,A,B, &
                        CURRENT_CONTRACT_ENERGY, &
                        LEFT,RIGHT,REMAINING_ENERGY,ISTART,LAST_POINT, &
                        ENERGY_ONLY_FOR_DISPATCHING, &
                        ENRG_ONLY_ENRG_USED)
!

               ELSE
                  B = MIN(A+MAXIMUM_CAPACITY(CONTRACT), &
                                                   LODDUR(LAST_POINT))
                  CALL CALC_CONTRACT_ENERGY(LODDUR,LPROB,DX,A,B, &
                        CURRENT_CONTRACT_ENERGY,LEFT,RIGHT, &
                        MIN(MINIMUM_ENERGY(CONTRACT)/HOURS, &
                                                     REMAINING_ENERGY), &
                        ISTART,LAST_POINT, &
                        ENERGY_ONLY_FOR_DISPATCHING, &
                        ENRG_ONLY_ENRG_USED)
!
!
!                 CURRENT_CONTRACT_CAPACITY MAY BE RESET BY
!                           CALC_CONTRACT_ENERGY
!
                  CURRENT_CONTRACT_CAPACITY = B - A
               ENDIF
               IF(LAST_ENERGY_ONLY_PRICE < &
                                 CONTRACT_VARIABLE_COST(CONTRACT)) &
                  REMAINING_ENRG_ONLY_ENRG = ENERGY_ONLY_FOR_DISPATCHING
            ENDIF ! END IMPOSE MINIMUM ENERGY /CAPACITY
            IF(MINIMUM_ENERGY(CONTRACT) .GT. 0. .OR. &
                                 MINIMUM_CAPACITY(CONTRACT) .GT. 0) THEN
!
!
               CUM_CONTRACT_CAP = CUM_CONTRACT_CAP + B - A
               CONTRACT_CAPACITY(CONTRACT) = CURRENT_CONTRACT_CAPACITY
               REMAINING_ENERGY = REMAINING_ENERGY - &
                           CURRENT_CONTRACT_ENERGY - ENRG_ONLY_ENRG_USED
!
!
               CONTRACT_ENERGY(CONTRACT) = CURRENT_CONTRACT_ENERGY
               TOTAL_CONTRACT_ENRG = TOTAL_CONTRACT_ENRG + &
                                                 CURRENT_CONTRACT_ENERGY
               IF(CONTRACT_CAPACITY(CONTRACT)*1.000001 .GE. &
                        MAXIMUM_CAPACITY(CONTRACT) .OR. &
                        CONTRACT_ENERGY(CONTRACT)*1.000001 .GE. &
                        MAXIMUM_ENERGY(CONTRACT)/HOURS) THEN
                  AVAILABLE_CONTRACT(CONTRACT) = .FALSE.
                  AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO - 1
               ENDIF
            ENDIF
            IF(REMAINING_ENERGY .GE. .0001) THEN
!
!
               IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. 'V') THEN
                  TEMP_CONTRACT_COST = &
                           (CONTRACT_VARIABLE_COST(CONTRACT) + &
                            CT_ENERGY_COST_ADDER(CONTRACT) + &
                           12*1000*CONTRACT_FIXED_COST(CONTRACT)/(HOURS* &
                           RIGHT*PRODUCTION_COST_PERIODS))*10.
               ELSE
                  TEMP_CONTRACT_COST = 10. * &
                            (CT_ENERGY_COST_ADDER(CONTRACT) + &
                                       CONTRACT_VARIABLE_COST(CONTRACT))
               ENDIF
               IF(TEMP_CONTRACT_COST .LT. CONTRACT_DISPATCH_COST .AND. &
                                     AVAILABLE_CONTRACT(CONTRACT)) THEN
                  CONTRACT_DISPATCH_COST = TEMP_CONTRACT_COST
                  LOWEST_COST_CONTRACT = CONTRACT
               ENDIF
            ELSE
               REMAINING_ENERGY = 0.0
               CONTRACTS_ACTIVE = .FALSE.
            ENDIF
         ENDIF ! AVAILABLE CONTRACT .AND. .NOT. ENERGY ONLY
      ENDDO ! CONTRACTS COUNTER FOR MINIMUM'S
!
      IF(SURPLUS_ENERGY .LT. 0. .AND. LOWEST_COST_CONTRACT .GT. 0) THEN
         CURRENT_CONTRACT_ENERGY = CONTRACT_ENERGY(LOWEST_COST_CONTRACT)
         CONTRACT_ENERGY(LOWEST_COST_CONTRACT) = &
               MIN(MAXIMUM_ENERGY(LOWEST_COST_CONTRACT)/HOURS, &
               CONTRACT_ENERGY(LOWEST_COST_CONTRACT) - SURPLUS_ENERGY)
         TOTAL_CONTRACT_ENRG = TOTAL_CONTRACT_ENRG + &
                                 CONTRACT_ENERGY(LOWEST_COST_CONTRACT) - &
                                 CURRENT_CONTRACT_ENERGY
      ENDIF
      IF(AVAILABLE_CONTRACT_NO .EQ. 0) CONTRACTS_ACTIVE = .FALSE.
      RETURN
      END

!                                                                      C
!     A SUBROUTINE FOR CONTRACTS IN PRODUCTION DISPATCH                C
!                                                                      C

      SUBROUTINE CONTRACTS(CL_DISPATCH_COST, &
                           LPROB,LODDUR,HOURS,DX, &
                           A,B,REMAINING_ENERGY, &
                           ISTART, &
                           CONTRACTS_ACTIVE,CONTRACT_DISPATCH_COST, &
                           CONTRACT_ENERGY,CONTRACT_CAPACITY, &
                           LAST_POINT,AVAILABLE_CONTRACT, &
                           AVAILABLE_CONTRACT_NO, &
                           MAXIMUM_ENERGY,MAXIMUM_CAPACITY, &
                           CUM_CONTRACT_CAP, &
                           REMAINING_ENRG_ONLY_ENRG, &
                           TOTAL_CONTRACT_ENRG,ISEAS)
!
!     CONTRACTS PROVIDES AN ABILITY TO INCORPORATE PURCHASE POWER
! CONTRACTS INTO THE PRODUCTION COSTING OF CL UNITS.
!
!     SUBROUTINE CONTRACTS IS CALLED BY DR_BOOTH INSIDE THE UNITS
! COUNTER.  IF A UNIT SEGMENT IS NOT MUST RUN, AND THE
! CL_DISPATCH_COST IS GREATER THAN THE LOWEST AVAILABLE
! CONTRACT_DISPATCH_COST, AND THE CONTRACTS OPTION IS ACTIVE,
! THE CONTRACTS SUBROUTINE IS CALLED.
!
!     CONTRACTS HAVE THE FOLLOWING CHARACTERISTICS:
!
!     ENERGY_COST
!     CAPACITY_COST (VARIES WITH THE MAXIMUM CONTRACTED CAPACITY)
!     FIXED_CAPACITY_COST (CAN BE DIFFERENT THAN CAPACITY COST)
!     MINIMUM_ENERGY (CONSTRAINT)
!     MAXIMUM_ENERGY (CONSTRAINT)
!     MINIMUM_CAPACITY (CONSTRAINT)
!     MAXIMUM_CAPACITY (CONSTRAINT)
!     CONTRACT_ENERGY (STATE VARIABLE)
!     CONTRACT_CAPACITY (STATE VARIABLE)
!     AVAILABLE_CONTRACT (STATE VARIABLE)
!
! NOTES ON INTERFACE WITH MIDAS:
!
!     THE PARAMETERS THAT MIDAS AND THE CONTRACTS MODEL PASS BACK
!     AND FOURTH ARE: PRODUCTION DISPATCH AND CONVOLVING INFO,
!     COST INFORMATION.
!     PRODUCTION DISPATCH PARAMETERS ARE: A,RIGHT,LODDUR,LPROB,
!     CL_DISPATCH_COST AND REMAINING_ENERGY
!     SUBROUTINE ASSUMES UNITS ARE LOADED INSIDE
!     THE UNITS COUNTER, AFTER MAINTENANCE CALC AND BEFORE ANY ENER
!     ALLOCATION.  THE ALGORITHM TAKES THE CURRENT RIGHT ELDC
!     BOUNDARY AND LOADS CONTRACTS UNDER THE ELDC UNTIL A STOPPING
!     CONDITION IS REACHED.  THEN, THE RIGHT BOUNDARY, REMAINING_
!     ENERGY, AND CL_DISPATCH_COST ARE CHANGED TO REFLECT
!     CONTRACTS.  SUBROUTINE ASSUMES A IS THE NEW LEFT BOUNDARY.
!
! VARIABLE DECLARATIONS:
!
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      use contracts_data
	  use production
      implicit none


      INTEGER (kind=2) ::  LOWEST_COST_CONTRACT
      INTEGER (kind=2) ::  CONTRACT
      INTEGER (kind=2) ::  LD_PTS, PRODUCTION_PERIODS
      INTEGER (kind=2) ::  ISTART
      INTEGER (kind=2) ::  ISTOP
      INTEGER (kind=2) ::  LAST_POINT
      INTEGER (kind=2) ::  AVAILABLE_CONTRACT_NO
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  PEAK_MONTH
      REAL ::  PRODUCTION_COST_PERIODS
      REAL ::  HOURS
      REAL ::  REMAINING_ENRG_ONLY_ENRG
      REAL ::  A
      REAL ::  B
      REAL ::  LOWEST_COST
      REAL ::  REMAINING_ENERGY
      REAL ::  LPROB(1000)
      REAL ::  LODDUR(1000)
      REAL ::  DX
      REAL ::  CURRENT_CONTRACT_ENERGY
      REAL ::  CURRENT_CONTRACT_CAPACITY
      REAL ::  LEFT
      REAL ::  RIGHT
      REAL ::  CL_DISPATCH_COST
      REAL ::  CROSSOVER_COST
      REAL ::  CONTRACT_CROSSOVER_PROB
      REAL ::  TEMP_CROSSOVER_COST
      REAL ::  TEMP_CROSSOVER_PROB
      REAL ::  CL_CROSSOVER_PROB
      REAL ::  CONTRACT_MINIMUM_PROB
      REAL ::  TEMP_CONTRACT_COST
      REAL ::  CONTRACT_DISPATCH_COST
      REAL ::  CONTRACT_CAPACITY(MAX_CONTRACTS)
      REAL ::  CONTRACT_ENERGY(MAX_CONTRACTS)
      REAL ::  INTPL8
      REAL ::  X1
      REAL ::  Y1
      REAL ::  X3
      REAL ::  Y2
      REAL ::  X2
      REAL ::  TEMP_FIX_CURRENT
      REAL ::  FIX_LOWEST
      REAL ::  MAXIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  CUM_CONTRACT_CAP
      REAL ::  ENRG_ONLY_ENRG_USED
      REAL ::  TOTAL_CONTRACT_ENRG
      REAL ::  CAP_3808
      REAL ::  RETURN_CAP_3808
      REAL ::  LIMITED_ENERGY
      REAL ::  DISPATCHING_CAPACITY
      REAL ::  FIXED_COST_FACTOR
      REAL ::  LOWEST_VARIABLE_COST, CURRENT_VARIABLE_COST
      LOGICAL (kind=1) ::  AVAILABLE_CONTRACT(MAX_CONTRACTS)
      LOGICAL (kind=1) ::  CONTRACTS_ACTIVE
      LOGICAL (kind=1) ::  ACTIVE_3808
      LOGICAL (kind=1) ::  FIXED_COST_LOGIC
! PALO ALTO 2 TIER VARIABLES
!
      LOGICAL (kind=1) ::  PALO_ALTO_ACTIVE,R_PALO_ALTO_ACTIVE
      INTEGER (kind=2) ::  TIER_2_CT
      REAL ::  TIER_2_ENERGY
      REAL ::  TIER_2_TEMP
      REAL ::  CANDIDATE_COST
      REAL ::  TIER_2_MARGINAL_COST
! SURPLUS CAPACITY VARIABLES
      LOGICAL (kind=1) ::  SURPLUS_CAPACITY_ACTIVE
! 10/19/93 GAT
      REAL ::  ENERGY_ONLY_FOR_DISPATCHING
      REAL ::  LAST_ENERGY_ONLY_PRICE
      REAL ::  NEXT_LOWEST_VARIABLE_COST
      REAL ::  NEXT_LOWEST_TOTAL_COST
      REAL ::  GET_LAST_ENERGY_ONLY_PRICE
      SAVE PALO_ALTO_ACTIVE
!
!
!
! INTEGRATE ALONG THE ELDC UNTIL THE TARGET CAPACITY
! AND ENERGY IS MET, OR ALL POINTS ON THE LOAD CURVE ARE EVALUATED.
!
! DETERMINE THE LOWEST COST CONTRACT FOR THE NEXT LOAD INTERVAL(S)
! THE ORDERED PAIR (RIGHT,A) ACTS AS THE STARTING POINT FOR THE
! EVALUATION.
!
      INTPL8(X1,Y1,X3,Y2,X2) = Y1 + (Y2-Y1) * (X3-X1)/(X2-X1)
      CALL RETURN_TIER_2(TIER_2_ENERGY,TIER_2_CT)
      CALL USING_FIXED_LOGIC(FIXED_COST_LOGIC)
      LAST_ENERGY_ONLY_PRICE = GET_LAST_ENERGY_ONLY_PRICE()

      PRODUCTION_COST_PERIODS = get_PRODUCTION_PERIODS_in()
      IF(B .GT. LODDUR(1)) THEN
         ISTOP = MAX(ISTART,INT(1,2))
         DO WHILE(.NOT.(LODDUR(ISTOP).LE.B .AND. B.LE.LODDUR(ISTOP+1)))
            IF(LODDUR(ISTOP) .GT. B) THEN
               ISTOP = ISTOP - 1
            ELSE
               ISTOP = ISTOP + 1
            ENDIF
         ENDDO
         RIGHT = INTPL8(LODDUR(ISTOP),LPROB(ISTOP),B, &
                 LPROB(ISTOP+1),LODDUR(ISTOP+1))
         IF(RIGHT .LT. .0001) RIGHT = 0.0
      ELSE
         RIGHT = 1.0
         ISTOP = 1
      ENDIF

      IF(REMAINING_ENERGY .LT. .000001) REMAINING_ENERGY = 0.

      DO WHILE(RIGHT .GT. 0. .AND. CONTRACTS_ACTIVE)
         IF(RIGHT > .0000001 .AND. REMAINING_ENERGY == 0.) THEN

            CONTRACTS_ACTIVE = .FALSE.
            RETURN
         ENDIF
         TEMP_CONTRACT_COST = 9999999.
         LOWEST_COST = CL_DISPATCH_COST
         LOWEST_COST_CONTRACT = 0
         NEXT_LOWEST_VARIABLE_COST = 999999999.
         NEXT_LOWEST_TOTAL_COST =    999999999.
         DO CONTRACT = 1 , NUMBER_OF_CONTRACTS
            IF(AVAILABLE_CONTRACT(CONTRACT) .AND. &
                           CNTR_CAPACITY_SWITCH(CONTRACT) .NE. "E") THEN
               IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. 'V') THEN
                  TEMP_CONTRACT_COST = &
                     (CONTRACT_VARIABLE_COST(CONTRACT) + &
                            CT_ENERGY_COST_ADDER(CONTRACT) + &
                  12*1000*CONTRACT_FIXED_COST(CONTRACT)/(HOURS* &
                     (RIGHT)*PRODUCTION_COST_PERIODS))*10.
               ELSE
                  TEMP_CONTRACT_COST = 10. * &
                                      (CT_ENERGY_COST_ADDER(CONTRACT) + &
                                       CONTRACT_VARIABLE_COST(CONTRACT))
               ENDIF
               IF(TEMP_CONTRACT_COST < LOWEST_COST .AND. &
                                      AVAILABLE_CONTRACT(CONTRACT)) THEN
                  IF(LOWEST_COST_CONTRACT > 0) THEN
                     NEXT_LOWEST_VARIABLE_COST = &
                            CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT)
                     NEXT_LOWEST_TOTAL_COST = LOWEST_COST
                  ENDIF
                  LOWEST_COST = TEMP_CONTRACT_COST
                  LOWEST_COST_CONTRACT = CONTRACT
               ENDIF
            ENDIF
         ENDDO
!        CHECK WHETHER ANY CONTRACTS ARE ECONOMIC
         IF(LOWEST_COST_CONTRACT .LT. 1) THEN
            CONTRACT_DISPATCH_COST = TEMP_CONTRACT_COST
            RETURN
         ENDIF
!
!     LOAD THE LOWEST_COST_CONTRACT INTO THE ELDC, UNTIL EITHER:
!
!        (1) MAXIMUM_ENERGY IS VIOLATED, OR
!        (2) MAXIMUM_CAPACITY IS VIOLATED, OR
!        (3) ANOTHER CONTRACT IS LOWEST_COST, OR
!        (4) THE NEXT CL UNIT IS LOWEST_COST
!        (5) CONTRACTS EVALUATED BEYOND THE END OF THE NEXT CL UNIT
!
!     CONSTRAINT (1) IS TESTED INSIDE CONTRACT_ENERGY.
!     CONSTRAINTS 2-5 ARE CAPACITY CONSTRAINTS.  THE MINIMUM OF THE
!     FOUR IS BINDING.
!
!
!     MAXIMUM_CAPACITY PROB - CONSTRAINT (2) (INPUT ASSUMPTION)
!
!        NO CALCULATION NECESSARY. KEEP IN MW'S UNITS.
!
!     NEXT BEST CONTRACT PROBABILITY - CONSTRAINT (3)
!
         FIXED_COST_FACTOR = &
                          (12.*1000./(HOURS*RIGHT* &
                                           PRODUCTION_COST_PERIODS))*10.
!
         IF(CNTR_CAPACITY_SWITCH(LOWEST_COST_CONTRACT) .EQ. 'V') THEN
            FIX_LOWEST = 12*1000* &
                        CONTRACT_FIXED_COST(LOWEST_COST_CONTRACT)/ &
                        (HOURS*PRODUCTION_COST_PERIODS)
            CROSSOVER_COST = 999999.
            CONTRACT_CROSSOVER_PROB = 0.0000001
            DO CONTRACT = 1, NUMBER_OF_CONTRACTS
               LOWEST_VARIABLE_COST = &
                          CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT) + &
                              CT_ENERGY_COST_ADDER(LOWEST_COST_CONTRACT)
               CURRENT_VARIABLE_COST=CONTRACT_VARIABLE_COST(CONTRACT) + &
                                          CT_ENERGY_COST_ADDER(CONTRACT)
!
! MARK QUESTIONS: SHOULD THE ADDER BE IN THIS
!
! UPDATE: 9/8/94. GAT.
               IF( CONTRACT_FIXED_COST(LOWEST_COST_CONTRACT) == &
                       CONTRACT_FIXED_COST(CONTRACT) .AND. &
                            CNTR_CAPACITY_SWITCH(CONTRACT) == 'V') CYCLE
               IF(LOWEST_VARIABLE_COST /= CURRENT_VARIABLE_COST .AND. &
                                      AVAILABLE_CONTRACT(CONTRACT) .AND. &
                                  CONTRACT /= LOWEST_COST_CONTRACT .AND. &
                             CNTR_CAPACITY_SWITCH(CONTRACT) /= "E") THEN
!
!              FIND THE POINT WHERE THE CURRENT CONTRACT INTERSECTS THE
!              LOWEST COST CONTRACT.
!
                  IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. 'V') THEN
                     TEMP_FIX_CURRENT = 12.*1000.* &
                        CONTRACT_FIXED_COST(CONTRACT)/ &
                        (HOURS*PRODUCTION_COST_PERIODS)
                  ELSE
                     TEMP_FIX_CURRENT = 0.
                  ENDIF
!
! MARK QUESTIONS: SHOULD THE ADDER BE IN THIS
!
                  TEMP_CROSSOVER_COST = &
                     (LOWEST_VARIABLE_COST * TEMP_FIX_CURRENT - &
                                 CURRENT_VARIABLE_COST * FIX_LOWEST ) / &
                     (TEMP_FIX_CURRENT - FIX_LOWEST)
                  TEMP_CROSSOVER_PROB = (FIX_LOWEST - TEMP_FIX_CURRENT)/ &
                              (CURRENT_VARIABLE_COST - &
                                                   LOWEST_VARIABLE_COST)
                  IF(TEMP_CROSSOVER_COST  < CROSSOVER_COST .AND. &
                        TEMP_CROSSOVER_PROB+.0001 .LT. RIGHT .AND. &
                        TEMP_CROSSOVER_PROB+.0001 .GT. 0.0 .AND. &
                        TEMP_CROSSOVER_COST .GT. 0.) THEN
!
!
                     FIXED_COST_FACTOR = &
                          (12.*1000./(HOURS*RIGHT* &
                                           PRODUCTION_COST_PERIODS))*10.
                     IF(CNTR_CAPACITY_SWITCH(CONTRACT) .EQ. 'V') THEN
                        NEXT_LOWEST_TOTAL_COST = &
                           10.*CONTRACT_VARIABLE_COST(CONTRACT) + &
                                     CONTRACT_FIXED_COST(CONTRACT) * &
                                                       FIXED_COST_FACTOR
                     ELSE
                        NEXT_LOWEST_TOTAL_COST = &
                                    10.*CONTRACT_VARIABLE_COST(CONTRACT)
                     ENDIF
                     CROSSOVER_COST = TEMP_CROSSOVER_COST
                     CONTRACT_CROSSOVER_PROB = TEMP_CROSSOVER_PROB
                  ENDIF
               ENDIF
            ENDDO
!
!           NEXT BEST CL UNIT - CONSTRAINT (4)
!           6/21/93. GAT. TRAPPED FOR 10.*CONTRACT...
!
! MARK QUESTIONS: SHOULD THE ADDER BE IN THIS
!
            LOWEST_VARIABLE_COST = &
                          CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT) + &
                              CT_ENERGY_COST_ADDER(LOWEST_COST_CONTRACT)
            IF(CL_DISPATCH_COST /= 10.* LOWEST_VARIABLE_COST) &
               CL_CROSSOVER_PROB = 10. * FIX_LOWEST/ &
                         (CL_DISPATCH_COST - 10. * LOWEST_VARIABLE_COST)
!
! COMPARE CONSTRAINTS (2), (3), (4) AND (5):
!
            CONTRACT_MINIMUM_PROB = &
               MAX(CONTRACT_CROSSOVER_PROB, CL_CROSSOVER_PROB, 0.000001)
!
!           FIND THE CAPACITY ASSOCIATED WITH CONTRACT_MINIMUM_PROB AND
!           COMPARE TO CONSTRAINT (2)
!
            LD_PTS = ISTOP
            DO WHILE( CONTRACT_MINIMUM_PROB .LT. LPROB(LD_PTS))
               LD_PTS = LD_PTS + 1
            ENDDO
            IF(LD_PTS .EQ. 1 ) THEN
!
! IMPLIES AN IDENTICALLY PRICED
!   OR UNECONOMIC COMPETING RESOURCE. NO PROBLEM.
!
               CURRENT_CONTRACT_CAPACITY = &
                              MAXIMUM_CAPACITY(LOWEST_COST_CONTRACT)
            ELSE
               CURRENT_CONTRACT_CAPACITY = &
                 INTPL8(LPROB(LD_PTS-1),LODDUR(LD_PTS-1), &
                 CONTRACT_MINIMUM_PROB,LODDUR(LD_PTS),LPROB(LD_PTS))-B
            ENDIF
            IF(LPROB(LD_PTS) .EQ. 0.0) THEN
               CURRENT_CONTRACT_CAPACITY = MIN(LODDUR(LD_PTS) - B, &
                  MAXIMUM_CAPACITY(LOWEST_COST_CONTRACT) - &
                  CONTRACT_CAPACITY(LOWEST_COST_CONTRACT))
            ELSE
               CURRENT_CONTRACT_CAPACITY = &
                  MIN(CURRENT_CONTRACT_CAPACITY, &
                  MAXIMUM_CAPACITY(LOWEST_COST_CONTRACT) - &
                  CONTRACT_CAPACITY(LOWEST_COST_CONTRACT))
            ENDIF
         ELSE
            CURRENT_CONTRACT_CAPACITY = &
               MAXIMUM_CAPACITY(LOWEST_COST_CONTRACT) - &
                                CONTRACT_CAPACITY(LOWEST_COST_CONTRACT)
            LD_PTS = ISTOP
         ENDIF

!        SURPLUS CAPACITY AVAILABLE

         LIMITED_ENERGY = &
                  MAXIMUM_ENERGY(LOWEST_COST_CONTRACT)/HOURS - &
                                  CONTRACT_ENERGY(LOWEST_COST_CONTRACT)
            A = B
            B = B + MAX(CURRENT_CONTRACT_CAPACITY*1.0001,.0001)
! 12/23/94. GAT. TESTING FOR PALO ALTO
            IF(LAST_ENERGY_ONLY_PRICE < NEXT_LOWEST_VARIABLE_COST) THEN
               ENERGY_ONLY_FOR_DISPATCHING = MAX(0., &
                           MIN(LIMITED_ENERGY,REMAINING_ENRG_ONLY_ENRG))
            ELSE
               ENERGY_ONLY_FOR_DISPATCHING = 0.0
            ENDIF
!
!
!     LOAD THE LOWEST_COST_CONTRACT INTO THE ELDC FROM B TO
!     B PLUS CONTRACT_CAPACITY, OR UNTIL MAXIMUM_CONTRACT_ENERGY
!
         CALL CALC_CONTRACT_ENERGY(LODDUR,LPROB,DX,A,B, &
                           CURRENT_CONTRACT_ENERGY, &
                           LEFT,RIGHT,LIMITED_ENERGY, &
                           ISTART,LAST_POINT, &
                           ENERGY_ONLY_FOR_DISPATCHING, &
!
                           ENRG_ONLY_ENRG_USED)
!
! 12/23/94. GAT. TESTING FOR PALO ALTO
         IF(LAST_ENERGY_ONLY_PRICE < &
                                    NEXT_LOWEST_VARIABLE_COST) &
                  REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG - &
                                                     ENRG_ONLY_ENRG_USED
!
!        TAKE INTO ACCOUNT PROBLEM WITH PEAK SHIFT ALGORITHM
!
         IF((CURRENT_CONTRACT_ENERGY + ENRG_ONLY_ENRG_USED) .GT. &
                                                 REMAINING_ENERGY .OR. &
                     (RIGHT .EQ. 0 .AND. REMAINING_ENERGY .GT. 0.)) THEN
            REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG + &
                                                     ENRG_ONLY_ENRG_USED
! 3/18/93. GAT. THE CONDITIONAL BELOW WAS PUT BACK-IN FOR PALO ALTO
            IF(CURRENT_CONTRACT_ENERGY .GE. REMAINING_ENERGY) THEN
               ENRG_ONLY_ENRG_USED = 0.
               CURRENT_CONTRACT_ENERGY = REMAINING_ENERGY
            ELSE
               ENRG_ONLY_ENRG_USED = REMAINING_ENERGY - &
                                                 CURRENT_CONTRACT_ENERGY
               REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG - &
                                                     ENRG_ONLY_ENRG_USED
            ENDIF
         ENDIF

! PALO ALTO 2 TIER

         IF(CNTR_ENERGY_SWITCH(LOWEST_COST_CONTRACT) == 'T' .AND. &
               (CONTRACT_CAPACITY(LOWEST_COST_CONTRACT)+B-A > 0.0) .AND. &
               (CONTRACT_ENERGY(LOWEST_COST_CONTRACT) + &
                                          CURRENT_CONTRACT_ENERGY)/ &
               (CONTRACT_CAPACITY(LOWEST_COST_CONTRACT)+B-A)> 0.70) THEN
            TIER_2_ENERGY = &
               (CONTRACT_ENERGY(LOWEST_COST_CONTRACT) + &
                                    CURRENT_CONTRACT_ENERGY) - &
               (CONTRACT_CAPACITY(LOWEST_COST_CONTRACT)+B-A)  * 0.70
            TIER_2_CT = LOWEST_COST_CONTRACT
!
!
            IF(ABS(CURRENT_CONTRACT_ENERGY - REMAINING_ENERGY) < .002 &
                                   .AND. AVAILABLE_CONTRACT_NO > 1) THEN
! 12/23/94. GAT. MOVED FROM ABOVE THE IF
               IF(REMAINING_ENRG_ONLY_ENRG > 0. .AND. &
                        LAST_ENERGY_ONLY_PRICE < &
                                 CT_SECOND_ENERGY_PRICE(TIER_2_CT)) THEN
                  TIER_2_TEMP = MIN(REMAINING_ENRG_ONLY_ENRG, &
                               TIER_2_ENERGY,CONTRACT_ENERGY(TIER_2_CT))
                  CONTRACT_ENERGY(TIER_2_CT)=CONTRACT_ENERGY(TIER_2_CT) &
                                                           - TIER_2_TEMP
                  TIER_2_ENERGY = TIER_2_ENERGY - TIER_2_TEMP
                  REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG - &
                                                             TIER_2_TEMP
               ENDIF
               CONTRACT = 1
               DO WHILE(CONTRACT <= NUMBER_OF_CONTRACTS .AND. &
                                                   TIER_2_ENERGY > 0.0 )
                  IF(AVAILABLE_CONTRACT(CONTRACT) .AND. &
                        CNTR_ENERGY_SWITCH(CONTRACT) /= 'T' .AND. &
                        CNTR_CAPACITY_SWITCH(CONTRACT) /= 'E') THEN
                     TIER_2_MARGINAL_COST = &
                        CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT)*.7+ &
                        CT_SECOND_ENERGY_PRICE(LOWEST_COST_CONTRACT)*.3+ &
                        CONTRACT_FIXED_COST(LOWEST_COST_CONTRACT)*12000/ &
                        (HOURS*PRODUCTION_COST_PERIODS)
                     IF(CNTR_CAPACITY_SWITCH(CONTRACT) /= 'V') THEN
                        CANDIDATE_COST = &
                          CONTRACT_VARIABLE_COST(CONTRACT)
                     ELSE
                        CANDIDATE_COST = &
                           CONTRACT_VARIABLE_COST(CONTRACT) + &
                           CONTRACT_FIXED_COST(CONTRACT)*12000 / &
                           (HOURS*PRODUCTION_COST_PERIODS)
                     ENDIF
                     IF(CANDIDATE_COST < TIER_2_MARGINAL_COST) THEN
                        TIER_2_TEMP = MIN( &
                           ( (CONTRACT_ENERGY(LOWEST_COST_CONTRACT) + &
                                            CURRENT_CONTRACT_ENERGY) &
                           - 0.7*( &
                               CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) &
                                                         + B - A) )/0.3, &
                           MAXIMUM_ENERGY(CONTRACT)/HOURS - &
                           CONTRACT_ENERGY(CONTRACT), &
                           MAXIMUM_CAPACITY(CONTRACT) - &
                           CONTRACT_CAPACITY(CONTRACT))
                        CONTRACT_ENERGY(CONTRACT) = &
                           CONTRACT_ENERGY(CONTRACT) + TIER_2_TEMP
                        CONTRACT_CAPACITY(CONTRACT) = &
                           CONTRACT_CAPACITY(CONTRACT) + TIER_2_TEMP
                        CURRENT_CONTRACT_ENERGY = &
                           CURRENT_CONTRACT_ENERGY - TIER_2_TEMP
                        B = B  - TIER_2_TEMP
                        TIER_2_ENERGY = TIER_2_ENERGY - TIER_2_TEMP
                     ENDIF
                  ENDIF
                  CONTRACT = CONTRACT + 1
               ENDDO
            ENDIF
         ELSEIF(TIER_2_ENERGY > 0.0 .AND. &
                  CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT) &
                           < CT_SECOND_ENERGY_PRICE(TIER_2_CT) .AND. &
                  LOWEST_COST_CONTRACT /= TIER_2_CT .AND. &
                  CONTRACT_ENERGY(LOWEST_COST_CONTRACT) + &
                                              CURRENT_CONTRACT_ENERGY < &
                  MAXIMUM_ENERGY(LOWEST_COST_CONTRACT)/HOURS) THEN
            TIER_2_TEMP = &
               MIN(TIER_2_ENERGY, &
                  CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) + B - A - &
                     CONTRACT_ENERGY(LOWEST_COST_CONTRACT) - &
                                                CURRENT_CONTRACT_ENERGY, &
                  MAXIMUM_ENERGY(LOWEST_COST_CONTRACT)/HOURS - &
                  CONTRACT_ENERGY(LOWEST_COST_CONTRACT) - &
                                                CURRENT_CONTRACT_ENERGY)
            TIER_2_ENERGY = TIER_2_ENERGY - TIER_2_TEMP
            CONTRACT_ENERGY(TIER_2_CT) = CONTRACT_ENERGY(TIER_2_CT) - &
                                                          TIER_2_TEMP
            IF( .NOT. AVAILABLE_CONTRACT(TIER_2_CT) ) THEN
               AVAILABLE_CONTRACT(TIER_2_CT) = .TRUE.
               AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO + 1
            ENDIF
            CONTRACT_ENERGY(LOWEST_COST_CONTRACT) = &
                  CONTRACT_ENERGY(LOWEST_COST_CONTRACT) + TIER_2_TEMP
!
         ENDIF
! END PALO ALTO 2 TIER
!
         CUM_CONTRACT_CAP = CUM_CONTRACT_CAP + B - A
         REMAINING_ENERGY = REMAINING_ENERGY - CURRENT_CONTRACT_ENERGY - &
                            ENRG_ONLY_ENRG_USED
         CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) = &
            CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) + B - A
         CONTRACT_ENERGY(LOWEST_COST_CONTRACT) = &
            CONTRACT_ENERGY(LOWEST_COST_CONTRACT) + &
            CURRENT_CONTRACT_ENERGY
         TOTAL_CONTRACT_ENRG = TOTAL_CONTRACT_ENRG + &
                                                 CURRENT_CONTRACT_ENERGY
!
!     1/11/93. ADDED DUE TO CHECK DIFFERENCES IN PROGRAMS BETWEEN
!     MARK'S AND GREG'S CODE.
!
         IF(CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) .GE. &
                  MAXIMUM_CAPACITY(LOWEST_COST_CONTRACT) .OR. &
                  CONTRACT_ENERGY(LOWEST_COST_CONTRACT) .GE. &
                  MAXIMUM_ENERGY(LOWEST_COST_CONTRACT)/HOURS) THEN
               AVAILABLE_CONTRACT(LOWEST_COST_CONTRACT) = .FALSE.
               AVAILABLE_CONTRACT_NO = AVAILABLE_CONTRACT_NO - 1
         ENDIF
!
!         ADJUST OTHER VARIABLES
!
         IF(RIGHT .LT. .0001) RIGHT = 0.0
         IF(AVAILABLE_CONTRACT_NO .EQ. 0) CONTRACTS_ACTIVE = .FALSE.
      ENDDO
      IF(RIGHT .GT. 0.0 .AND. REMAINING_ENERGY .GT. 0.) THEN
         CONTRACT_DISPATCH_COST = &
                   (CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT) + &
                    CT_ENERGY_COST_ADDER(LOWEST_COST_CONTRACT) + &
                   12*1000*CONTRACT_FIXED_COST(LOWEST_COST_CONTRACT)/ &
                   (HOURS*RIGHT*PRODUCTION_COST_PERIODS))*10.
      ELSE
         REMAINING_ENERGY = 0.0
         CONTRACTS_ACTIVE = .FALSE.
         IF(TIER_2_CT > 0) THEN
           TIER_2_ENERGY = &
               MAX(0.,CONTRACT_ENERGY(TIER_2_CT) - &
                                   CONTRACT_CAPACITY(TIER_2_CT) * 0.70)
            IF(REMAINING_ENRG_ONLY_ENRG > 0. .AND. &
                        LAST_ENERGY_ONLY_PRICE < &
                                 CT_SECOND_ENERGY_PRICE(TIER_2_CT)) THEN
!
                  TIER_2_TEMP = MIN(REMAINING_ENRG_ONLY_ENRG, &
                               TIER_2_ENERGY,CONTRACT_ENERGY(TIER_2_CT))
!
               CONTRACT_ENERGY(TIER_2_CT) = CONTRACT_ENERGY(TIER_2_CT) &
                                                           - TIER_2_TEMP
               TIER_2_ENERGY = TIER_2_ENERGY - TIER_2_TEMP
               REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG - &
                                                             TIER_2_TEMP
            ENDIF
!
            IF( CONTRACT_CAPACITY(TIER_2_CT) > 0. .AND. &
                         CONTRACT_ENERGY(TIER_2_CT)/ &
                             CONTRACT_CAPACITY(TIER_2_CT) > 1.0001) THEN
               CONTRACT_CAPACITY(TIER_2_CT) = &
                                  MIN(CONTRACT_ENERGY(TIER_2_CT), &
                                            MAXIMUM_CAPACITY(TIER_2_CT))
            ENDIF
!
         ENDIF
      ENDIF
      CALL SAVE_TIER_2(TIER_2_ENERGY,TIER_2_CT)
      RETURN
      ENTRY UTILITY_TYPE_IN_CONTRACTS(R_PALO_ALTO_ACTIVE)
         PALO_ALTO_ACTIVE = R_PALO_ALTO_ACTIVE
      RETURN
      END

      SUBROUTINE TIER_2_OBJECT

      REAL ::  TIER_2_ENERGY,R_TIER_2_ENERGY
      INTEGER (kind=2) ::  TIER_2_CT,R_TIER_2_CT
      SAVE TIER_2_ENERGY,TIER_2_CT
      ENTRY INIT_TIER_2
         TIER_2_ENERGY = 0.0
         TIER_2_CT = 0
      RETURN
      ENTRY RETURN_TIER_2(R_TIER_2_ENERGY,R_TIER_2_CT)
         R_TIER_2_ENERGY = TIER_2_ENERGY
         R_TIER_2_CT = TIER_2_CT
      RETURN
      ENTRY SAVE_TIER_2(R_TIER_2_ENERGY,R_TIER_2_CT)
         TIER_2_ENERGY = R_TIER_2_ENERGY
         TIER_2_CT = R_TIER_2_CT
      RETURN
      END

      SUBROUTINE SURPLUS_CAP_OBJECT

      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use contracts_data

      INTEGER (kind=2) ::  CONTRACT
      INTEGER (kind=2) ::  SURPLUS_CAP_ORDER(MAX_CONTRACTS)
      INTEGER (kind=2) ::  MAX_SUR
      INTEGER (kind=2) ::  CUR_SUR
      INTEGER (kind=2) ::  LOWEST_COST_CONTRACT
      INTEGER (kind=2) ::  CUR_POS
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  START_SURPLUS
      INTEGER (kind=2) ::  END_SURPLUS
      INTEGER (kind=2) ::  FIRST_ECONOMIC_RESOURCE
      REAL ::  SURPLUS_CAP(MAX_CONTRACTS)
      REAL ::  DISPATCHING_CAPACITY
      REAL ::  A
      REAL ::  B
      REAL ::  DELTA
      REAL ::  TOTAL_SURPLUS_CAPACITY
      REAL ::  CAPACITY
      REAL ::  R_DISPATCHING_CAPACITY
      REAL ::  CURRENT_CONTRACT_CAPACITY
      REAL ::  CONTRACT_CAPACITY(MAX_CONTRACTS)
      REAL ::  CURRENT_CONTRACT_ENERGY
      REAL ::  MAXIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  UNUSED_SURPLUS
      REAL ::  USED_SURPLUS
      REAL ::  ADJUSTMENT_CAPACITY
      REAL ::  NEXT_LOWEST_TOTAL_COST
      REAL ::  FIXED_COST_FACTOR
      REAL ::  S_NEXT_LOWEST_TOTAL_COST
      REAL ::  S_FIXED_COST_FACTOR
      REAL ::  TEST_TOTAL_COST
      REAL ::  CURRENT_SURPLUS
      REAL ::  CAPACITY_ZERO=.00001
      REAL ::  ECONOMIC_CAPACITY_AVAILABLE
      LOGICAL (kind=1) ::  SURPLUS_CAPACITY_ACTIVE
      LOGICAL (kind=1) ::  R_SURPLUS_CAPACITY_ACTIVE
      LOGICAL (kind=1) ::  SURPLUS_CAP_CONTRACT(MAX_CONTRACTS)
      LOGICAL (kind=1) ::  FIXED_COST_LOGIC
      LOGICAL (kind=1) ::  R_FIXED_COST_LOGIC
      LOGICAL (kind=1) ::  PASSED_ECONOMIC_TEST
      LOGICAL (kind=1) ::  SURPLUS_HAS_CHEAPER_CAPACITY
      LOGICAL (kind=1) ::  DOESNT_DISPLACE_CAPACITY
      LOGICAL (kind=1) ::  FIRST_SURPLUS_DISPATCHED
      LOGICAL (kind=1) ::  FIRST_ECONOMIC_RESOURCE_FOUND
      LOGICAL (kind=4) ::  FROM_INITIALIZE_ROUTINE
      SAVE SURPLUS_CAP,SURPLUS_CAP_ORDER,MAX_SUR,CUR_SUR, &
            TOTAL_SURPLUS_CAPACITY,SURPLUS_CAPACITY_ACTIVE, &
            FIXED_COST_LOGIC,SURPLUS_CAP_CONTRACT,USED_SURPLUS, &
            ADJUSTMENT_CAPACITY,PASSED_ECONOMIC_TEST, &
            S_NEXT_LOWEST_TOTAL_COST,S_FIXED_COST_FACTOR, &
            TEST_TOTAL_COST,START_SURPLUS,END_SURPLUS, &
            FIRST_ECONOMIC_RESOURCE

      ENTRY INIT_SURPLUS_CAP ! INSIDE INITIALIZE CONTRACTS

         TOTAL_SURPLUS_CAPACITY = 0.0
         SURPLUS_CAPACITY_ACTIVE = .FALSE.
         FIXED_COST_LOGIC = .TRUE.
         MAX_SUR = 0
         CUR_SUR = 0
         ADJUSTMENT_CAPACITY = 0.0
         DO CONTRACT = 1 , MAX_CONTRACTS
            SURPLUS_CAP_CONTRACT(CONTRACT) = .FALSE.
            SURPLUS_CAP_ORDER(CONTRACT) = 0
            SURPLUS_CAP(CONTRACT) = 0.0
         ENDDO

      RETURN
      ENTRY IS_SURPLUS_CAP_ACTIVE(R_SURPLUS_CAPACITY_ACTIVE)
         R_SURPLUS_CAPACITY_ACTIVE = SURPLUS_CAPACITY_ACTIVE
      RETURN
      ENTRY USING_FIXED_LOGIC(R_FIXED_COST_LOGIC)
         R_FIXED_COST_LOGIC = FIXED_COST_LOGIC
      RETURN

      ENTRY DISPATCH_SURPLUS_CAPACITY( R_DISPATCHING_CAPACITY, &
                                       LOWEST_COST_CONTRACT, &
                                       NEXT_LOWEST_TOTAL_COST, &
                                       FIXED_COST_FACTOR)

         DISPATCHING_CAPACITY = R_DISPATCHING_CAPACITY
         S_NEXT_LOWEST_TOTAL_COST = NEXT_LOWEST_TOTAL_COST
         S_FIXED_COST_FACTOR = FIXED_COST_FACTOR
         START_SURPLUS = 0
         END_SURPLUS = 0
         FIRST_SURPLUS_DISPATCHED = .TRUE.
         FIRST_ECONOMIC_RESOURCE = 999
         FIRST_ECONOMIC_RESOURCE_FOUND = .FALSE.
         CONTRACT = 1
         CUR_POS = SURPLUS_CAP_ORDER(CONTRACT)
         DO WHILE(CONTRACT <= MAX_SUR)
            IF(SURPLUS_CAP(CUR_POS) <= 0. .OR. &
                                   CUR_POS == LOWEST_COST_CONTRACT) THEN
               CONTRACT = CONTRACT + 1
               CUR_POS = SURPLUS_CAP_ORDER(CONTRACT)
               CYCLE
            ENDIF
            IF(CNTR_CAPACITY_SWITCH(CUR_POS) == 'V') THEN
               TEST_TOTAL_COST = &
                    10.*CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT) + &
                        S_FIXED_COST_FACTOR*CONTRACT_FIXED_COST(CUR_POS)
            ELSE
               TEST_TOTAL_COST = &
                            CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT)
            ENDIF
            PASSED_ECONOMIC_TEST = &
                                TEST_TOTAL_COST < NEXT_LOWEST_TOTAL_COST
            SURPLUS_HAS_CHEAPER_CAPACITY = &
                           CNTR_CAPACITY_SWITCH(CUR_POS) /= 'V' .OR. &
                           (CNTR_CAPACITY_SWITCH(CUR_POS) == 'V' .AND. &
                           CNTR_CAPACITY_SWITCH(LOWEST_COST_CONTRACT) &
                                                            == 'V' .AND. &
                           CONTRACT_FIXED_COST(LOWEST_COST_CONTRACT) > &
                                     CONTRACT_FIXED_COST(CUR_POS))
            IF( SURPLUS_HAS_CHEAPER_CAPACITY .OR. &
                                              PASSED_ECONOMIC_TEST) THEN
               IF( .NOT. FIRST_ECONOMIC_RESOURCE_FOUND .AND. &
                          PASSED_ECONOMIC_TEST .AND. &
                               .NOT. SURPLUS_HAS_CHEAPER_CAPACITY) THEN
                  FIRST_ECONOMIC_RESOURCE = CONTRACT
                  FIRST_ECONOMIC_RESOURCE_FOUND = .TRUE.
               ENDIF
               R_DISPATCHING_CAPACITY = R_DISPATCHING_CAPACITY + &
                                                    SURPLUS_CAP(CUR_POS)
               IF(FIRST_SURPLUS_DISPATCHED) THEN
                  START_SURPLUS = CONTRACT
                  FIRST_SURPLUS_DISPATCHED = .FALSE.
               ENDIF
               END_SURPLUS = CONTRACT
            ELSE
               EXIT
            ENDIF
            CONTRACT = CONTRACT + 1
            CUR_POS = SURPLUS_CAP_ORDER(CONTRACT)
         ENDDO
      RETURN

      ENTRY REDUCE_SURPLUS_CAPACITY(A,B,CURRENT_CONTRACT_CAPACITY, &
                     CURRENT_CONTRACT_ENERGY,CONTRACT_CAPACITY, &
                     LOWEST_COST_CONTRACT,MAXIMUM_CAPACITY, &
                     FROM_INITIALIZE_ROUTINE)

!
         CAPACITY = B - A - MAX(0.,CURRENT_CONTRACT_ENERGY - &
                                CONTRACT_CAPACITY(LOWEST_COST_CONTRACT))
!
         ECONOMIC_CAPACITY_AVAILABLE = MAX(0., B - A + &
                               CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) - &
                                 MAXIMUM_CAPACITY(LOWEST_COST_CONTRACT))
         UNUSED_SURPLUS = CAPACITY
         USED_SURPLUS = 0.
!
         IF(START_SURPLUS > 0 .AND. CAPACITY > CAPACITY_ZERO .AND. &
                                     .NOT. FROM_INITIALIZE_ROUTINE) THEN
!
            CURRENT_SURPLUS = 0.
!
            CONTRACT = START_SURPLUS
            CUR_POS = SURPLUS_CAP_ORDER(CONTRACT)
!
            DO WHILE( CONTRACT <= END_SURPLUS .AND. &
                                               CAPACITY > CAPACITY_ZERO)
!
               IF(SURPLUS_CAP(CUR_POS) <= 0. .OR. &
                                   CUR_POS == LOWEST_COST_CONTRACT) THEN
                  CONTRACT = CONTRACT + 1
                  CUR_POS = SURPLUS_CAP_ORDER(CONTRACT)
                  CYCLE
               ENDIF
!
               DELTA = MIN(SURPLUS_CAP(CUR_POS),CAPACITY)
!
               IF(CONTRACT >= FIRST_ECONOMIC_RESOURCE) THEN
                  IF(ECONOMIC_CAPACITY_AVAILABLE <= 0.) EXIT
                  DELTA = MIN(DELTA,ECONOMIC_CAPACITY_AVAILABLE)
               ENDIF
               ECONOMIC_CAPACITY_AVAILABLE = &
                                     ECONOMIC_CAPACITY_AVAILABLE - DELTA
!
               CAPACITY = CAPACITY - DELTA
!
               CONTRACT_CAPACITY(CUR_POS) = &
                                 CONTRACT_CAPACITY(CUR_POS) + DELTA
               SURPLUS_CAP(CUR_POS) = SURPLUS_CAP(CUR_POS) - DELTA
               IF(SURPLUS_CAP(CUR_POS) == 0.0) THEN
                  SURPLUS_CAP_CONTRACT(CUR_POS) = .FALSE.
                  CONTRACT = CONTRACT + 1
                  CUR_POS = SURPLUS_CAP_ORDER(CONTRACT)
               ENDIF
!
               CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) = &
                     CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) - DELTA
!
               TOTAL_SURPLUS_CAPACITY = TOTAL_SURPLUS_CAPACITY - DELTA
            ENDDO
         ENDIF
!
         USED_SURPLUS = UNUSED_SURPLUS - CAPACITY
         IF(SURPLUS_CAP_CONTRACT(LOWEST_COST_CONTRACT)) THEN
            UNUSED_SURPLUS = &
                      MIN((B - A) - USED_SURPLUS, &
                      SURPLUS_CAP(LOWEST_COST_CONTRACT))
            SURPLUS_CAP(LOWEST_COST_CONTRACT) = &
                      SURPLUS_CAP(LOWEST_COST_CONTRACT) - UNUSED_SURPLUS
            TOTAL_SURPLUS_CAPACITY = &
                                 TOTAL_SURPLUS_CAPACITY - UNUSED_SURPLUS
         ENDIF
         IF(TOTAL_SURPLUS_CAPACITY == 0.0) &
                                       SURPLUS_CAPACITY_ACTIVE = .FALSE.
      RETURN

      ENTRY ALTER_CURRENT_SURPLUS_CAPACITY(A,B, &
                               CURRENT_CONTRACT_CAPACITY, &
                               LOWEST_COST_CONTRACT)

         CAPACITY = CURRENT_CONTRACT_CAPACITY - B + A
         IF(SURPLUS_CAP(LOWEST_COST_CONTRACT) > 0.0 ) THEN
            CAPACITY = CAPACITY + USED_SURPLUS
            DELTA = CAPACITY - SURPLUS_CAP(LOWEST_COST_CONTRACT)
         ELSEIF(CURRENT_CONTRACT_CAPACITY == 0.0 ) THEN
            RETURN ! NOT ACTIVE SURPLUS
         ELSEIF(MAX_SUR > 0) THEN
            DELTA = CAPACITY
            MAX_SUR = MAX_SUR + 1
            SURPLUS_CAP_ORDER(MAX_SUR) = LOWEST_COST_CONTRACT
            DO CONTRACT = CUR_SUR, MAX_SUR-1
               CUR_POS = SURPLUS_CAP_ORDER(CONTRACT)
               IF(  (CNTR_CAPACITY_SWITCH(CUR_POS) /= 'V' .AND. &
                  CNTR_CAPACITY_SWITCH(LOWEST_COST_CONTRACT)/='V') .OR. &
                    (CNTR_CAPACITY_SWITCH(CUR_POS) /= 'V' .AND. &
                  CNTR_CAPACITY_SWITCH(LOWEST_COST_CONTRACT)=='V') .OR. &
                           (CNTR_CAPACITY_SWITCH(CUR_POS) == 'V' .AND. &
                 CNTR_CAPACITY_SWITCH(LOWEST_COST_CONTRACT) == 'V' .AND. &
                           CONTRACT_FIXED_COST(LOWEST_COST_CONTRACT) > &
                                 CONTRACT_FIXED_COST(CUR_POS))   ) CYCLE
!
!
               DO I = MAX_SUR, CONTRACT + 1 , -1
                  SURPLUS_CAP_ORDER(I) = SURPLUS_CAP_ORDER(I-1)
               ENDDO

               SURPLUS_CAP_ORDER(CONTRACT) = LOWEST_COST_CONTRACT
               EXIT
            ENDDO
         ELSE
            DELTA = CAPACITY
            MAX_SUR = 1
            CUR_SUR = 1
            SURPLUS_CAP_ORDER(1) = LOWEST_COST_CONTRACT
         ENDIF
         SURPLUS_CAP(LOWEST_COST_CONTRACT) = CAPACITY
         IF(SURPLUS_CAP(LOWEST_COST_CONTRACT) > 0.0) THEN
            SURPLUS_CAP_CONTRACT(LOWEST_COST_CONTRACT) = .TRUE.
         ELSE
            SURPLUS_CAP_CONTRACT(LOWEST_COST_CONTRACT) = .FALSE.
         ENDIF
         TOTAL_SURPLUS_CAPACITY = TOTAL_SURPLUS_CAPACITY + DELTA
         IF(TOTAL_SURPLUS_CAPACITY > 0.0) THEN
            SURPLUS_CAPACITY_ACTIVE = .TRUE.
         ELSE
            SURPLUS_CAPACITY_ACTIVE = .FALSE.
         ENDIF
      RETURN

      ENTRY OLD_REDUCE_SURPLUS_CAPACITY(A,B,CURRENT_CONTRACT_CAPACITY, &
                    CURRENT_CONTRACT_ENERGY,CONTRACT_CAPACITY, &
                    LOWEST_COST_CONTRACT,MAXIMUM_CAPACITY)

         CAPACITY = B - A - MAX(0.,CURRENT_CONTRACT_ENERGY - &
                                CONTRACT_CAPACITY(LOWEST_COST_CONTRACT))
         UNUSED_SURPLUS = CAPACITY
         USED_SURPLUS = 0.
         CURRENT_SURPLUS = 0.
         CUR_POS = SURPLUS_CAP_ORDER(CUR_SUR)
         DO WHILE(CUR_SUR <= MAX_SUR .AND. CAPACITY > 0.)
! 9/13/94. GAT. TEMP ADD.
            IF(CUR_POS == LOWEST_COST_CONTRACT .AND. &
                                              PASSED_ECONOMIC_TEST) THEN
               SURPLUS_CAP_CONTRACT(CUR_POS) = .FALSE.
               CUR_SUR = CUR_SUR + 1
               CUR_POS = SURPLUS_CAP_ORDER(CUR_SUR)
               IF(CUR_SUR > MAX_SUR) EXIT
            ENDIF
            IF(CNTR_CAPACITY_SWITCH(CUR_POS) == 'V') THEN
               TEST_TOTAL_COST = &
                      10.*CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT) + &
                        S_FIXED_COST_FACTOR*CONTRACT_FIXED_COST(CUR_POS)
            ELSE
               TEST_TOTAL_COST = &
                            CONTRACT_VARIABLE_COST(LOWEST_COST_CONTRACT)
            ENDIF
            CURRENT_SURPLUS = CURRENT_SURPLUS + SURPLUS_CAP(CUR_POS)
            DOESNT_DISPLACE_CAPACITY = &
                     (B-A+CONTRACT_CAPACITY(LOWEST_COST_CONTRACT)) - &
                     CURRENT_SURPLUS > &
                                  MAXIMUM_CAPACITY(LOWEST_COST_CONTRACT)
            SURPLUS_HAS_CHEAPER_CAPACITY = &
                           CNTR_CAPACITY_SWITCH(CUR_POS) /= 'V' .OR. &
                           (CNTR_CAPACITY_SWITCH(CUR_POS) == 'V' .AND. &
                           CNTR_CAPACITY_SWITCH(LOWEST_COST_CONTRACT) &
                                                            == 'V' .AND. &
                           CONTRACT_FIXED_COST(LOWEST_COST_CONTRACT) > &
                                     CONTRACT_FIXED_COST(CUR_POS))
            IF(SURPLUS_HAS_CHEAPER_CAPACITY .OR. &
                        (DOESNT_DISPLACE_CAPACITY .AND. &
                     TEST_TOTAL_COST < S_NEXT_LOWEST_TOTAL_COST)  ) THEN

! ADDED 1/6/94. GAT. GIRISH WWP PROBLEM
               DELTA = MIN(SURPLUS_CAP(CUR_POS),CAPACITY)

            ELSE
               EXIT
            ENDIF
            CAPACITY = CAPACITY - DELTA
!
            CONTRACT_CAPACITY(CUR_POS) = &
                                 CONTRACT_CAPACITY(CUR_POS) + DELTA
            SURPLUS_CAP(CUR_POS) = SURPLUS_CAP(CUR_POS) - DELTA
            IF(SURPLUS_CAP(CUR_POS) == 0.0) THEN
               SURPLUS_CAP_CONTRACT(CUR_POS) = .FALSE.
               CUR_SUR = CUR_SUR + 1
               CUR_POS = SURPLUS_CAP_ORDER(CUR_SUR)
            ENDIF
!
            CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) = &
                     CONTRACT_CAPACITY(LOWEST_COST_CONTRACT) - DELTA
!
            TOTAL_SURPLUS_CAPACITY = TOTAL_SURPLUS_CAPACITY - DELTA

         ENDDO
         IF(SURPLUS_CAP_CONTRACT(LOWEST_COST_CONTRACT)) THEN
            USED_SURPLUS = UNUSED_SURPLUS - CAPACITY
            UNUSED_SURPLUS = &
                      MIN((B - A) - USED_SURPLUS, &
                      SURPLUS_CAP(LOWEST_COST_CONTRACT))
            SURPLUS_CAP(LOWEST_COST_CONTRACT) = &
                     SURPLUS_CAP(LOWEST_COST_CONTRACT) - UNUSED_SURPLUS
            TOTAL_SURPLUS_CAPACITY = &
                                TOTAL_SURPLUS_CAPACITY - UNUSED_SURPLUS
         ENDIF
         IF(TOTAL_SURPLUS_CAPACITY == 0.0) &
                           SURPLUS_CAPACITY_ACTIVE = .FALSE.
         ADJUSTMENT_CAPACITY = 0.0
      RETURN

      ENTRY CREDIT_SURPLUS_CAPACITY(A,B,LOWEST_COST_CONTRACT)

         IF(SURPLUS_CAP_CONTRACT(LOWEST_COST_CONTRACT) ) THEN
            DELTA = MIN(B-A,SURPLUS_CAP(LOWEST_COST_CONTRACT))
            SURPLUS_CAP(LOWEST_COST_CONTRACT) = &
                              SURPLUS_CAP(LOWEST_COST_CONTRACT) - DELTA
            TOTAL_SURPLUS_CAPACITY = TOTAL_SURPLUS_CAPACITY - DELTA

            IF(TOTAL_SURPLUS_CAPACITY == 0.0) &
                           SURPLUS_CAPACITY_ACTIVE = .FALSE.
         ENDIF
      RETURN
      END

      FUNCTION ENERGY_ONLY_PRICE_OBJECT()

      REAL ::  R_LAST_ENERGY_ONLY_PRICE
      REAL ::  GET_LAST_ENERGY_ONLY_PRICE
      REAL ::  SAVE_ENERGY_ONLY_PRICE
      REAL ::  S_LAST_ENERGY_ONLY_PRICE
      REAL ::  ENERGY_ONLY_PRICE_OBJECT
      REAL ::  INIT_ENERGY_ONLY_PRICE
      SAVE R_LAST_ENERGY_ONLY_PRICE
      ENERGY_ONLY_PRICE_OBJECT = 1.
      ENTRY INIT_ENERGY_ONLY_PRICE()
         R_LAST_ENERGY_ONLY_PRICE = 999999999.
         INIT_ENERGY_ONLY_PRICE = 2.
      RETURN
      ENTRY SAVE_ENERGY_ONLY_PRICE(S_LAST_ENERGY_ONLY_PRICE)
         R_LAST_ENERGY_ONLY_PRICE = S_LAST_ENERGY_ONLY_PRICE
         SAVE_ENERGY_ONLY_PRICE = 3.
      RETURN
      ENTRY GET_LAST_ENERGY_ONLY_PRICE()
         GET_LAST_ENERGY_ONLY_PRICE = R_LAST_ENERGY_ONLY_PRICE
      RETURN
      END

      FUNCTION CONTRACT_3808()
      use enrglimit
      implicit none
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  S_ISEAS
      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  PEAK_MONTH
      INTEGER (kind=2) ::  S_PEAK_MONTH
      INTEGER (kind=2) ::  R_PEAK_MONTH
      REAL ::  CAPACITY_3808
      REAL ::  SAVE_CAP_3808
      REAL ::  RETURN_CAP_3808
      REAL ::  S_CAP_3808
      REAL ::  INIT_CAP_3808
      LOGICAL (kind=1) ::  CONTRACT_3808
      LOGICAL (kind=1) ::  STATUS_3808=.FALSE.
      LOGICAL (kind=1) ::  SAVE_ACTIVE_3808
      LOGICAL (kind=1) ::  R_ACTIVE_3808
      LOGICAL (kind=1) ::  IS_3808_ACTIVE
      LOGICAL (kind=4) ::   R_STATUS_3808
      SAVE ISEAS,PEAK_MONTH,CAPACITY_3808
      CONTRACT_3808 = .TRUE.
      ENTRY INIT_CAP_3808(S_ISEAS,S_PEAK_MONTH,R_ACTIVE_3808)
         CAPACITY_3808 = 0.0
         ISEAS = S_ISEAS
         PEAK_MONTH = S_PEAK_MONTH
         INIT_CAP_3808 = CAPACITY_3808
         R_ACTIVE_3808 = STATUS_3808
      RETURN
      ENTRY SAVE_ACTIVE_3808(R_STATUS_3808)
         STATUS_3808 = R_STATUS_3808
         SAVE_ACTIVE_3808 = STATUS_3808
      RETURN
      ENTRY SAVE_CAP_3808(S_CAP_3808)
         CAPACITY_3808 = S_CAP_3808
         SAVE_CAP_3808 = CAPACITY_3808
      RETURN
      ENTRY RETURN_CAP_3808(R_ISEAS,R_PEAK_MONTH,R_ACTIVE_3808)
         RETURN_CAP_3808 = CAPACITY_3808
         R_ISEAS = ISEAS
         R_PEAK_MONTH = PEAK_MONTH
         R_ACTIVE_3808 = STATUS_3808
      RETURN
      ENTRY IS_3808_ACTIVE()
         IS_3808_ACTIVE = STATUS_3808
      RETURN
      END
