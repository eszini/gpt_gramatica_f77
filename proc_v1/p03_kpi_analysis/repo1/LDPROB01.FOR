!     ******************************************************************
!     LDPROB01.FOR
!     Copyright(c)  2000
!
!     Created: 11/15/2006 3:49:10 PM
!     Author : MARK S GERBER
!     Last change: MSG 11/15/2006 3:49:10 PM
!     ******************************************************************

C***********************************************************************
C
C ROUTINE TO CALCULATE THE LOAD PROBABILITY CURVE ON A 51 POINT GRID
C
C***********************************************************************
C
      SUBROUTINE LOADPROB(HOURS,HOURS_INCREMENT,FRCLOD,TENRG,
     +                    PEAK_BEFORE_DSM,BASE_BEFORE_DSM,
     +                    PEAK_AFTER_DSM,BASE_AFTER_DSM,
     +                    LDMGT_ACTIVE,ISEAS)
C

      USE SpinDriftLib
      USE prod_arrays_dimensions
      USE SIZECOM
      use eightyeight
      LOGICAL(kind=1) :: LDMGT_ACTIVE
      INTEGER(kind=2) :: I,HR,IMAX,HOURS,HOURS_INCREMENT,IPEAK,COUNT,

     +            INTERVALS,COUNTER,POINTS_IN_CURVE,ISEAS
      INTEGER(kind=2) :: IPNT,IPOT,STORE_IPNT
      REAL :: PEAK,BASE,OBS(LOAD_CURVE_POINTS),DELTA_PROB,AREA,
     +     AVE_BASE_RR,MIN_LPROB,OBSERVATIONS
      REAL :: LPROB(LOAD_CURVE_POINTS),DX,LODDUR(LOAD_CURVE_POINTS),
     +     RUNNING_RATES(LOAD_CURVE_POINTS),
     +     FRCLOD(8800),AVE_RUNNING_RATE(LOAD_CURVE_POINTS),
     +     PEAK_BEFORE_DSM,PEAK_AFTER_DSM,
     +     BASE_BEFORE_DSM,BASE_AFTER_DSM,LOAD_VAL,PEAK_DX,BASE_DX,
     +     REF_LOAD,BASE_ADJUSTMENT

      LOGICAL(kind=1) :: ECON_SWITCH,ECON_SWITCH_ACTIVE

      REAL(kind=8) :: TENRG,DEMAND,ENERGY(LOAD_CURVE_POINTS),
     +                ALPHA,PRECISN



C
      REAL(kind=4) :: TEMP_INTERVALS
      LOGICAL(kind=1) :: SET_POINTS
      INTEGER(kind=2) :: SET_CURRENT_IPNT
C
!     END OF DATA DECLARATIONS
C      
C
C     THERE ARE TO BE IPNT POINTS ON THE LDC, 
C     WHERE IPNT IS AN ODD NUMBER, AND IPNT-1 ENERGY INTERVALS.
C     THE BASE IS CONTAINED IN THE FIRST ENERGY INTERVAL AND THE
C     PEAK IS CONTAINED IN THE LAST ENERGY INTERVAL. THE LOWER BOUND
C     OF THE LDC IS THE BASE AND THE UPPER BOUND IS THE PEAK. THE
C     PROBABILITY ASSOCIATED WITH THE FIRST LDC POINT IS 1 AND THE 
C     PROBABILITY ASSOCIATED WITH THE LAST LDC POINT IS ZERO. 
C
      ECON_SWITCH_ACTIVE = ECON_SWITCH()
!
! MOVED INSIDE SNSZMDS.FOR. 1/28/97. GAT.
!
!      AFTER_EL_MARGINAL_COSTS = SORT_EL_MARGINAL_COSTS(FRCLOD,
!     +                                      HOURS,HOURS_INCREMENT,ISEAS)
      BASE_ADJUSTMENT = -999.
      IF(SET_POINTS(IPOT,DX)) THEN
C        IPOT = IPNT()
      ELSE
         IF(PEAK_BEFORE_DSM-BASE_BEFORE_DSM < 2.*DX) THEN
            BASE_ADJUSTMENT = 2.*DX 
            FRCLOD(HOURS) = FRCLOD(HOURS) - BASE_ADJUSTMENT
            BASE_BEFORE_DSM = BASE_BEFORE_DSM - BASE_ADJUSTMENT
            TENRG = TENRG - BASE_ADJUSTMENT ! MAKE THIS MORE ROBUST IN THE FUTURE.
         ENDIF
         TEMP_INTERVALS = (PEAK_BEFORE_DSM-BASE_BEFORE_DSM)/DX
         IPOT = 2. * TEMP_INTERVALS
         IF(2*(IPOT/2) == IPOT) THEN
            IPOT = IPOT-1
         ENDIF
         IPOT = SET_CURRENT_IPNT(IPOT)
      ENDIF
      INTERVALS = IPOT/2 + 1
      DX = MAX((PEAK_BEFORE_DSM-BASE_BEFORE_DSM)/FLOAT(INTERVALS-1),1.)
      POINTS_IN_CURVE = IPOT
      COUNT = IPOT
      IF(LDMGT_ACTIVE) THEN
         PEAK = PEAK_AFTER_DSM
         BASE = BASE_AFTER_DSM
      ELSE
         PEAK = PEAK_BEFORE_DSM
         BASE = BASE_BEFORE_DSM
      ENDIF
      DO I = 1, LOAD_CURVE_POINTS
         ENERGY(I)= 0.
         OBS(I) = 0.
         LPROB(I) = 0.
         RUNNING_RATES(I) = 0.
      ENDDO
      LPROB(1) = 1.
      IF(LDMGT_ACTIVE .AND. ((PEAK_BEFORE_DSM .NE. PEAK_AFTER_DSM) .OR.
     +                      (BASE_BEFORE_DSM .NE. BASE_AFTER_DSM))) THEN
         LODDUR(1) = BASE_AFTER_DSM
         COUNT = 1
         IF(ABS(BASE_AFTER_DSM - BASE_BEFORE_DSM) < DX/2.) THEN
            LODDUR(3) = BASE_BEFORE_DSM + DX
            COUNT = 3
         ELSE IF(BASE_AFTER_DSM < BASE_BEFORE_DSM) THEN
            BASE_DX = DX
            IF(2.*(MIN(PEAK_BEFORE_DSM,PEAK_AFTER_DSM)-BASE_AFTER_DSM)/
     +                                DX  > (FLOAT(IPOT)*.8 + 20.)) THEN
               BASE_DX = (BASE_BEFORE_DSM - BASE_AFTER_DSM)/1.9999
            ENDIF
            DOWHILE (LODDUR(COUNT) < BASE_BEFORE_DSM)
               COUNT = COUNT + 2
               LODDUR(COUNT) = LODDUR(COUNT-2) + BASE_DX
            ENDDO
            IF(BASE_BEFORE_DSM - LODDUR(COUNT-2) < .01*DX) 
     +                                                 COUNT = COUNT - 2
            LODDUR(COUNT) = BASE_BEFORE_DSM
         ELSE IF(BASE_AFTER_DSM > BASE_BEFORE_DSM) THEN
            LOAD_VAL = BASE_BEFORE_DSM
            DOWHILE (LOAD_VAL < BASE_AFTER_DSM)
               LOAD_VAL = LOAD_VAL + DX
            ENDDO
            LODDUR(3) = LOAD_VAL
            COUNT = 3
         ENDIF
         REF_LOAD = MIN(PEAK_BEFORE_DSM,PEAK_AFTER_DSM)
         DOWHILE (LODDUR(COUNT) < REF_LOAD)
            COUNT = COUNT + 2
            LODDUR(COUNT) = LODDUR(COUNT-2) + DX
         ENDDO
         IF(REF_LOAD - LODDUR(COUNT-2) < .01*DX) COUNT = COUNT - 2
         LODDUR(COUNT) = REF_LOAD
         PEAK_DX = DX
         IF(LODDUR(COUNT) < PEAK) THEN
            IF(2.*(PEAK-LODDUR(COUNT))/PEAK_DX + FLOAT(COUNT) >
     +                                       FLOAT(LOAD_CURVE_POINTS-3))
     +            PEAK_DX = 2.*(PEAK-LODDUR(COUNT))/
     +                                  FLOAT(LOAD_CURVE_POINTS-3-COUNT)
            IF(PEAK_DX > DX) THEN
               I = PEAK_DX/DX
               PEAK_DX = FLOAT(I+1)*DX
            ENDIF
            DOWHILE (LODDUR(COUNT) < PEAK)
               COUNT = COUNT + 2
               LODDUR(COUNT) = LODDUR(COUNT-2) + PEAK_DX
            ENDDO
         ENDIF
         IF(PEAK - LODDUR(COUNT-2) < .01*DX) COUNT = COUNT - 2
         LODDUR(COUNT) = PEAK
         IMAX = COUNT
         POINTS_IN_CURVE = COUNT
         DO I = COUNT + 1, LOAD_CURVE_POINTS
            LODDUR(I) = LODDUR(I-1) + PEAK_DX
         ENDDO
C
C CALCULATE FREQUENCY DISTRIBUTION
C
         INTERVALS = COUNT/2 + 1
         IPEAK = INTERVALS - 1
         AVE_RUNNING_RATE(COUNT) = 0.
         AVE_BASE_RR = 9999.
         DO HR = HOURS,HOURS+HOURS_INCREMENT-1
            IF(FRCLOD(HR) .GE. BASE) THEN
               LOAD_VAL = FRCLOD(HR)
               I = MIN(MAX(2*int(AINT((LOAD_VAL+.0001-BASE)/DX),2)+1,2),
     +                                              LOAD_CURVE_POINTS-3)
               IF(MOD(I,INT(2,2)) == 0) I = I - 1
               DOWHILE (.NOT. (LODDUR(I) <= LOAD_VAL .AND.
     +                                         LOAD_VAL < LODDUR(I+2)))
                  IF(LODDUR(I) < LOAD_VAL) THEN
                     I = I + 2
                     IF(I >= IMAX) EXIT
                  ELSE
                     I = I - 2
                     IF(I < 1) EXIT
                  ENDIF
               ENDDO
C TEST THE INCREASED INTERVAL
               I = MIN(MAX(I/int(2,2) + int(1,2),int(1,2)),IPEAK)
C CHECK I IN THE INTERVAL
               IF(I>3) THEN
                  IF( LOAD_VAL >  LODDUR(I*2+1) .OR. 
     +                               LOAD_VAL < LODDUR(I*2-1)) THEN
C                     WRITE(6,*) '  '
                  ENDIF
               ENDIF
               OBS(I) = OBS(I) + 1.
               ENERGY(I) = ENERGY(I) + LOAD_VAL
               IF(ECON_SWITCH_ACTIVE) THEN
                  AVE_BASE_RR = MIN(AVE_BASE_RR,HOURLY_RR(HR))
                  AVE_RUNNING_RATE(COUNT) = MAX(AVE_RUNNING_RATE(COUNT),
     +                                                    HOURLY_RR(HR))
                  RUNNING_RATES(I) = MAX(RUNNING_RATES(I),HOURLY_RR(HR))
               ENDIF
            ENDIF
         ENDDO
      ELSE
         LODDUR(1) = BASE
         COUNT = 1
         DOWHILE (LODDUR(COUNT) < PEAK)
            COUNT = COUNT + 2
            LODDUR(COUNT) = LODDUR(COUNT-2) + DX
         ENDDO
         IF(PEAK - LODDUR(COUNT-2) < .01*DX) COUNT = COUNT - 2
         LODDUR(COUNT) = PEAK
         POINTS_IN_CURVE = COUNT
         IMAX = COUNT
         INTERVALS = COUNT/2 + 1
         IPEAK = INTERVALS - 1
         DO I = COUNT + 1, LOAD_CURVE_POINTS
            LODDUR(I) = LODDUR(I-1) + DX
         ENDDO
C
C     PLACES LOADS AND RUNNING RATES INTO IPOT/2 - 1 INTERVALS
C
         AVE_RUNNING_RATE(IPOT) = 0.
         AVE_BASE_RR = 9999.
         DO HR = HOURS,HOURS+HOURS_INCREMENT-1
            IF(FRCLOD(HR) .GE. BASE) THEN
               I = AINT((FRCLOD(HR) +.0001 - BASE)/DX) + 1
               IF(I.GT.IPEAK) I = IPEAK
               OBS(I) = OBS(I) + 1.
               ENERGY(I) = ENERGY(I) + FRCLOD(HR)
               IF(ECON_SWITCH_ACTIVE) THEN
                  AVE_BASE_RR = MIN(AVE_BASE_RR,HOURLY_RR(HR))
                  AVE_RUNNING_RATE(IPOT) = MAX(AVE_RUNNING_RATE(IPOT),
     +                                                    HOURLY_RR(HR))
                  RUNNING_RATES(I) = MAX(RUNNING_RATES(I),HOURLY_RR(HR))
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      AVE_RUNNING_RATE(1) = AVE_BASE_RR
C
C     ALGORITHM CREATES THE LODDUR,LPROB FOR THE LDC. EXACT
C     VALUES FOR THE LDC ARE FOUND FOR "INTERVALS" POINTS.
C     THE REMAINING POINTS, BETWEEN THE "INTERVALS" POINTS, ARE
C     FOUND BY TAKING THE ENERGY FOUND BETWEEN TWO "INTERVALS",
C     FINDING THE WEIGHTED AVERAGE BETWEEN THE TWO INTERVALS
C     (AVE_ENERGY) AND SOLVING FOR THAT VALUE THAT WILL ACHEIVE
C     EXACTLY THE SAME ENERGY FOUND IN THE ORIGINAL INTERVAL.
C
C      AVE_RUNNING_RATE(1) = RUNNING_RATES(1) / OBS(1)
      MIN_LPROB = 1./FLOAT(HOURS_INCREMENT)
      OBSERVATIONS = 0.
      DO I = 2 , INTERVALS
         COUNT = 2*(I) - 2
         OBSERVATIONS = OBSERVATIONS + OBS(I-1)
         DELTA_PROB = 1. - OBSERVATIONS/FLOAT(HOURS_INCREMENT)
C        DELTA_PROB = OBS(I-1)/FLOAT(HOURS_INCREMENT)
C        IF(I .LT. INTERVALS) THEN
C           IF(OBS(I) .GT. 0 ) THEN
C              AVE_RUNNING_RATE(COUNT+1) = RUNNING_RATES(I) / OBS(I))
C           ELSE
C              AVE_RUNNING_RATE(COUNT+1) = AVE_RUNNING_RATE(COUNT-1)
C           ENDIF
C        ENDIF
         IF(OBS(I-1) .LE. 0.) THEN
            LPROB(COUNT+1) = LPROB(COUNT-1)
            LPROB(COUNT) = LPROB(COUNT-1)
            LODDUR(COUNT) = (LODDUR(COUNT+1)+LODDUR(COUNT-1))/2.
         ELSE
            LODDUR(COUNT) = ENERGY(I-1)/OBS(I-1)
            LPROB(COUNT+1) = DELTA_PROB
C           LPROB(COUNT+1) = LPROB(COUNT-1) - DELTA_PROB
            IF(LPROB(COUNT+1) < MIN_LPROB) LPROB(COUNT+1) = 0.
            AREA = (ENERGY(I-1)-LODDUR(COUNT-1)*OBS(I-1))/ 
     +         FLOAT(HOURS_INCREMENT) + 
     +         LPROB(COUNT+1)*(LODDUR(COUNT+1)-LODDUR(COUNT-1))
            LPROB(COUNT) = (2*AREA -
     +         (LPROB(COUNT-1)*(LODDUR(COUNT)-LODDUR(COUNT-1)) +
     +         LPROB(COUNT+1)*(LODDUR(COUNT+1)-LODDUR(COUNT)))  ) /
     +         (LODDUR(COUNT+1)-LODDUR(COUNT-1))
C
C           ADDED 4/30/92 TO TRAP FOR DELTA LOAD = 0
C           ADDED 3/10/93. GAT. TRAP FOR MULTIPLE IDENTICLE RIGHT VALS
C
            IF( ENERGY(I-1)/(OBS(I-1) * LODDUR(COUNT+1))
     +                                  > .999999 ) THEN
               LODDUR(COUNT) = LODDUR(COUNT+1)
               IF(LODDUR(COUNT+1) < 10000) THEN
                  LODDUR(COUNT+1) = LODDUR(COUNT+1) + .001
               ELSE
                  LODDUR(COUNT+1) = LODDUR(COUNT+1) + .01
               ENDIF
            ENDIF
            IF(LPROB(COUNT) .GT. LPROB(COUNT-1)) THEN
               LPROB(COUNT) = LPROB(COUNT-1)
            ELSEIF(LPROB(COUNT) .LT. LPROB(COUNT+1) ) THEN
               LPROB(COUNT) = LPROB(COUNT+1)
            ENDIF
         ENDIF
         IF(ECON_SWITCH_ACTIVE) THEN
            IF(OBS(I-1).GT.0) THEN
               AVE_RUNNING_RATE(COUNT+1) = RUNNING_RATES(I-1)
            ELSE
               AVE_RUNNING_RATE(COUNT+1) = AVE_RUNNING_RATE(COUNT-1)
            ENDIF
            AVE_RUNNING_RATE(COUNT) = AVE_RUNNING_RATE(COUNT-1) +
     +                                (LODDUR(COUNT)-LODDUR(COUNT-1)) *
     +        (AVE_RUNNING_RATE(COUNT+1) - AVE_RUNNING_RATE(COUNT-1)) /
     +                  (LODDUR(COUNT+1) - LODDUR(COUNT-1))
         ENDIF
      ENDDO
C
C     THIS SECTION TAKES THE ROUNDING ERROR FROM THE 
C     PREVIOUS ALGORITHM (SINGLE PRECISION CALC.) AND
C     DISTRIBUTES IT EVENLY ACROSS IPNT-1 POINTS.
C
      COUNTER = 0.
      DO
         CALL INTEG8(DEMAND,LODDUR,LPROB,IMAX,HOURS_INCREMENT,LODDUR(1))
C     ALPHA = (BASE + DX/2.) * FLOAT(HOURS_INCREMENT) OUT 3/25/92
         IF(BASE_ADJUSTMENT == -999.) THEN
            ALPHA = LODDUR(2) * FLOAT(HOURS_INCREMENT)
            ALPHA = (TENRG-ALPHA)/(DEMAND-ALPHA)
         ELSE
            ALPHA = 1.
         ENDIF
         DO I = 2,IMAX
            LPROB(I) = LPROB(I)*ALPHA
         ENDDO
         LPROB(IMAX) = 0.
         CALL INTEG8(DEMAND,LODDUR,LPROB,IMAX,HOURS_INCREMENT,LODDUR(1))
         PRECISN = 1.
         IF(TENRG .GT. DEMAND) THEN
            PRECISN = DEMAND/TENRG
         ELSE
            PRECISN = TENRG/DEMAND
         ENDIF
         COUNTER = COUNTER + 1
         IF(PRECISN > .999999 .OR. COUNTER > 3) EXIT 
      ENDDO
C   
C     WRITE THE RESULTS TO BE READ BY PROCOST
C
      HOURS = HOURS + HOURS_INCREMENT
      I = POINTS_IN_CURVE
      DOWHILE (LPROB(I) == 0.)
         I = I - 1
      ENDDO
      POINTS_IN_CURVE = I + 1
      DX = (LODDUR(POINTS_IN_CURVE)-BASE)/FLOAT(POINTS_IN_CURVE-1)
      DX = MAX(DX,1.)
      WRITE(8800,REC=ISEAS) DEMAND,DX,LODDUR(POINTS_IN_CURVE),BASE,
     +                     LPROB,LODDUR,POINTS_IN_CURVE,AVE_RUNNING_RATE
      RETURN
      END

