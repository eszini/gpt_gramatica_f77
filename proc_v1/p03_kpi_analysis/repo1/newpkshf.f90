
!
!     A SUBROUTINE TO INTEGRATE THE PROB CURVE TO FIND THE ENERGY
!     OF CONTRACTS
!

      SUBROUTINE CALC_CONTRACT_ENERGY(LODDUR,LPROB,DX,A,B,ENERGY,   &
                           LEFT,RIGHT,AVAILABLE_CONTRACT_ENRG,   &
                           ISTART,LAST_POINT,   &
                           REMAINING_ENRG_ONLY_ENRG,   &
                           ENRG_ONLY_ENRG_USED)
      use end_routine, only: end_program, er_message
      LOGICAL (kind=1) ::  DECREASING
      INTEGER (kind=2) ::  ISTART,ISTOP,I,LAST_POINT
      REAL ::  OFF_LOAD_ENRG,TOTAL_AVAILABLE_CONTRACT_ENRG,   &
           ENRG_ONLY_ENRG_USED
      REAL ::  LPROB(1000),ENERGY,   &
           LODDUR(1000),DX,A,B,BASE,INTPL8,   &
           X1,Y1,X3,Y2,X2,LEFT,RIGHT,AVAILABLE_CONTRACT_ENRG,INCREMENT,   &
           REMAINING_ENRG_ONLY_ENRG,B_FOR_RIGHT_BOUNDARY
      INTPL8(X1,Y1,X3,Y2,X2) = Y1 + (Y2-Y1) * (X3-X1)/(X2-X1)

! END DATA DECLARATIONS

      BASE = LODDUR(1)
      ENRG_ONLY_ENRG_USED = 0.

! ADDED 10/22/92 TO TRAP THE CASE WHERE ALL THE ENERGY NEEDED IS UNDER
! THE LOAD CURVE BELOW THE BASE

      ENERGY = MAX(MIN(B,BASE)-A,0.)
      IF(ENERGY >= AVAILABLE_CONTRACT_ENRG +   &
                                  MAX(REMAINING_ENRG_ONLY_ENRG,0.)) THEN
         ENRG_ONLY_ENRG_USED = MAX(REMAINING_ENRG_ONLY_ENRG,0.)
         ENERGY = AVAILABLE_CONTRACT_ENRG
         B = A + ENRG_ONLY_ENRG_USED + ENERGY
         REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG -   &
                                           ENRG_ONLY_ENRG_USED
         ISTART = 2
         RETURN
      ELSEIF(B <= BASE) THEN
         IF(ENERGY >= AVAILABLE_CONTRACT_ENRG) THEN
            ENRG_ONLY_ENRG_USED = MIN(MAX(REMAINING_ENRG_ONLY_ENRG,0.),   &
                               MAX(ENERGY - AVAILABLE_CONTRACT_ENRG,0.))
            ENERGY = AVAILABLE_CONTRACT_ENRG
            B = A + ENRG_ONLY_ENRG_USED + ENERGY
         ENDIF
         REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG -   &
                                           ENRG_ONLY_ENRG_USED
         ISTART = 2
         RETURN
      ENDIF
!
      IF(A .LT. BASE) THEN
         ISTART = 1
      ELSE
         IF(A .EQ. BASE) ISTART = 2
    3    CONTINUE
         IF(.NOT.(LODDUR(ISTART-1).LE.A .AND. A.LE.LODDUR(ISTART))) THEN
            IF(A .GT. LODDUR(ISTART)) THEN
               ISTART = ISTART + 1
            ELSE
               ISTART = ISTART - 1
            ENDIF
            GOTO 3
         ENDIF
      ENDIF
!
      ISTOP = ISTART + NINT(MIN(30000,(B-LODDUR(ISTART))/DX))
      ISTOP = MAX(1,MIN(ISTOP,ISTART+90))
    7 CONTINUE
      IF( .NOT.(LODDUR(ISTOP) <= B .AND. B <= LODDUR(ISTOP+1)) ) THEN
         IF(LODDUR(ISTOP) .GT. B) THEN
            ISTOP = ISTOP - 1
            DECREASING = .TRUE.
         ELSE
            ISTOP = ISTOP + 1
            DECREASING = .FALSE.
         ENDIF
         IF(LPROB(ISTOP+1) > 0. .OR. DECREASING) THEN
            GOTO 7 ! 7/19/96. GAT. MOVED FROM ABOVE
         ENDIF
      ENDIF
!
      IF(B > LODDUR(ISTOP+1)) THEN
         B_FOR_RIGHT_BOUNDARY = LODDUR(ISTOP+1)
      ELSE
         B_FOR_RIGHT_BOUNDARY = B
      ENDIF
!
      ISTOP = MIN(ISTOP,LAST_POINT)
!
!     LIMITS OF INTEGRATION ARE FOUND.
!     CALCULATE THE RIGHT AND LEFT NON-COMPLETE DX INTERVALS
!
      IF(ISTART .EQ. 1) THEN
         LEFT = 1.0
      ELSE
         LEFT = INTPL8(LODDUR(ISTART-1),LPROB(ISTART-1),A,   &
                  LPROB(ISTART),LODDUR(ISTART))
      ENDIF
      RIGHT = INTPL8(LODDUR(ISTOP),LPROB(ISTOP),B_FOR_RIGHT_BOUNDARY,   &
                                 LPROB(ISTOP+1),LODDUR(ISTOP+1))
      IF(RIGHT > 1.0 .OR. RIGHT < -0.0001) THEN
         WRITE(4,*) "*** STOP at line 146 CALC_CONTRACT_ENERGY"
         WRITE(4,*) "Energy calculation exceeded floating point"
         WRITE(4,*) "in CALC_CONTRACT_ENERGY"
         er_message='See WARNING MESSAGES -NEWPKSHF.FOR-1'
         call end_program(er_message)
      ENDIF
!
!     THREE CASES:
!     CASE #1: LEFT TO ISTART,ISTART TO ISTOP, ISTOP TO RIGHT
!     CASE #2: LEFT TO ISTART=ISTOP, ISTART=ISTOP TO RIGHT
!     CASE #3: LEFT TO RIGHT
!
      TOTAL_AVAILABLE_CONTRACT_ENRG = AVAILABLE_CONTRACT_ENRG +   &
                                               REMAINING_ENRG_ONLY_ENRG
!
      IF(ISTOP .GT. ISTART) THEN
!
         IF(ISTART .GT. 1) THEN
            INCREMENT =   &
               ((LODDUR(ISTART)-A) * (LPROB(ISTART) + LEFT)) /2.
!
!           ENERGY VIOLATED?
!
            IF(INCREMENT .GT. TOTAL_AVAILABLE_CONTRACT_ENRG) THEN
               CALL FIND_MAXIMUM_ENERGY(A,LODDUR(ISTART),LEFT,   &
                                        LPROB(ISTART),ENERGY,   &
                                        TOTAL_AVAILABLE_CONTRACT_ENRG,   &
                                        B,RIGHT)
               GOTO 100
            ENDIF
            ENERGY = INCREMENT
         ENDIF
         DO I = ISTART, ISTOP-1
            INCREMENT = (LPROB(I) + LPROB(I+1)) *   &
                              (LODDUR(I+1) - LODDUR(I))/2.
!
!           ENERGY VIOLATED?
!
            IF(INCREMENT+ENERGY .GT. TOTAL_AVAILABLE_CONTRACT_ENRG) THEN
               CALL FIND_MAXIMUM_ENERGY(LODDUR(I),LODDUR(I+1),   &
                                        LPROB(I),LPROB(I+1),ENERGY,   &
                                        TOTAL_AVAILABLE_CONTRACT_ENRG,   &
                                        B,RIGHT)
               GOTO 100
            ELSEIF(LPROB(I+1) == 0.0) THEN
               ENERGY = ENERGY + INCREMENT
               B = LODDUR(I+1)
               RIGHT = 0.0
               GOTO 100
            ENDIF
            ENERGY = ENERGY + INCREMENT
         ENDDO
         INCREMENT = (B-LODDUR(ISTOP)) * (LPROB(ISTOP) + RIGHT)/2.
!
!        ENERGY VIOLATED?
!
         IF(INCREMENT+ENERGY .GT. TOTAL_AVAILABLE_CONTRACT_ENRG) THEN
            CALL FIND_MAXIMUM_ENERGY(LODDUR(ISTOP),B,   &
                                     LPROB(ISTOP),RIGHT,ENERGY,   &
                                     TOTAL_AVAILABLE_CONTRACT_ENRG,   &
                                     B,RIGHT)
            GOTO 100
         ENDIF
         ENERGY = ENERGY + INCREMENT
      ELSE IF(ISTOP .EQ. ISTART) THEN
         IF(ISTART .EQ. 1) THEN
            RIGHT = INTPL8(BASE,1.,B,LPROB(2),LODDUR(2))
            INCREMENT =  (B-BASE)*(1. + RIGHT)/2.
!
!           ENERGY VIOLATED?
!
            IF(INCREMENT+ENERGY .GT. TOTAL_AVAILABLE_CONTRACT_ENRG) THEN
               CALL FIND_MAXIMUM_ENERGY(BASE,B,1.,RIGHT,ENERGY,   &
                                        TOTAL_AVAILABLE_CONTRACT_ENRG,   &
                                        B,RIGHT)
               GOTO 100
            ENDIF
            ENERGY = ENERGY + INCREMENT
         ELSE
            INCREMENT = (LODDUR(ISTART)-A) * (LPROB(ISTART) + LEFT)/2.
            IF(INCREMENT .GT. TOTAL_AVAILABLE_CONTRACT_ENRG) THEN
               CALL FIND_MAXIMUM_ENERGY(A,LODDUR(ISTART),   &
                                        LEFT,LPROB(ISTART),ENERGY,   &
                                        TOTAL_AVAILABLE_CONTRACT_ENRG,   &
                                        B,RIGHT)
               GOTO 100
            ENDIF
            ENERGY = INCREMENT
!
            INCREMENT = (B-LODDUR(ISTART)) * (RIGHT + LPROB(ISTART))/2.
!
!           ENERGY VIOLATED?
!
            IF(INCREMENT+ENERGY .GT. TOTAL_AVAILABLE_CONTRACT_ENRG) THEN
               CALL FIND_MAXIMUM_ENERGY(LODDUR(ISTART),B,   &
                                        LPROB(ISTART),RIGHT,ENERGY,   &
                                        TOTAL_AVAILABLE_CONTRACT_ENRG,   &
                                        B,RIGHT)
               GOTO 100
            ENDIF
            ENERGY = ENERGY + INCREMENT
         ENDIF
      ELSE
!
!        WHEN THE UNIT DOES NOT CROSS A LOAD INTERVAL
!
         LEFT = INTPL8(LODDUR(ISTOP),LPROB(ISTOP),A,   &
                   LPROB(ISTART),LODDUR(ISTART))
         RIGHT = INTPL8(LODDUR(ISTOP),LPROB(ISTOP),B,   &
               LPROB(ISTART),LODDUR(ISTART))
         INCREMENT = (B-A) * (LEFT + RIGHT)/2.
!
!        ENERGY VIOLATED?
!
         IF(INCREMENT .GT. TOTAL_AVAILABLE_CONTRACT_ENRG) THEN
            CALL FIND_MAXIMUM_ENERGY(A,B,LEFT,RIGHT,ENERGY,   &
                                     TOTAL_AVAILABLE_CONTRACT_ENRG,   &
                                     B,RIGHT)
            GOTO 100
         ENDIF
         ENERGY = INCREMENT
      ENDIF
  100 ISTART = MAX(ISTOP,2)
      IF(ENERGY .GT. AVAILABLE_CONTRACT_ENRG) THEN
         ENRG_ONLY_ENRG_USED = ENERGY - AVAILABLE_CONTRACT_ENRG
         REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG -   &
                                                     ENRG_ONLY_ENRG_USED
         ENERGY = AVAILABLE_CONTRACT_ENRG
      ENDIF
      RETURN
      END


!
!     A SUBROUTINE TO FIND THE RIGHT BOUNDARY NEEDED TO SATISFY THE
!     MAXIMUM ENERGY CONSTRAINT.
!

      SUBROUTINE FIND_MAXIMUM_ENERGY(MIN_CAP,MAX_CAP,MAX_PROB,MIN_PROB,   &
                           ENERGY,MAXIMUM_ENERGY,INT_CAP,INT_PROB)
      use end_routine, only: end_program, er_message
      REAL ::  MIN_CAP,MAX_CAP,MAX_PROB,MIN_PROB,   &
           ENERGY,MAXIMUM_ENERGY,INT_CAP,INT_PROB
      REAL ::  B2_4AC,SQRT_B2_4AC,   &
           SOLUTION1,SOLUTION2,SLOPE,A,B,C
!
! ALTERNATIVE SOLUTION TO THE QUADRADTIC  1/28/93
!
      SLOPE = (MIN_PROB-MAX_PROB)/(MAX_CAP-MIN_CAP)
      IF( ABS(SLOPE) < 0.0000001) THEN
         INT_CAP = (MAXIMUM_ENERGY - ENERGY)/MAX_PROB + MIN_CAP
         INT_PROB = MIN_PROB
      ELSE
         A = SLOPE/2.
         B = MAX_PROB
         C = ENERGY - MAXIMUM_ENERGY
         B2_4AC = B*B - 4.*A*C
         IF(B2_4AC > 0) THEN
            SQRT_B2_4AC = SQRT(B2_4AC)
         ELSE IF(B2_4AC == 0) THEN
            SQRT_B2_4AC = 0.
         ELSE
            WRITE(4,*) "*** STOP at line 318 FIND_MAXIMUM_ENERGY"
            WRITE(4,*)"REAL SOLUTION TO ENERGY MAX CONSTRAINT NOT FOUND"
            WRITE(4,*) 'B**2-4AC =',B2_4AC
            WRITE(4,*) 'A =',A
            WRITE(4,*) 'B =',B
            WRITE(4,*) 'C =',C
            WRITE(4,*) '(Min_Cap,Max_Prob) (',MIN_CAP,',',MAX_PROB,')'
            WRITE(4,*) '(Max_Cap,Min_Prob) (',MAX_CAP,',',MIN_PROB,')'
            WRITE(4,*) 'Maximum_Energy,Mnergy',MAXIMUM_ENERGY,ENERGY
            er_message='See WARNING MESSAGES -NEWPKSHF.FOR-2'
            call end_program(er_message)
         ENDIF
         SOLUTION1 = (-B + SQRT_B2_4AC)/(2.*A)
         SOLUTION2 = (-B - SQRT_B2_4AC)/(2.*A)
         IF(SOLUTION1 > 0. .OR. MIN_CAP+SOLUTION2 > MAX_CAP) THEN
            IF(SOLUTION2 > 0.) THEN
               INT_CAP = MIN_CAP + MIN(SOLUTION1,SOLUTION2)
            ELSE
               INT_CAP = MIN_CAP + SOLUTION1
            ENDIF
         ELSE
            INT_CAP = MIN_CAP + SOLUTION2
         ENDIF
         INT_PROB = SLOPE * (INT_CAP - MIN_CAP) + MAX_PROB
      ENDIF
      ENERGY = MAXIMUM_ENERGY
      RETURN
      END


!
!     A SUBROUTINE TO LOAD A RESOURCE WITH MINIMUM ENERGY AND CAPACITY
!

      SUBROUTINE NEW_PEAK_SHIFT(LODDUR,LPROB,CAPACITY,ENERGY,   &
                                 MAX_LD_PTS,A,B,CURRENT_CAPACITY,   &
                                 AREA,HOURS,   &
                                 MAXIMUM_ENERGY,CNTRNM,ISEAS,DX,   &
                                 REMAINING_ENRG_ONLY_ENRG,   &
                                 ENRG_ONLY_ENRG_USED)
!
!     DECLARE VARIABLES
!
      REAL (kind=8) ::  TEMP_ENER_BEFORE,TEMP_ENER_AFTER
      REAL ::  LPROB(1000),LODDUR(1000),CAPACITY,ENERGY,   &
           A,B,CONTRACT_LOAD_FACTOR,CURVE_LOAD_FACTOR,AREA,L_A,L_B,   &
           L_LEFT,L_RIGHT,INCREMENT,CURRENT_CAPACITY,DELTA_X,HOURS,   &
           LOAD_ADJUSTMENT,MAXIMUM_ENERGY,DX,NEW_TOP,   &
           NEW_BOTTOM,DX_FOR_TWO_AREAS,B_LIMIT,REMAINING_ENRG_ONLY_ENRG,   &
           ENRG_ONLY_ENRG_USED,TOTAL_ENERGY_AVAILABLE
      INTEGER (kind=2) ::  MAX_LD_PTS
      INTEGER (kind=2) ::  NEW_MAX_LD_PTS,I,BOTTOM,TOP,ISTART,   &
                  ISEAS,LAST_I,TOP_LIMIT,BOTTOM_LIMIT
!
      REAL ::  INCREMENTAL_ENERGY,INCREMENTAL_CAPACITY
      INTEGER (kind=2) ::  J
      LOGICAL (kind=1) ::    SEARCH_PARTIAL_SEGMENTS
      LOGICAL (kind=1) ::    UPDATE_TWO_AREAS_DATA,   &
                  WKP_ACTIVE=.FALSE. ,SECOND_CHECK
!
      CHARACTER (len=20) ::  CNTRNM
!
      CALL INTEG8(TEMP_ENER_BEFORE,LODDUR,LPROB,MAX_LD_PTS,   &
                                           INT(HOURS),LODDUR(1))
      CONTRACT_LOAD_FACTOR = MIN(1.0,MAXIMUM_ENERGY/CAPACITY) ! TO ACCOMODATE WKP. 7/31/96. GAT.
!
!     SEARCH FOR THE CROSSOVER PROB
!
      AREA = 0.
      I = 2
      DOWHILE(LPROB(I) .GT. CONTRACT_LOAD_FACTOR)
         I = I + 1
      ENDDO
!
!     SEARCH FOR THE LOAD SEGMENTS CONTAINING THE MINIMUM
!
      BOTTOM = I
      TOP = I
      LAST_I = I
      DELTA_X = LODDUR(I) - LODDUR(I-1)
      CURVE_LOAD_FACTOR = (LPROB(I) + LPROB(I-1))/2.
      AREA = DELTA_X * CURVE_LOAD_FACTOR
!
      TOTAL_ENERGY_AVAILABLE = MAXIMUM_ENERGY + REMAINING_ENRG_ONLY_ENRG
!
      DOWHILE ( (LODDUR(BOTTOM) - LODDUR(TOP-1) + B - A <CAPACITY) .AND.   &
                          (B < LODDUR(1) .OR. BOTTOM < MAX_LD_PTS) .AND.   &
                                          AREA < TOTAL_ENERGY_AVAILABLE)
         IF( (CURVE_LOAD_FACTOR .GT. CONTRACT_LOAD_FACTOR .OR.   &
                 B .GE. LODDUR(1)) .AND. BOTTOM .LT. MAX_LD_PTS) THEN
            BOTTOM = BOTTOM + 1
            LAST_I = I
            I = BOTTOM
            AREA = AREA +   &
                     (LPROB(I)+LPROB(I-1))*(LODDUR(I)-LODDUR(I-1))/2.
         ELSEIF(TOP .EQ. 2) THEN
            LAST_I = I
            I = -999
            IF(LODDUR(1) .GT. B + DX) THEN
               B = B + DX
               AREA = AREA + DX
            ELSE
               AREA = AREA + MAX(0.,LODDUR(1) - B)
               B = MAX(LODDUR(1),A)
            ENDIF
         ELSE
            TOP = TOP  - 1
            LAST_I = I
            I = TOP
            AREA = AREA +   &
                     (LPROB(I)+LPROB(I-1))*(LODDUR(I)-LODDUR(I-1))/2.
         ENDIF
         CURVE_LOAD_FACTOR = AREA/(LODDUR(BOTTOM)-LODDUR(TOP-1)+B-A)
      ENDDO
!
!
!     ATTEMPT TO ADJUST USING TWO_AREAS
!
!     ISSUES: 1 USING LAST I; 2 USING THE MINIMUM PART OF THE LDC
!
      SEARCH_PARTIAL_SEGMENTS = .TRUE.
      GOTO 1234
      UPDATE_TWO_AREAS_DATA = .FALSE.
      B_LIMIT = B
      TOP_LIMIT = TOP
      BOTTOM_LIMIT = BOTTOM
      INCREMENTAL_CAPACITY =  CAPACITY -   &
              (LODDUR(BOTTOM_LIMIT) - LODDUR(TOP_LIMIT-1) + B_LIMIT - A)
      INCREMENTAL_ENERGY =  TOTAL_ENERGY_AVAILABLE - AREA
      IF(TOP_LIMIT == 2) TOP_LIMIT = 1
      IF(INCREMENTAL_CAPACITY <= 0. .AND. INCREMENTAL_ENERGY <= 0.) THEN
         IF(TOP_LIMIT == 1) THEN
            DX_FOR_TWO_AREAS = MIN(DX,B_LIMIT-A)
            AREA = AREA - DX_FOR_TWO_AREAS
            INCREMENTAL_ENERGY = INCREMENTAL_ENERGY +   &
                                                  DX_FOR_TWO_AREAS
            INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY +   &
                                                  DX_FOR_TWO_AREAS
            B_LIMIT = MAX(A,B_LIMIT-DX)
         ELSE
            INCREMENTAL_ENERGY =   &
                      (LPROB(TOP_LIMIT)+LPROB(TOP_LIMIT-1))*   &
                      (LODDUR(TOP_LIMIT)-LODDUR(TOP_LIMIT-1))/2.
            INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY +   &
                                LODDUR(TOP_LIMIT) - LODDUR(TOP_LIMIT-1)
         ENDIF
         IF(BOTTOM_LIMIT > 2) THEN
            BOTTOM_LIMIT = BOTTOM_LIMIT - 1
            INCREMENTAL_ENERGY = INCREMENTAL_ENERGY +   &
                   (LPROB(BOTTOM_LIMIT+1)+LPROB(BOTTOM_LIMIT))*   &
                   (LODDUR(BOTTOM_LIMIT+1)-LODDUR(BOTTOM_LIMIT))/2.
            INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY +   &
                           LODDUR(BOTTOM_LIMIT+1) - LODDUR(BOTTOM_LIMIT)
            CALL TWO_AREAS(TOP_LIMIT,BOTTOM_LIMIT,LODDUR,LPROB,   &
                        INCREMENTAL_CAPACITY,   &
                        INCREMENTAL_ENERGY,SEARCH_PARTIAL_SEGMENTS,   &
                        B_LIMIT,LODDUR(1) - B_LIMIT,   &
                        UPDATE_TWO_AREAS_DATA)
            IF(.NOT. SEARCH_PARTIAL_SEGMENTS) THEN
               TOP = TOP_LIMIT
               BOTTOM = BOTTOM_LIMIT
               B = B_LIMIT
            ENDIF
         ELSE
            TOP = 1
            SEARCH_PARTIAL_SEGMENTS = .FALSE.
            B_LIMIT = MAX(A,B_LIMIT - (AREA - TOTAL_ENERGY_AVAILABLE))
            AREA = AREA - MAX(0.,AREA - TOTAL_ENERGY_AVAILABLE)
! IF TEST FAILS IN TWO AREAS, AREA IS NOT RESET.
         ENDIF
      ELSE
         IF(INCREMENTAL_ENERGY > INCREMENTAL_CAPACITY   &
                                           .AND. BOTTOM_LIMIT > 2) THEN
            BOTTOM_LIMIT = BOTTOM - 1
            INCREMENTAL_ENERGY = INCREMENTAL_ENERGY +   &
                     (LPROB(BOTTOM_LIMIT+1)+LPROB(BOTTOM_LIMIT))*   &
                     (LODDUR(BOTTOM_LIMIT+1)-LODDUR(BOTTOM_LIMIT))/2.
            INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY +   &
                      LODDUR(BOTTOM_LIMIT+1) - LODDUR(BOTTOM_LIMIT)
         ELSEIF(TOP_LIMIT == 1) THEN
            AREA = AREA - MIN(B_LIMIT-A,DX)
            B_LIMIT = MAX(A,B_LIMIT-DX)
            INCREMENTAL_ENERGY = INCREMENTAL_ENERGY + DX
            INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY + DX
         ELSE
            BOTTOM_LIMIT = BOTTOM
            INCREMENTAL_ENERGY =   &
                  (LPROB(TOP_LIMIT)+LPROB(TOP_LIMIT-1))*   &
                           (LODDUR(TOP_LIMIT)-LODDUR(TOP_LIMIT-1))/2.
            INCREMENTAL_CAPACITY = LODDUR(TOP_LIMIT)-LODDUR(TOP_LIMIT-1)
         ENDIF
         IF(AREA+INCREMENTAL_ENERGY >= TOTAL_ENERGY_AVAILABLE .AND.   &
               LODDUR(BOTTOM_LIMIT+1) - LODDUR(TOP_LIMIT) + B - A +   &
                                  INCREMENTAL_CAPACITY >= CAPACITY) THEN
           CALL TWO_AREAS(TOP_LIMIT,BOTTOM_LIMIT,LODDUR,LPROB,   &
                        INCREMENTAL_CAPACITY,INCREMENTAL_ENERGY,   &
                        SEARCH_PARTIAL_SEGMENTS,B_LIMIT,LODDUR(1)-B,   &
                        UPDATE_TWO_AREAS_DATA)
           IF(UPDATE_TWO_AREAS_DATA) THEN
               TOP = TOP_LIMIT
               BOTTOM = BOTTOM_LIMIT
               B = B_LIMIT
            ENDIF
         ENDIF
      ENDIF
1234  CONTINUE
!
!     SEARCH FOR THE FRACTIONAL SEGMENTS AND ADJUST THE LDC
!
      IF(SEARCH_PARTIAL_SEGMENTS) THEN
         SECOND_CHECK = .TRUE.
         IF(A /= B) THEN
            TOP = TOP - 1
            IF(LODDUR(BOTTOM)-LODDUR(TOP)+B-A > CAPACITY) THEN
               AREA = AREA -   &
                  MAX(0.,(LODDUR(BOTTOM)-LODDUR(TOP)+B-A-CAPACITY))
               B = MAX(A,B - (LODDUR(BOTTOM)-LODDUR(TOP)+B-A-CAPACITY))
            ELSEIF(AREA > TOTAL_ENERGY_AVAILABLE) THEN
               B = MAX(A,B - (AREA - TOTAL_ENERGY_AVAILABLE))
               AREA = AREA - MAX(0.,AREA - TOTAL_ENERGY_AVAILABLE)
            ENDIF
            IF(LODDUR(BOTTOM)-LODDUR(TOP)+B-A > CAPACITY) THEN
               IF(I == TOP+1) THEN
                  IF(LODDUR(TOP) == LODDUR(TOP+1)) THEN
                     SECOND_CHECK = .FALSE.
                  ELSE
                     TOP = TOP + 1
                  ENDIF
               ELSE
                  IF(LODDUR(BOTTOM) == LODDUR(BOTTOM-1)) THEN
                     SECOND_CHECK = .FALSE.
                  ELSE
                     IF(I == -999) I = BOTTOM
                     TOP = TOP + 1
                  ENDIF
               ENDIF
            ELSE
               SECOND_CHECK = .FALSE.
            ENDIF
         ENDIF ! 1/1/95. GAT. ALLOWS FOR DOUBLE CHECK OF CAPACITY

         IF(SECOND_CHECK) THEN
            IF(LODDUR(BOTTOM) - LODDUR(TOP-1) + B-A > CAPACITY) THEN
               DELTA_X = LODDUR(I) - LODDUR(I-1)
               INCREMENT = (LPROB(I)+LPROB(I-1)) * DELTA_X/2.
               IF(I == BOTTOM) THEN
                  L_B = LODDUR(TOP-1) + CAPACITY - B + A
                  L_RIGHT = LPROB(I-1) +   &
                     (LPROB(I)-LPROB(I-1))*(L_B-LODDUR(I-1))/DELTA_X
                  AREA = AREA - INCREMENT +   &
                     (LPROB(I-1) + L_RIGHT)*(L_B - LODDUR(I-1))/2.
                  LPROB(BOTTOM) = L_RIGHT
                  LODDUR(BOTTOM) = L_B
                  TOP = TOP - 1
               ELSE
                  L_A = LODDUR(BOTTOM) - CAPACITY - B + A
                  L_LEFT = LPROB(I) -   &
                     (LPROB(I)-LPROB(I-1))*(LODDUR(I)-L_A)/DELTA_X
                  AREA = AREA - INCREMENT +   &
                          (L_LEFT + LPROB(I))*(LODDUR(I) - L_A)/2.
                  LPROB(TOP) = L_LEFT
                  LODDUR(TOP) = L_A
               ENDIF
            ELSEIF(AREA > TOTAL_ENERGY_AVAILABLE) THEN
               I = TOP
               INCREMENT = (LPROB(I)+LPROB(I-1))*   &
                                            (LODDUR(I)-LODDUR(I-1))/2.
               CALL FIND_MAXIMUM_ENERGY(LODDUR(I-1),   &
                     LODDUR(I),LPROB(I-1),   &
                     LPROB(I),TOTAL_ENERGY_AVAILABLE,AREA,L_A,L_LEFT)
               AREA = AREA - INCREMENT +   &
                                (L_LEFT + LPROB(I))*(LODDUR(I) - L_A)/2.
               LPROB(TOP) = L_LEFT
               LODDUR(TOP) = L_A
            ENDIF ! CAPACITY OR ENERGY EXCEEDED
!
         ENDIF ! USED THE LDC BELOW BASE
      ENDIF ! USE PARTIAL SEGMENTS
      CURRENT_CAPACITY = LODDUR(BOTTOM) - LODDUR(TOP)
!     CREATE THE NEW LDC
!
      ISTART = TOP + 2
      NEW_MAX_LD_PTS = MAX_LD_PTS + 1 - (BOTTOM - TOP)
      LOAD_ADJUSTMENT = MAX(LODDUR(ISTART-1)*.0001*   &
                                (LPROB(TOP)-LPROB(BOTTOM)),.0001)
      LODDUR(ISTART-1) = LODDUR(TOP) + LOAD_ADJUSTMENT
      LPROB(ISTART-1)  = LPROB(BOTTOM)
      DO I = ISTART , MAX_LD_PTS
         LODDUR(I) =  LODDUR(BOTTOM + 1 + I - ISTART) +   &
            LOAD_ADJUSTMENT - CURRENT_CAPACITY
         IF(I .LT. NEW_MAX_LD_PTS) THEN
            LPROB(I)  = LPROB(BOTTOM + 1 + I - ISTART)
         ELSE
            LPROB(I)  = 0.0
         ENDIF
      ENDDO
      MAX_LD_PTS = NEW_MAX_LD_PTS
      CALL INTEG8(TEMP_ENER_AFTER,LODDUR,LPROB,MAX_LD_PTS,   &
                                           INT(HOURS),LODDUR(1))
      AREA = (TEMP_ENER_BEFORE - TEMP_ENER_AFTER)/HOURS + B - A
!
      IF(.NOT. WKP_ACTIVE .AND. AREA > MAXIMUM_ENERGY) THEN ! 7/31/96. OUT. GAT.
         REMAINING_ENRG_ONLY_ENRG = REMAINING_ENRG_ONLY_ENRG -   &
                                                     ENRG_ONLY_ENRG_USED
         AREA = MAXIMUM_ENERGY
      ENDIF
!
      CURRENT_CAPACITY = CURRENT_CAPACITY + B - A
      RETURN

      END


      SUBROUTINE TWO_AREAS(TOP,BOTTOM,LODDUR,LPROB,INCREMENTAL_CAPACITY,   &
                           INCREMENTAL_ENERGY,SEARCH_PARTIAL_SEGMENTS,   &
                           B_LIMIT,DX_FOR_TWO_AREAS,   &
                           UPDATE_TWO_AREAS_DATA)

      INTEGER (kind=2) ::  TOP,BOTTOM
      REAL ::  LPROB(1000),LODDUR(1000),   &
            TOP_SLOPE,TOP_INTERCEPT,BOTTOM_SLOPE,BOTTOM_INTERCEPT
      REAL ::   B2_4AC,SQRT_B2_4AC,   &
            SOLUTION1,SOLUTION2,A,B,C,INCREMENTAL_CAPACITY,   &
            INCREMENTAL_ENERGY,TOP_INCREMENTAL_CAPACITY,   &
            BOTTOM_INCREMENTAL_CAPACITY,DX_FOR_TWO_AREAS,B_LIMIT
      LOGICAL (kind=1) ::  SEARCH_PARTIAL_SEGMENTS,UPDATE_TWO_AREAS_DATA
      IF(TOP == 1) THEN
         TOP_SLOPE = 0.0
         TOP_INTERCEPT = 1.0
      ELSE
         TOP_SLOPE = (-1*LPROB(TOP) + LPROB(TOP-1))/   & !  NOTE ROTATION
                     (LODDUR(TOP) - LODDUR(TOP-1))
         TOP_INTERCEPT = LPROB(TOP)                 ! NOTE TRANSLATION
      ENDIF
      BOTTOM_SLOPE = (LPROB(BOTTOM+1) - LPROB(BOTTOM))/   &
                     (LODDUR(BOTTOM+1) - LODDUR(BOTTOM))
      BOTTOM_INTERCEPT = LPROB(BOTTOM)           ! NOTE TRANSLATION
      A = TOP_SLOPE + BOTTOM_SLOPE
      B = 2.*(TOP_INTERCEPT - BOTTOM_INTERCEPT -   &
                                BOTTOM_SLOPE*INCREMENTAL_CAPACITY)
      C = 2.*BOTTOM_INTERCEPT*INCREMENTAL_CAPACITY +   &
              BOTTOM_SLOPE * INCREMENTAL_CAPACITY**2 -   &
              2.*INCREMENTAL_ENERGY
      B2_4AC = B*B - 4.*A*C
      IF(A == 0.) THEN
         RETURN
      ELSEIF(B2_4AC > 0.) THEN
         SQRT_B2_4AC = SQRT(B2_4AC)
      ELSE IF(B2_4AC == 0.) THEN
         SQRT_B2_4AC = 0.
      ELSE
         RETURN
      ENDIF

      SOLUTION1 = (-B + SQRT_B2_4AC)/(2.*A)
      SOLUTION2 = (-B - SQRT_B2_4AC)/(2.*A)
      IF(SOLUTION1 > 0.) THEN
         IF(SOLUTION2 > 0.) THEN
            TOP_INCREMENTAL_CAPACITY =  MIN(SOLUTION1,SOLUTION2)
         ELSE
            TOP_INCREMENTAL_CAPACITY =  SOLUTION1
         ENDIF
      ELSE
         TOP_INCREMENTAL_CAPACITY = SOLUTION2
      ENDIF
      BOTTOM_INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY -   &
                                            TOP_INCREMENTAL_CAPACITY
      IF(LODDUR(BOTTOM+1) - LODDUR(BOTTOM) <   &
                                  BOTTOM_INCREMENTAL_CAPACITY) THEN
         TOP_INCREMENTAL_CAPACITY = INCREMENTAL_CAPACITY -   &
                                   (LODDUR(BOTTOM+1)- LODDUR(BOTTOM))
         IF((TOP==1 .AND.DX_FOR_TWO_AREAS >=   &
                                        TOP_INCREMENTAL_CAPACITY) .OR.   &
            (TOP>1) .AND. LODDUR(TOP) - LODDUR(TOP-1) >=   &
                                        TOP_INCREMENTAL_CAPACITY) THEN
            BOTTOM = BOTTOM + 1
            B_LIMIT = LODDUR(1) - DX_FOR_TWO_AREAS
            TOP = MAX(2,TOP)
            UPDATE_TWO_AREAS_DATA = .TRUE.
         ENDIF
         RETURN
      ENDIF
      IF(TOP == 1) THEN
         IF(DX_FOR_TWO_AREAS >= TOP_INCREMENTAL_CAPACITY) THEN
            B_LIMIT = B_LIMIT + TOP_INCREMENTAL_CAPACITY
         ELSE
            RETURN
         ENDIF
      ELSE
         IF(LODDUR(TOP) - LODDUR(TOP-1)>=TOP_INCREMENTAL_CAPACITY) THEN
            LODDUR(TOP) = LODDUR(TOP) - TOP_INCREMENTAL_CAPACITY
            LPROB(TOP) = TOP_INTERCEPT +   &
                           TOP_SLOPE*TOP_INCREMENTAL_CAPACITY
         ELSE
            RETURN
         ENDIF
      ENDIF
      LODDUR(BOTTOM) = LODDUR(BOTTOM) + BOTTOM_INCREMENTAL_CAPACITY
      LPROB(BOTTOM) = BOTTOM_INTERCEPT + BOTTOM_SLOPE *   &
                                        BOTTOM_INCREMENTAL_CAPACITY
      SEARCH_PARTIAL_SEGMENTS = .FALSE.
      UPDATE_TWO_AREAS_DATA = .TRUE.
      RETURN
      END

