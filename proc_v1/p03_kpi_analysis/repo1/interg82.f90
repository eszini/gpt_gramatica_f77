! interg82.f90

module interg82
    use shared_vars_interg82
    implicit none

contains

!***********************************************************************
!
!     A SUBROUTINE TO INTEGRATE THE PROB CURVE TO FIND THE ENERGY
!
!***********************************************************************
      SUBROUTINE CALENRG(LODDUR,LPROB,A,B,ENRG,MAXLIM,  &
                                                   DX,LEFT,RIGHT,ISTART)
      use energy_trace
      implicit none
      
      INTEGER (kind=2) ::   ISTART,I
      INTEGER (kind=2) ::   ARG_ISTART,ARG_ISTOP
      REAL :: LPROB(*),ENRG,LODDUR(*),DX,A,B,MAXLIM,BASE,INTPL8, &
           X1,Y1,X3,Y2,X2,LEFT,RIGHT,ENERGY
!      SAVE ISTOP,CAL_ENRGY_ISTART
!
      INTPL8(X1,Y1,X3,Y2,X2) = Y1 + (Y2-Y1) * (X3-X1)/(X2-X1)
!
      BASE = LODDUR(1)
      ENRG = MAX(BASE-A,0.0)
      call write_energy_trace("CALENRG", "interg82:0001", ENRG)
      IF(A >= MAXLIM) RETURN
      IF(A < BASE) THEN
         ISTART = 1
      ELSE
         IF(ABS(A-BASE) < DX) ISTART = 2
         DO WHILE(.NOT.(LODDUR(ISTART-1) <= A .AND. A <= LODDUR(ISTART)))
            IF(A > LODDUR(ISTART)) THEN
               ISTART = ISTART + 1
            ELSE
               ISTART = ISTART - 1
            ENDIF
         ENDDO
      ENDIF
      ISTOP = ISTART + MAX(0,NINT((B-LODDUR(ISTART))/DX))
      ISTOP = MIN(ISTOP,ISTART+ int(90,kind=2) )
      DO WHILE(.NOT.(LODDUR(ISTOP) <= B .AND. B <= LODDUR(ISTOP+1)))
         IF(LODDUR(ISTOP) > B) THEN
            ISTOP = ISTOP - 1
         ELSE
            ISTOP = ISTOP + 1
         ENDIF
      ENDDO
!
      IF(ISTART == 1) THEN
         LEFT = 1.0
      ELSE
         LEFT = INTPL8(LODDUR(ISTART-1),LPROB(ISTART-1),A,                                &
                       LPROB(ISTART),LODDUR(ISTART))
      ENDIF
      RIGHT = INTPL8(LODDUR(ISTOP),LPROB(ISTOP),B,                                        &
                     LPROB(ISTOP+1),LODDUR(ISTOP+1))
      IF(ISTOP > ISTART) THEN
         ENERGY = 0.0
         DO I = ISTART, ISTOP-1
            ENERGY = ENERGY + (LPROB(I) + LPROB(I+1)) *                                   &
                              (LODDUR(I+1) - LODDUR(I))
         ENDDO
         IF(ISTART == 1) THEN
            ENERGY = ENERGY/2. +                                                          &
               ((B-LODDUR(ISTOP)) * (LPROB(ISTOP) + RIGHT))/2.
         ELSE
            ENERGY = ENERGY/2. +                                                          &
               ((LODDUR(ISTART)-A) * (LPROB(ISTART) + LEFT) +                             &
               (B-LODDUR(ISTOP)) * (LPROB(ISTOP) + RIGHT))/2.
         ENDIF
      ELSE IF(ISTOP == ISTART) THEN
         IF(ISTART == 1) THEN
            RIGHT = INTPL8(BASE,1.,B,LPROB(2),LODDUR(2))
            ENERGY=(B-BASE)*(1. + RIGHT)/2.
         ELSE
            ENERGY = ((LODDUR(ISTART)-A) * (LPROB(ISTART) + LEFT) +                       &
               (B-LODDUR(ISTART)) * (LPROB(ISTART) + RIGHT))/2.
         ENDIF
      ELSE
         LEFT = INTPL8(LODDUR(ISTOP),LPROB(ISTOP),A,                                      &
                   LPROB(ISTART),LODDUR(ISTART))
         RIGHT = INTPL8(LODDUR(ISTOP),LPROB(ISTOP),B,                                     &
               LPROB(ISTART),LODDUR(ISTART))
         ENERGY = (B-A) * (LEFT + RIGHT)/2.
      ENDIF
      ENRG = ENRG + ENERGY
      call write_energy_trace("CALENRG", "interg82:0002", ENRG)
      CAL_ENRGY_ISTART = ISTART
      ISTART = MAX(ISTOP,int(2,kind=2) )
      RETURN
      END SUBROUTINE CALENRG
!
!      ENTRY RETURN_ISTART_ISTOP(ARG_ISTART,ARG_ISTOP)
!      ARG_ISTART = CAL_ENRGY_ISTART
!      ARG_ISTOP = ISTOP
!      RETURN
!      END
!

      SUBROUTINE RETURN_ISTART_ISTOP (ARG_ISTART,ARG_ISTOP)
      IMPLICIT NONE
      INTEGER (kind=2), INTENT(out) :: ARG_ISTART,ARG_ISTOP

      ARG_ISTART = CAL_ENRGY_ISTART
      ARG_ISTOP  = ISTOP
      RETURN
      END SUBROUTINE RETURN_ISTART_ISTOP




!***********************************************************************
!
!     A SUBROUTINE TO INTEGRATE THE PROB CURVE TO FIND THE HEAT USAGE
!
!***********************************************************************
      SUBROUTINE CALHEAT(ENERGY,LEFT,RIGHT,COEFF,MW_ch,HEAT,    &
                         EFFECTIVE_CAPACITY,EA1,EA2)
      REAL :: MW_ch(2)
      REAL :: COEFF(3),A,B,C,MW1,MW2,MW3,ENERGY(2),LEFT,   &
           RIGHT,AREA1,AREA2,AREA3,EFFECTIVE_CAPACITY(2),  &
           EA1,EA2,AREA4,AREA5
      REAL (kind=8) ::   HEAT
      IF(ENERGY(2) < 0.0001) THEN
         ENERGY(1) = EA1 * ENERGY(1)
         HEAT = DBLE(ENERGY(1) * COEFF(1))
         LEFT = SNGL(HEAT)
         RIGHT = 0.
      ELSE
         MW1 = MW_ch(1)
         MW2 = MW_ch(2)
         MW3 = (MW1+MW2)/2.

         C = (COEFF(2)-COEFF(3))/(2.*(MW1-MW2))
         B = COEFF(2)-2.*C*MW1
         A = (COEFF(1) - B - C*MW1)*MW1
         IF(RIGHT == 1) THEN
            ENERGY(2) = EA2 * (ENERGY(1)+ENERGY(2))
            HEAT =  DBLE(ENERGY(2) * (A/MW2+B+C*MW2))
            RIGHT = SNGL(HEAT)
            LEFT = 0.
            ENERGY(1) = 0.
         ELSE
            AREA5 = MIN(ENERGY(2),RIGHT*EFFECTIVE_CAPACITY(2))
            AREA4 = MAX(0.,ENERGY(2)-AREA5)
            AREA3 = MIN(ENERGY(1),RIGHT*EFFECTIVE_CAPACITY(1))
            AREA2 = MIN(ENERGY(1)-AREA3,LEFT*EFFECTIVE_CAPACITY(1) -   &
               AREA3)
            AREA1 = EA1*MAX(0.,ENERGY(1)-AREA2-AREA3)
            AREA2 = EA1*AREA2 + EA2*AREA4
            AREA3 = EA1*AREA3 + EA2*AREA5
            LEFT = AREA1 * COEFF(1)
            HEAT = DBLE(LEFT) + DBLE(AREA2*(A/MW3+B+C*MW3)) +  &
                                             DBLE(AREA3*(A/MW2+B+C*MW2))
            RIGHT = SNGL(HEAT) - LEFT
            ENERGY(1) = AREA1
            ENERGY(2) = AREA2 + AREA3
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE CALHEAT

!***********************************************************************

end module interg82
!

