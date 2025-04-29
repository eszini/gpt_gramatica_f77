
!     ELREPORT.FOR
!     Copyright(c)  2000
!
!     Created: 11/15/2006 2:00:02 PM
!     Author : MARK S GERBER
!     Last change: MSG 11/15/2006 2:00:02 PM



!
!             THE REPORT WRITER FOR ENERGY LIMITED UNITS
!                       COPYRIGHT (C) 1987
!                 M.S. GERBER & ASSOCIATES, INC.
!                       ALL RIGHTS RESERVED
!

!
      SUBROUTINE ENRG_LIMITED_REPORT(EL_SO2_ANNUAL,EL_SO2_PERIOD)

      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use enrg_prod_costs
      use prodcom
      use prod2com
	  use production
      use elrptcom
      use enrg_prod_costs
	  implicit none
      INTEGER (KIND=2) :: I
      REAL :: EL_SO2_ANNUAL,EL_SO2_PERIOD(12)
      CHARACTER (LEN=6) :: MONTH_NAMES(12)=(/'Jan  ','Feb  ','March',
     + 'April','May  ','June ','July ','Aug  ','Sept ','Oct  ','Nov  ',
     + 'Dec  '/)
      CHARACTER (LEN=6) :: SEASON_NAMES(5)=(/'   1','   2','   3','   4'
     + ,'   5'/)

      IF(TESTING_PLAN .or. .true.) RETURN
      CALL DETAILED_REPORT_HEADER
      WRITE(9,1040)
      IF(get_PRODUCTION_PERIODS_in() == 12) THEN
         DO I = 1, 12
             WRITE(9,2010) MONTH_NAMES(I),MONTHLY_BEFORE_ENERGY(I),
     +              MONTHLY_BEFORE_PEAK(I),MONTHLY_BEFORE_BASE(I),
     +              MONTHLY_AFTER_ENERGY(I),
     +              MONTHLY_AFTER_PEAK(I),MONTHLY_AFTER_BASE(I),
     +              (MONTHLY_BEFORE_ENERGY(I)-MONTHLY_AFTER_ENERGY(I)),
     +              FLOAT(MONTHLY_ENERGY_COSTS(I))/1000000.,
     +              EL_SO2_PERIOD(I)
         ENDDO
      ELSE IF(get_PRODUCTION_PERIODS_in() > 1) THEN
         DO I = 1, get_PRODUCTION_PERIODS_in()
             WRITE(9,2010) SEASON_NAMES(I),MONTHLY_BEFORE_ENERGY(I),
     +              MONTHLY_BEFORE_PEAK(I),MONTHLY_BEFORE_BASE(I),
     +              MONTHLY_AFTER_ENERGY(I),
     +              MONTHLY_AFTER_PEAK(I),MONTHLY_AFTER_BASE(I),
     +              (MONTHLY_BEFORE_ENERGY(I)-MONTHLY_AFTER_ENERGY(I)),
     +              FLOAT(MONTHLY_ENERGY_COSTS(I))/1000000.,
     +              EL_SO2_PERIOD(I)
         ENDDO
      ENDIF
      WRITE(9,2010) 'Annual',ANNUAL_BEFORE_ENRG/1000,ANNUAL_PEAK1,
     +      ANNUAL_BASE1,ANNUAL_AFTER_ENRG/1000,ANNUAL_PEAK2,
     +      ANNUAL_BASE2,ANNUAL_DEFERENCE_ENRG/1000,
     +      (ENRG_LIMITED_VAR_COST(1) +
     +       ENRG_LIMITED_VAR_COST(2) +
     +       ENRG_LIMITED_VAR_COST(3) +
     +       ENRG_LIMITED_FIXED_COST(1) +
     +       ENRG_LIMITED_FIXED_COST(2) +
     +       ENRG_LIMITED_FIXED_COST(3))/1000000.,
     +      EL_SO2_ANNUAL
      RETURN
 1040 FORMAT('0','   Energy Limited Report'//
     + 9X,'Before Modification',10X,'After Modification',9X,'Energy'
     +/11X,' Energy     Peak     Base     Energy     Peak     Base',
     +    '   Difference  Cost    SO2'
     +/13X,2('(GWH)     (MW)     (MW)      '),'(GWH)    (M$)')
 2010 FORMAT(4X,A,1X,2(I7,2X,I7,2X,I7,4X),I7,F8.1,F8.2)
      END
