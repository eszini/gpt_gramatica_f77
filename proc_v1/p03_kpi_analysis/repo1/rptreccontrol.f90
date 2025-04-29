module RptRecControl
implicit none
contains

!     ******************************************************************
!     RptRecControl.F95
!     Copyright(c)  2000
!
!     Created: 1/5/2010 11:27:30 AM
!     Author : MARK S GERBER
!     Last change: MSG 4/6/2010 11:33:00 AM
!     ******************************************************************
! routines to provide correct output write record and end point number
! used becasue of iterations in GRX routines
      INTEGER FUNCTION GRX_HORIZON_REPORTING(YR_ITER)
      USE GRX_PLANNING_ROUTINES
      use rpt_data
      !use irec_endpoint_control
         REAL (KIND=4):: YR_ITER(2)
         CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
         GRX_HORIZON_REPORTING = 1
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND. &
                            GREEN_MRX_METHOD() == 'GX' .AND. &
                                            .NOT. HORIZONS_ACTIVE) THEN
             YR_ITER(2) = FLOAT(GRX_ITERATIONS)
             GRX_HORIZON_REPORTING = 2
         ENDIF
      END FUNCTION GRX_HORIZON_REPORTING



end module rptreccontrol