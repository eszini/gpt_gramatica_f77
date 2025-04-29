!     ******************************************************************
!     ABBCapacityMarketRept.F95
!     Copyright(c) DrG Solutions 2013
!
!     Created: 9/15/2015 11:10:15 AM
!     Author : Mark S Gerber
!     Last change: msg 9/14/2016 12:03:32 PM
!			MSG 6/1/2016 2:44:53 PM

!     ******************************************************************

!***********************************************************************
      SUBROUTINE ABB_CapacityMarketReport(year_ord)
!***********************************************************************
         USE ABB_CapMarketRptData
         use grx_planning_routines
         use endpoint
         use tf_objt_interfaces
         
         INTEGER (KIND=2) :: MO,Market,TotalMarkets,year_ord
         REAL (KIND=4) :: Zero = 0.0,       &
                          BalancingRatio,                &
                          ReserveRequirement,            &
                          PPR,                           &
                          NECapacityPenalty,             &
                          Cone,                          &
                          NetCone,                       &
                          CapacitySupplyObligation,      &
                          PerfScore,                     &
                          NEMimimumPyament,              &
                          NEMaximumMonthlyPayment,       &
                          THE_RATIO_OF_A_TO_B
!
         LOGICAL (KIND=4), SAVE :: FILE_IS_OPEN = .FALSE.
         INTEGER (KIND=4) :: ABB_CapacityMarketReptHeader
         INTEGER (KIND=4), SAVE :: NEXT_REC
!
         CHARACTER (LEN=9) ::  CL_MONTH_NAME(14) =       &
                            ( / 'January  ','February ', &
                                'March    ','April    ', &
                                'May      ','June     ', &
                                'July     ','August   ', &
                                'September','October  ', &
                                'November ','December ', &
                                'Annual   ','Fiscal Yr'/ )
!
         TotalMarkets = GET_NUMBER_OF_ACTIVE_GROUPS()
         IF(.NOT. FILE_IS_OPEN) THEN
            NEXT_REC = ABB_CapacityMarketReptHeader()
            FILE_IS_OPEN = .true.
         ENDIF
         NEWVars = 0.
         PPR = 0.
         IF(year_ord >= 2018 .AND. year_ord <= 2021) PPR=2000.
         IF(year_ord >= 2022 .AND. year_ord <= 2024) PPR=3500.
         IF(year_ord >= 2025) PPR=5455.
         DO Market = 1, TotalMarkets 
            DO MO = 1, 12
               Cone = MXVars(MO,Market,26)
               NetCone = 0.
               CapacitySupplyObligation = UZVars(MO,Market,20)
               PerfScore = UZVars(MO,Market,21)
!
               ReserveRequirement = THE_RATIO_OF_A_TO_B(        &
                                       RXVars(MO,Market,28),    &
                                       RXVars(MO,Market,27))
!
               BalancingRatio = THE_RATIO_OF_A_TO_B(            &
                                       RXVars(MO,Market,28),    &
                                       CapacitySupplyObligation)
!
               NECapacityPenalty =(PerfScore*BalancingRatio*PPR)/  &
                                               1000000.               
               NEMimimumPyament = (PPR * BalancingRatio *3)/1000000.
               NEMaximumMonthlyPayment = (MAX(1.6*NetCone,Cone) *  &
                                      CapacitySupplyObligation)/1000.
!
               WRITE(8327,REC=NEXT_REC)              &
                     PRT_ENDPOINT(),                 &
                     FLOAT(year_ord),                    &
                     CL_MONTH_NAME(MO),              &
                     CAPACITY_AREA_NAME(Market),     &
                     Zero,                           & 
                     UZVars(MO,Market,1:22),         &
                     TBVars(MO,Market,23:25),        &
                     MXVars(MO,Market,26:26),        &
                     RXVars(MO,Market,27:28),        &
                     BalancingRatio,                 &
                     ReserveRequirement,             &   ! 30
                     PPR,                            &
                     NECapacityPenalty,              &
                     NEWVars(MO,Market,33:34),       &
                     NEMimimumPyament,               &   ! 35
                     NEMaximumMonthlyPayment,        &   ! 36
                     NEWVars(MO,Market,37:40)
!
               NEXT_REC = NEXT_REC + 1
            ENDDO
         ENDDO   
      END SUBROUTINE
!***********************************************************************
      FUNCTION ABB_CapacityMarketReptHeader()
!***********************************************************************
!
        CHARACTER (LEN=256) :: GET_RESULTS_DIRECTORY
        CHARACTER (LEN=5) :: GET_SCENAME
        CHARACTER (LEN=256) :: FILE_NAME
        INTEGER (KIND=2) :: RECORD_LENGTH,ESTABLISH_DETAIL_REPORT_FILE
        INTEGER (KIND=2) :: NEXT_REC,RPT_UNIT_NUM
        INTEGER (KIND=4) :: ABB_CapacityMarketReptHeader
!
        FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"//   &
                                TRIM(GET_SCENAME())//".ABD"
        RPT_UNIT_NUM = 8327
        RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE(FILE_NAME,    &
                  64_2,                     &  ! OVERHEAD_LENGTH  
                   50_2,                     &  ! VARIABLE_NUMBER
                         4_2,                      &  ! DIMENSIONS
                                         RPT_UNIT_NUM,             &
                                         NEXT_REC)
!
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC)                          &
                     'Period              ','C',INT(9,2),'V','D'
         WRITE(RPT_UNIT_NUM,REC=NEXT_REC+1)                        &
                     'Transaction Group   ','C',INT(35,2),'V','D'
         ABB_CapacityMarketReptHeader = NEXT_REC + 2
!         
      RETURN
      END FUNCTION
