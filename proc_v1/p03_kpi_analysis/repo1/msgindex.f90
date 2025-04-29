!*****************************************************************
!     msg95RWindex.for
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 4/20/2003 11:45:49 AM
!     Author : MARK S GERBER
!     Last change: MSG 10/26/2010 9:30:42 AM
!     ******************************************************************
!***********************************************************************
      SUBROUTINE  INDEX_RESULT_FILES(StudyName)
!***********************************************************************
!
      use WinFileFindData
      USE IREC_ENDPOINT_CONTROL
      use end_routine
      use logging
      use file_indexing
      CHARACTER (LEN=256) :: STUDY_DIRECTORY,GET_RESULTS_DIRECTORY
      CHARACTER (LEN=256) :: COMMAND_LINE,SOFTWARE_DIRECTORY
      CHARACTER (LEN=5) :: StudyName
      CHARACTER (LEN=2) :: ReportCode
      LOGICAL :: INDEX_MADE
      INTEGER :: I,LENGHT,StartTime,EndTime
      REAL :: REAL_TIME
      character (len=*) :: R_MESSAGE_TEXT
      CHARACTER (LEN=30) :: ReportName
      INTEGER (KIND=2) :: MaxReportCodes
      PARAMETER(MaxReportCodes=122)
      CHARACTER (LEN=2), DIMENSION(MaxReportCodes) :: ReportCodeList= &
                    (/"DP","TB","CC","PD","UD","TE","TA","TQ","TZ","MZ", &
                      "EM","DO","MS","DW","CX","HX","TO","HM","TP","SD", &
                      "HC","TI","TK","TR","PI","TD","BH","TL","TN","LM", &
                      "PB","PA","AR","AO","AL","MX","RM","EP","TS","PS", &
                      "TM","TT","UM","UM","EC","CX","FD","FM","LD","DC", &
                      "MD","DT","RA","FA","ST","SA","FI","FX","MC","AI", &
                      "DI","CD","BD","XD","AE","AF","DD","DB","NF","OX", &
                      "HE","HA","VH","VD","VM","VP","GK","WO","WE","MI", &
                      "MM","MB","MT","D2","D3","R1","R2","R3","R4","U1", &
                      "U2","U3","U4","T1","T2","T3","T4","QL","Q1","Q2", &
                      "Q3","M3","M4","M2","M5","GM","GK","NB","QS","QC", &
                      "GI","GE","GX","GB","GB","GB","GP","GZ","GT","GN", &
                      "GJ","GW"/)
      CHARACTER (LEN=30),DIMENSION(MaxReportCodes) :: ReportTitle
      CHARACTER (LEN=80) :: SCREEN_MESSAGES
      type(WIN32_FIND_DATA) :: FileData

      integer :: FileHandle, FileNameLenght, CurrentFileHandle
      character (len=256) :: FileMask,MESSAGE_TEXT
      logical :: void_logical,FILE_EXISTS
      CHARACTER (LEN=256) :: GET_WORKING_FILE_DIRECTORY,FILE_NAME
!
!         IF(.NOT. HORIZONS_ACTIVE) THEN
            STUDY_DIRECTORY  = GET_RESULTS_DIRECTORY()
            CALL TIMER(StartTime)
            FILE_NAME = TRIM(SOFTWARE_DIRECTORY())//"MSGINDXR.EXE"
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) THEN
               FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSGINDXR.EXE"
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               IF(.NOT. FILE_EXISTS) THEN
                  FILE_NAME=TRIM(GET_WORKING_FILE_DIRECTORY())// &
                                                          "MSGINDXR.EXE"
                  INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               ENDIF
            ENDIF
            IF(FILE_EXISTS) THEN
               COMMAND_LINE = TRIM(FILE_NAME)//" "// &
                             TRIM(StudyName)//" "//TRIM(STUDY_DIRECTORY)
               CALL SYSTEM(COMMAND_LINE,.FALSE.,.true.)
            ENDIF
            CALL TIMER(EndTime)
            RETURN
!         ENDIF
!
         ReportCode= 'A '
!
         ReportTitle( 1) = "Energy Products"
         ReportTitle( 2) = "Monthly Summary"
         ReportTitle( 3) = "Transact Class Revenue"
         ReportTitle( 4) = "Market Duration"
         ReportTitle( 5) = "Transaction Group Duration"
         ReportTitle( 6) = "Monthly CL Trans Unit"
         ReportTitle( 7) = "Annual CL Unit"
         ReportTitle( 8) = "Monthly Transactions"
         ReportTitle( 9) = "Transact Fuel"
         ReportTitle(10) = "Monthly Group"
         ReportTitle(11) = "Monthly EL Unit"
         ReportTitle(12) = "CL Dispatch Order"
         ReportTitle(13) = "Transact Dispatch Order"
         ReportTitle(14) = "Calendar Day of Week"
         ReportTitle(15) = "Daily Utilization"
         ReportTitle(16) = "Hourly Utilization"
         ReportTitle(17) = "Load After Trans"
         ReportTitle(18) = "Regional Price"
         ReportTitle(19) = "MC After Trans"
         ReportTitle(20) = "Capacity Cost"
         ReportTitle(21) = "Capacity"
         ReportTitle(22) = "Hourly Transaction"
         ReportTitle(23) = "Last Traded Price"
         ReportTitle(24) = "Trans Revenues"
         ReportTitle(25) = "Increm Prod Cost"
         ReportTitle(26) = "Hourly Margins"
         ReportTitle(27) = "Hourly Load B4H"
         ReportTitle(28) = "Hourly Load B4 AFH"
         ReportTitle(29) = "Hourly MC B4"
         ReportTitle(30) = "Hourly Lamda"
         ReportTitle(31) = "Prod Cost B4"
         ReportTitle(32) = "Prod Cost AF"
         ReportTitle(33) = "Resource Additions"
         ReportTitle(34) = "Resources Tested"
         ReportTitle(35) = "Resource Options"
         ReportTitle(36) = "MRX Annual Resources"
         ReportTitle(37) = "Reserve Margin"
         ReportTitle(38) = "Expansion Pattern"
         ReportTitle(39) = "Annual CL Summary"
         ReportTitle(40) = "Annual Pool Summary"
         ReportTitle(41) = "Monthly CL Unit"
         ReportTitle(42) = "Monthly CL Summary"
         ReportTitle(43) = "Maintenance Report"
         ReportTitle(44) = "Availability Report"
         ReportTitle(45) = "Economy Report"
         ReportTitle(46) = "Monthly Contracts"
         ReportTitle(47) = "Forecasted Days"
         ReportTitle(48) = "Forecasted Months"
         ReportTitle(49) = "Load Duration Curves"
         ReportTitle(50) = "Load Management Cost"
         ReportTitle(51) = "Monthly LM Days"
         ReportTitle(52) = "DSM Rate Impact"
         ReportTitle(53) = "Class Revenue"
         ReportTitle(54) = "Cost Distribution"
         ReportTitle(55) = "Service Transactions"
         ReportTitle(56) = "Annual Service Transactions"
         ReportTitle(57) = "Fuel Inventory"
         ReportTitle(58) = "Resource Decisions"
         ReportTitle(59) = "Marginal Cost Report"
         ReportTitle(60) = "Asset Class Financial"
         ReportTitle(61) = "Income Drilling"
         ReportTitle(62) = "Cash Drilling"
         ReportTitle(63) = "Balance Sheet Drilling"
         ReportTitle(64) = "Tax Drilling"
         ReportTitle(65) = "Existing Assets"
         ReportTitle(66) = "Future Assets"
         ReportTitle(67) = "Debits"
         ReportTitle(68) = "Debt"
         ReportTitle(69) = "Nuclear Fuel"
         ReportTitle(70) = "Hourly Outage"
         ReportTitle(71) = "HEC Hourly"
         ReportTitle(72) = "Hydro Aggregation"
         ReportTitle(73) = "Hourly Products"
         ReportTitle(74) = "Daily Products"
         ReportTitle(75) = "Monthly Products"
         ReportTitle(76) = "Portfolio Detail"
         ReportTitle(77) = "Monthly Greeks"
         ReportTitle(78) = "WVPA Work Orders"
         ReportTitle(79) = "WVPA Department Expenditures"
         ReportTitle(80) = "Monthly Income"
         ReportTitle(81) = "Monthly Cash"
         ReportTitle(82) = "Monthly Balance Sheet"
         ReportTitle(83) = "Monthly Income Tax"
         ReportTitle(84) = "Market Prices by Price Point"
         ReportTitle(85) = "Summary Price by Price Point"
         ReportTitle(86) = "Res Mar by Price Point"
         ReportTitle(87) = "Res Mar by NERC SubRegion"
         ReportTitle(88) = "Res Mar by NERC Region"
         ReportTitle(89) = "Res Mar by Country"
         ReportTitle(90) = "Prod by Price Point"
         ReportTitle(91) = "Prod by NERC SubRegion"
         ReportTitle(92) = "Prod by NERC Region"
         ReportTitle(93) = "Prod by Country"
         ReportTitle(94) = "Fuel by Price Point"
         ReportTitle(95) = "Fuel by NERC SubRegion"
         ReportTitle(96) = "Fuel by NERC Region"
         ReportTitle(97) = "Fuel by Country"
         ReportTitle(98) = "All Thermal Assets Report"
         ReportTitle(99) = "Selected Asset Report"
         ReportTitle(100) = "Selected Assets by Prime Mover"
         ReportTitle(101) = "All Assets by Prime Mover"
         ReportTitle(102) = "Emission by State/Province"
         ReportTitle(103) = "Emission by NERC SubRegion"
         ReportTitle(104) = "Emission by Emission Region"
         ReportTitle(105) = "Emission by Country"
         ReportTitle(106) = "Gas Monthly Node Price"
         ReportTitle(107) = "Gas Monthly Basis Differential"
         ReportTitle(108) = "World LNG Net-Forward Report"
         ReportTitle(109) = "World LNG Summary Report"
         ReportTitle(110) = "World LNG Continent Summary"
         ReportTitle(111) = "Gas Monthly Reserve Volume"
         ReportTitle(112) = "Gas Monthly Link Volume"
         ReportTitle(113) = "Gas Monthly Link Price"
         ReportTitle(114) = "Gas Monthly Node Volume"
         ReportTitle(115) = "Gas Monthly Supply Node Volume"
         ReportTitle(116) = "Gas Monthly Demand Node Volume"
         ReportTitle(117) = "Gas Daily Node Price"
         ReportTitle(118) = "Gas Daily Node Ave Price"
         ReportTitle(119) = "Gas Daily Node Volume"
         ReportTitle(120) = "Gas Daily Basis Differential"
         ReportTitle(121) = "Gas Daily Link Volume"
         ReportTitle(122) = "Gas Daily Link Price"

!
         STUDY_DIRECTORY  = GET_RESULTS_DIRECTORY()
         FileMask = trim(STUDY_DIRECTORY)// &
                                         '???'//trim(StudyName)//'.*D'
         FileMask = trim(FileMask)//CHAR(0)
         FileHandle = FindFirstFileA(carg(FileMask),FileData)
         if(FileHandle > 0) then
            CurrentFileHandle = FileHandle
            DO WHILE (CurrentFileHandle > 0)
               FileNameLenght = INDEX(FileData%FileName,CHAR(0))
               ReportCode = FileData%FileName(FileNameLenght-3: &
                                                       FileNameLenght-2)
               if(ReportCode /= 'LC') then
                  ReportName = "New Detailed Report"
                  DO I = 1, MaxReportCodes
                     if(i==36) then
                        ReportName=ReportName ! Debugstop
                     end if
                     
                     IF(ReportCodeList(I) == ReportCode) THEN
                        ReportName = ReportTitle(I)
                        EXIT
                     ENDIF
                  ENDDO

                  call write_log_entry("msgindex:0001", &
                  "calling index_this_detailed_report. " // &
                  "StudyName: " // trim(StudyName) // ", " // &
                  "ReportCode: " // trim(ReportCode) // ", " // &
                  "ReportName: " // trim(ReportName) // ", " // &
                  "Study_directory: " // trim(STUDY_DIRECTORY) // ".")
                  
                  INDEX_MADE = INDEX_THIS_DETAILED_REPORT(StudyName, &
                                                        ReportCode, &
                                                        ReportName, &
                                                        STUDY_DIRECTORY)
               endif
               CurrentFileHandle = &
                                FindNextFileA(carg(FileHandle),FileData)
            ENDDO
            void_logical = FindClose(carg(FileHandle))
         endif
!
         CALL TIMER(EndTime)
         REAL_TIME = FLOAT(EndTime-StartTime)/100.
         IF(REAL_TIME <= 60.) THEN
            IF(REAL_TIME < 10.) THEN
                  WRITE(SCREEN_MESSAGES,'(A,F6.3,A)') ' The reports took', & 
                                         REAL_TIME,' seconds to index .'
            ELSE
                  WRITE(SCREEN_MESSAGES,'(A,F7.3,A)') ' The reports took', & 
                                         REAL_TIME,' seconds to index .'
            ENDIF
         ELSE
            REAL_TIME = REAL_TIME/60.
                WRITE(SCREEN_MESSAGES,'(A,F6.3,A)') ' The reports took', &
                                         REAL_TIME,' minutes to index .'
         ENDIF
         CALL write_scroll_line_RW(SCREEN_MESSAGES,1)

         RETURN
!***********************************************************************
         ENTRY TWO_DIMENSION_REPORT
!***********************************************************************
!
            WRITE(4,*) ReportCode," ",ReportName
            WRITE(4,*)'***Because there are less than two dimensions,'
            WRITE(4,*) 'This file cannot be indexed for use by this ', &
                      'program for use in the MIDAS Gold Results module'
            WRITE(4,*) 'The results module will index it.***'
         RETURN
!***********************************************************************
         ENTRY PROCESS_ERROR_MESSAGES(R_MESSAGE_TEXT)
!***********************************************************************
            WRITE(4,*) trim(R_MESSAGE_TEXT)
         RETURN
      END
!***********************************************************************
      SUBROUTINE FILE_INQUIRE_95(FILE_NAME,EXISTS,FILE_DATE,LENGHT)
!***********************************************************************
!
      USE SERVICE_ROUTINES
      INTEGER (KIND=4) :: FILE_DATE,LENGHT
      LOGICAL (KIND=4) :: EXISTS
      CHARACTER (LEN=*) :: FILE_NAME
      INTEGER,DIMENSION(13) :: ST
      INTEGER :: IY
         IY = STAT(FILE_NAME,ST)
         IF(IY == 0) THEN
            EXISTS = .TRUE.
            LENGHT = ST(8)
            FILE_DATE = ST(10)
         ELSE
            EXISTS = .FALSE.
            LENGHT = 0
            FILE_DATE = 0
         ENDIF
      RETURN
      END
!***********************************************************************
      SUBROUTINE FILE_INQUIRE(FILE_NAME,EXISTS,R_DATE,R_TIME,R_SIZE)
!***********************************************************************
!
      USE SERVICE_ROUTINES
      INTEGER (KIND=4) :: R_DATE,R_TIME,R_SIZE
      LOGICAL (KIND=4) :: EXISTS
      CHARACTER (LEN=*) :: FILE_NAME
      INTEGER,DIMENSION(13) :: ST
      INTEGER IY
         IY = STAT(FILE_NAME,ST)
         IF(IY == 0) THEN
            EXISTS = .TRUE.
            R_SIZE = ST(8)
            R_DATE = ST(10)
            R_TIME = 0
         ELSE
            EXISTS = .FALSE.
            R_SIZE = 0
            R_DATE = 0
            R_TIME = 0
         ENDIF
      RETURN
      END
!***********************************************************************
      FUNCTION LAHEY_LF95()
!***********************************************************************
!
      logical (KIND=1) :: LAHEY_LF95
         LAHEY_LF95 = .TRUE.
      RETURN
      END
!***********************************************************************
      SUBROUTINE ERASEWC(DESCRIPTION_FIELD)
!***********************************************************************
!
!      INCLUDE 'SPINLIB.MON'
      use WinFileFindData
      type(WIN32_FIND_DATA) :: FileData

      integer :: FileHandle, CurrentFileHandle
      character (len=256) :: FileMask
      logical :: void_logical,file_is_open
      INTEGER :: IOS
      CHARACTER (LEN=*) :: DESCRIPTION_FIELD
!
         FileMask = trim(DESCRIPTION_FIELD)//CHAR(0)
         FileHandle = FindFirstFileA(carg(FileMask),FileData)
         if(FileHandle > 0) then
            CurrentFileHandle = FileHandle
            DO WHILE (CurrentFileHandle > 0)
               INQUIRE(file=FileData%FileName,opened=file_is_open)
               IF(.NOT. file_is_open) open(10,file=FileData%FileName)
               close(10,status='delete',IOSTAT=IOS)
               CurrentFileHandle = &
                                FindNextFileA(carg(FileHandle),FileData)
            ENDDO
            void_logical = FindClose(carg(FileHandle))
         endif
      RETURN
      END
!***********************************************************************
      subroutine Sec2Date(ETSecSince1Jan1970,cYear,cMonth,cDay) ! ,cHour
!***********************************************************************
!
      integer (KIND=4) :: SecPerDay=86400 ! 86400=24*60*60
      integer (KIND=2) :: DaysInNLYMo(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
!     end of parameter declarations
      integer (KIND=4) :: ETSecSince1Jan1970
      integer (KIND=4) :: cYear,cMonth,cDay,cHour,cMinute,cSecond
!     end of argument declarations
      integer (KIND=4) :: DaysInPriorYear,DaysInPriorYearsSince1970(0:99)=0, &
        SecInDay
      integer (KIND=2) :: iYr,iMo,WholeDaysSince1Jan1970,JulianDay, &
        DaysInPriorMonths(13)
!     end of local-variable declarations
      save SecPerDay,DaysInNLYMo,DaysInPriorYearsSince1970
!
      if(DaysInPriorYearsSince1970(1)==0) then ! initialize array
        DaysInPriorYearsSince1970(0)=0   ! for 1970
!       DaysInPriorYearsSince1970(1)=366 ! for 1971
        do iYr=1,99 ! assign values through calendar-year 2069
          DaysInPriorYear=365
          if(mod(iYr,int(4,2))==1) DaysInPriorYear=366 ! note that 2000 is leap
          DaysInPriorYearsSince1970(iYr)= &
          DaysInPriorYearsSince1970(iYr-1)+DaysInPriorYear
        end do
      end if
!
!     assign defaults valid for extreme cases
      cMonth=1
      cDay=1
      cHour=0
      cMinute=0
      cSecond=0
      WholeDaysSince1Jan1970=ETSecSince1Jan1970/SecPerDay
      do iYr=99,0,-1
        if(DaysInPriorYearsSince1970(iYr)<=WholeDaysSince1Jan1970) &
          exit
      end do
      if(iYr<0) then
        cYear=1970
        return ! with minimal year, indicating underflow
      end if
      JulianDay=WholeDaysSince1Jan1970-DaysInPriorYearsSince1970(iYr)+1
      if((iYr==99).and.(JulianDay>365)) then ! 2069 was not a leap-year
        cYear=2070
        return ! with maximal year, indicating overflow
      end if
      cYear=1970+iYr
!
    ! fill array DaysInPriorMonths for cYear
      DaysInPriorMonths(1)=0
      DaysInPriorMonths(2)=DaysInNLYMo(1)
      DaysInPriorMonths(3)=DaysInNLYMo(2)+DaysInPriorMonths(2)
      if(mod(cYear,4)==0) DaysInPriorMonths(3)=  & ! February of leap-years
                          DaysInPriorMonths(3)+1
      do iMo=4,13
        DaysInPriorMonths(iMo)= &
        DaysInPriorMonths(iMo-1)+DaysInNLYMo(iMo-1)
      end do
      JulianDay=JulianDay-1 ! now 0-based count of prior days
      do cMonth=12,1,-1 ! loop should exit with cMonth>=1
        if(DaysInPriorMonths(cMonth)<=JulianDay) exit
      end do ! with cMonth determined, expected to be on [1,12]
!
      cDay=1+JulianDay-DaysInPriorMonths(cMonth)+1  ! 1-based result on [1:31]
!
!     disable next 4 lines if Hour/Minute/Second is not of interest
      SecInDay=ETSecSince1Jan1970-WholeDaysSince1Jan1970*SecPerDay
      cHour=SecInDay/3600                    ! 0-based result on [0:23]
      cMinute=(SecInDay/60)-cHour*60         ! 0-based result on [0:59]
      cSecond=SecInDay-60*(cMinute+60*cHour) ! 0-based result on [0:59]
!
      end ! subroutine Sec2Date
!***********************************************************************
      Subroutine StoreWINDOWS_Info(r_scroll_handle)
!***********************************************************************
      use realwin
      integer :: r_scroll_handle
      integer, save :: scroll_handle
      CHARACTER (LEN=*) :: SCREEN_MESSAGES
      integer :: WINDOW
      logical (kind=1) :: SP_CAPEX_DISPLAY_ACTIVE
        scroll_handle = r_scroll_handle
      return
!***********************************************************************
      entry write_scroll_line_RW(SCREEN_MESSAGES,WINDOW)
!***********************************************************************
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         IF(WINDOW == 0) THEN
            CALL RW_UPDATE_RUNTIME_MESSAGES(SCREEN_MESSAGES)
         ELSEIF(WINDOW == 2) THEN
            CALL RW_PROGRAM_LOCATION(SCREEN_MESSAGES)
         ELSE
            CALL write_scroll_line(window = scroll_handle, &
                                         text = trim(SCREEN_MESSAGES))
         ENDIF
      return
      end
!***********************************************************************
      subroutine MyName(PathName) ! path & name of current .exe-file
!***********************************************************************
      use realwin
      character (LEN=*) :: PathName
      integer :: Stat
         call get_exe_name(file_name=PathName,STAT=Stat)
      RETURN
      end ! subroutine MyName
!
! ERROR BOX ROUTINE
!
!***********************************************************************
      SUBROUTINE ERROR_BOX(LINE,COLUMN,LENGHT,WITHD)
!***********************************************************************
!
      INTEGER (KIND=2) :: LINE,COLUMN,LENGHT,WITHD
      RETURN
      END
      SUBROUTINE WRITE_CONSOL_LINE(SCREEN_MESSAGES)
      CHARACTER (LEN=*) :: SCREEN_MESSAGES
      RETURN
      END
      SUBROUTINE WRITE_LF77_SCREEN(SCREEN_MESSAGES)
      CHARACTER (LEN=*) :: SCREEN_MESSAGES
      RETURN
      END
!***********************************************************************
!
!     A SUBROUTINE TO REVERSE VIDEO THE MODULE BEING RUN
!
!***********************************************************************
      SUBROUTINE WRITE_INVERSE_VIDEO(WND1,NAME,ROW,COLUMN)
      INCLUDE 'SPINLIB.MON'
      CHARACTER (LEN=*) :: NAME
      integer :: WND1
      INTEGER :: ROW,COLUMN
      INTEGER (KIND=2) :: R_NORMAL,R_INVERSE
!
         CALL MG_LOCATE_WRITE(ROW,COLUMN,NAME,3,WND1)
      RETURN
!***********************************************************************
      ENTRY WRITE_NORMAL_VIDEO(WND1,NAME,ROW,COLUMN)
!***********************************************************************
      RETURN
!***********************************************************************
      ENTRY STORE_VIDEO_MODES(R_INVERSE,R_NORMAL)
!***********************************************************************
      RETURN
      END
