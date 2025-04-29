
!     WVPA_DATA_BASES.FOR
!     Copyright(c)  2004
!
!     Created: 2/3/2004 4:44:35 PM
!     Author : MARK S GERBER
!     Last change: MSG 3/30/2004 12:59:22 PM


      SUBROUTINE WVPA_DATA_BASES()
	  use mod_base_year
	  implicit none

!
      INTEGER :: ACCTNO
      CHARACTER (LEN=*) :: DATA_BASE_CLASSIFICATION
      REAL (KIND=4) :: GPV(0:12,*),CUM_BOOK_DEP(0:12,*),NPV(0:12,*)
      REAL (KIND=4) :: BOKDP(0:12,*),RETIRE(0:12,*),AFUDC_CASH(0:12,*)      
      REAL (KIND=4) :: FA_GPV(0:12,0:*),FA_CUM_BOOK_DEP(0:12,0:*)
      REAL (KIND=4) :: FA_NPV(0:12,0:*),FA_BOKDP(0:12,0:*)
      INTEGER (KIND=2) :: FINANCIAL_SIMULATION_YEARS,COMP_ID
      INTEGER (KIND=2) :: ASSET_CLASS_NUM,WVPA_COMPANY_ID,TEMP
      INTEGER (KIND=2), PARAMETER :: MAX_COMP_ID=3
      REAL (KIND=4) :: CAPX_ADDITIONS(0:12,0:*), &
        CASH_EXPENDITURES(0:12,0:*),CWIP_BALANCE(0:12,0:*), &
            ADDENDUM_DATA(0:FINANCIAL_SIMULATION_YEARS,0:12)
      REAL (KIND=4) :: ADDEN_SUM

      REAL (KIND=4), ALLOCATABLE ::  REPORT_DATA(:,:,:,:,:) ! ACCT,VARIABLE,MONTHS,YEARS
      REAL (KIND=4),ALLOCATABLE,SAVE :: CONSTRUCTION_DATABASE(:,:,:,:,:) ! ACCT_PTR,VARIABLES,MO,YR,WORKORDER
      INTEGER (KIND=2), ALLOCATABLE, SAVE :: ACTIVE_WORKORDERS(:,:), &
        WORKORDER_4_COMP(:,:,:)
      CHARACTER (LEN=34), ALLOCATABLE, SAVE :: &
        CONSTRUCTION_WORKORDER_NAMES(:,:,:)
      CHARACTER (LEN=34) :: WORKORDER_NAME
      INTEGER (KIND=2) :: PROJECT_START_MO,PROJECT_START_YR,PROJECT_END_MO, &
        PROJECT_END_YR,WORKORDER
      INTEGER (KIND=2) :: ACCT_PTR,MO,YR,YR1,COL_POS,SUM_ACCT,ACCT,DATA_PTR
      INTEGER (KIND=4), PARAMETER :: MAX_ACCOUNTS_TRACKED = 150, &
        MAX_YEARS_2_REPORT = 5
      INTEGER (KIND=4) :: ACCOUNTS_LOOKUP(0:MAX_ACCOUNTS_TRACKED)
      SAVE ACCOUNTS_LOOKUP,REPORT_DATA
      REAL (KIND=4) :: OUT_RECORD(:,:)
      ALLOCATABLE OUT_RECORD
      CHARACTER (LEN=10) :: COMPANY_NAME(0:MAX_COMP_ID)= &
         (/'Total     ', &
          'WVPA      ',&
          '          ',&
          'SGS       '/)
!
! REPORT OUTPUT VARIABLES
!
      CHARACTER (LEN=6) :: SHORT_MONTH_NAMES
      INTEGER (KIND=2) :: WVPA_PLNT_BALANCES_RPT_HEADR
      LOGICAL (KIND=1) :: WVPA_PLANT_BAL_RPT_OPEN=.FALSE.
      INTEGER :: WVPA_PLANT_BAL_REC
      INTEGER (KIND=2) :: WVPA_PLANT_BAL_UNIT
      SAVE WVPA_PLANT_BAL_REC,WVPA_PLANT_BAL_UNIT
      INTEGER (KIND=2) :: WVPA_CUM_DEP_RPT_HEADR
      LOGICAL (KIND=1) :: WVPA_CUM_DEP_RPT_OPEN=.FALSE.
      INTEGER :: WVPA_ACCUM_DEP_REC
      INTEGER (KIND=2) :: WVPA_ACCUM_DEP_UNIT
      SAVE WVPA_ACCUM_DEP_UNIT,WVPA_ACCUM_DEP_REC
      LOGICAL (KIND=1), SAVE :: WVPA_COMP_CONSTR_SUMRY_RPT_OPEN=.FALSE.
      INTEGER (KIND=2), SAVE :: WVPA_COMP_CONSTR_SUMRY_UNIT
      INTEGER (KIND=4), SAVE :: WVPA_COMP_CONSTR_SUMRY_REC
      INTEGER (KIND=2) :: WVPA_COMP_CONSTR_SUMY_RPT_HEADR
      CHARACTER (LEN=40) :: ACCT_NAME_WORKORDER
!
! REPORT CONSTRUTION SUMMARY VARIABLES
!
      LOGICAL (KIND=1) :: WVPA_CONSTR_SUMRY_RPT_OPEN=.FALSE.
      INTEGER (KIND=2) :: WVPA_CONSTR_SUMRY_UNIT, &
        WVPA_CONSTR_SUMY_RPT_HEADR,CONSTR_SUMRY_REPT_COLS, &
        WRKORDR,WORKORDERS_WRITTEN
      INTEGER (KIND=4) :: WVPA_CONSTR_SUMRY_REC
      CHARACTER (LEN=20) :: ACCT_NAME
      CHARACTER (LEN=34) :: SUB_TOTAL_NAME
      REAL (KIND=4) :: GRAND_TOTALS(0:10),SUB_TOTALS(0:10)
!
! REPORT SPECIFIC VARIABLES
!
      INTEGER (KIND=2) :: ENDPOINT
      INTEGER (KIND=2) :: PLANT_BAL_REPT_COLS
      PARAMETER (PLANT_BAL_REPT_COLS=13)
      CHARACTER (LEN=30) ::  PLANT_BAL_REPT_NAMES(13)=(/ &
                     'Gross Plant Beg Balance   ', &
                     'Construction Additions    ', &
                     'Accounting Adjustments    ', &
                     'Retirements               ', &
                     'Gross Plant Ending Balance', &
                     'Net Additions             ', &
                     'Accum Depr Beg Balance    ', &
                     'Depreciation              ', &
                     'Accounting Adjustments    ', &
                     'Retirements               ', &
                     'Net Salvage Proceeds      ', &
                     'Accum Depr End Balance    ', &
                     'Net Change                '/)
!      
         IF(ALLOCATED(REPORT_DATA)) &
             DEALLOCATE(REPORT_DATA,CONSTRUCTION_DATABASE,ACTIVE_WORKORDERS, &
                WORKORDER_4_COMP,CONSTRUCTION_WORKORDER_NAMES)
                
         ALLOCATE(REPORT_DATA(0:MAX_COMP_ID,0:MAX_ACCOUNTS_TRACKED, &
            0:15,0:12,0:30), &               
                  CONSTRUCTION_DATABASE(0:MAX_ACCOUNTS_TRACKED, &
                  0:10,0:12,0:30,0:150), &             
                  ACTIVE_WORKORDERS(0:MAX_ACCOUNTS_TRACKED,0:30), &
                  WORKORDER_4_COMP(0:MAX_ACCOUNTS_TRACKED,0:30,0:150), &
                    CONSTRUCTION_WORKORDER_NAMES(0:MAX_ACCOUNTS_TRACKED, &
                                                            0:30,0:150))
         REPORT_DATA = 0.
         ACCOUNTS_LOOKUP = 0.
         CONSTRUCTION_DATABASE = 0.
         ACTIVE_WORKORDERS = 0.
         WORKORDER_4_COMP = -99
         CONSTRUCTION_WORKORDER_NAMES = " "
! NOTE THAT SUB TOTAL POINTS ARE AT 10, 20, 30, 40, 55, 65, 75, 90, 97 AND 104
! GRAND TOTAL IS AT 120
! STEAM 1-10    
         ACCOUNTS_LOOKUP(1) = 31000        
         ACCOUNTS_LOOKUP(2) = 31100        
         ACCOUNTS_LOOKUP(3) = 31200        
         ACCOUNTS_LOOKUP(4) = 31400        
         ACCOUNTS_LOOKUP(5) = 31500        
         ACCOUNTS_LOOKUP(6) = 31600        
         ACCOUNTS_LOOKUP(7) = 31900        
         ACCOUNTS_LOOKUP(8) = 31111
         ACCOUNTS_LOOKUP(9) = 31701 ! ASSET RETIRE LANDFILL 
! OTHER 11-20     
         ACCOUNTS_LOOKUP(11) = 34000        
         ACCOUNTS_LOOKUP(12) = 34100        
         ACCOUNTS_LOOKUP(13) = 34200        
         ACCOUNTS_LOOKUP(14) = 34300        
         ACCOUNTS_LOOKUP(15) = 34400        
         ACCOUNTS_LOOKUP(16) = 34500        
         ACCOUNTS_LOOKUP(17) = 34600        
! VERMILLON CO. 21-30     
         ACCOUNTS_LOOKUP(21) = 34010        
         ACCOUNTS_LOOKUP(22) = 34110        
         ACCOUNTS_LOOKUP(23) = 34210        
         ACCOUNTS_LOOKUP(24) = 34310        
         ACCOUNTS_LOOKUP(25) = 34410        
         ACCOUNTS_LOOKUP(26) = 34510        
         ACCOUNTS_LOOKUP(27) = 34610        
! LAWRENCE CO. 31-40     
         ACCOUNTS_LOOKUP(31) = 34020        
         ACCOUNTS_LOOKUP(32) = 34120        
         ACCOUNTS_LOOKUP(33) = 34220        
         ACCOUNTS_LOOKUP(34) = 34320        
         ACCOUNTS_LOOKUP(35) = 34420        
         ACCOUNTS_LOOKUP(36) = 34520        
         ACCOUNTS_LOOKUP(37) = 34620        
! TRANSMISSION  41-55
         ACCOUNTS_LOOKUP(41) = 35000        
         ACCOUNTS_LOOKUP(42) = 35010        
         ACCOUNTS_LOOKUP(43) = 35200        
         ACCOUNTS_LOOKUP(44) = 35300        
         ACCOUNTS_LOOKUP(45) = 35400        
         ACCOUNTS_LOOKUP(46) = 35500        
         ACCOUNTS_LOOKUP(47) = 35600        
         ACCOUNTS_LOOKUP(48) = 35610       
         ACCOUNTS_LOOKUP(49) = 30300       
! LOAD MANAGEMENT 56-65
         ACCOUNTS_LOOKUP(56) = 35350        
         ACCOUNTS_LOOKUP(57) = 35351        
         ACCOUNTS_LOOKUP(58) = 36050        
         ACCOUNTS_LOOKUP(59) = 36250        
         ACCOUNTS_LOOKUP(60) = 36251        
         ACCOUNTS_LOOKUP(61) = 37150        
! DISTRIBUTION 66-75 
         ACCOUNTS_LOOKUP(66) = 36000        
         ACCOUNTS_LOOKUP(67) = 36100        
         ACCOUNTS_LOOKUP(68) = 36200        
         ACCOUNTS_LOOKUP(69) = 36400        
         ACCOUNTS_LOOKUP(70) = 36500        
         ACCOUNTS_LOOKUP(71) = 37000        
! GENERAL 76-90
         ACCOUNTS_LOOKUP(76) = 38900   
         ACCOUNTS_LOOKUP(77) = 39000   
         ACCOUNTS_LOOKUP(78) = 39100   ! Office Furniture & Equipment
         ACCOUNTS_LOOKUP(79) = 39110   ! Computer Equipment
         ACCOUNTS_LOOKUP(80) = 39200   ! Transportation Equipment
         ACCOUNTS_LOOKUP(81) = 39700   
         ACCOUNTS_LOOKUP(82) = 39800 
         ACCOUNTS_LOOKUP(83) = 39112       
         ACCOUNTS_LOOKUP(84) = 39111
         ACCOUNTS_LOOKUP(85) = 39400   ! Tools, Shop and Garage Equipment
         ACCOUNTS_LOOKUP(86) = 39500   ! Laboratory Equipment
! FUTURE PLANT 91-97
         ACCOUNTS_LOOKUP(91) = 88888       
! NON UTILITY PROPERTY 98-105
         ACCOUNTS_LOOKUP(98) = 99999       
! PLANT RECONCILING ITEMS 106-109  THE TOTAL IS IN 110
         ACCOUNTS_LOOKUP(106) = 10200 ! PLANT          
         ACCOUNTS_LOOKUP(107) = 11400 ! ELECTRIC PLANT ACQ ADJ         
         ACCOUNTS_LOOKUP(108) = 10699 ! CCNC Clearing
         ACCOUNTS_LOOKUP(109) = 10500 ! PLANT HELD FOR FUTURE USE
! DEPRECIATION RECONCILING ITEMS 111-124; 132-137  total in 125
         ACCOUNTS_LOOKUP(111) = 10819 ! ACCUM DEPR STEAM G/L          
         ACCOUNTS_LOOKUP(112) = 10858 ! ACCUM DEPR LM CENTRAL G/L   
         ACCOUNTS_LOOKUP(113) = 10859 ! ACCUM DEPR TRANS G/L        
         ACCOUNTS_LOOKUP(114) = 10868 ! ACCUM DEPR LM LOC G/L       
         ACCOUNTS_LOOKUP(115) = 10869 ! ACCUM DEPR DIST G/L         
         ACCOUNTS_LOOKUP(116) = 10879 ! ACCUM DEPR GENERAL G/L      
         ACCOUNTS_LOOKUP(117) = 10881 ! RWIP PRODUCTION             
         ACCOUNTS_LOOKUP(118) = 10882 ! RWIP TRANSMISSION           
         ACCOUNTS_LOOKUP(119) = 10883 ! RWIP DISTRIBUTION           
         ACCOUNTS_LOOKUP(120) = 10886 ! RWIP GENERAL PLANT          
         ACCOUNTS_LOOKUP(121) = 10890 ! EST OC-PH RET             
         ACCOUNTS_LOOKUP(122) = 10810 ! Steam Production LTD Depre
         ACCOUNTS_LOOKUP(123) = 10840 ! Other Production LTD Depre
         ACCOUNTS_LOOKUP(124) = 10850 ! Transmission LTD Depre
!
         ACCOUNTS_LOOKUP(132) = 10855 ! Load Mgmt Central LTD Depre
         ACCOUNTS_LOOKUP(133) = 10860 ! Distribution LTD Depre
         ACCOUNTS_LOOKUP(134) = 10865 ! Load Mgmt Local LTD Depre
         ACCOUNTS_LOOKUP(135) = 10870 ! General Plant LTD Depre
!
! Manufactured Gas Production Plant 141-149 sum in 150
!
         ACCOUNTS_LOOKUP(141) = 30500 ! Structures and Improvements
         ACCOUNTS_LOOKUP(142) = 31300 ! Generating Equipment - Other Processes
         ACCOUNTS_LOOKUP(143) = 31400 ! Coal, Coke, and Ash Handling Equipment
         ACCOUNTS_LOOKUP(144) = 31700 ! Purification Equipment
         ACCOUNTS_LOOKUP(145) = 31800 ! Residual Refining Equipment
         ACCOUNTS_LOOKUP(146) = 32000 ! Other Equipment
! END SET ACCOUNT POSITIONS
!
      RETURN

      ENTRY WVPA_EA_PLANT_DATA_BASE(ACCTNO, &
        WVPA_COMPANY_ID,GPV,CUM_BOOK_DEP, &
            NPV,BOKDP,RETIRE)


         DO ACCT_PTR = 1, MAX_ACCOUNTS_TRACKED
            IF(ACCOUNTS_LOOKUP(ACCT_PTR) == ACCTNO) EXIT
         ENDDO
         IF(ACCT_PTR > MAX_ACCOUNTS_TRACKED) RETURN

         COMP_ID = WVPA_COMPANY_ID

         DO YR = 1, MAX_YEARS_2_REPORT
            YR1 = YR + 1 
            DO MO = 0, 12 
               REPORT_DATA(COMP_ID,ACCT_PTR,1,MO,YR) = &
                    REPORT_DATA(COMP_ID,ACCT_PTR,1,MO,YR) + GPV(MO,YR1)
               REPORT_DATA(COMP_ID,ACCT_PTR,2,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,2,MO,YR) + CUM_BOOK_DEP(MO,YR1)
               REPORT_DATA(COMP_ID,ACCT_PTR,3,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,3,MO,YR) + NPV(MO,YR1)
               REPORT_DATA(COMP_ID,ACCT_PTR,4,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,4,MO,YR) + BOKDP(MO,YR1)
               REPORT_DATA(COMP_ID,ACCT_PTR,5,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,5,MO,YR) + RETIRE(MO,YR1)
            ENDDO 
         ENDDO
      RETURN

      ENTRY WVPA_FA_PLANT_DATA_BASE(ACCTNO,WVPA_COMPANY_ID,FA_GPV,FA_CUM_BOOK_DEP,FA_NPV,FA_BOKDP,CAPX_ADDITIONS, &
                                   CASH_EXPENDITURES,CWIP_BALANCE)


         DO ACCT_PTR = 1, MAX_ACCOUNTS_TRACKED
            IF(ACCOUNTS_LOOKUP(ACCT_PTR) == ACCTNO) EXIT
         ENDDO
         IF(ACCT_PTR > MAX_ACCOUNTS_TRACKED) RETURN
         COMP_ID = WVPA_COMPANY_ID

         DO YR = 1, MAX_YEARS_2_REPORT
            DO MO = 0, 12 
               REPORT_DATA(COMP_ID,ACCT_PTR,1,MO,YR) = &
                 REPORT_DATA(COMP_ID,ACCT_PTR,1,MO,YR) + FA_GPV(MO,YR)
               REPORT_DATA(COMP_ID,ACCT_PTR,2,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,2,MO,YR) + FA_CUM_BOOK_DEP(MO,YR)
               REPORT_DATA(COMP_ID,ACCT_PTR,3,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,3,MO,YR) + FA_NPV(MO,YR)
               REPORT_DATA(COMP_ID,ACCT_PTR,4,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,4,MO,YR) + &
                    FA_BOKDP(MO,YR)
               REPORT_DATA(COMP_ID,ACCT_PTR,6,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,6,MO,YR) + &
                    CAPX_ADDITIONS(MO,YR)
               REPORT_DATA(COMP_ID,ACCT_PTR,7,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,7,MO,YR) &
                    + CASH_EXPENDITURES(MO,YR)
               REPORT_DATA(COMP_ID,ACCT_PTR,8,MO,YR) &
                = REPORT_DATA(COMP_ID,ACCT_PTR,8,MO,YR) + CWIP_BALANCE(MO,YR)
            ENDDO 
         ENDDO
      RETURN

      ENTRY WVPA_ADDENDUMS_DATA_BASE(ACCTNO, &
        WVPA_COMPANY_ID,DATA_BASE_CLASSIFICATION,ADDENDUM_DATA, &
            FINANCIAL_SIMULATION_YEARS)

!
         DO ACCT_PTR = 1, MAX_ACCOUNTS_TRACKED
            IF(ACCOUNTS_LOOKUP(ACCT_PTR) == ACCTNO) EXIT
         ENDDO
         IF(ACCT_PTR > MAX_ACCOUNTS_TRACKED) RETURN
         COMP_ID = WVPA_COMPANY_ID
!
         DATA_PTR = 9 
         IF(INDEX(DATA_BASE_CLASSIFICATION,'Plant Additions') /= 0 .OR. &
             INDEX(DATA_BASE_CLASSIFICATION, &
                       'Gross Plant Value') /= 0) DATA_PTR = 10 
         IF(INDEX(DATA_BASE_CLASSIFICATION, &
            'Depreciation Additions') /= 0  .OR. &
                 INDEX(DATA_BASE_CLASSIFICATION, &
                     'Cumulated Plant Depreciation') /= 0) DATA_PTR = 11 
!
         DO YR = 1, MAX_YEARS_2_REPORT
            DO MO = 1, 12
               REPORT_DATA(COMP_ID,ACCT_PTR,DATA_PTR,MO,YR) = &
                REPORT_DATA(COMP_ID,ACCT_PTR,DATA_PTR,MO,YR) + ADDENDUM_DATA(YR,MO)
            ENDDO
         ENDDO
      RETURN 

      ENTRY WVPA_WRITE_PLANT_BAL_RPT(ENDPOINT)


! OPEN REPORT FILE
         IF(.NOT. WVPA_PLANT_BAL_RPT_OPEN) THEN
            WVPA_PLANT_BAL_UNIT = &
                WVPA_PLNT_BALANCES_RPT_HEADR(WVPA_PLANT_BAL_REC)
            WVPA_PLANT_BAL_RPT_OPEN = .TRUE. 
         ENDIF
         IF(.NOT. WVPA_CUM_DEP_RPT_OPEN) THEN
            WVPA_ACCUM_DEP_UNIT = &
                WVPA_CUM_DEP_RPT_HEADR(WVPA_ACCUM_DEP_REC)
            WVPA_CUM_DEP_RPT_OPEN = .TRUE. 
         ENDIF
!
         ALLOCATE(OUT_RECORD(0:MAX_ACCOUNTS_TRACKED, &
            PLANT_BAL_REPT_COLS))
! SUB TOTAL REPORT DATA BASE
! NOTE THAT SUB TOTAL POINTS ARE AT 10, 20, 30, 40, 55, 65, 75, 90, 97,
!                                   105, 110, 150
!
         REPORT_DATA(0,:,:,:,:) = SUM(REPORT_DATA(1:,:,:,:,:),DIM=1)

         DO COMP_ID = 0, 3
         IF(COMP_ID == 2) CYCLE
         
         DO YR = 1, MAX_YEARS_2_REPORT
            DO MO = 0, 12 
               OUT_RECORD = 0.
               DO ACCT = 1, MAX_ACCOUNTS_TRACKED
                  IF(ACCT == 106) THEN
                     TEMP = 106
                  ENDIF
                  IF(ACCT == 10 .OR. ACCT == 20 .OR. ACCT == 30 .OR. &
                     ACCT == 40 .OR. ACCT == 65 .OR. ACCT == 75 .OR. &
                   ACCT == 150) THEN
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS 
                        DO SUM_ACCT = ACCT-9, ACCT-1
                           OUT_RECORD(ACCT,COL_POS) = OUT_RECORD(ACCT,COL_POS) + OUT_RECORD(SUM_ACCT,COL_POS)  
                        ENDDO
                     ENDDO
                     CYCLE
                  ELSEIF(ACCT == 55 .OR. ACCT == 90) THEN
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS 
                        DO SUM_ACCT = ACCT-14, ACCT-1
                           OUT_RECORD(ACCT,COL_POS) &
                            = OUT_RECORD(ACCT,COL_POS) + &
                            OUT_RECORD(SUM_ACCT,COL_POS)  
                        ENDDO
                     ENDDO
                     CYCLE
                  ELSEIF(ACCT == 97) THEN
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS 
                        DO SUM_ACCT = 91, 96
                           OUT_RECORD(ACCT,COL_POS) = &
                            OUT_RECORD(ACCT,COL_POS) + &
                            OUT_RECORD(SUM_ACCT,COL_POS)  
                        ENDDO
                     ENDDO
                  ELSEIF(ACCT == 105) THEN
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS 
                        DO SUM_ACCT = 98, 104
                           OUT_RECORD(ACCT,COL_POS) = &
                            OUT_RECORD(ACCT,COL_POS) + &
                                OUT_RECORD(SUM_ACCT,COL_POS)  
                        ENDDO
                     ENDDO
                  ELSEIF(ACCT == 110) THEN
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS 
                        OUT_RECORD(ACCT,COL_POS) = &
                             SUM(OUT_RECORD(106:109,COL_POS))
                     ENDDO
                  ELSEIF(ACCT == 125) THEN
                     CYCLE
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS 
                        OUT_RECORD(ACCT,COL_POS) = &
                            OUT_RECORD(107,COL_POS)  &
                          + SUM(OUT_RECORD(111:124,COL_POS)) &
                          + SUM(OUT_RECORD(132:135,COL_POS))
                     ENDDO
! NOTE THAT SUB TOTAL POINTS ARE AT 10, 20, 30, 40, 55, 65, 75, 90, 97 AND 104
                  ELSEIF(ACCT == 130) THEN ! PLANT GRAND TOTAL
                     CYCLE
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS
                        OUT_RECORD(ACCT,COL_POS) = &
                        OUT_RECORD(10,COL_POS) + OUT_RECORD(20,COL_POS) &
                        + OUT_RECORD(30,COL_POS)  &
                        + OUT_RECORD(40,COL_POS) + OUT_RECORD(55,COL_POS) &
                        + OUT_RECORD(65,COL_POS) + OUT_RECORD(75,COL_POS)  &
                        + OUT_RECORD(90,COL_POS) + OUT_RECORD(97,COL_POS) &
                        + OUT_RECORD(105,COL_POS) + OUT_RECORD(110,COL_POS)  
                     ENDDO
                  ELSEIF(ACCT == 131) THEN ! DEPRECIATION GRAND TOTAL
                     CYCLE
                     DO COL_POS = 1, PLANT_BAL_REPT_COLS
                        OUT_RECORD(ACCT,COL_POS) = &
                        OUT_RECORD(10,COL_POS) + &
                        OUT_RECORD(20,COL_POS) + &
                        OUT_RECORD(30,COL_POS) & 
                        + OUT_RECORD(40,COL_POS) &
                        + OUT_RECORD(55,COL_POS) &
                        + OUT_RECORD(65,COL_POS) &
                        + OUT_RECORD(75,COL_POS) &
                        + OUT_RECORD(90,COL_POS) &
                        + OUT_RECORD(97,COL_POS) &
                        + OUT_RECORD(105,COL_POS) &
                        + OUT_RECORD(125,COL_POS)  
                     ENDDO
                     EXIT
                  ELSE
                     IF(YR == 1) THEN
                        IF(MO == 0) THEN
                           OUT_RECORD(ACCT,1) = &
                            REPORT_DATA(COMP_ID,ACCT,1,0,YR)   ! GPV OPEN BAL
                        ELSE
                           OUT_RECORD(ACCT,1) = &
                            REPORT_DATA(COMP_ID,ACCT,1,MO-1,YR)   ! GPV OPEN BAL
                        ENDIF   
                     ELSE
                        IF(MO == 0) THEN
                           OUT_RECORD(ACCT,1) = &
                            REPORT_DATA(COMP_ID,ACCT,1,12,YR-1)   ! GPV OPEN BAL
                        ELSE
                           OUT_RECORD(ACCT,1) = &
                            REPORT_DATA(COMP_ID,ACCT,1,MO-1,YR)   ! GPV OPEN BAL
                        ENDIF
                     ENDIF
                     OUT_RECORD(ACCT,2) = &
                        REPORT_DATA(COMP_ID,ACCT,6,MO,YR)   ! CONST ADDITIONS
                     OUT_RECORD(ACCT,3) = &
                        REPORT_DATA(COMP_ID,ACCT,10,MO,YR)   ! PLANT ACCOUNTING ADJ
                     OUT_RECORD(ACCT,4) = &
                        REPORT_DATA(COMP_ID,ACCT,5,MO,YR)   ! RETIREMENTS
                     OUT_RECORD(ACCT,5) = OUT_RECORD(ACCT,1) &
                     + OUT_RECORD(ACCT,2) + OUT_RECORD(ACCT,3) &
                     - OUT_RECORD(ACCT,4)  
                     ! GPV CLOSING BAL
     
                     REPORT_DATA(COMP_ID,ACCT,1,MO,YR) = OUT_RECORD(ACCT,5)  ! GPV ENDING BAL
                        
                     ! NET ADDITIONS   
                     OUT_RECORD(ACCT,6) = OUT_RECORD(ACCT,2) &
                        + OUT_RECORD(ACCT,3) - OUT_RECORD(ACCT,4)
     
                     IF(YR == 1) THEN
                        IF(MO == 0) THEN
                           OUT_RECORD(ACCT,7) = REPORT_DATA(COMP_ID,ACCT,2,0,YR)   ! GPV OPEN BAL
                        ELSE
                           OUT_RECORD(ACCT,7) = REPORT_DATA(COMP_ID,ACCT,2,MO-1,YR)   ! GPV OPEN BAL
                        ENDIF   
                     ELSE
                        IF(MO == 0) THEN
                           OUT_RECORD(ACCT,7) = REPORT_DATA(COMP_ID,ACCT,2,12,YR-1)   ! GPV OPEN BAL
                        ELSE
                           OUT_RECORD(ACCT,7) = REPORT_DATA(COMP_ID,ACCT,2,MO-1,YR)   ! GPV OPEN BAL
                        ENDIF
                     ENDIF
!
                     OUT_RECORD(ACCT,8) = REPORT_DATA(COMP_ID,ACCT,4,MO,YR)         ! ANNUAL DEP
                     OUT_RECORD(ACCT,9) = REPORT_DATA(COMP_ID,ACCT,11,MO,YR)        ! ACCOUNT ADJ
                     OUT_RECORD(ACCT,10) = REPORT_DATA(COMP_ID,ACCT,5,MO,YR)        ! RETIREMENTS
                     OUT_RECORD(ACCT,11) = REPORT_DATA(COMP_ID,ACCT,9,MO,YR)        ! NET SALVAGE
                     OUT_RECORD(ACCT,13) = OUT_RECORD(ACCT,8) + OUT_RECORD(ACCT,9) - OUT_RECORD(ACCT,10) + OUT_RECORD(ACCT,11) 
                                                                                                                    ! NET ADDITIONS
     
                     OUT_RECORD(ACCT,12) = OUT_RECORD(ACCT,7) + OUT_RECORD(ACCT,13)                ! ENDING BALANCE

                     REPORT_DATA(COMP_ID,ACCT,2,MO,YR) = OUT_RECORD(ACCT,12)       ! CUM DEP ENDING BAL

                  ENDIF
               ENDDO
!
! SUM NEW VALUES
!
               DO COL_POS = 1, PLANT_BAL_REPT_COLS
                  OUT_RECORD(125,COL_POS) = &
                    OUT_RECORD(107,COL_POS) + &
                    SUM(OUT_RECORD(111:124,COL_POS)) &
                     + SUM(OUT_RECORD(132:135,COL_POS))
                  OUT_RECORD(130,COL_POS) = &
                    OUT_RECORD(10,COL_POS) + &
                    OUT_RECORD(20,COL_POS) + OUT_RECORD(30,COL_POS) &
                  + OUT_RECORD(40,COL_POS) &
                  + OUT_RECORD(55,COL_POS) &
                  + OUT_RECORD(65,COL_POS) &
                  + OUT_RECORD(75,COL_POS) &
                  + OUT_RECORD(90,COL_POS) &
                  + OUT_RECORD(97,COL_POS) &
                  + OUT_RECORD(105,COL_POS) &
                  + OUT_RECORD(110,COL_POS) &
                  + OUT_RECORD(150,COL_POS)
                  OUT_RECORD(131,COL_POS) = &
                    OUT_RECORD(10,COL_POS) + &
                    OUT_RECORD(20,COL_POS) + &
                    OUT_RECORD(30,COL_POS) &
                  + OUT_RECORD(40,COL_POS) &
                    + OUT_RECORD(55,COL_POS) + &
                    OUT_RECORD(65,COL_POS) + &
                    OUT_RECORD(75,COL_POS) &
                  + OUT_RECORD(90,COL_POS) + &
                  OUT_RECORD(97,COL_POS) + &
                    OUT_RECORD(105,COL_POS) + &
                    OUT_RECORD(125,COL_POS) &
                    + OUT_RECORD(150,COL_POS)
               ENDDO

!
! WRITE THE PLANT BALANCES REPORT
!
               DO COL_POS = 1, 6
                  WRITE(WVPA_PLANT_BAL_UNIT,REC=WVPA_PLANT_BAL_REC) &
                   FLOAT(ENDPOINT),FLOAT(get_BASE_YEAR()+YR), &
                   SHORT_MONTH_NAMES(MO), &
                  COMPANY_NAME(COMP_ID),PLANT_BAL_REPT_NAMES(COL_POS), &
                  (OUT_RECORD(ACCT,COL_POS),ACCT=0,MAX_ACCOUNTS_TRACKED)
                  WVPA_PLANT_BAL_REC = WVPA_PLANT_BAL_REC + 1
               ENDDO            
!
! WRITE THE ACCUMLATED DEPRECIATION REPORT
!
               DO COL_POS = 7, 13
                  WRITE(WVPA_ACCUM_DEP_UNIT,REC=WVPA_ACCUM_DEP_REC) FLOAT(ENDPOINT),FLOAT(get_BASE_YEAR()+YR),SHORT_MONTH_NAMES(MO), &
                  COMPANY_NAME(COMP_ID),PLANT_BAL_REPT_NAMES(COL_POS),(OUT_RECORD(ACCT,COL_POS),ACCT=0,MAX_ACCOUNTS_TRACKED)
                  WVPA_ACCUM_DEP_REC = WVPA_ACCUM_DEP_REC + 1
               ENDDO            
            ENDDO
         ENDDO
         ENDDO ! COMP_ID
         REPORT_DATA = 0.
         DEALLOCATE(OUT_RECORD)
      RETURN

      ENTRY WVPA_CONSTRUCTION_DATA_BASE(ACCTNO,WVPA_COMPANY_ID,WORKORDER_NAME,PROJECT_START_YR,PROJECT_END_YR,FA_GPV, &
                                       FA_CUM_BOOK_DEP,FA_NPV,FA_BOKDP,CAPX_ADDITIONS,CASH_EXPENDITURES,CWIP_BALANCE,AFUDC_CASH)

!
         DO ACCT_PTR = 1, MAX_ACCOUNTS_TRACKED
            IF(ACCOUNTS_LOOKUP(ACCT_PTR) == ACCTNO) EXIT
         ENDDO
         IF(ACCT_PTR > MAX_ACCOUNTS_TRACKED) RETURN
!
         DO YR = 1, MAX_YEARS_2_REPORT
            IF(YR < PROJECT_START_YR .OR. YR > PROJECT_END_YR) CYCLE
            ACTIVE_WORKORDERS(ACCT_PTR,YR) = 1 + ACTIVE_WORKORDERS(ACCT_PTR,YR)
            WORKORDER = ACTIVE_WORKORDERS(ACCT_PTR,YR)
            WORKORDER_4_COMP(ACCT_PTR,YR,WORKORDER) = WVPA_COMPANY_ID
            CONSTRUCTION_WORKORDER_NAMES(ACCT_PTR,YR,WORKORDER) = WORKORDER_NAME             
            DO MO = 0, 12
               CONSTRUCTION_DATABASE(ACCT_PTR,1,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,1,MO,YR,WORKORDER) + FA_GPV(MO,YR)
               CONSTRUCTION_DATABASE(ACCT_PTR,2,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,2,MO,YR,WORKORDER) &
                      + FA_CUM_BOOK_DEP(MO,YR)
               CONSTRUCTION_DATABASE(ACCT_PTR,3,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,3,MO,YR,WORKORDER) + FA_NPV(MO,YR)
               CONSTRUCTION_DATABASE(ACCT_PTR,4,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,4,MO,YR,WORKORDER) &
                      + FA_BOKDP(MO,YR)
               CONSTRUCTION_DATABASE(ACCT_PTR,6,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,6,MO,YR,WORKORDER) &
                      + CAPX_ADDITIONS(MO,YR)
               CONSTRUCTION_DATABASE(ACCT_PTR,7,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,7,MO,YR,WORKORDER) &
                      + CASH_EXPENDITURES(MO,YR)
               IF(MO == 0) THEN ! OPENING BALANCE
                  CONSTRUCTION_DATABASE(ACCT_PTR,8,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,8,MO,YR,WORKORDER) &
                      + CWIP_BALANCE(MO,YR)
               ELSE
                  CONSTRUCTION_DATABASE(ACCT_PTR,8,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,8,MO,YR,WORKORDER) &
                      + CWIP_BALANCE(MO-1,YR)
               ENDIF
               CONSTRUCTION_DATABASE(ACCT_PTR,9,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,9,MO,YR,WORKORDER) &
                      + AFUDC_CASH(MO,YR)
               IF(MO == 0) THEN
                  CONSTRUCTION_DATABASE(ACCT_PTR,10,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,10,MO,YR,WORKORDER) &
                      + CWIP_BALANCE(12,YR)
               ELSE
                  CONSTRUCTION_DATABASE(ACCT_PTR,10,MO,YR,WORKORDER) = CONSTRUCTION_DATABASE(ACCT_PTR,10,MO,YR,WORKORDER) &
                      + CWIP_BALANCE(MO,YR)
               ENDIF
            ENDDO 
         ENDDO
      RETURN

      ENTRY WVPA_CONSTRUCTION_SUMMARY_RPT(ENDPOINT)

!
! OPEN REPORT FILE
      
         IF(.NOT. WVPA_CONSTR_SUMRY_RPT_OPEN) THEN
            WVPA_CONSTR_SUMRY_UNIT = WVPA_CONSTR_SUMY_RPT_HEADR(WVPA_CONSTR_SUMRY_REC)
            WVPA_CONSTR_SUMRY_RPT_OPEN = .TRUE. 
         ENDIF

!
! WRITE THE PLANT BALANCES REPORT
!
         DO YR = 1, MAX_YEARS_2_REPORT
            DO MO = 0, 12 
               GRAND_TOTALS = 0.
               DO ACCT = 1, MAX_ACCOUNTS_TRACKED
                  WORKORDER = ACTIVE_WORKORDERS(ACCT,YR)
                  IF(WORKORDER == 0) CYCLE
                  WRITE(ACCT_NAME,*) ACCOUNTS_LOOKUP(ACCT)
                  SUB_TOTALS = 0.
                  DO WRKORDR = 1, WORKORDER 
                     WRITE(WVPA_CONSTR_SUMRY_UNIT,REC=WVPA_CONSTR_SUMRY_REC) FLOAT(ENDPOINT),FLOAT(get_base_year()+YR), &
                           SHORT_MONTH_NAMES(MO),ACCT_NAME,CONSTRUCTION_WORKORDER_NAMES(ACCT,YR,WRKORDR), &
                           (CONSTRUCTION_DATABASE(ACCT,COL_POS,MO,YR,WRKORDR),COL_POS=0,10)
!
! TRY AT IMPLIED SHAPE
!    
                     SUB_TOTALS = SUB_TOTALS + CONSTRUCTION_DATABASE(ACCT,0:10,MO,YR,WRKORDR)
                     WVPA_CONSTR_SUMRY_REC = WVPA_CONSTR_SUMRY_REC + 1
                  ENDDO
!
! WRITE SUB TOTALS
!
                  IF(WORKORDER > 1) THEN
                     SUB_TOTAL_NAME = "SUBTOTAL-"//ACCT_NAME
                     WRITE(WVPA_CONSTR_SUMRY_UNIT,REC=WVPA_CONSTR_SUMRY_REC) FLOAT(ENDPOINT),FLOAT(get_base_year()+YR), &
                           SHORT_MONTH_NAMES(MO),ACCT_NAME,SUB_TOTAL_NAME,(SUB_TOTALS(COL_POS),COL_POS = 0, 10)
                     WVPA_CONSTR_SUMRY_REC = WVPA_CONSTR_SUMRY_REC + 1
                  ENDIF
                  GRAND_TOTALS = GRAND_TOTALS + SUB_TOTALS
               ENDDO            
               SUB_TOTAL_NAME = "GRAND TOTALS"
               ACCT_NAME = "GRAND TOTALS"
               WRITE(WVPA_CONSTR_SUMRY_UNIT,REC=WVPA_CONSTR_SUMRY_REC) FLOAT(ENDPOINT),FLOAT(get_BASE_YEAR()+YR),SHORT_MONTH_NAMES(MO), &
               ACCT_NAME,SUB_TOTAL_NAME,(GRAND_TOTALS(COL_POS),COL_POS = 0, 10)
               WVPA_CONSTR_SUMRY_REC = WVPA_CONSTR_SUMRY_REC + 1
            ENDDO
         ENDDO
         CONSTRUCTION_DATABASE = 0.
         ACTIVE_WORKORDERS = 0
         CONSTRUCTION_WORKORDER_NAMES = " "            
      RETURN
! ENTRY WVPA_COMPANY_CONSTRUCTION_SUMMARY_RPT removed for not being called.
      END
