! ** WARNING **
! This file has an .f90 extension, but if compiled as anything other
! than FORTRAN 77, it will compile but break at runtime.
!
! See TODOs in the this file for more information.
! ** /WARNING **

!       INITIALIZE THE SCREEN CONTROL VARIABLES
      SUBROUTINE DSSCREEN(SCENAME)
      use spindriftlib
      use prod_arrays_dimensions
      use cl_data
      use sizecom
      use globecom
      use forecast
      use mod_base_year

      implicit none
      integer (kind=2) :: get_endyr ! External
      character (len=screen_msg_len) :: screen_messages_ord
      CHARACTER (LEN=1) :: UTILITY_TYPE,OBJ_FUNCTION_DESC*50
      INTEGER (KIND=2) :: PROCMETH,COVERAGE_RATIO
      LOGICAL (KIND=1) :: SHORT_FORM
      LOGICAL (KIND=1) :: POOLING_TRANSACTIONS,CONTROL_AREA_FORECAST
      LOGICAL (KIND=1) :: CAP_PLANNING_METHOD_ANNDECOMP,FACET_IS_ACTIVE
      LOGICAL (KIND=1) :: ASSET_ANALYST_ONLY,TRANSACT_ANALYST_ONLY
      LOGICAL (KIND=1) :: INIT_FILE_IS_ACTIVE
      CHARACTER (LEN=5) :: CHR_POINTS
      CHARACTER (LEN=1) :: OPMETH
      INTEGER (KIND=4) :: SCREEN1(1024)
      INTEGER (KIND=4) :: SCREEN2(1024)
      INTEGER (KIND=2) :: NORMAL,INVERSE
      INTEGER (KIND=2) :: NORMAL_VIDEO,INVERSE_VIDEO
      INTEGER (KIND=2) :: END_POINTS,I,GET_NUM_END_POINTS_IN_OVL_FILE
      CHARACTER (LEN=4) :: END_POINT_STR
      CHARACTER (LEN=5) :: SCENAME
      CHARACTER (LEN=80) :: BANNER
      CHARACTER (LEN=4) :: TOTAL_NUM_OF_END_POINTS_STR="1"
      CHARACTER (LEN=50) :: STUDY_PERIOD_STR=""
      SAVE BANNER,NORMAL,INVERSE,SCREEN1,CAP_PLANNING_METHOD_ANNDECOMP
      CHARACTER (LEN=256) :: SIM_FILE_NAME,LC_SIM_FILE_NAME
      CHARACTER (LEN=5) :: MONTH_DAY,SIM_MONTH_DAY
      CHARACTER (LEN=2) :: SIM_MONTH
      LOGICAL (KIND=1) :: LAHEY_LF95,VERSION_IS_RW
      CHARACTER (LEN=5) :: VERSION
      INTEGER (KIND=2) :: EXTENSION_YEARS ! External
      integer (kind=2) :: ext_years_result ! For debugging
!
      CALL MYNAME(LC_SIM_FILE_NAME)

         if(trim(STUDY_PERIOD_STR)=="2023-   0") then
           call end_program("dsscreen:0002 - SCENAME " // &
           "is incorrect and doesn't contain end year.")
         end if
      CALL UPC(LC_SIM_FILE_NAME,SIM_FILE_NAME)
      SIM_MONTH_DAY = MONTH_DAY(SIM_FILE_NAME)
      SIM_MONTH = SIM_MONTH_DAY(1:2)
      IF(SIM_MONTH(2:2) == '/') THEN
         SIM_MONTH(2:2) = SIM_MONTH(1:1)
         SIM_MONTH(1:1) = '0'
      ENDIF
      SHORT_FORM = .FALSE.
      END_POINTS = GET_NUM_END_POINTS_IN_OVL_FILE()

      IF(.NOT. ASSET_ANALYST_ONLY() .AND. &
                                      .NOT. INIT_FILE_IS_ACTIVE()) THEN
         BANNER = " MIDAS Gold  2003."//SIM_MONTH// &
                 "      TransAct Analyst & T"// &
                 "ransAct C              Version "//"7A" ! VERSION
      ELSEIF(UTILITY_TYPE() == 'K') THEN
            BANNER = " MIDAS Gold                       Simulation" &
                               //" Module               KEPCO Release "
      ELSEIF(INDEX(SIM_FILE_NAME,'5B') /= 0) THEN
         IF(SHORT_FORM) THEN
            BANNER=" MIDAS Gold 99             Short Form Simulation"// &
                          " Module     Asset Classes  "//VERSION()
         ELSE IF(UTILITY_TYPE() == 'P') THEN
            BANNER = " MIDAS Gold 99                  Muni Simulation"// &
                          " Module      Asset Classes  "//VERSION()
         ELSE
            BANNER = " MIDAS Gold Analyst 2003.01         " &
                        //"Simulation Module         Transact Analyst "

         ENDIF
      ELSE
         IF(SHORT_FORM) THEN
         BANNER= " MIDAS Gold 99             Short Form Simulation"// &
                          " Module     32 Bit Version "//VERSION()
         ELSE IF(UTILITY_TYPE() == 'P') THEN
         BANNER = " MIDAS Gold 99                  Muni Simulation"// &
                          " Module      32 Bit Version "//VERSION()
         ELSE
            IF(INDEX(SIM_FILE_NAME,'7A') /= 0) THEN
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                          "      Monthly Financial and"// &
                     " Transact Analyst      Version "//"7A" ! VERSION
               ELSE
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                          "            Annual Transact"// &
                     " Analyst               Version "//"7A" ! VERSION
               ENDIF
            ELSEIF(INDEX(SIM_FILE_NAME,'7M') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "      Monthly Financial and"// &
                       " Transact Analyst      Version "//"7M" ! VERSION
               BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "                    Monthly"// &
                       " MIDAS GOLD            Version "//"7M" ! VERSION
               MONTHLY_MIDAS_ACTIVE = .TRUE.
            ELSEIF(INDEX(SIM_FILE_NAME,'6A') /= 0) THEN
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "      Monthly Financial and"// &
                       " Transact Analyst      Version "//"6A" ! VERSION
               ELSE
                  BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "            Annual Transact"// &
                       " Analyst               Version "//"6A" ! VERSION
               ENDIF
            ELSEIF(INDEX(SIM_FILE_NAME,'6M') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "           Monthly Transact"// &
                       " Analyst               Version "//"6M" ! VERSION
               MONTHLY_MIDAS_ACTIVE = .TRUE.
            ELSEIF(INDEX(SIM_FILE_NAME,'AA') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "                     Asset "// &
                       "Analyst                Version "//"AA" ! VERSION
            ELSEIF(INDEX(SIM_FILE_NAME,'MM') /= 0) THEN
               BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "                    Monthly"// &
                       " MIDAS GOLD            Version "//"MM" ! VERSION
               MONTHLY_MIDAS_ACTIVE = .TRUE.
            ELSE
               BANNER=" MIDAS Gold 2003."//SIM_MONTH// &
                           "            Annual Transact"// &
                       " Analyst               Version "//"7A" ! VERSION
            ENDIF
         ENDIF
      ENDIF
      CAP_PLANNING_METHOD_ANNDECOMP = .FALSE.
      CALL LWC(SIM_FILE_NAME,LC_SIM_FILE_NAME)

! FIND TOTAL NUMBER OF ENDPOINTS

      IF(END_POINTS > 1) THEN
         END_POINT_STR = ' '
         WRITE(END_POINT_STR,"(I4)") END_POINTS
         IF(END_POINTS < 1000) THEN
            I = 4
            DO WHILE ((END_POINT_STR(I:I) /= ' ') .AND. (I > 1))
               I = I - 1
            ENDDO
            TOTAL_NUM_OF_END_POINTS_STR = END_POINT_STR(I+1:4)
         ELSE
            TOTAL_NUM_OF_END_POINTS_STR = END_POINT_STR
         ENDIF
       ELSE
         TOTAL_NUM_OF_END_POINTS_STR = '1'
       ENDIF
      ext_years_result=EXTENSION_YEARS()
      IF(ext_years_result > 0) THEN
       ! Smoking gun - writes end year to STUDY_PERIOD_STR
       WRITE(STUDY_PERIOD_STR,"(I4,A1,I4,A1,I4)") get_BASE_YEAR()+1,'-', &
                                       get_endyr(),'-',get_endyr()+EXTENSION_YEARS()
      ELSE
        WRITE(STUDY_PERIOD_STR,"(I4,A1,I4)") get_BASE_YEAR()+1,'-',get_endyr()
      ENDIF

      IF(LAHEY_LF95()) THEN
         CALL LF95_VERSION_IS_RW(VERSION_IS_RW)
         IF(VERSION_IS_RW) THEN
                  if(trim(study_period_str)=="2023-   0") then
           call end_program("dsscreen:0003 - study period string " // &
           "is incorrect.")
         end if

            CALL RW_INITIALIZE_STATUS_LINE(SCENAME, &
                                           TOTAL_NUM_OF_END_POINTS_STR, &
                                           STUDY_PERIOD_STR, &
                                           SIM_MONTH_DAY)
         ELSE
            BANNER="MIDAS Gold 2003."//SIM_MONTH// &
                      "  Annual Transact Analyst Version 95XP" ! VERSION
            CALL MG_LOCATE_WRITE(5,60,BANNER,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(5,60,'Study: '//SCENAME, &
           ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(6,62, &
                             'Study Period: '//trim(STUDY_PERIOD_STR), &
                                                         ALL_VERSIONS,0)
            IF(END_POINTS > 1) THEN
               CALL MG_LOCATE_WRITE(8,64,'Running '// &
                     trim(TOTAL_NUM_OF_END_POINTS_STR)//' endpoints.', &
                                                         ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(8,64,'Running the Base Case', &
                                                         ALL_VERSIONS,0)
            ENDIF
            CALL MG_LOCATE_WRITE(8,64,'Using simulation module '// &
             trim(LC_SIM_FILE_NAME)//' dated '//trim(SIM_MONTH_DAY), &
                                                         ALL_VERSIONS,0)
         ENDIF
      ELSE

         NORMAL = NORMAL_VIDEO()
         INVERSE = INVERSE_VIDEO()
         CALL WINDBUF(SCREEN1," ",NORMAL)
         CALL OPENWIND(1,0,0,0,79,0,INVERSE)
         CALL FILLB(1," ",INVERSE)
         CALL BLDB(1,0,0,BANNER,INVERSE)
         CALL OPENWIND(7,24,0,24,79,0,INVERSE)
         CALL FILLB(7," ",INVERSE)
         CALL BLDB(7,0,0," "//trim(SIM_MONTH_DAY)//" "// &
                      trim(LC_SIM_FILE_NAME)//" ",INVERSE)
         CALL OPENWIND(2,2,1,23,79,0,NORMAL)
         CALL FILLB(2," ",NORMAL)

      CALL BLDB(2,1,0, "            " // &
        "Execution Status                    Operating Parameters", &
        NORMAL)
      IF(ASSET_ANALYST_ONLY()) THEN
          ! TODO: Fix so that wrapped elements aren't truncated/
          ! put together mid-line.  Also, arguments should probably be
          ! put into a variable and passed. Tokens and string constants
          ! should not be broken as they are here.

          ! ** Removing trailing spaces from this file ruins the calls
          ! to BLDB. You know what to do. Do it without line-continuation.
          ! *** Using visible spaces in your editor is
          !                           ------> highly-recommended.<-----
! Merge3Issue_0007 - make positional string data identical, including all
! spaces by comparing with Jun 22 rev.
      CALL BLDB(2,2,0, "      ษอออออออออออออออออออออออออออออป       ษอออ &
      ออออออออออออออออออออออออออป",NORMAL)
      CALL BLDB(2,3,0, "      บ                             บ       บ  S &
      tudy name:                บ",NORMAL)
      CALL BLDB(2,4,0, "      บ          Year               บ       บ  S &
      tudy Period:              บ",NORMAL)
      CALL BLDB(2,5,0, "      บ                             บ       บ  E &
      xtension Period to        บ",NORMAL)
      CALL BLDB(2,6,0, "      บ        Financial            บ       บ  E &
      nd Point:                 บ",NORMAL)
      CALL BLDB(2,7,0, "      บ                             บ       บ  O &
      verlay:                   บ",NORMAL)
      CALL BLDB(2,8,0, "      บ                             บ       บ &
                                บ",NORMAL)
      CALL BLDB(2,9,0, "      บ                             บ       ศอออ &
      ออออออออออออออออออออออออออผ",NORMAL)
      CALL BLDB(2,10,0,"      บ                             บ",NORMAL)
      CALL BLDB(2,11,0,"      บ                             บ       ษอออ &
      ออออออออออออออออออออออออออป",NORMAL)
      CALL BLDB(2,12,0,"      บ                             บ       บ De &
      bt Accounts               บ",NORMAL)
      CALL BLDB(2,13,0,"      บ                             บ       บ Ex &
      isting Asset Accounts     บ",NORMAL)
      CALL BLDB(2,14,0,"      บ                             บ       บ De &
      ferred Debit Accounts     บ",NORMAL)
      CALL BLDB(2,15,0,"      บ                             บ       บ Ex &
      pense Accounts            บ",NORMAL)
      CALL BLDB(2,16,0,"      บ                             บ       บ Fu &
      ture Asset Accounts       บ",NORMAL)
      CALL BLDB(2,17,0,"      บ                             บ       บ Nu &
      clear Fuel Accounts       บ",NORMAL)
      CALL BLDB(2,18,0,"      บ                             บ       บ &
                                บ",NORMAL)
      CALL BLDB(2,19,0,"      ศอออออออออออออออออออออออออออออผ       ศอออ &
      ออออออออออออออออออออออออออผ",NORMAL)
      ELSE
      CALL BLDB(2,2,0, "      ษอออออออออออออออออออออออออออออป       ษอออ &
      ออออออออออออออออออออออออออป",NORMAL)
      CALL BLDB(2,3,0, "      บ                             บ       บ  S &
      tudy name:                บ",NORMAL)
      CALL BLDB(2,4,0, "      บ          Year               บ       บ  S &
      tudy Period:              บ",NORMAL)
      CALL BLDB(2,5,0, "      บ                             บ       บ  E &
      xtension Period to        บ",NORMAL)
      CALL BLDB(2,6,0, "      บ                             บ       บ  E &
      nd Point:                 บ",NORMAL)
      CALL BLDB(2,7,0, "      บ        Markets              บ       บ  O &
      verlay:                   บ",NORMAL)
      CALL BLDB(2,8,0, "      บ        Resources            บ       บ &
                                บ",NORMAL)
      CALL BLDB(2,9,0, "      บ        Operations           บ       ศอออ &
      ออออออออออออออออออออออออออผ",NORMAL)

      IF(INIT_FILE_IS_ACTIVE()) THEN
      CALL BLDB(2,10,0,"      บ        Financial            บ",NORMAL)

      CALL BLDB(2,11,0,"      บ                             บ       ษอออ &
      ออออออออออออออออออออออออออป",NORMAL)
      CALL BLDB(2,12,0,"      บ                             บ       บ De &
      bt Accounts               บ",NORMAL)
      CALL BLDB(2,13,0,"      บ                             บ       บ Ex &
      isting Asset Accounts     บ",NORMAL)
      CALL BLDB(2,14,0,"      บ                             บ       บ De &
      ferred Debit Accounts     บ",NORMAL)
      CALL BLDB(2,15,0,"      บ                             บ       บ Ex &
      pense Accounts            บ",NORMAL)
      CALL BLDB(2,16,0,"      บ                             บ       บ Fu &
      ture Asset Accounts       บ",NORMAL)
      CALL BLDB(2,17,0,"      บ                             บ       บ Nu &
      clear Fuel Accounts       บ",NORMAL)
      CALL BLDB(2,18,0,"      บ                             บ       บ Ge &
      nerating Units            บ",NORMAL)
      CALL BLDB(2,19,0,"      ศอออออออออออออออออออออออออออออผ       ศอออ &
      ออออออออออออออออออออออออออผ",NORMAL)
      ELSE
      CALL BLDB(2,10,0,"      บ                             บ",NORMAL)

      CALL BLDB(2,11,0,"      บ                             บ       ษอออ &
      ออออออออออออออออออออออออออป",NORMAL)
      CALL BLDB(2,12,0,"      บ                             บ       บ Ge &
      nerataing Units:          บ",NORMAL)
      CALL BLDB(2,13,0,"      บ                             บ       บ  T &
      hermal Units:             บ",NORMAL)
      CALL BLDB(2,14,0,"      บ                             บ       บ &
      Dispatched Blocks:        บ",NORMAL)
      CALL BLDB(2,15,0,"      บ                             บ       บ  H &
      ydro Units:               บ",NORMAL)
      CALL BLDB(2,16,0,"      บ                             บ       บ Tr &
      ansction Groups:          บ",NORMAL)
      CALL BLDB(2,17,0,"      บ                             บ       บ &
                                บ",NORMAL)
      CALL BLDB(2,18,0,"      บ                             บ       บ &
                                บ",NORMAL)
      CALL BLDB(2,19,0,"      ศอออออออออออออออออออออออออออออผ       ศอออ &
      ออออออออออออออออออออออออออผ",NORMAL)
      ENDIF
      ENDIF
      CALL BLDB(2,21,0,"                Copyright (c) 2003 M.S. Gerber " &
       // " & Associates, Inc.          ",NORMAL)
         CALL DISPSCN(SCREEN1)

         CALL SETATTR(NORMAL)
         IF(END_POINTS > 1) CALL MG_LOCATE_WRITE(8,64, &
                                 'of '//TOTAL_NUM_OF_END_POINTS_STR, &
                                                         ALL_VERSIONS,0)
         CALL MG_LOCATE_WRITE(5,60,SCENAME,ALL_VERSIONS,0)
        WRITE(screen_messages_ord,"(I4,A1,I4)") get_BASE_YEAR()+1,'-', &
            get_endyr()
         CALL MG_LOCATE_WRITE(6,62,trim(screen_messages_ord), &
                                                         ALL_VERSIONS,0)
         IF(gc_last_extension_year > get_endyr()) THEN
            WRITE(screen_messages_ord,"(I4)") gc_last_extension_year
            CALL MG_LOCATE_WRITE(7,67,trim(screen_messages_ord), &
                                                         ALL_VERSIONS,0)
         ENDIF
      ENDIF ! LF95
      RETURN

      ENTRY ANN_DECOMP_SCREEN
         CAP_PLANNING_METHOD_ANNDECOMP = .TRUE.
         CALL WINDBUF(SCREEN2," ",NORMAL)
         CALL OPENWIND(6,0,0,0,79,0,INVERSE)
         CALL FILLB(6," ",INVERSE)
         CALL BLDB(6,0,0,BANNER,INVERSE)
         CALL OPENWIND(9,24,0,24,79,0,INVERSE)
         CALL FILLB(9," ",INVERSE)
         CALL OPENWIND(10,8,9,10,73,0,NORMAL)
         CALL FILLB(10," ",NORMAL)
         CALL OPENWIND(11,13,9,20,73,0,NORMAL)
         CALL FILLB(11," ",NORMAL)
         CALL OPENWIND(5,1,1,23,79,0,NORMAL)
         CALL FILLB(5," ",NORMAL)
         CALL BLDB(5,1,6, "ษออออออออออออออออ Annualized Decomposition"// &
                                   " Planning ออออออออออออออออป",NORMAL)
         CALL BLDB(5,2,6, "บ Planning Year:       Best Value:        "// &
                                   "  Testing Plan:           บ",NORMAL)
         CALL BLDB(5,3,6, "บ       Load  Production  Financial  Rates"// &
                                   "  Object Value:           บ",NORMAL)
         CALL BLDB(5,4,6, "บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,4,40,trim(OBJ_FUNCTION_DESC()))
         CALL BLDB(5,5,6, "ฬอออออออออออออออออออออ Options Being Teste"// &
                                   "d ออออออออออออออออออออออออน",NORMAL)
         CALL BLDB(5,6,6, "บ Resource                  Type  Resource"// &
                                   "                  Type    บ",NORMAL)
         CALL BLDB(5,7,6, "บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,8,6, "บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,9,6, "บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,10,6,"ฬอออออออออออออออออออออออ Selected Options "// &
                                   "ออออออออออออออออออออออออออน",NORMAL)
         CALL BLDB(5,11,6,"บ Year  Obj. Val.  Cap     Cost  Options A"// &
                                   "dded                      บ",NORMAL)
         CALL BLDB(5,12,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,13,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,14,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,15,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,16,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,17,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,18,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,19,6,"บ                                         "// &
                                   "                          บ",NORMAL)
         CALL BLDB(5,20,6,"ศอออออออออออออออออออออออออออออออออออออออออ"// &
                                   "ออออออออออออออออออออออออออผ",NORMAL)
         CALL BLDB(5,22,6,"          Copyright (c) 2003 M.S. Gerbe"// &
                                "r & Associates, Inc.          ",NORMAL)

         CALL DISPSCN(SCREEN2)
         CALL LOCATEW(7,0,54)
         CALL PRINTW(7,'End Point     /'//TOTAL_NUM_OF_END_POINTS_STR)

! SET UP VIRTUAL SCREEN FOR OPTIONS SELCECTED AND OPTIONS TESTED

         CALL OPENVS(1000,255,55)
         CALL CLSVS(1000)
         CALL SET_VIRTUAL_ROW_TO_ZERO
      RETURN

      ENTRY DISPLAY_FORECAST_TYPE
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,1)
            CALL PRINTW(7,'Load: ')
            IF(SYSTEM_BASED_FORECAST()) THEN
               CALL PRINTW(7,'System      ')
            ELSEIF(POOLING_TRANSACTIONS()) THEN
               CALL PRINTW(7,'Power Pool  ')
            ELSEIF(CONTROL_AREA_FORECAST()) THEN
               CALL PRINTW(7,'Control Area')
            ELSE
               CALL PRINTW(7,'Class       ')
            ENDIF
         ELSE
         ENDIF
      RETURN
      ENTRY DISPLAY_PROC_METHOD
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,20)
            CALL PRINTW(7,'Prod: ')
            IF(FACET_IS_ACTIVE()) THEN
               CALL PRINTW(7,'Facet')
            ELSEIF(PROCMETH() == 1) THEN
               CALL PRINTW(7,'Booth')
            ELSE IF(PROCMETH() == 2) THEN
               CALL PRINTW(7,'DRate')
            ELSE IF(PROCMETH() == 3) THEN
               CALL PRINTW(7,'BB&DR')
            ELSE IF(PROCMETH() == 4) THEN
               CALL PRINTW(7,'EnBal')
            ENDIF
         ELSE
            IF(FACET_IS_ACTIVE()) THEN
               CALL MG_LOCATE_WRITE(11,9,"Facet",ALL_VERSIONS,0)
            ELSEIF(PROCMETH() == 1) THEN
               CALL MG_LOCATE_WRITE(11,9,"Booth",ALL_VERSIONS,0)
            ELSE IF(PROCMETH() == 2) THEN
               CALL MG_LOCATE_WRITE(11,9,"DRate",ALL_VERSIONS,0)
            ELSE IF(PROCMETH() == 3) THEN
               CALL MG_LOCATE_WRITE(11,9,"BB&DR",ALL_VERSIONS,0)
            ELSE IF(PROCMETH() == 4) THEN
               CALL MG_LOCATE_WRITE(11,9,"EnBal",ALL_VERSIONS,0)
            ENDIF
         ENDIF
      RETURN

      ENTRY DISPLAY_FINANCIAL_DRIVER(OPMETH,COVERAGE_RATIO)
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,35)
            CALL PRINTW(7,'Finance:')
            IF(OPMETH == 'P') THEN
               CALL PRINTW(7,' Price')
            ELSE IF(OPMETH == 'O') THEN
               CALL PRINTW(7,' Op Rv')
            ELSE IF(OPMETH == 'R') THEN
               CALL PRINTW(7,' ROR  ')
            ELSE IF(OPMETH == 'E') THEN
               CALL PRINTW(7,' ROE  ')
            ELSE IF(OPMETH == 'C') THEN
               CALL PRINTW(7,' Covrg')
            ELSE IF(OPMETH == 'N') THEN
               CALL PRINTW(7,' ROR  ')
            ENDIF
         ELSE
            IF(OPMETH == 'P') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"Price",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'O') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"Op Rv",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'R') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"ROR",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'E') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"ROE",ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'C') THEN
               WRITE(screen_messages_ord,"('Cov',I2)") COVERAGE_RATIO
               CALL MG_CLEAR_LINE_WRITE(12,9,13, &
                               trim(screen_messages_ord),ALL_VERSIONS,0)
            ELSE IF(OPMETH == 'N') THEN
               CALL MG_CLEAR_LINE_WRITE(12,9,13,"ROR",ALL_VERSIONS,0)
            ENDIF
         ENDIF
      RETURN
      ENTRY DISPLAY_DSM_ACTIVE
         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,50)
            CALL PRINTW(7,'DSM')
         ELSE
            CALL MG_CLEAR_LINE_WRITE(9,9,13,"DSM",ALL_VERSIONS,0)
         ENDIF
      RETURN
 1000 FORMAT('&',A,A,A,A)
      END


      SUBROUTINE MG_SCREEN_WRITE_FUNCTIONS()
      use SpinDriftLib
      use sizecom
      use prod_arrays_dimensions
      use dsex_obj_interfaces
      implicit none

      INTEGER :: ROW1,ROW2,COL1,COL2,WINDOW,VERSION
      CHARACTER (len=256) :: SIM_FILE_NAME,lc_SIM_FILE_NAME
      character (len=screen_msg_len) :: screen_messages_ord
      character(len=*) :: time_str
      character(len=*) :: time_5_str

      INTEGER (KIND=4) :: SET_VERSION,LENGHT

      LOGICAL (KIND=1) :: THIS_VERSION_IS_WINDOWS=.FALSE., &
                THIS_VERSION_IS_LF95=.FALSE., &
                THIS_VERSION_IS_LF77=.FALSE., &
                THIS_VERSION_IS_RealWIN=.FALSE., &
                THIS_VERSION_IS_VSW=.FALSE.
      LOGICAL (KIND=1) :: R_RW_IS_ACTIVE, &
                          SP_CAPEX_DISPLAY_ACTIVE

         IF(SP_CAPEX_ACTIVE()) THEN
            IF(SP_CAPEX_DISPLAY_ACTIVE()) THEN
               THIS_VERSION_IS_RealWIN = .TRUE.
            ELSE
               THIS_VERSION_IS_LF95 = .TRUE.
            ENDIF
            RETURN
         ENDIF
         CALL MYNAME(lc_SIM_FILE_NAME)
         CALL UPC(lc_SIM_FILE_NAME,SIM_FILE_NAME)
         IF(INDEX(SIM_FILE_NAME,'WIN') /= 0) THEN
            THIS_VERSION_IS_WINDOWS = .TRUE.
         ELSEIF(INDEX(SIM_FILE_NAME,'RW') /= 0) THEN
            THIS_VERSION_IS_RealWIN = .TRUE.
         ELSEIF(INDEX(SIM_FILE_NAME,'VSW') /= 0) THEN
            THIS_VERSION_IS_VSW = .TRUE.
         ELSEIF(INDEX(SIM_FILE_NAME,'95') /= 0) THEN
            THIS_VERSION_IS_LF95 = .TRUE.
         ELSE
            THIS_VERSION_IS_RealWIN = .TRUE.
         ENDIF
            THIS_VERSION_IS_RealWIN = .TRUE.
      RETURN

      ENTRY LF95_VERSION_IS_RW(R_RW_IS_ACTIVE)

         R_RW_IS_ACTIVE =  THIS_VERSION_IS_RealWIN
      RETURN

      ENTRY MG_CLEAR_LINE_WRITE(ROW1,COL1,COL2,time_str, &
                                                         VERSION,WINDOW)

         IF((VERSION==ALL_VERSIONS .OR. VERSION==LF95_VERSION) .AND. &
                   (THIS_VERSION_IS_VSW .OR. THIS_VERSION_IS_LF95)) THEN
            CALL WRITE_CONSOL_LINE(time_str)
         ELSEIF((VERSION==ALL_VERSIONS .OR. VERSION==WIN95_VERSION).AND. &
            (THIS_VERSION_IS_RealWIN .OR. THIS_VERSION_IS_WINDOWS)) THEN

            CALL write_scroll_line_RW(trim(time_str),WINDOW)
         ELSEIF(VERSION==ALL_VERSIONS .OR. VERSION==LF77_VERSION) THEN
            !LF77 VERSION
            CALL CLS(ROW1,COL1,ROW2,COL2)
            CALL LOCATE(ROW1,COL1)
            CALL WRITE_LF77_SCREEN(time_str)
         ENDIF
      RETURN


      ENTRY MG_LOCATE_WRITE(ROW1,COL1,time_5_str,VERSION, &
       WINDOW)

         IF((VERSION==ALL_VERSIONS .OR. VERSION==LF95_VERSION) .AND. &
                   (THIS_VERSION_IS_VSW .OR. THIS_VERSION_IS_LF95)) THEN
            CALL WRITE_CONSOL_LINE(time_5_str)
         ELSEIF((VERSION==ALL_VERSIONS .OR. VERSION==WIN95_VERSION).AND. &
            (THIS_VERSION_IS_RealWIN .OR. THIS_VERSION_IS_WINDOWS)) THEN
            CALL write_scroll_line_RW(trim(time_5_str),WINDOW)
         ELSEIF(VERSION==ALL_VERSIONS .OR. VERSION==LF77_VERSION) THEN
            !LF77 VERSION
            LENGHT = LEN(time_5_str)
            CALL LOCATE(ROW1,COL1)
            CALL WRITE_LF77_SCREEN(time_5_str)
         ENDIF

      RETURN


      ENTRY MG_CLEAR_LINE(ROW1,COL1,COL2,VERSION,WINDOW)

         IF((VERSION==ALL_VERSIONS .OR. VERSION==LF95_VERSION) .AND. &
                   (THIS_VERSION_IS_VSW .OR. THIS_VERSION_IS_LF95)) THEN
         ELSEIF((VERSION==ALL_VERSIONS .OR. VERSION==WIN95_VERSION).AND. &
            (THIS_VERSION_IS_RealWIN .OR. THIS_VERSION_IS_WINDOWS)) THEN
         ELSEIF(VERSION==ALL_VERSIONS .OR. VERSION==LF77_VERSION) THEN
             !LF77 VERSION
            CALL CLS(ROW1,COL1,COL2)
         ENDIF

      RETURN

      ENTRY UPDATE_TIME_ON_STATUS_LINE(time_str)

         IF(THIS_VERSION_IS_RealWIN) THEN
            CALL RW_UPDATE_STATUS_TIME(time_str)
         ENDIF
      RETURN
      END
