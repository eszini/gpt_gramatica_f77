
!     CF_OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 1/29/2007 3:53:38 PM
!     Author : MARK S GERBER
!     Last change: gt 5/28/2013 12:28:38 PM
        module cf_objt_shared
        implicit none
            REAL, allocatable, save :: SUM_CLASS_PEAKS(:,:,:,:)
            real, allocatable, save :: CLASS_ANN_COIN_PEAK(:,:)
        end module cf_objt_shared

      SUBROUTINE CF_OBJECT
      use end_routine, only: end_program, er_message
      use logging
      use filemod
      use mod_base_year
      use spindriftlib
      use prod_arrays_dimensions
      use kepcocom
      use sizecom
      implicit none
!            ADDL Expansion: Probably need to change LRECL
      INTEGER(kind=2) :: I,INUNIT,IREC,DELETE,LRECL=280
      INTEGER :: IOS
      CHARACTER(len=5) :: CLASS_OVL_NAME(MAX_LOAD_CLASSES), &
                  RFC_NAME,IFC_NAME,CFC_NAME, &
                  O1F_NAME,O2F_NAME,O3F_NAME
      CHARACTER(len=30) :: COMMENT
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,BASE_DATA_DIR
      CHARACTER(len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL(kind=4) :: FILE_EXISTS
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  LAM FILE NAMES
      CHARACTER(len=5) :: RESFRC,INDFRC,COMFRC,OTH1FRC,OTH2FRC,OTH3FRC

!  DECLARATION FOR CLAS S FORECASTS
      INTEGER(kind=2) :: IS,YEAR,CLASS_NUM
      LOGICAL(kind=1) :: CLASS_EXISTS(MAX_LOAD_CLASSES)=.FALSE., &
                R_CLASS_EXISTS(MAX_LOAD_CLASSES), &
                OVERLAY_CLASS(MAX_LOAD_CLASSES), &
                CFC_EXISTS,RFC_EXISTS,IFC_EXISTS,O1F_EXISTS, &
                O2F_EXISTS,O3F_EXISTS, &
                CLASS_1_TO_5_FOUND, &
                POOLING_TRANSACTIONS, &
                CONTROL_AREA_FORECAST, &
                CONTRIBUTES_TO_TOTAL, &
                CLASS_6_AND_WABASH_VALLEY=.FALSE., &
                R_CLASS_6_AND_WV
      CHARACTER(len=5) :: SCENARIO_NAME(MAX_LOAD_CLASSES)
      CHARACTER(len=256) :: FILE_NAME1
      CHARACTER(len=32) :: CLASS_NAME
      CHARACTER(len=2) :: CLASS_OVL(MAX_LOAD_CLASSES), &
                  R_CLASS_OVL(MAX_LOAD_CLASSES), &
                  CFC_OL='BC',RFC_OL='BC',IFC_OL='BC', &
                  O1F_OL='BC',O2F_OL='BC',O3F_OL='BC'
      CHARACTER(len=2) :: CLASS_TYPE(MAX_LOAD_CLASSES)= &
                                       (/'CF','RF','IF','O1','O2','O3'/)
      CHARACTER(len=5) :: CLASS_BIN_NAME(MAX_LOAD_CLASSES)= &
                     (/'COMFC','RESFC','INDFC','OTH1F','OTH2F','OTH3F'/)
      CHARACTER(len=25) :: CLASS_NAMES(MAX_LOAD_CLASSES)= (/ &
                   'Commercial Forecast   ', &
                   'Residential Forecast  ', &
                   'Industrial Forecast   ', &
                   'Other Forecast Class 1', &
                   'Other Forecast Class 2', &
                   'Other Forecast Class 3'/)
      REAL :: LOSSES,CUSTOMERS,CLASS_PEAK_LOSSES
      REAL :: CLSMWH1(12),CLSMW1(12),CLSMWH2(12),CLSMW2(12)
      REAL :: CLASS_COIN_FACTOR
      CHARACTER(len=15) :: FILE_TYPE=' Class Forecast' ! leading space needed
      INTEGER(kind=1) :: FORECAST_GROWTH_YEARS
      INTEGER(kind=2) :: CLASS_IN_CONTROL_AREA
      REAL :: CLASS_RESERVE_MARGIN, &
           CLASS_ENERGY_PRICE, &
           CLASS_DEMAND_PRICE, &
           CLASS_CUSTOMERS_PRICE, &
           TRANSMISSION_LOSSES
      INTEGER(kind=2) :: ASSET_CLASS_NUM, &
                ASSET_CLASS_VECTOR, &
                MONTHY_ENRG_COST_PATTERN, &
                MONTHY_DEMAND_COST_PATTERN
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
!

!
!           ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!           COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
!  CONVERT THE CLASS-FORECAST-DATA FILES
      ENTRY CF_MAKEBIN
!
      FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      SCENARIO_NAME(1) = COMFRC()
      SCENARIO_NAME(2) = RESFRC()
      SCENARIO_NAME(3) = INDFRC()
      SCENARIO_NAME(4) = OTH1FRC()
      SCENARIO_NAME(5) = OTH2FRC()
      SCENARIO_NAME(6) = OTH3FRC()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      BASE_DATA_DIR = BASE_FILE_DIRECTORY()
      CLASS_1_TO_5_FOUND = .FALSE.
      DO I = 1, MAX_LOAD_CLASSES
         FILE_NAME = trim(BASE_DATA_DIR)//CLASS_TYPE(I)//'B'// &
                                        trim(SCENARIO_NAME(I))//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            CLASS_EXISTS(I) = .TRUE.
            IF(I < 6) THEN
               CLASS_1_TO_5_FOUND = .TRUE.
            ELSEIF(WABASH_VALLEY) THEN
               CLASS_6_AND_WABASH_VALLEY = .TRUE.
            ENDIF
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(CLASS_NAMES(I))//'-'// &
                                                        SCENARIO_NAME(I)
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(16,30,SCENARIO_NAME(I), &
                                                         ALL_VERSIONS,0)
               CALL MG_CLEAR_LINE_WRITE(17,9,36,trim(CLASS_NAMES(I)), &
                                                         ALL_VERSIONS,0)
            ENDIF
!
            CUSTOMERS = 1.
            LOSSES = 0.
!
            CLASS_COIN_FACTOR = 100.
            CLASS_PEAK_LOSSES = 0.
            CLASS_IN_CONTROL_AREA = -9999
            CLASS_RESERVE_MARGIN = 0.
            CLASS_NUM = -9999
            CLASS_ENERGY_PRICE = 0.
            CLASS_DEMAND_PRICE = 0.
            CLASS_CUSTOMERS_PRICE = 0.
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            TRANSMISSION_LOSSES = 0.
            MONTHY_ENRG_COST_PATTERN = 0
            MONTHY_DEMAND_COST_PATTERN = 0
! 6/13/97. GAT.
            CLSMWH1 = 0.
            CLSMW1 = 0.
            CLSMWH2 = 0.
            CLSMW2 = 0.
!
            OPEN(10,FILE=FILE_NAME)
!
            FILE_NAME1 = trim(DATA_DRIVE)//"BC"// &
                                               CLASS_BIN_NAME(I)//".BIN"
            OPEN(11,FILE=FILE_NAME1,ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
            IREC = 1
            READ(10,*) DELETE
            DO WHILE (IREC <= FORECAST_GROWTH_YEARS)
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /=0) EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,CLASS_NUM, &
                 CLASS_NAME,CUSTOMERS,LOSSES,(CLSMWH1(IS),CLSMW1(IS), &
                 CLSMWH2(IS),CLSMW2(IS),IS=1,12),COMMENT, &
                 CLASS_COIN_FACTOR,CLASS_PEAK_LOSSES, &
                 CLASS_IN_CONTROL_AREA, &
                 CLASS_RESERVE_MARGIN, &
                 CLASS_ENERGY_PRICE, &
                 CLASS_DEMAND_PRICE, &
                 CLASS_CUSTOMERS_PRICE, &
                 ASSET_CLASS_NUM, &
                 ASSET_CLASS_VECTOR, &
                 TRANSMISSION_LOSSES, &
                 MONTHY_ENRG_COST_PATTERN, &
                 MONTHY_DEMAND_COST_PATTERN
!
               IF(CLASS_NUM == -9999) then
                CLASS_NUM = I
               end if
               IF(CLASS_IN_CONTROL_AREA == -9999) &
                                       CLASS_IN_CONTROL_AREA = CLASS_NUM
               IF(.NOT. WABASH_VALLEY) THEN
                  CLASS_IN_CONTROL_AREA = CLASS_NUM
               ENDIF
               WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NUM,CLASS_NAME, &
                  CUSTOMERS,LOSSES,(CLSMWH1(IS),CLSMW1(IS), &
                  CLSMWH2(IS),CLSMW2(IS),IS=1,12),CLASS_COIN_FACTOR, &
                  CLASS_PEAK_LOSSES, &
                  CLASS_IN_CONTROL_AREA, &
                  CLASS_RESERVE_MARGIN, &
                  CLASS_ENERGY_PRICE, &
                  CLASS_DEMAND_PRICE, &
                  CLASS_CUSTOMERS_PRICE, &
                  ASSET_CLASS_NUM, &
                  ASSET_CLASS_VECTOR, &
                  TRANSMISSION_LOSSES, &
                  MONTHY_ENRG_COST_PATTERN, &
                  MONTHY_DEMAND_COST_PATTERN
!
               IREC = IREC + 1
            ENDDO
            CLOSE(10)

            CLOSE(11)
         ELSE IF(trim(SCENARIO_NAME(I)) .NE. 'NONE') THEN
            CALL STOP_NOFILE(CLASS_BIN_NAME(I)//FILE_TYPE,FILE_NAME, &
       "cf_objt:0008")
         ENDIF
      ENDDO
      IF(REALLY_KEPCO .AND. CLASS_EXISTS(6)) NUMBER_OF_AREAS = 6
      IF(POOLING_TRANSACTIONS() .AND. &
                (.NOT. (CLASS_1_TO_5_FOUND .AND. CLASS_EXISTS(6)))) THEN
         IF(.NOT. (CLASS_1_TO_5_FOUND)) THEN
            CALL MG_LOCATE_WRITE(16,9, &
                       'There is no forecast information in'// &
                               ' the primary utility in classes 1-5.', &
                                                         ALL_VERSIONS,0)
         ENDIF
         IF(.NOT. CLASS_EXISTS(6)) THEN
            CALL MG_LOCATE_WRITE(76,9, &
                       'There is no forecast information for the'// &
                            ' secondary pool member in class 6.', &
                                                         ALL_VERSIONS,0)
         ENDIF
         er_message='stop requested from CF_OBJT SIID15'
         call end_program(er_message)
      ENDIF
      IF(CONTROL_AREA_FORECAST() .AND. &
                (.NOT. (CLASS_1_TO_5_FOUND .OR. CLASS_EXISTS(6)))) THEN
         CALL MG_LOCATE_WRITE(17,9, &
                     'There is no control area forecast information.', &
                                                         ALL_VERSIONS,0)
         er_message='stop requested from CF_OBJT SIID16'
         call end_program(er_message)
      ENDIF
      RETURN
      ENTRY IS_CLASS_6_AND_WV(R_CLASS_6_AND_WV)
         R_CLASS_6_AND_WV = CLASS_6_AND_WABASH_VALLEY
      RETURN

!
!           ROUTINE TO CREATE OVERLAY FILES
!           COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!           COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
!  OVERLAY THE CLAS S-FORECAST-DATA FILES
      ENTRY CF_MAKEOVL(RFC_NAME,IFC_NAME,CFC_NAME, &
                       O1F_NAME,O2F_NAME,O3F_NAME, &
                       RFC_EXISTS,IFC_EXISTS,CFC_EXISTS, &
                       O1F_EXISTS,O2F_EXISTS,O3F_EXISTS)
!
      OVERLAY_CLASS(1) = CFC_EXISTS
      OVERLAY_CLASS(2) = RFC_EXISTS
      OVERLAY_CLASS(3) = IFC_EXISTS
      OVERLAY_CLASS(4) = O1F_EXISTS
      OVERLAY_CLASS(5) = O2F_EXISTS
      OVERLAY_CLASS(6) = O3F_EXISTS
      CLASS_OVL(1) = CFC_OL ! Consider use of EQUIVALENCE here and below
      CLASS_OVL(2) = RFC_OL
      CLASS_OVL(3) = IFC_OL
      CLASS_OVL(4) = O1F_OL
      CLASS_OVL(5) = O2F_OL
      CLASS_OVL(6) = O3F_OL
      CLASS_OVL_NAME(1) = CFC_NAME ! Consider use of EQUIVALENCE here and below
      CLASS_OVL_NAME(2) = RFC_NAME
      CLASS_OVL_NAME(3) = IFC_NAME
      CLASS_OVL_NAME(4) = O1F_NAME
      CLASS_OVL_NAME(5) = O2F_NAME
      CLASS_OVL_NAME(6) = O3F_NAME
      DATA_DRIVE = OUTPUT_DIRECTORY()
      DO I = 1, MAX_LOAD_CLASSES
         IF(OVERLAY_CLASS(I) .AND. CLASS_EXISTS(I)) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(CLASS_NAMES(I))//'-'// &
                                                       CLASS_OVL_NAME(I)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(17,9,36,CLASS_NAMES(I), &
                                                         ALL_VERSIONS,0)
            ENDIF
            FILE_NAME = trim(DATA_DRIVE)//CLASS_TYPE(I)//'O'// &
                                       trim(CLASS_OVL_NAME(I))//".DAT"
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            INUNIT = 12
            IF(CLASS_OVL(I) == 'BC') THEN
               OPEN(11,FILE=trim(DATA_DRIVE)//'BC'// &
                              CLASS_BIN_NAME(I)//".BIN",ACCESS="DIRECT")
               INUNIT = 11
            ENDIF
            OPEN(12,FILE=trim(DATA_DRIVE)//'OL'// &
                   CLASS_BIN_NAME(I)//".BIN",ACCESS="DIRECT",RECL=LRECL)
            IREC = 0
            DO
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,YEAR, &
                     CLASS_NUM,CLASS_NAME,CUSTOMERS,LOSSES, &
                    (CLSMWH1(IS),CLSMW1(IS),CLSMWH2(IS), &
                     CLSMW2 (IS),IS=1,12),CLASS_COIN_FACTOR, &
                     CLASS_PEAK_LOSSES, &
                     CLASS_IN_CONTROL_AREA, &
                     CLASS_RESERVE_MARGIN, &
                     CLASS_ENERGY_PRICE, &
                     CLASS_DEMAND_PRICE, &
                     CLASS_CUSTOMERS_PRICE, &
                     ASSET_CLASS_NUM, &
                     ASSET_CLASS_VECTOR, &
                     TRANSMISSION_LOSSES, &
                     MONTHY_ENRG_COST_PATTERN, &
                     MONTHY_DEMAND_COST_PATTERN
               IF(IOS /= 0) EXIT
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,YEAR,CLASS_NUM, &
                     CLASS_NAME,CUSTOMERS,LOSSES,(CLSMWH1(IS), &
                     CLSMW1(IS),CLSMWH2(IS),CLSMW2(IS),IS=1,12), &
                     COMMENT,CLASS_COIN_FACTOR, &
                     CLASS_PEAK_LOSSES, &
                     CLASS_IN_CONTROL_AREA, &
                     CLASS_RESERVE_MARGIN, &
                     CLASS_ENERGY_PRICE, &
                     CLASS_DEMAND_PRICE, &
                     CLASS_CUSTOMERS_PRICE, &
                     ASSET_CLASS_NUM, &
                     ASSET_CLASS_VECTOR, &
                     TRANSMISSION_LOSSES, &
                     MONTHY_ENRG_COST_PATTERN, &
                     MONTHY_DEMAND_COST_PATTERN

               ENDIF
               WRITE(12,REC=IREC) DELETE,YEAR,CLASS_NUM,CLASS_NAME, &
                  CUSTOMERS,LOSSES,(CLSMWH1(IS),CLSMW1(IS), &
                  CLSMWH2(IS),CLSMW2(IS),IS=1,12),CLASS_COIN_FACTOR, &
                  CLASS_PEAK_LOSSES, &
                  CLASS_IN_CONTROL_AREA, &
                  CLASS_RESERVE_MARGIN, &
                  CLASS_ENERGY_PRICE, &
                  CLASS_DEMAND_PRICE, &
                  CLASS_CUSTOMERS_PRICE, &
                  ASSET_CLASS_NUM, &
                  ASSET_CLASS_VECTOR, &
                  TRANSMISSION_LOSSES, &
                  MONTHY_ENRG_COST_PATTERN, &
                  MONTHY_DEMAND_COST_PATTERN
            ENDDO
            CLOSE(10)
            CLOSE(12)
            IF(CLASS_OVL(I) == 'BC') CLOSE(11)
            CLASS_OVL(I) = 'OL'
         ENDIF
      ENDDO  ! CLAS S LOOP
      CFC_OL = CLASS_OVL(1) ! Consider use of EQUIVALENCE here and above
      RFC_OL = CLASS_OVL(2)
      IFC_OL = CLASS_OVL(3)
      O1F_OL = CLASS_OVL(4)
      O2F_OL = CLASS_OVL(5)
      O3F_OL = CLASS_OVL(6)
      RETURN

      ENTRY CF_GET_PEAKS

!
      CLASS_OVL(1) = CFC_OL ! Consider use of EQUIVALENCE here and below
      CLASS_OVL(2) = RFC_OL
      CLASS_OVL(3) = IFC_OL
      CLASS_OVL(4) = O1F_OL
      CLASS_OVL(5) = O2F_OL
      CLASS_OVL(6) = O3F_OL
      DATA_DRIVE = OUTPUT_DIRECTORY()
      DO I = 1, MAX_LOAD_CLASSES
         IF(CLASS_EXISTS(I)) THEN
            OPEN(10,FILE=trim(DATA_DRIVE)//CLASS_OVL(I)// &
                   CLASS_BIN_NAME(I)//".BIN",ACCESS="DIRECT",RECL=LRECL)
            IREC = 0
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) DELETE,YEAR, &
                     CLASS_NUM,CLASS_NAME,CUSTOMERS,LOSSES, &
                    (CLSMWH1(IS),CLSMW1(IS),CLSMWH2(IS), &
                     CLSMW2 (IS),IS=1,12),CLASS_COIN_FACTOR, &
                     CLASS_PEAK_LOSSES,CLASS_IN_CONTROL_AREA
               IF(IOS /= 0) EXIT
               IF(WABASH_VALLEY .AND. CLASS_IN_CONTROL_AREA /= 1) THEN
                  CONTRIBUTES_TO_TOTAL = .FALSE.
               ELSE
                  CONTRIBUTES_TO_TOTAL = .TRUE.
               ENDIF
               CALL CALCULATE_CLASS_PEAKS(YEAR-get_BASE_YEAR(), &
                                          CLASS_COIN_FACTOR/ &
                                    (100.*(1.-CLASS_PEAK_LOSSES)), &
                                          CLSMW1,CLSMW2,CLASS_NUM, &
                                                   CONTRIBUTES_TO_TOTAL)
            ENDDO
            CLOSE(10)
         ENDIF
      ENDDO  ! CLASS LOOP
      CALL FIND_AND_STORE_SYSTEM_PEAKS
      CALL SET_FIRST_CLASS_TRUE
      RETURN

      ENTRY RESET_CLASS_FORECAST_OL

!
      CFC_OL = 'BC'
      RFC_OL = 'BC'
      IFC_OL = 'BC'
      O1F_OL = 'BC'
      O2F_OL = 'BC'
      O3F_OL = 'BC'
      RETURN
!

      ENTRY GET_CLASS_OVLS(R_CLASS_OVL)

         R_CLASS_OVL(1) = CFC_OL
         R_CLASS_OVL(2) = RFC_OL
         R_CLASS_OVL(3) = IFC_OL
         R_CLASS_OVL(4) = O1F_OL
         R_CLASS_OVL(5) = O2F_OL
         R_CLASS_OVL(6) = O3F_OL
      RETURN



  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CF_OBJT SIID17'
      call end_program(er_message)

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!

      SUBROUTINE CS_OBJECT
      use end_routine, only: end_program, er_message
      use logging

!
      USE SIZECOM
      USE SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      INTEGER(kind=2) :: IREC,INUNIT,DELETE,LRECL=240
      INTEGER IOS
      CHARACTER(len=5) :: OVERLAY_FAMILY_NAME
      CHARACTER(len=5) :: CLSHAPE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL(kind=4) :: FILE_EXISTS
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR CLA SS-LOAD VARIABLES
      INTEGER(kind=2) :: CLASS_NUM,SEASON,UNITS
      REAL :: CLSLOD1(24),CLSLOD2(24)
      CHARACTER(len=32) :: CLASS_NAME
      CHARACTER(len=17) :: FILE_TYPE='Class Load Shapes'
      CHARACTER(len=2) :: CLS_OL='BC',R_CLS_OL
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT


!
!           ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!           COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
!  CONVERT THE CLAS S LOAD-SHAPE FILE
      ENTRY CS_MAKEBIN
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//"CSB"// &
                                               trim(CLSHAPE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//CLSHAPE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,CLSHAPE(),ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSLS.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS_NUM,CLASS_NAME,SEASON, &
                                                   UNITS,CLSLOD1,CLSLOD2
            WRITE(11,REC=IREC) DELETE,CLASS_NUM,CLASS_NAME,SEASON, &
                                                   UNITS,CLSLOD1,CLSLOD2
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
      ELSE IF(INDEX(CLSHAPE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "CF_OBJT:0002")
      ELSE
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSLS.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      ENDIF
!      ENDFILE(11)
      CLOSE(11)
      RETURN

!
!           ROUTINE TO CREATE OVERLAY FILES
!           COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!           COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
!  OVERLAY THE CLAS S LOAD-SHAPE FILE
      ENTRY CS_MAKEOVL(OVERLAY_FAMILY_NAME)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"CSO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CLS_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSLS.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLCLSLS.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,CLASS_NUM, &
            CLASS_NAME,SEASON,UNITS,CLSLOD1,CLSLOD2
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS_NUM, &
               CLASS_NAME,SEASON,UNITS,CLSLOD1,CLSLOD2
         ENDIF
         WRITE(12,REC=IREC) DELETE,CLASS_NUM, &
            CLASS_NAME,SEASON,UNITS,CLSLOD1,CLSLOD2
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CLS_OL == 'BC') CLOSE(11)
      CLS_OL = 'OL'
      RETURN
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CF_OBJT SIID18'
      call end_program(er_message)

      ENTRY RESET_CLS_OL

         CLS_OL = 'BC'
      RETURN

      ENTRY GET_CLASS_LOAD_SHAPE_OL(R_CLS_OL)

         R_CLS_OL = CLS_OL
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!

      SUBROUTINE CH_OBJECT
      use end_routine, only: end_program, er_message
      use logging

!
      USE SIZECOM
      USE SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      
      INTEGER(kind=2) :: IREC,INUNIT,DELETE,LRECL=140
      INTEGER :: IOS
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=5) :: CLSHIST,OVERLAY_FAMILY_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR CLAS S HISTORY FILE
      INTEGER(kind=2) :: CLASS=0

      REAL :: VALUES_24(24)
      CHARACTER(len=13) :: FILE_TYPE='Class History'
      CHARACTER(len=32) :: CLASS_NAME
      CHARACTER(len=2) :: CLASS_HIST_OL='BC',R_CLASS_HIST_OL
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      integer (kind=2) :: class_loc

!
!           SUBROUTINE TO CONVERT ACSII FILES TO DIRECT ACESS BINARY
!           COPYRIGHT (C) 1993  M.S. GERBER & ASSOCIATES, INC.
!            ALL RIGHTS RESERVED

!
!  CONVERT THE CLAS S-HISTORY FILE
      ENTRY CH_MAKEBIN
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                        "CHB"//trim(CLSHIST())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//CLSHIST()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,CLSHIST(),ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCLHIST.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS,CLASS_NAME,VALUES_24
            WRITE(11,REC=IREC) DELETE,CLASS,CLASS_NAME,VALUES_24
            IREC = IREC + 1
         ENDDO
         CLOSE(10)

         CLOSE(11)
      ELSE
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "CF_OBJT:0003")
      ENDIF
      RETURN
!  OVERLAY THE CLASS-HISTORY FILE

      ENTRY CH_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"CHO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CLASS_HIST_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLHIST.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLCLHIST.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,CLASS,CLASS_NAME, &
                                          VALUES_24
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,CLASS,CLASS_NAME,VALUES_24
         ENDIF
         WRITE(12,REC=IREC) DELETE,CLASS,CLASS_NAME,VALUES_24
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CLASS_HIST_OL == 'BC') CLOSE(11)
      CLASS_HIST_OL = 'OL'
      RETURN
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from CF_OBJT SIID19'
      call end_program(er_message)

      ENTRY RESET_CLASS_HIST_OL

         CLASS_HIST_OL = 'BC'
      RETURN

      ENTRY GET_CLASS_HIST_OL(R_CLASS_HIST_OL)

         R_CLASS_HIST_OL = CLASS_HIST_OL
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!

      SUBROUTINE CALCULATE_CLASS_PEAKS(IYR,CLASS_COIN_LOSS_FACTOR, &
                               CLSMW1,CLSMW2,class_loc,CONTRIBUTES_TO_TOTAL)


      use end_routine
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use fyregycom
      use rangechecker
      use cf_objt_shared
      use miscmod
      implicit none
      REAL CLASS_COIN_LOSS_FACTOR,CLSMW1(*),CLSMW2(*)
      REAL PLANNING_PEAK,CLASS_PEAK,R_PEAK
      INTEGER(kind=2) :: IYR,MO,YEAR,class_loc,TOTAL_CLASSES,PEAK_MONTH, &
                PEAK_DAY,R_CLASS,R_YR,R_MONTH,MAX_CLASS=7,MIN_CLASS=1
      LOGICAL(kind=1) :: FIRST_CLASS=.TRUE.
!  SYSTEM PEAK VARIABLES
      REAL :: PEAKS(12,2),SYS_FC_DATA(*),REAL_TEMP,R_CLASS_PEAK
      REAL :: STORE_ANNUAL_PEAK_AND_MONTH
      INTEGER(kind=2) :: MO_OFFSET
      integer :: times_csp=0
!
!  INCLUDE COMMON BLOCK OF YEARLY SYSTEM FORECAST STATISTICS
!

!  VARIBLES FOR CAPACITY PLANNING PEAKS
      INTEGER(kind=1) :: FORECAST_GROWTH_YEARS
      PARAMETER (FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS)
      
      LOGICAL(kind=1) :: CONTRIBUTES_TO_TOTAL
      
      REAL :: CLASS_GROWTH_TRIGGER,CLASS_GROWTH_TRIGGER_LEVEL
      REAL :: SYS_GROWTH_TRIGGER,SYS_GROWTH_TRIGGER_LEVEL
      integer :: allocstat

      real :: max_peaks, peak1, peak2


!
      TOTAL_CLASSES = MAX_LOAD_CLASSES + 1

      IF(IYR == 1 .AND. FIRST_CLASS) THEN
         IF(ALLOCATED(SUM_CLASS_PEAKS)) then
            DEALLOCATE(SUM_CLASS_PEAKS)
         end if
         ALLOCATE(SUM_CLASS_PEAKS(12,2,FORECAST_GROWTH_YEARS, &
          TOTAL_CLASSES), stat=allocstat)

          call check_alloc("cf_objt:0005", "sum_class_peaks", allocstat)
          
         SUM_CLASS_PEAKS = 0.
         IF(ALLOCATED(CLASS_ANN_COIN_PEAK)) &
                                         DEALLOCATE(CLASS_ANN_COIN_PEAK)
         ALLOCATE(CLASS_ANN_COIN_PEAK(MAX_LOAD_CLASSES, &
             FORECAST_GROWTH_YEARS), stat=allocstat)
         call check_alloc("cf_objt:0006", "CLASS_ANN_COIN_PEAK", &
            allocstat)
         CLASS_ANN_COIN_PEAK = 0.
         FIRST_CLASS = .FALSE.
      ENDIF
      CLASS_GROWTH_TRIGGER_LEVEL = CLASS_GROWTH_TRIGGER()
      ! call check_bounds_r_4s(sum_class_peaks, "cf_objt:0009",&
        ! "sum_class_peaks", int(12,2), int(2,2), int(IYR,2), &
        ! int(class_loc,2), &
         ! "times_csp=" // trim(itos(int(times_csp))))
      DO MO = 1, 12
         IF(IYR == 1) THEN
            PEAKS(MO,1) = CLSMW1(MO)
            PEAKS(MO,2) = CLSMW2(MO)
         ELSE
            CLASS_PEAK = CLSMW1(MO)
            IF(CLASS_PEAK <= CLASS_GROWTH_TRIGGER_LEVEL) THEN
               PEAKS(MO,1) = (1.+CLASS_PEAK/100.) * PEAKS(MO,1)
            ELSE
               PEAKS(MO,1) = CLSMW1(MO)
            ENDIF
            CLASS_PEAK = CLSMW2(MO)
            IF(CLASS_PEAK <= CLASS_GROWTH_TRIGGER_LEVEL) THEN
               PEAKS(MO,2) = (1.+CLASS_PEAK/100.) * PEAKS(MO,2)
            ELSE
               PEAKS(MO,2) = CLSMW2(MO)
            ENDIF
         ENDIF
!
! ADDED 2/11/98. GAT. LARRY KEY, SRP
!
         IF(class_loc < MIN_CLASS .OR. class_loc > MAX_CLASS) THEN
            WRITE(4,*) "A class forecast file contains class"
            WRITE(4,*) "number ",class_loc," In year ",IYR
            WRITE(4,*) "It has been reassigned to Class=1."
            class_loc = 1
         ENDIF
!
         IF(CONTRIBUTES_TO_TOTAL) THEN

            SUM_CLASS_PEAKS(MO,1,IYR,class_loc) = &
                       SUM_CLASS_PEAKS(MO,1,IYR,class_loc) + &
                                      PEAKS(MO,1)*CLASS_COIN_LOSS_FACTOR
            SUM_CLASS_PEAKS(MO,2,IYR,class_loc) = &
                       SUM_CLASS_PEAKS(MO,2,IYR,class_loc) + &
                                      PEAKS(MO,2)*CLASS_COIN_LOSS_FACTOR
!
            SUM_CLASS_PEAKS(MO,1,IYR,TOTAL_CLASSES) = &
                       SUM_CLASS_PEAKS(MO,1,IYR,TOTAL_CLASSES) + &
                                      PEAKS(MO,1)*CLASS_COIN_LOSS_FACTOR
            SUM_CLASS_PEAKS(MO,2,IYR,TOTAL_CLASSES) = &
                       SUM_CLASS_PEAKS(MO,2,IYR,TOTAL_CLASSES) + &
                                      PEAKS(MO,2)*CLASS_COIN_LOSS_FACTOR
         ENDIF
!
      ENDDO
      RETURN
!

      ENTRY FIND_AND_STORE_SYSTEM_PEAKS

!
         DO YEAR = 1, FORECAST_GROWTH_YEARS
            CLASS_PEAK = 0.
            PEAK_MONTH = 0
            PEAK_DAY = 0
            if (.not. allocated(SUM_CLASS_PEAKS)) then
                er_message="cf_objt:0001 - sum_class_peaks array " // &
      "has not been allocated."
                call end_program(er_message)
                
            endif
            DO MO = 1, 12
               IF(CLASS_PEAK > &
                   MAX(SUM_CLASS_PEAKS(MO,1,YEAR,TOTAL_CLASSES), &
                        SUM_CLASS_PEAKS(MO,2,YEAR,TOTAL_CLASSES))) CYCLE
               PEAK_MONTH = MO
               IF(SUM_CLASS_PEAKS(MO,1,YEAR,TOTAL_CLASSES) > &
                          SUM_CLASS_PEAKS(MO,2,YEAR,TOTAL_CLASSES)) THEN
                  PEAK_DAY = 1
               ELSE
                  PEAK_DAY = 2
               ENDIF
               CLASS_PEAK = &
                 SUM_CLASS_PEAKS(PEAK_MONTH,PEAK_DAY,YEAR,TOTAL_CLASSES)
            ENDDO
          PLANNING_PEAK = STORE_ANNUAL_PEAK_AND_MONTH(PEAK_MONTH,YEAR, &
                                                             CLASS_PEAK)
            DO class_loc = 1 , MAX_LOAD_CLASSES
               CLASS_ANN_COIN_PEAK(class_loc,YEAR) = &
                         SUM_CLASS_PEAKS(PEAK_MONTH,PEAK_DAY,YEAR,class_loc)
            ENDDO
         ENDDO
      RETURN
!

      ENTRY DEALLOCATE_SUM_CLASS_PEAKS

         IF(ALLOCATED(SUM_CLASS_PEAKS)) then
            DEALLOCATE(SUM_CLASS_PEAKS)
         end if
      RETURN

      ENTRY GET_CLASS_PEAK_FOR_CAPACITY(R_CLASS,R_YR,R_CLASS_PEAK)

         R_CLASS_PEAK = CLASS_ANN_COIN_PEAK(R_CLASS,R_YR)
      RETURN

      ENTRY CALCULATE_SYSTEM_PEAKS(SYS_FC_DATA,IYR)

         times_csp=times_csp+1
         IF(IYR == 1 .AND. FIRST_CLASS) THEN
            IF(ALLOCATED(SUM_CLASS_PEAKS)) then
                DEALLOCATE(SUM_CLASS_PEAKS)
            end if
            ALLOCATE(SUM_CLASS_PEAKS(12,2,FORECAST_GROWTH_YEARS,1), &
                stat=allocstat)
            call check_alloc("cf_objt:0007", "sum_class_peaks", allocstat)
            SUM_CLASS_PEAKS = 0.
            FIRST_CLASS = .FALSE.
         ENDIF
!
         PLANNING_PEAK = 0.
         SYS_GROWTH_TRIGGER_LEVEL = SYS_GROWTH_TRIGGER()
         DO MO = 1, 12
            MO_OFFSET = 4*(MO-1)
            IF(IYR == 1) THEN
               PEAKS(MO,1) = SYS_FC_DATA(MO_OFFSET+2)
               PEAKS(MO,2) = SYS_FC_DATA(MO_OFFSET+4)
            ELSE
               REAL_TEMP = SYS_FC_DATA(MO_OFFSET+2)
               IF(REAL_TEMP <= SYS_GROWTH_TRIGGER_LEVEL) THEN
                  PEAKS(MO,1) = (1. + REAL_TEMP/100.) * PEAKS(MO,1)
               ELSE
                  PEAKS(MO,1) = REAL_TEMP
               ENDIF
               REAL_TEMP = SYS_FC_DATA(MO_OFFSET+4)
               IF(REAL_TEMP <= SYS_GROWTH_TRIGGER_LEVEL) THEN
                  PEAKS(MO,2) = (1. + REAL_TEMP/100.) * PEAKS(MO,2)
               ELSE
                  PEAKS(MO,2) = REAL_TEMP
               ENDIF
            ENDIF


            IF(MAX(PEAKS(MO,1),PEAKS(MO,2)) >= PLANNING_PEAK) THEN
               PLANNING_PEAK = STORE_ANNUAL_PEAK_AND_MONTH(MO,IYR, &
                                           MAX(PEAKS(MO,1),PEAKS(MO,2)))
            ENDIF
            SUM_CLASS_PEAKS(MO,1,IYR,1) = SUM_CLASS_PEAKS(MO,1,IYR,1)+ &
                                                             PEAKS(MO,1)
            SUM_CLASS_PEAKS(MO,2,IYR,1) = SUM_CLASS_PEAKS(MO,2,IYR,1)+ &
                                                             PEAKS(MO,2)
         ENDDO
      RETURN

      ENTRY SET_FIRST_CLASS_TRUE

        FIRST_CLASS = .TRUE.
      RETURN

      ENTRY GET_CLASS_MONTH_COIN_PEAK(R_CLASS,R_MONTH,R_YR,R_PEAK)

         R_PEAK = MAX(SUM_CLASS_PEAKS(R_MONTH,1,R_YR,R_CLASS), &
                      SUM_CLASS_PEAKS(R_MONTH,2,R_YR,R_CLASS))
      RETURN
      END

