    
! FE_OBJT.FOR
! Copyright(c) Global Energy Decisions 2000
!
!     Created: 11/7/2006 10:00:09 AM
!     Author : Tom Sweet
!     Last change: TS 11/7/2006 10:05:39 AM
    

      SUBROUTINE FE_OBJECT
      use end_routine, only: end_program, er_message
	  use filename_tracker

      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom



      INTEGER (KIND=2) :: DELETE,INUNIT,IREC,LRECL=128
      INTEGER :: IOS,IOS_BASE
      INTEGER (KIND=2) :: UNIT_NUM=10,SAVE_BASE_RECORDS=0,R_RECORDS
      INTEGER (KIND=4) :: R_UNIT_NUM
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: FUEL_EMISSIONS_FILE
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (KIND=4) :: FILE_EXISTS=.FALSE.
      LOGICAL (KIND=4) :: R_EMISSIONS_RATES_FILE_ACTIVE
! DECLARATION FOR FUEL PRICE VARIABLES
      INTEGER (KIND=2) :: I
      REAL :: DUMMY
! DECLARATION FOR /FUEL INVENTORY FILE/
      CHARACTER (LEN=30) :: DESC
      CHARACTER (LEN=50) :: COMMENT
      INTEGER (KIND=2) :: FUEL_ID
! VARIABLE FOR THE FUEL EMISSIONS FILE 12/21/92
      REAL :: RESERVED,EMISSIONS_RATES(MAX_EMISSION_REPORT_GROUPS + &
      	MAX_EMISSION_DISPATCH_GROUPS)
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR LOCALS
      CHARACTER (LEN=23) :: FILE_TYPE='Emissions Rates by Fuel'
      CHARACTER (LEN=2) :: FUEL_EMISSIONS_FILE_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT

    
      ENTRY FE_MAKEBIN
    
!
      BASE_FILE_NAME = FUEL_EMISSIONS_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_makebin_filename(BASE_FILE_NAME, "FEB")
    
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFUELEM.BIN", &
         	ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         EMISSIONS_RATES = 0.
         IREC = 0
         READ(10,*) DELETE
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE,FUEL_ID, &
               (RESERVED,I=1,5),(EMISSIONS_RATES(I),I=1,3),RESERVED, & 
               (EMISSIONS_RATES(I),I=4,6),DESC,COMMENT,(EMISSIONS_RATES(I),I=7,16)
               IF(IOS /= 0) THEN

                  CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
                  CALL MG_LOCATE_WRITE(21,0,'Error reading Fuel ' // &
                  	'Emission record. Look for'//' a "," in a character name.' & 
                       ,ALL_VERSIONS,1)


                  er_message='stop requested from FE_OBJT SIID114'
                  call end_program(er_message)
               ENDIF
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,FUEL_ID,EMISSIONS_RATES,DESC
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
!         ENDFILE(11)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "FE_Objt:111")
      ENDIF
      SAVE_BASE_RECORDS = IREC
      RETURN
!
! OVERLAY THE EMISSIONS-RATES-BY-FUEL FILE
!
    
      ENTRY FE_MAKEOVL(OVERLAY_FAMILY_NAME)
    
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
       file_name=get_makeovl_filename(data_drive, "FEO",OVERLAY_FAMILY_NAME)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      DUMMY = 0.
      IF(FUEL_EMISSIONS_FILE_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFUELEM.BIN",RECL=LRECL,ACCESS="DIRECT",STATUS="UNKNOWN")
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLFUELEM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,FUEL_ID,EMISSIONS_RATES,DESC
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE,FUEL_ID,(RESERVED,I=1,5),(EMISSIONS_RATES(I),I=1,3),RESERVED, &
               (EMISSIONS_RATES(I),I=4,6),DESC,COMMENT,(EMISSIONS_RATES(I),I=7,16)
               IF(IOS /= 0) THEN


                  CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
                  CALL MG_LOCATE_WRITE(21,0,  &
                  	'Error reading Fuel Emission record. Look for' &
                  //' a "," in a character name.' &
                       ,ALL_VERSIONS,1)


                  er_message='stop requested from FE_OBJT SIID115'
                  call end_program(er_message)
               ENDIF
            ENDIF
            WRITE(12,REC=IREC) DELETE,FUEL_ID,EMISSIONS_RATES,DESC
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)  ! OVERLAY DATA
      CLOSE(12)  ! OL FILE
      IF(FUEL_EMISSIONS_FILE_OL == 'BC') CLOSE(11)  !BASE FILE IF OPEN
      FUEL_EMISSIONS_FILE_OL = 'OL'
      RETURN
!
!
    
      ENTRY RESET_FUEL_EMISSIONS_FILE_OL
    
         FUEL_EMISSIONS_FILE_OL = 'BC'
      RETURN
!
    
      ENTRY OPEN_FUEL_EMISSIONS_FILE(R_UNIT_NUM,R_EMISSIONS_RATES_FILE_ACTIVE)
    
         UNIT_NUM = R_UNIT_NUM
         R_EMISSIONS_RATES_FILE_ACTIVE = FILE_EXISTS
         IF(FILE_EXISTS) OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//FUEL_EMISSIONS_FILE_OL//"FUELEM.BIN",ACCESS="DIRECT", & 
         STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!
    
      ENTRY CLOSE_FUEL_EMISSIONS_FILE
    
         IF(FILE_EXISTS) CLOSE(UNIT_NUM)
      RETURN
    
      ENTRY GET_FUEL_EMISSIONS_RECORDS(R_RECORDS)
    
         R_RECORDS = SAVE_BASE_RECORDS
      RETURN
    
      ENTRY IS_EMISSION_RATES_FILE_ACTIVE(R_EMISSIONS_RATES_FILE_ACTIVE)
    
         R_EMISSIONS_RATES_FILE_ACTIVE = FILE_EXISTS
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!
    
!
!     COPYRIGHT (C) 1992 M.S. GERBER & ASSOCIATES, INC. 
!                      ALL RIGHT RESERVED
!
!
      SUBROUTINE SET_EMISSIONS_RATES(NUNITS)

      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use prodcom

      CHARACTER (LEN=1) :: FUEL_TYPE
      INTEGER (KIND=2) :: IREC,NUNITS,DELETE,I,J,R_ISEAS
      INTEGER :: IOS
      INTEGER (KIND=2) :: FUEL_ID
      LOGICAL (KIND=4) :: EMISSIONS_RATES_FILE_ACTIVE
      LOGICAL (KIND=1) :: EMIS_REPORT_GROUPS_NOT_USED=.TRUE.
      INTEGER (KIND=4) :: ERROR,TEMP_INT4
      INTEGER (KIND=2) :: POINTR_RECORD(:)
!      
      REAL :: P_EMIS_REPORT(:,:),S_EMIS_REPORT(:,:)
      REAL :: EMISS_FUEL_EMIS_REPORT(:,:)
      REAL :: ANNUAL_EMIS_REPORT_GROUPS(MAX_EMISSION_REPORT_GROUPS)
      REAL :: R_ANNUAL_EMIS_REPORT_GROUPS(*),RTEMP,GET_VAR
      REAL :: TONS_CONVERSION,GET_TONS_CONVERSION,EMISSION_RATES(:,:)
      ALLOCATABLE :: POINTR_RECORD,P_EMIS_REPORT,S_EMIS_REPORT, &
      	EMISS_FUEL_EMIS_REPORT,EMISSION_RATES
      SAVE  ANNUAL_EMIS_REPORT_GROUPS,P_EMIS_REPORT,S_EMIS_REPORT, &
      EMISS_FUEL_EMIS_REPORT,TONS_CONVERSION,EMISSION_RATES, &
      POINTR_RECORD
!
      LOGICAL (KIND=1) :: USING_MWH_CONVERSIONS=.FALSE.
      LOGICAL (KIND=1) :: USING_ANN_CONVERSIONS=.FALSE.
      INTEGER (KIND=2) :: R_UNIT_NO,DATE_1,DATE_2,BASE_DATE,ONLINE(*)
      INTEGER (KIND=2) :: OFLINE(*),R_RECORDS
      REAL :: R_PRIM_UNIT_HEAT,R_SEC_UNIT_HEAT,R_EMIS_UNIT_HEAT
      REAL :: TOTAL_HEAT,R_UNIT_ENRG,CONTRIBUTION
      CHARACTER (LEN=1) :: UTILITY_TYPE
      CHARACTER (LEN=3) :: EMIS_UNITS(MAX_EMISSION_REPORT_GROUPS)
      SAVE EMIS_UNITS

      IF(NUNITS < 1) RETURN
      IF(ALLOCATED(P_EMIS_REPORT) ) THEN 
         DEALLOCATE(P_EMIS_REPORT)
         DEALLOCATE(S_EMIS_REPORT) 
         DEALLOCATE(EMISS_FUEL_EMIS_REPORT)
      ENDIF
      ALLOCATE( P_EMIS_REPORT(MAX_EMISSION_REPORT_GROUPS,NUNITS))
      ALLOCATE( S_EMIS_REPORT(MAX_EMISSION_REPORT_GROUPS,NUNITS))
      ALLOCATE( EMISS_FUEL_EMIS_REPORT(MAX_EMISSION_REPORT_GROUPS,NUNITS))
!
      TEMP_INT4 = INT(NUNITS)*INT(MAX_EMISSION_REPORT_GROUPS)
!     
      P_EMIS_REPORT = 0.
      S_EMIS_REPORT = 0.
      EMISS_FUEL_EMIS_REPORT = 0.

      DO I = 1, NUNITS
         IOS = 0
         IF(PRIM_FUEL_EMISS_PTR(I) > 0) THEN
            IREC = POINTR_RECORD(PRIM_FUEL_EMISS_PTR(I))

            IF(IREC == 0) THEN 
               WRITE(4,*) "The emissions rates pointer",PRIM_FUEL_EMISS_PTR(I)
               WRITE(4,*) "for unit ",UNITNM(I)
               WRITE(4,*) "and Primary fuel type"
               WRITE(4,*) "is not found in the Fuel Emissions File"
               WRITE(4,*) "Emissions are reset to zero for the unit."
               WRITE(4,*) " " 
            ENDIF
            P_SO2(I) = EMISSION_RATES(IREC,1)
            P_NOX(I) = EMISSION_RATES(IREC,2)
            P_NOX_BK2(I) = EMISSION_RATES(IREC,3)
            P_PARTICULATES(I) = EMISSION_RATES(IREC,4)
            P_EMIS_OTH2(I) = EMISSION_RATES(IREC,5)
            P_EMIS_OTH3(I) = EMISSION_RATES(IREC,6)
            DO J = 1, MAX_EMISSION_REPORT_GROUPS 
               P_EMIS_REPORT(J,I) = EMISSION_RATES(IREC,J+6)
            ENDDO
!            
            IF(EMIS_REPORT_GROUPS_NOT_USED) THEN
               DO J = 1 , MAX_EMISSION_REPORT_GROUPS
                  IF(P_EMIS_REPORT(J,I) == 0.) CYCLE
                  EMIS_REPORT_GROUPS_NOT_USED = .FALSE.
                  EXIT
               ENDDO
            ENDIF
         ENDIF
         IOS = 0
         IF(SEC_FUEL_EMISS_PTR(I) >= 1) THEN
            IREC = POINTR_RECORD(SEC_FUEL_EMISS_PTR(I))

            IF(IREC == 0) THEN 
               WRITE(4,*) "The emissions rates pointer",SEC_FUEL_EMISS_PTR(I)
               WRITE(4,*) "for unit ",UNITNM(I)
               WRITE(4,*) "and Secondar fuel type"
               WRITE(4,*) "is not found in the Fuel Emissions File"
               WRITE(4,*) "Emissions are reset to zero for the unit."
               WRITE(4,*) " " 
            ENDIF
            S_SO2(I) = EMISSION_RATES(IREC,1)
            S_NOX_BK1(I) = EMISSION_RATES(IREC,2)
            S_NOX_BK2(I) = EMISSION_RATES(IREC,3)
            S_CO2(I) = EMISSION_RATES(IREC,4)
            S_OTHER2(I) = EMISSION_RATES(IREC,5)
            S_OTHER3(I) = EMISSION_RATES(IREC,6)
            DO J = 1, MAX_EMISSION_REPORT_GROUPS 
               S_EMIS_REPORT(J,I) = EMISSION_RATES(IREC,J+6)
            ENDDO
!            
            IF(EMIS_REPORT_GROUPS_NOT_USED) THEN
               DO J = 1 , MAX_EMISSION_REPORT_GROUPS
                  IF(S_EMIS_REPORT(J,I) == 0.) CYCLE
                  EMIS_REPORT_GROUPS_NOT_USED = .FALSE.
                  EXIT
               ENDDO
            ENDIF
         ENDIF
         IF(SEC_FUEL_EMISS_PTR(I) == 0) THEN
            S_SO2(I) = 0.
            S_NOX_BK1(I) = 0.
            S_NOX_BK2(I) = 0.
            S_CO2(I) = 0.
            S_OTHER2(I) = 0.
            S_OTHER3(I) = 0.
         ELSEIF(SEC_FUEL_EMISS_PTR(I) < 0 .OR. IOS /= 0) THEN
            S_SO2(I) = P_SO2(I)
            S_NOX_BK1(I) = P_NOX(I)
            S_NOX_BK2(I) = P_NOX_BK2(I)
            S_CO2(I) = P_PARTICULATES(I)
            S_OTHER2(I) = P_EMIS_OTH2(I)
            S_OTHER3(I) = P_EMIS_OTH3(I)
         ENDIF
         IOS = 0
         IF(EMISS_FUEL_EMISS_PTR(I) >= 1) THEN
            IREC = POINTR_RECORD(EMISS_FUEL_EMISS_PTR(I))

            IF(IREC == 0) THEN 
               WRITE(4,*) "The emissions rates pointer",EMISS_FUEL_EMISS_PTR(I)
               WRITE(4,*) "for unit ",UNITNM(I)
               WRITE(4,*) "and Emissions fuel type"
               WRITE(4,*) "is not found in the Fuel Emissions File"
               WRITE(4,*) "Emissions are reset to zero for the unit."
               WRITE(4,*) " " 
            ENDIF
            EMISS_FUEL_SO2(I) = EMISSION_RATES(IREC,1)
            EMISS_FUEL_NOX_BK1(I) = EMISSION_RATES(IREC,2)
            EMISS_FUEL_NOX_BK2(I) = EMISSION_RATES(IREC,3)
            EMISS_FUEL_CO2(I) = EMISSION_RATES(IREC,4)
            EMISS_FUEL_OTHER2(I) = EMISSION_RATES(IREC,5)
            EMISS_FUEL_OTHER3(I) = EMISSION_RATES(IREC,6)
            DO J = 1, MAX_EMISSION_REPORT_GROUPS 
               EMISS_FUEL_EMIS_REPORT(J,I) = EMISSION_RATES(IREC,J+6)
            ENDDO
!            
            IF(EMIS_REPORT_GROUPS_NOT_USED) THEN
               DO J = 1 , MAX_EMISSION_REPORT_GROUPS
                  IF(EMISS_FUEL_EMIS_REPORT(J,I) == 0.) CYCLE
                  EMIS_REPORT_GROUPS_NOT_USED = .FALSE.
                  EXIT
               ENDDO
            ENDIF
         ENDIF
         IF(EMISS_FUEL_EMISS_PTR(I) == 0) THEN
            EMISS_FUEL_SO2(I) = 0.
            EMISS_FUEL_NOX_BK1(I) = 0.
            EMISS_FUEL_NOX_BK2(I) = 0.
            EMISS_FUEL_CO2(I) = 0.
            EMISS_FUEL_OTHER2(I) = 0.
            EMISS_FUEL_OTHER3(I) = 0.
         ELSEIF(EMISS_FUEL_EMISS_PTR(I) <= 0 .OR. IOS /= 0) THEN
            EMISS_FUEL_SO2(I) = P_SO2(I)         
            EMISS_FUEL_NOX_BK1(I) = P_NOX(I)     
            EMISS_FUEL_NOX_BK2(I) = P_NOX_BK2(I) 
            EMISS_FUEL_CO2(I) = P_PARTICULATES(I)
            EMISS_FUEL_OTHER2(I) = P_EMIS_OTH2(I)
            EMISS_FUEL_OTHER3(I) = P_EMIS_OTH3(I)
         ENDIF
      ENDDO
      CALL CLOSE_FUEL_EMISSIONS_FILE

      RETURN ! SET_EMISSIONS_RATES      
! CALLED ANNUALLY FROM PROCOST ^^?^
    
      ENTRY INIT_EMIS_POINTR_RECORD


      CALL OPEN_FUEL_EMISSIONS_FILE(10,EMISSIONS_RATES_FILE_ACTIVE)
      CALL GET_FUEL_EMISSIONS_RECORDS(R_RECORDS)
!
      IF(ALLOCATED(POINTR_RECORD)) DEALLOCATE(POINTR_RECORD)
      ALLOCATE(POINTR_RECORD(0:1024),STAT=ERROR) ! SAVE POINTR_RECORD
      POINTR_RECORD = 0
!
      IF(ALLOCATED(EMISSION_RATES)) DEALLOCATE(EMISSION_RATES)
      ALLOCATE(EMISSION_RATES(0:R_RECORDS,TOTAL_EMISSION_GROUPS)) ! SAVE POINTR_RECORD
      EMISSION_RATES = 0.
!
      IF(EMISSIONS_RATES_FILE_ACTIVE) THEN
!
         IREC = 1
         DO I = 1, R_RECORDS ! NOTE: IREC <= R_RECORDS
            READ(10,REC=IREC,IOSTAT=IOS) DELETE,FUEL_ID,(EMISSION_RATES(IREC,J),J=1,TOTAL_EMISSION_GROUPS) ! SAVE EMISSION_RATES
            IF(IOS .NE. 0) EXIT
            IF(DELETE < 8) POINTR_RECORD(FUEL_ID) = IREC
            IREC = IREC + 1
         ENDDO
      ENDIF
      RETURN
    
      ENTRY INIT_EMISSION_REPORT_GROUPS(NUNITS,BASE_DATE,ONLINE,OFLINE)
    
         ANNUAL_EMIS_REPORT_GROUPS = 0.
         IF(EMIS_REPORT_GROUPS_NOT_USED) RETURN 
         IF(USING_ANN_CONVERSIONS) THEN
            DATE_1 = BASE_DATE + 1
            DATE_2 = BASE_DATE + 12
            DO J = 1, NUNITS
               IF(ONLINE(J) > DATE_2 .OR. OFLINE(J) < DATE_1) CYCLE
               DO I = 1, MAX_EMISSION_REPORT_GROUPS
                  IF(EMIS_UNITS(I) /= 'ANN') CYCLE
                  IF(P_EMIS_REPORT(I,J) /= 0.) THEN
                     IF(P_EMIS_REPORT(I,J) >= 0.) THEN
                        RTEMP = P_EMIS_REPORT(I,J)
                     ELSE
                        RTEMP = GET_VAR(P_EMIS_REPORT(I,J),globecom_YEAR,FUEL_TYPE)
                     ENDIF
                  ELSEIF(S_EMIS_REPORT(I,J) /= 0.) THEN
                     IF(S_EMIS_REPORT(I,J) >= 0.) THEN
                        RTEMP = S_EMIS_REPORT(I,J)
                     ELSE
                        RTEMP = GET_VAR(S_EMIS_REPORT(I,J),globecom_YEAR,FUEL_TYPE)
                     ENDIF
                  ELSEIF(EMISS_FUEL_EMIS_REPORT(I,J) /= 0.) THEN
                     IF(EMISS_FUEL_EMIS_REPORT(I,J) >= 0.) THEN
                        RTEMP = EMISS_FUEL_EMIS_REPORT(I,J)
                     ELSE
                        RTEMP = GET_VAR(EMISS_FUEL_EMIS_REPORT(I,J),globecom_YEAR,FUEL_TYPE)
                     ENDIF
                  ELSE
                     RTEMP = 0.
                  ENDIF
                  ANNUAL_EMIS_REPORT_GROUPS(I) = ANNUAL_EMIS_REPORT_GROUPS(I) + RTEMP
               ENDDO
            ENDDO
         ENDIF
      RETURN
!
    
      ENTRY INITIALIZE_EMIS_REPORTING ! CALLED ONCE FROM PROCOST
    
!
         TONS_CONVERSION = GET_TONS_CONVERSION()
!         
         DO I = 1, MAX_EMISSION_REPORT_GROUPS
            EMIS_UNITS(I) = 'BTU'
         ENDDO
         IF(UTILITY_TYPE() == 'T' .AND. MAX_EMISSION_REPORT_GROUPS > 9) THEN
            USING_MWH_CONVERSIONS = .TRUE.
            USING_ANN_CONVERSIONS = .TRUE.
            EMIS_UNITS(6) = 'MWH'
            EMIS_UNITS(7) = 'MWH'
            EMIS_UNITS(8) = 'MWH'
            EMIS_UNITS(9) = 'ANN'
            EMIS_UNITS(10) = 'ANN'
         ENDIF
      RETURN
!      
    
      ENTRY CALC_EMISSION_REPORT_GROUPS(  R_ISEAS,R_UNIT_NO,R_PRIM_UNIT_HEAT,R_SEC_UNIT_HEAT,R_EMIS_UNIT_HEAT,R_UNIT_ENRG)
    
         IF(EMIS_REPORT_GROUPS_NOT_USED) RETURN
         IF(USING_MWH_CONVERSIONS) TOTAL_HEAT = MAX(1., R_PRIM_UNIT_HEAT + R_SEC_UNIT_HEAT + R_EMIS_UNIT_HEAT)
         IF(R_PRIM_UNIT_HEAT > 0.) THEN
            DO I = 1 , MAX_EMISSION_REPORT_GROUPS
               IF(P_EMIS_REPORT(I,R_UNIT_NO) >= 0.) THEN
                  RTEMP = P_EMIS_REPORT(I,R_UNIT_NO)
               ELSE
                  RTEMP = GET_VAR(P_EMIS_REPORT(I,R_UNIT_NO),globecom_YEAR,FUEL_TYPE)
                  IF(RTEMP < 0.) THEN ! 11/6/97. GAT.
                     RTEMP = GET_VAR(RTEMP,R_ISEAS,FUEL_TYPE)
                  ENDIF
               ENDIF
               IF(EMIS_UNITS(I) == 'BTU') THEN
                  CONTRIBUTION = RTEMP*R_PRIM_UNIT_HEAT
               ELSEIF(EMIS_UNITS(I) == 'MWH') THEN
                  CONTRIBUTION = RTEMP*R_PRIM_UNIT_HEAT*R_UNIT_ENRG/TOTAL_HEAT
               ELSE
                  CONTRIBUTION = 0.
               ENDIF
               ANNUAL_EMIS_REPORT_GROUPS(I) = ANNUAL_EMIS_REPORT_GROUPS(I) + CONTRIBUTION
            ENDDO
         ENDIF
         IF(R_SEC_UNIT_HEAT > 0.) THEN
            DO I = 1 , MAX_EMISSION_REPORT_GROUPS
               IF(S_EMIS_REPORT(I,R_UNIT_NO) >= 0.) THEN
                  RTEMP = S_EMIS_REPORT(I,R_UNIT_NO)
               ELSE
                  RTEMP = &
            GET_VAR(S_EMIS_REPORT(I,R_UNIT_NO),globecom_YEAR,FUEL_TYPE)
                  IF(RTEMP < 0.) THEN ! 11/6/97. GAT.
                     RTEMP = GET_VAR(RTEMP,R_ISEAS,FUEL_TYPE)
                  ENDIF
               ENDIF
               IF(EMIS_UNITS(I) == 'BTU') THEN
                  CONTRIBUTION = RTEMP*R_SEC_UNIT_HEAT
               ELSEIF(EMIS_UNITS(I) == 'MWH') THEN
                  CONTRIBUTION = RTEMP*R_SEC_UNIT_HEAT*R_UNIT_ENRG/TOTAL_HEAT
               ELSE
                  CONTRIBUTION = 0.
               ENDIF
               ANNUAL_EMIS_REPORT_GROUPS(I) = ANNUAL_EMIS_REPORT_GROUPS(I) + CONTRIBUTION
            ENDDO
         ENDIF
         IF(R_EMIS_UNIT_HEAT > 0.) THEN
            DO I = 1 , MAX_EMISSION_REPORT_GROUPS
               IF(EMISS_FUEL_EMIS_REPORT(I,R_UNIT_NO) >= 0.) THEN
                  RTEMP = EMISS_FUEL_EMIS_REPORT(I,R_UNIT_NO)
               ELSE
                  RTEMP = GET_VAR(EMISS_FUEL_EMIS_REPORT(I,R_UNIT_NO),globecom_YEAR,FUEL_TYPE)
                  IF(RTEMP < 0.) THEN ! 11/6/97. GAT.
                     RTEMP = GET_VAR(RTEMP,R_ISEAS,FUEL_TYPE)
                  ENDIF
               ENDIF
               IF(EMIS_UNITS(I) == 'BTU') THEN
                  CONTRIBUTION = RTEMP*R_EMIS_UNIT_HEAT
               ELSEIF(EMIS_UNITS(I) == 'MWH') THEN
                  CONTRIBUTION = RTEMP*R_EMIS_UNIT_HEAT*R_UNIT_ENRG/TOTAL_HEAT
               ELSE
                  CONTRIBUTION = 0.
               ENDIF
               ANNUAL_EMIS_REPORT_GROUPS(I) = ANNUAL_EMIS_REPORT_GROUPS(I) + CONTRIBUTION
            ENDDO
         ENDIF
      RETURN
!      
    
       ENTRY RETURN_ANN_EMIS_RPT_VARIABLES(R_ANNUAL_EMIS_REPORT_GROUPS)
    
         DO I = 1 , MAX_EMISSION_REPORT_GROUPS
            IF(EMIS_UNITS(I) == 'BTU')  THEN
               R_ANNUAL_EMIS_REPORT_GROUPS(I) = ANNUAL_EMIS_REPORT_GROUPS(I)/TONS_CONVERSION
            ELSE
               R_ANNUAL_EMIS_REPORT_GROUPS(I) =ANNUAL_EMIS_REPORT_GROUPS(I)
            ENDIF
         ENDDO
      RETURN
      END