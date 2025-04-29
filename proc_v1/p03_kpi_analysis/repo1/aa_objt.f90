!     ******************************************************************
!     AA_OBJT.FOR
!     Copyright(c) Global Energy Decisions 2000
!
!     Created: 11/7/2006 9:55:52 AM
!     Author : Tom Sweet
!     Last change: MSG 10/28/2012 11:28:42 AM
!     ******************************************************************

      SUBROUTINE AA_OBJECT
      use end_routine, only: end_program, er_message
      use logging
      use filename_tracker


!
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom

      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=128,I
      INTEGER :: IOS
      CHARACTER(len=1) :: RESOURCE_ALLOCATION_METHOD
      INTEGER(kind=2) :: UNIT_NUM=10
      INTEGER(kind=4) :: R_UNIT_NUM
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  AREA_ALLOCATORS_FILE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR AREA ALLOCATORS
      REAL :: AREA_VALUES(18)
      REAL :: CAPACITY_LOSS_FACTORS(6)
      INTEGER(kind=2) :: RESOURCE_ID,ALLOCATION_PERIOD
      CHARACTER(len=16) :: FILE_TYPE='Area Allocations'
      CHARACTER(len=20) :: RESOURCE_NAME,COMMENT*50
      CHARACTER(len=2) :: RESOURCE_TYPE,AREA_OL='BC'
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      logical :: exists
      CHARACTER(len=255) :: old_filename

! ***********************************************************************
!
!   ROUTINE TO CONVERT PRODUCTION PARAMETER FILE DATA
!
!                             COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                             ALL RIGHTS RESERVED
!
! ***********************************************************************
!
! CONVERT THE CONTROL-AREA ALLOCATORS FILE
!
      ENTRY AA_MAKEBIN
!
! ***********************************************************************


      ! If file doesn't exist, BASE_FILE_NAME is checked for "NONE" TO
      ! determine whether nonexistent file is an error. Filename is
      ! retrieved from get_aab_filename()
      BASE_FILE_NAME = AREA_ALLOCATORS_FILE()
      file_name=get_aab_filename()

      INQUIRE(FILE=FILE_NAME,EXIST=exists)
      IF(exists) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         data_drive=OUTPUT_DIRECTORY()
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCAREA.BIN", &
        ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            CAPACITY_LOSS_FACTORS = -99999.
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE, &
                               RESOURCE_NAME,RESOURCE_ID, &
                               RESOURCE_TYPE, &
                               ALLOCATION_PERIOD,AREA_VALUES,COMMENT, &
                               CAPACITY_LOSS_FACTORS, &
                               RESOURCE_ALLOCATION_METHOD

! TODO: integration note
            DO I = 1, 6
!          TODO: Determine correct iteration strategy. Previously,
!          that meant iterating 1..6. Now the actual array size
!          is being used. It's believed that this will remain correct
!          after the array is resized, but in this model it's impossible
!          to be sure.  -jtr 02052024

               IF(CAPACITY_LOSS_FACTORS(I) == -99999.) &
                  CAPACITY_LOSS_FACTORS(I) = AREA_VALUES(3*(I-1)+3)
            ENDDO
            WRITE(11,REC=IREC) DELETE, &
                            RESOURCE_NAME,RESOURCE_ID, &
                            RESOURCE_TYPE, &
                            ALLOCATION_PERIOD,AREA_VALUES, &
                            CAPACITY_LOSS_FACTORS, &
                            RESOURCE_ALLOCATION_METHOD
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "AA_OBJT:0001")
      ENDIF
      RETURN



! OVERLAY THE CONTROL-AREA ALLOCATORS FILE
! ***********************************************************************
      ENTRY AA_MAKEOVL(OVERLAY_FAMILY_NAME)
! ***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      file_name=get_aao_filename(OVERLAY_FAMILY_NAME)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(AREA_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCAREA.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLAREA.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                                   RESOURCE_NAME,RESOURCE_ID, &
                                   RESOURCE_TYPE, &
                                   ALLOCATION_PERIOD,AREA_VALUES, &
                                   CAPACITY_LOSS_FACTORS, &
                                   RESOURCE_ALLOCATION_METHOD
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE, &
                                  RESOURCE_NAME,RESOURCE_ID, &
                                  RESOURCE_TYPE, &
                                  ALLOCATION_PERIOD,AREA_VALUES,COMMENT, &
                                  CAPACITY_LOSS_FACTORS, &
                                  RESOURCE_ALLOCATION_METHOD
         ENDIF
         WRITE(12,REC=IREC) DELETE, &
                            RESOURCE_NAME,RESOURCE_ID, &
                            RESOURCE_TYPE, &
                            ALLOCATION_PERIOD,AREA_VALUES, &
                            CAPACITY_LOSS_FACTORS, &
                            RESOURCE_ALLOCATION_METHOD
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(AREA_OL == 'BC') CLOSE(11)
      AREA_OL = 'OL'
      RETURN

!  200 CALL LOCATE(20,0)
!      WRITE(6,1010) trim(RECLN)
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from AA_OBJT SIID0'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                     'Error reading the Control Area record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from AA_OBJT SIID1'
      call end_program(er_message)

      ENTRY RESET_AREA_OL
         AREA_OL = 'BC'
      RETURN
!
      ENTRY OPEN_AREA_ALLOCATOR_FILE(R_UNIT_NUM)
         UNIT_NUM = R_UNIT_NUM
         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//AREA_OL// &
                 "AREA.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!
      ENTRY CLOSE_AREA_ALLOCATOR_FILE
         CLOSE(UNIT_NUM)
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
! ***********************************************************************
      SUBROUTINE READ_AREA_ALLOCATORS
! ***********************************************************************
!
      use spindriftlib
      use prod_arrays_dimensions
      use kepcocom
      use sizecom
      implicit none

      INTEGER(kind=2) :: DELETE,RESOURCE_ID,ALLOCATION_PERIOD,IREC, &
                         I,MO,AREA,ARRAY_POINTR,TEMP_POINTR
      CHARACTER(len=20) :: RESOURCE_NAME
      CHARACTER(len=2) ::  RESOURCE_TYPE
      CHARACTER(len=1) ::  RESOURCE_ALLOCATION_METHOD
      REAL :: CAP_ALLOCATORS(6), &
           ENRG_ALLOCATORS(6), &
           RESOURCE_LOSES(6), &
           CAPACITY_LOSS_FACTORS(6)

!
      CL_ALLOCATE_POINTR = 0
      CT_ALLOCATE_POINTR = 0
      EL_ALLOCATE_POINTR = 0
      CALL OPEN_AREA_ALLOCATOR_FILE(10)
      IREC = 1
      ARRAY_POINTR = 0
      DO
         READ(10,REC=IREC,ERR=100) DELETE, &
                                RESOURCE_NAME,RESOURCE_ID, &
                                RESOURCE_TYPE, &
                                ALLOCATION_PERIOD, &
                               (CAP_ALLOCATORS(I), &
                                ENRG_ALLOCATORS(I), &
                                RESOURCE_LOSES(I), I=1, 6), &
                                CAPACITY_LOSS_FACTORS, &
                                RESOURCE_ALLOCATION_METHOD
         IREC = IREC + 1
         IF(DELETE > 8) CYCLE
         IF(INDEX(RESOURCE_TYPE,'CL') .NE. 0) THEN
            IF(CL_ALLOCATE_POINTR(RESOURCE_ID) == 0) THEN
               ARRAY_POINTR = ARRAY_POINTR + 1
               TEMP_POINTR = ARRAY_POINTR
               CL_ALLOCATE_POINTR(RESOURCE_ID) = ARRAY_POINTR
            ELSE
               TEMP_POINTR = CL_ALLOCATE_POINTR(RESOURCE_ID)
            ENDIF
         ELSEIF(INDEX(RESOURCE_TYPE,'CT') .NE. 0) THEN
            IF(CT_ALLOCATE_POINTR(RESOURCE_ID) == 0) THEN
               ARRAY_POINTR = ARRAY_POINTR + 1
               TEMP_POINTR = ARRAY_POINTR
               CT_ALLOCATE_POINTR(RESOURCE_ID) = ARRAY_POINTR
            ELSE
               TEMP_POINTR = CT_ALLOCATE_POINTR(RESOURCE_ID)
            ENDIF
         ELSEIF(INDEX(RESOURCE_TYPE,'EL') .NE. 0) THEN
            IF(EL_ALLOCATE_POINTR(RESOURCE_ID) == 0) THEN
               ARRAY_POINTR = ARRAY_POINTR + 1
               TEMP_POINTR = ARRAY_POINTR
               EL_ALLOCATE_POINTR(RESOURCE_ID) = ARRAY_POINTR
            ELSE
               TEMP_POINTR = EL_ALLOCATE_POINTR(RESOURCE_ID)
            ENDIF
         ELSE
            WRITE(4,*) 'Missing resource type for ', &
                        trim(RESOURCE_NAME), &
                       ' in Control Area Allocation file.'
            CYCLE
         ENDIF
        AREA_ALLOCATION_METHOD(ARRAY_POINTR)=RESOURCE_ALLOCATION_METHOD
         IF(ALLOCATION_PERIOD == 13) THEN
            DO MO = 1, 12
               DO AREA = 1, NUMBER_OF_AREAS
                  AREA_CAP_ALLOCATORS(AREA,MO,TEMP_POINTR) = &
                                                    CAP_ALLOCATORS(AREA)
                  AREA_ENRG_ALLOCATORS(AREA,MO,TEMP_POINTR) = &
                                                   ENRG_ALLOCATORS(AREA)
                  AREA_RESOURCE_LOSES(AREA,MO,TEMP_POINTR) = &
                                                    RESOURCE_LOSES(AREA)
                  AREA_CAPACITY_LOSSES(AREA,MO,TEMP_POINTR) = &
                                             CAPACITY_LOSS_FACTORS(AREA)
               ENDDO
            ENDDO
         ELSE
            DO AREA = 1, NUMBER_OF_AREAS
               AREA_CAP_ALLOCATORS(AREA,ALLOCATION_PERIOD, &
         TEMP_POINTR)=CAP_ALLOCATORS(AREA)
               AREA_ENRG_ALLOCATORS(AREA, &
                   ALLOCATION_PERIOD,TEMP_POINTR)=ENRG_ALLOCATORS(AREA)
               AREA_RESOURCE_LOSES(AREA,ALLOCATION_PERIOD,TEMP_POINTR)= &
                                                    RESOURCE_LOSES(AREA)
               AREA_CAPACITY_LOSSES(AREA,MO,TEMP_POINTR) = &
                                             CAPACITY_LOSS_FACTORS(AREA)
            ENDDO
         ENDIF
      ENDDO
  100 CALL CLOSE_AREA_ALLOCATOR_FILE
      RETURN
      END










