!     ******************************************************************
!     UP_OBJT.FOR
!     Created: 11/11/02 10:23:26 AM
!     Author : msg
!     Last change: msg 10/11/2016 6:13:43 PM
!     ******************************************************************

      ! Suspect memory corruption, so moved vector_pointr array
      ! to this module. Module only used in this file.
      module up_objt_mod
          implicit none
          LOGICAL(kind=1) :: VECTOR_POINTER_ALLOCATED=.FALSE.
          INTEGER(kind=2), allocatable :: VECTOR_POINTR(:)
          integer (kind=2), save :: MAX_VALID_VECTOR=-999
      end module up_objt_mod

      MODULE UNIT_PARAMETER_FILE_VARIABLES
         INTEGER (KIND=2), PARAMETER :: MAX_UP_FILES=10,LRECL_UP=156
         CHARACTER (LEN=2) :: UP_OL(0:MAX_UP_FILES-1)
         CHARACTER (LEN=2) :: UP_FILE_CODES(0:MAX_UP_FILES-1)=(/ &
                     'UP','U1','U2','U3','U4','U5','U6','U7','U8','U9'/)
        CHARACTER (LEN=11):: UP_FILE_BINARY_NAMES(0:MAX_UP_FILES-1)=(/ &
                                                'UNTPR      ', &
                                                'UNIT_PARAM1', &
                                                'UNIT_PARAM2', &
                                                'UNIT_PARAM3', &
                                                'UNIT_PARAM4', &
                                                'UNIT_PARAM5', &
                                                'UNIT_PARAM6', &
                                                'UNIT_PARAM7', &
                                                'UNIT_PARAM8', &
                                                'UNIT_PARAM9'/)
         LOGICAL (KIND=1) :: UP_BASE_FILE_ACTIVE(0:MAX_UP_FILES-1)
         INTEGER (KIND=2) :: BASE_MAX_VECTOR_NUM(0:MAX_UP_FILES-1), &
                             OVLY_MAX_VECTOR_NUM(0:MAX_UP_FILES-1)
         INTEGER (KIND=4) :: BASE_INPUT_RECORDS(0:MAX_UP_FILES-1)=0, &
                             OVERLAY_INPUT_RECORDS(0:MAX_UP_FILES-1)=0
      END MODULE
! ***********************************************************************
      SUBROUTINE UP_OBJECT
      use end_routine, only: end_program, er_message
	  use miscmod
! ***********************************************************************
!
      USE UNIT_PARAMETER_FILE_VARIABLES
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      integer (kind=2) :: vn
      INTEGER(kind=2) :: I,IREC,OL_REC,INUNIT,DELETE,DUMMY, &
                UNIT_NO, &
                VECTOR_NO
      INTEGER :: IOS
      INTEGER(kind=2) :: LAST_INPUT_RECORD=0
      LOGICAL(kind=1) :: VOID_LOGIC,VARIABLE_UNITS_OBJECT, &
                         ACTIVE_GRX_RPS_VECTOR
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=5) :: OVERLAY_FAMILY_NAME,UNIT_PARMS
      CHARACTER(len=256) :: FILE_NAME,FILE_NAME2
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR GENERATING UNIT PARAMETERS
      REAL :: VALUES_30(30)
      CHARACTER(len=40) :: DESCRIPTION
      CHARACTER(len=26) :: FILE_TYPE='Variable Unit Parameters'
      CHARACTER(len=30) :: SCREEN_OUTPUT
!
!  VARIABLE ADDITIONS AND CHANGES 02/18/07 DR.G
!
      INTEGER (KIND=2) :: FILE_NO
      INTEGER (KIND=4) :: R_FILE_NO ,MAX_VECTOR_NUM
      CHARACTER (LEN=5) :: UP_FILE_BASE_NAMES(0:MAX_UP_FILES-1), &
                           VOID_CHR
!
! ***********************************************************************
      ENTRY UP_MAKEBIN
! ***********************************************************************
!
      UP_OL = 'BC'
      BASE_MAX_VECTOR_NUM = 0
      UP_BASE_FILE_ACTIVE = .FALSE.
      VOID_CHR = UNIT_PARMS(UP_FILE_BASE_NAMES)
      DO FILE_NO = 0, MAX_UP_FILES-1
      	FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                        TRIM(UP_FILE_CODES(FILE_NO))//"B"// &
                           TRIM(UP_FILE_BASE_NAMES(FILE_NO))//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) CYCLE
         SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'// &
                                          UP_FILE_BASE_NAMES(FILE_NO)
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
              TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN", &
                     ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL_UP)
         UP_BASE_FILE_ACTIVE(FILE_NO) = .TRUE.



         IREC = 0
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            IF(RECLN(1:1) == '7') CYCLE
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            VALUES_30 = -999999.
            READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO, &
                                                            VALUES_30
            VECTOR_NO = ABS(VECTOR_NO)
            BASE_MAX_VECTOR_NUM(FILE_NO) = MAX(VECTOR_NO, &
                                        BASE_MAX_VECTOR_NUM(FILE_NO))
            IREC = IREC + 1
            IF(VALUES_30(1) == -999999.) VALUES_30(1) = 0.
            DO I = 2, AVAIL_DATA_YEARS
               IF(VALUES_30(I)==-999999.) VALUES_30(I)=VALUES_30(I-1)
            ENDDO
            WRITE(11,REC=IREC) VECTOR_NO,VALUES_30,DELETE
            VECTOR_NO = VECTOR_NO + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
         BASE_INPUT_RECORDS(FILE_NO) = IREC
      ENDDO ! FILES
      RETURN
!
!  OVERLAY THE GENERATING-UNIT PARAMETER FILE
!
! ***********************************************************************
      ENTRY UP_MAKEOVL(OVERLAY_FAMILY_NAME,R_FILE_NO)
! ***********************************************************************
!
      IF(.NOT. UP_BASE_FILE_ACTIVE(R_FILE_NO)) RETURN
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
      CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      FILE_NO = R_FILE_NO
      MAX_VECTOR_NUM = OVLY_MAX_VECTOR_NUM(FILE_NO)
      IF(UP_OL(R_FILE_NO) == 'BC') THEN
         FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"BC"// &
                             TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN"
         FILE_NAME2 = TRIM(OUTPUT_DIRECTORY())//"OL"// &
                             TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN"
         CALL COPY_FILE_2_FILE(FILE_NAME,FILE_NAME2)
         MAX_VECTOR_NUM = BASE_MAX_VECTOR_NUM(FILE_NO)
         UP_OL(FILE_NO) = 'OL'
      ENDIF
      OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"// &
              TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN", &
                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL_UP)
!
      OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())//UP_FILE_CODES(FILE_NO) &
                               //"O"//TRIM(OVERLAY_FAMILY_NAME)//".DAT")
      READ(10,*) DELETE
      OL_REC = 1
      DO
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS /= 0) EXIT
         IF(RECLN(1:1) == '7') CYCLE
         RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(12,REC=OL_REC,IOSTAT=IOS) VECTOR_NO,VALUES_30
         IF(IOS /= 0) EXIT
         READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO,VALUES_30
         VECTOR_NO = ABS(VECTOR_NO)
         MAX_VECTOR_NUM = MAX(MAX_VECTOR_NUM,int(VECTOR_NO,4))
         WRITE(12,REC=OL_REC) VECTOR_NO,VALUES_30,DELETE
         OL_REC = OL_REC + 1
      ENDDO
      CLOSE(10)
      CLOSE(12)
      OVLY_MAX_VECTOR_NUM(FILE_NO) = MAX_VECTOR_NUM
      OVERLAY_INPUT_RECORDS(FILE_NO) = OL_REC - 1
      RETURN
!
! ***********************************************************************
      ENTRY RESET_UPE_OL
! ***********************************************************************
         UP_OL(:) = 'BC'
         OVLY_MAX_VECTOR_NUM(:) = 0
         OVERLAY_INPUT_RECORDS(:) = 0
      RETURN
! ***********************************************************************
      ENTRY WRITE_GRX_RPS_CAPACITY
! ***********************************************************************
         IF(.TRUE.) THEN
            VOID_CHR = UNIT_PARMS(UP_FILE_BASE_NAMES)
            FILE_NO = 9
      	   FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                           "U9B"// &
                           TRIM(UP_FILE_BASE_NAMES(FILE_NO))//".DAT"
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
               OPEN(10,FILE=FILE_NAME)
               OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"U9OXXXXX.DAT")
!              READ(10,*) DELETE
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /= 0) EXIT
                  IF(RECLN(1:1) == '7') THEN
                     WRITE(11,1000) RECLN ! '7, ,'
                  ELSEIF(RECLN(1:1) == '9') THEN
                     WRITE(11,1000) RECLN ! '9, ,'
                  ELSE
                     RECLN = &
                            TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                     VALUES_30 = -999999.
                     READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO, &
                                                            VALUES_30
                     IF(ACTIVE_GRX_RPS_VECTOR(VECTOR_NO,VALUES_30)) THEN
                        WRITE(11,1015) DELETE, &
                                          TRIM(DESCRIPTION), &
                                          VECTOR_NO, &
                                          VALUES_30
!                        RECLN = TRIM(RECLN)
!                        WRITE(11,*) RECLN
                     ELSE
                        WRITE(11,1020) DELETE,TRIM(DESCRIPTION), &
                               VECTOR_NO,',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                     ENDIF
                  ENDIF
               ENDDO
               CLOSE(10)
               CLOSE(11)
            ENDIF
         ENDIF
      RETURN
 1015 FORMAT(I2,',"',A,'",',I5,30(',',F9.2))
 1020 FORMAT(I2,',"',A,'",',I5,',',A)
! ***********************************************************************
  200 CALL MG_LOCATE_WRITE(20,0,TRIM(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from UP_OBJT SIID338'
      call end_program(er_message)
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END SUBROUTINE UP_OBJECT
!
! ***********************************************************************
      SUBROUTINE OPEN_VAR_UNITS_FILE
! ***********************************************************************
!
      LOGICAL(kind=1) :: VARIABLE_UNITS_OBJECT, VOID_LOGIC
!
         VOID_LOGIC = VARIABLE_UNITS_OBJECT()
      RETURN
!
      END subroutine OPEN_VAR_UNITS_FILE
!
! ***********************************************************************
      FUNCTION VARIABLE_UNITS_OBJECT()
      use end_routine, only: end_program, er_message
! ***********************************************************************
!
      USE UNIT_PARAMETER_FILE_VARIABLES
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      use rangechecker
      use filemod
      use up_objt_mod
      use miscmod
      use program_state
      use debugtrace
      

!  CONTRACT ENERGY PATTERNS
      ! TODO: Delete this variable (passval)
      character (len=512) :: passval
      REAL :: GET_MONTHLY_VAR_SUM
      INTEGER(kind=2) :: R_VECTOR_NUM,MO,DELETE
      REAL :: R_MONTHLY_VECTOR(12),GET_MONTHLY_VAR_VALUES

      REAL :: GET_VECTOR
      REAL :: GET_EL_MW_FROM_POINTR, &
           GET_CL_MW_FROM_POINTR, &
           GET_CT_MW_FROM_POINTR, &
           GET_VAR
      CHARACTER(len=*) UNITNM
      INTEGER(kind=2) :: ACTIVE_VECTORS, &
                VECTOR_NUMBER,I,J,YR
      LOGICAL(kind=1) :: VARIABLE_UNITS_OBJECT
      REAL (kind=4), allocatable :: VECTOR_VALUES(:,:)
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      INTEGER (KIND=4) :: RECORDS_IN_FILE(0:MAX_UP_FILES-1),REC_READ
      INTEGER (KIND=2) :: FILE_NO
      integer (kind=2) :: second_idx, vn
      integer :: ady
      integer :: file_trace=0, file_trace_2=0, file_trace_3=0
      integer :: file_trace_4=0, file_trace_gvs=0, file_trace_gvc=0
      character (len=256) :: txt_trace, txt_trace_2
      character (len=256) :: txt_trace_3, txt_trace_4
      character (len=256) :: read_filename
      integer :: allocstat
      
      

!
         IF(COUNT(UP_BASE_FILE_ACTIVE(:)) == 0) RETURN
         RECORDS_IN_FILE = 0
         MAX_VALID_VECTOR = 0
         DO FILE_NO = 0, MAX_UP_FILES-1
            IF(UP_OL(FILE_NO) == 'OL') THEN
               RECORDS_IN_FILE(FILE_NO) = OVERLAY_INPUT_RECORDS(FILE_NO)
               MAX_VALID_VECTOR = MAX(OVLY_MAX_VECTOR_NUM(FILE_NO), &
                                                       MAX_VALID_VECTOR)
            ELSE
               RECORDS_IN_FILE(FILE_NO) = BASE_INPUT_RECORDS(FILE_NO)
               MAX_VALID_VECTOR = MAX(BASE_MAX_VECTOR_NUM(FILE_NO),  & !
                                                       MAX_VALID_VECTOR)
            ENDIF
         ENDDO
         ACTIVE_VECTORS = SUM(RECORDS_IN_FILE(:))
         IF(ACTIVE_VECTORS == 0) RETURN
         IF(ALLOCATED(VECTOR_VALUES)) THEN
            DEALLOCATE(VECTOR_VALUES)
         ENDIF
         if(allocated(vector_pointr)) then
            deallocate(VECTOR_POINTR)
         end if
         ALLOCATE(VECTOR_VALUES(ACTIVE_VECTORS,AVAIL_DATA_YEARS), &
            STAT=allocstat)
            
         call check_alloc("up_objt:0008", "vector_values", &
            allocstat)
         allocate(VECTOR_POINTR(MAX_VALID_VECTOR), stat=allocstat)
         call check_alloc("up_objt:0009", "VECTOR_POINTR", &
            allocstat)
            
         
         VECTOR_POINTER_ALLOCATED = .TRUE.
         VECTOR_POINTR = 0
         REC_READ = 1
         
          if(file_trace_4==0) then
            
            txt_trace_4="vector_pointr_vn.trace"

            file_trace_4=open_trace(txt_trace_4, rq_vpvn)
            call write_trace_message(file_trace_4,"See tag issue_200.a.")
          end if
          
         DO FILE_NO = 0, MAX_UP_FILES-1
         
            if(get_debug_tracing_on().and. file_trace_4/=BAD_TRACE_HANDLE) then
                write(file_trace_4,*) &
                    "up_objt:0010", "FILE_NO", FILE_NO, &
                    "MAX_UP_FILES", MAX_UP_FILES, &
                    "UP_BASE_FILE_ACTIVE", &
                    UP_BASE_FILE_ACTIVE(FILE_NO)
            end if
            
            IF(.NOT. UP_BASE_FILE_ACTIVE(FILE_NO)) then
            
                if(file_trace_4/= BAD_TRACE_HANDLE) then
                    write(file_trace_4,*) "cycling on", &
                    UP_BASE_FILE_ACTIVE(FILE_NO)
                end if
                
                CYCLE
            end if
            
            OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
                         TRIM(UP_OL(FILE_NO))// &
                         TRIM(UP_FILE_BINARY_NAMES(FILE_NO))//".BIN", &
                    ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL_UP)
                    
            
            read_filename=get_filename_from_unit(10)
            if(get_debug_tracing_on().and. file_trace_4/= BAD_TRACE_HANDLE) then
                write(file_trace_4,*) "read_filename="//trim(read_filename)
                write(file_trace_4,*) "FILE_NO=", FILE_NO
                write(file_trace_4,*) "RECORDS_IN_FILE:", &
                    records_in_file(file_no)
            end if
            
            DO I = 1, RECORDS_IN_FILE(FILE_NO)

               READ(10,REC=I) VECTOR_NUMBER, &
                              VECTOR_VALUES(REC_READ,:), &
                              DELETE
         if(get_debug_tracing_on().and. file_trace_4/= BAD_TRACE_HANDLE) then                     
               write(file_trace_4,*) "Read vector_pointr(", &
                "REC_READ(:...) from " // trim(read_filename)
                
               write(file_trace_4,*) "REC_READ=", REC_READ
               
               write(file_trace_4,*) "Vector_values(REC_READ:-)", &
                Vector_values(REC_READ,:)
         end if
               IF(DELETE > 7) then
                CYCLE
               end if
               VECTOR_NUMBER = ABS(VECTOR_NUMBER)
               IF(VECTOR_POINTR(VECTOR_NUMBER) == 0) THEN
       if(get_debug_tracing_on().and. file_trace_4/= BAD_TRACE_HANDLE) then
                  write(file_trace_4, *) "REC_READ=", REC_READ
                  write(file_trace_4, *) "VECTOR_NUMBER ", VECTOR_NUMBER
                  write(file_trace_4, *) &
      "Before setting, VECTOR_POINTR(VECTOR_NUMBER)=", &
                    vector_pointr(VECTOR_NUMBER)
       end if
                    
                  VECTOR_POINTR(VECTOR_NUMBER) = REC_READ
                  if(get_debug_tracing_on().and. file_trace_4/= BAD_TRACE_HANDLE) then
                      write(file_trace_4, *) "After setting, " // &
                        "VECTOR_POINTR(VECTOR_NUMBER)=", &
                        vector_pointr(VECTOR_NUMBER)
                  end if
                  
                  REC_READ = REC_READ + 1
               ELSE
                  WRITE(4,*) "DUPLICATE VECTOR NUMBER ",VECTOR_NUMBER
                  WRITE(4,*) "IN UNIT PARAMETER FILE WITH CODE ", &
                                                  UP_FILE_CODES(FILE_NO)
                  WRITE(4,*) "CHECK BASE AND OVERLAY FILES "
                  WRITE(4,*) "AT COLUMN POSITION ",I
               ENDIF
            ENDDO
            CLOSE(10)
         ENDDO
         VARIABLE_UNITS_OBJECT = .TRUE.
      RETURN
!
! ***********************************************************************
      ENTRY GET_CL_MW_FROM_POINTR(GET_VECTOR,YR)
! ***********************************************************************
         ! if(yr<5) then
             ! call check_bounds_r4_2s(vector_values, "up_objt:0001", &
                   ! "vector_values", vector_number, second_idx)
         ! end if
         IF(VECTOR_POINTER_ALLOCATED) THEN

             if(yr==0) then
              call end_program("up_objt:0004 - YR argument is zero.")
             end if


            
            VECTOR_NUMBER = INT(ABS(GET_VECTOR))
            GET_CL_MW_FROM_POINTR = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               vn=vector_number
               
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               vn=vector_number
               if(yr==0) then
                call end_program("up_objt:0003 - YR argument is zero.")
               end if
               
               if(vn==489) then
                    vn=vn ! debugstop
               end if
               IF(VECTOR_NUMBER /= 0) then
                second_idx=MIN(YR,AVAIL_DATA_YEARS)
                ady=avail_data_years
                if(second_idx==0 .or. ady==0) then
                    call end_program("up_objt:up_objt:0002 - " // &
                    "Year=" // trim(itos(int(YR))) // ", " // &
                    "AVAIL_DATA_YEARS=" //trim(itos(int(ady))))
                end if
                

                GET_CL_MW_FROM_POINTR = &
                   VECTOR_VALUES(VECTOR_NUMBER,second_idx)
               end if
            ELSE
!
               GET_CL_MW_FROM_POINTR = 1.0 ! 5/6/02. Per Mark Gerber.
!
               WRITE(4,*) 'Invalid capacity pointer',GET_VECTOR, &
                          ' in Capacity Limited file.'
            ENDIF
         ELSE
   er_message='SIID340 - Unit Parameter File NOT Active for CL pointer.'
            call end_program(er_message)
         ENDIF
      RETURN
!
! ***********************************************************************
      ENTRY GET_EL_MW_FROM_POINTR(GET_VECTOR,YR)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = INT(ABS(GET_VECTOR))
            GET_EL_MW_FROM_POINTR = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) GET_EL_MW_FROM_POINTR = &
                   VECTOR_VALUES(VECTOR_NUMBER,MIN(YR,AVAIL_DATA_YEARS))
            ELSE
               WRITE(4,*) 'Invalid capacity pointer',GET_VECTOR, &
                          ' in Energy Limited file.'
            ENDIF
         ELSE
            er_message='SIID341 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
         
          

          if(file_trace_2==0) then
              ! No explicit close - doesn't close until Fortran encounters STOP
              txt_trace_2=trim(adjustl(itos(int(I)))) ! Based on index
              txt_trace_2="up_objt_102_H_" // trim(txt_trace) //".txt"
              file_trace_2=open_trace(txt_trace, rq_up_102_h)
          end if
          if(get_debug_tracing_on().and. file_trace_4/= BAD_TRACE_HANDLE) then
              write(file_trace_2, *) &
              "GET_VECTOR", GET_VECTOR, "YR", YR, &
              "VECTOR_POINTER_ALLOCATED", VECTOR_POINTER_ALLOCATED, &
              "GET_EL_MW_FROM_POINTR() result", GET_EL_MW_FROM_POINTR, &
              "MAX_VALID_VECTOR", MAX_VALID_VECTOR, &
              "AVAIL_DATA_YEARS", AVAIL_DATA_YEARS
          end if

          ! TODO:  Remove code fragment ^^
          
      RETURN
!
! ***********************************************************************
      ENTRY GET_CT_MW_FROM_POINTR(GET_VECTOR,YR)
! ***********************************************************************

         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = INT(ABS(GET_VECTOR))
            GET_CT_MW_FROM_POINTR = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) GET_CT_MW_FROM_POINTR = &
                   VECTOR_VALUES(VECTOR_NUMBER,MIN(YR,AVAIL_DATA_YEARS))
            ELSE
               WRITE(4,*) 'Invalid capacity pointer',GET_VECTOR, &
                          ' in Contracts file.'
            ENDIF
         ELSE
            er_message='SIID342 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
         

          if(file_trace==0) then
              ! No explicit close - doesn't close until Fortran encounters STOP
              txt_trace="up_objt_102_H.trace"
              file_trace=open_trace(txt_trace, rq_up_102_h)
          end if
          if(get_debug_tracing_on().and. file_trace/= BAD_TRACE_HANDLE) then
              write(file_trace, *) &
                "VECTOR_NUMBER", VECTOR_NUMBER, &
                "RESULT", GET_CT_MW_FROM_POINTR, &
                "GET_VECTOR", GET_VECTOR, &
                "YR", YR
          end if
          
      RETURN
! **********************************************************************
!
!              FUNCTION TO READ VARIABLE UNIT PARAMETERS FOR MIDAS
!                    COPYRIGHT (C) 1986
!                         M.S. GERBER & ASSOCIATES, INC.
!                               ALL RIGHTS RESERVED
!
! **********************************************************************
!
      ENTRY GET_VAR(GET_VECTOR,YR,UNITNM)
           get_var_call_count=get_var_call_count+1


            if(file_trace_gvc==0) then
             file_trace_gvc=open_trace("get_var_calls.trace", rq_uo_gvc)
             call write_trace_message (file_trace_gvc, &
                "get_var calls, in order...")
            end if

            
            if(file_trace_gvc/=BAD_TRACE_HANDLE) then
                write(file_trace_gvc,*) "GET_VECTOR(", &
                    GET_VECTOR, YR, UNITNM, ")"
                    
                if(len_trim(get_var_called_from)==0) then
                    get_var_call_count=get_var_call_count ! Debugstop
                    call end_program("up_objt:0011 - Client unknown")
                else
                   passval=get_var_called_from
                   call write_trace_string(file_trace_gvc, "get_var called_from ", &
                       passval)
              
              write(file_trace_gvc,*) "(",GET_VECTOR,YR,UNITNM, ")"
                end if
           end if
            
            get_var_called_from="" ! reset for next call.


            if(file_trace_gvs==0) then

                file_trace_gvs=open_trace("get_var_sequence.trace", rq_uo_gvs)
                call write_trace_message(file_trace_gvs, &
                    "get_var sequence left/right")
                call write_trace_message(file_trace_gvs,"First call.")
            end if
            
            if(get_debug_tracing_on().and. file_trace_gvs/= BAD_TRACE_HANDLE) then
                write(file_trace_gvs,*) "CALL", get_var_call_count
                write(file_trace_gvs,*) "GET_VECTOR ", GET_VECTOR
                write(file_trace_gvs,*) "YR ", YR
                write(file_trace_gvs,*) "1. UNITNM ", UNITNM
            end if

            
            if(file_trace_3==0) then
              !No explicit close - doesn't close until Fortran encounters STOP
              txt_trace_3="up_objt_102_J_right" //".txt"
              file_trace_3=open_trace(txt_trace_3, rq_uo_102_J)
              call write_trace_message(file_trace_3, &
                "GET_VAR results over time." )
                
              call write_trace_message(file_trace_3, "...............")
          end if
          
          if(file_trace_gvs/= BAD_TRACE_HANDLE) then
              write(file_trace_3, *) "Arguments:"
              write(file_trace_3, *) "GET_VECTOR ", GET_VECTOR
              write(file_trace_3, *) "YR ", YR
              write(file_trace_3, *) "2. UNITNM ", UNITNM
              write(file_trace_3, *) "---------"
        write(file_trace_3,*) "call count (entry)", get_var_call_count
          end if
          
          

          

          if(file_trace_3/= BAD_TRACE_HANDLE) then
              write(file_trace_3, *) VECTOR_POINTER_ALLOCATED
          end if
          
          if (get_var_call_count>=45879-1) then
            ! ! Current revision discontinues here, while legacy continues.
            get_var_call_count=get_var_call_count ! Debugstop
          end if

         IF(VECTOR_POINTER_ALLOCATED) THEN
           VECTOR_NUMBER = INT(ABS(GET_VECTOR))
          call write_trace_int2(file_trace_3,  &
               "VECTOR_NUMBER )(", VECTOR_NUMBER)
           GET_VAR = 0.

            
          IF(VECTOR_NUMBER > 0 .AND. &
                           VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
             if(file_trace_3/= BAD_TRACE_HANDLE) then
               write(file_trace_3, *) "IF entered with vector number ",  &
                VECTOR_NUMBER
             end if
             VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
!  I left this out of << and it didn't appear to have been called
! because no exception from GET_VAR
           get_var_called_from="up_objt:656"
           
             if(file_trace_3/= BAD_TRACE_HANDLE) then
               if(vector_number==0) then
                vector_number=vector_number ! Debugstop
               end if
               write(file_trace_3, *) "1. VECTOR_NUMBER became ", &
                 VECTOR_NUMBER
             end if
             


                 call write_trace_message(file_trace_3, &
                    "Second if. Setting GET_VAR...")   

            IF(VECTOR_NUMBER /= 0) GET_VAR = &
                 VECTOR_VALUES(VECTOR_NUMBER,MIN(YR,AVAIL_DATA_YEARS))
          ELSE
            !
             get_var_called_from=" "
             
             WRITE(4,*) 'Pointer ',GET_VECTOR,' does not exist'// &
                        ' for '//TRIM(UNITNM)
            call write_trace_message(file_trace_3, "No pointer for ")
            call write_trace_real(file_trace_3, "GET_VECTOR", get_vector)
          ENDIF
         call write_trace_real(file_trace_3, "2. GET_VAR ", real(GET_VAR))          

        ELSE
           er_message='SIID343 - Unit parameter file not active'
           call end_program(er_message)
        ENDIF
         

         if(file_trace_3/= BAD_TRACE_HANDLE) then
             write(file_trace_3, *) "Exiting routine. ", &
              "GET_VECTOR", GET_VECTOR, &
              "YR", YR, "UNITNM ", " ", UNITNM, " ", &
              "MAX_VALID_VECTOR", MAX_VALID_VECTOR, &
              "VECTOR_NUMBER", VECTOR_NUMBER, &
          "VECTOR_POINTR(VECTOR_NUMBER)", VECTOR_POINTR(VECTOR_NUMBER), &
              "VECTOR_VALUES(...)", VECTOR_VALUES(VECTOR_NUMBER, &
                min(yr,avail_data_years)), &
              "AVAIL_DATA_YEARS", AVAIL_DATA_YEARS, &
              "GET_VAR RESULT:", GET_VAR
              
             write(file_trace_3,*) "Call count ", get_var_call_count
          end if
         

      RETURN
! ***********************************************************************
      ENTRY GET_MONTHLY_VAR_SUM(R_VECTOR_NUM)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            GET_MONTHLY_VAR_SUM = 0.
            VECTOR_NUMBER = ABS(R_VECTOR_NUM)
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) THEN
                  DO MO = 1, 12
                     GET_MONTHLY_VAR_SUM = GET_MONTHLY_VAR_SUM + &
                                         VECTOR_VALUES(VECTOR_NUMBER,MO)
                  ENDDO
               ENDIF
            ELSE
               WRITE(4,*) 'Contract energy pattern ',GET_VECTOR, &
                                                     ' does not exist.'
            ENDIF
         ELSE
            er_message='SIID344 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
      RETURN
! ***********************************************************************
      ENTRY GET_MONTHLY_VAR_VALUES(R_VECTOR_NUM,R_MONTHLY_VECTOR)
! ***********************************************************************
!
         IF(VECTOR_POINTER_ALLOCATED) THEN
            VECTOR_NUMBER = ABS(R_VECTOR_NUM)
            GET_MONTHLY_VAR_VALUES = 0.
            IF(VECTOR_NUMBER > 0 .AND. &
                                 VECTOR_NUMBER <= MAX_VALID_VECTOR) THEN
               VECTOR_NUMBER = VECTOR_POINTR(VECTOR_NUMBER)
               IF(VECTOR_NUMBER /= 0) THEN
                  DO MO = 1, 12
                     R_MONTHLY_VECTOR(MO) = &
                                         VECTOR_VALUES(VECTOR_NUMBER,MO)
                  ENDDO
                  GET_MONTHLY_VAR_VALUES = GET_MONTHLY_VAR_VALUES/12.
               ENDIF
            ELSE
               WRITE(4,*) 'Monthly data vector ',VECTOR_NUMBER, &
                                                     ' does not exist.'
            ENDIF
         ELSE
            er_message='SIID345 - Unit parameter file not active'
            call end_program(er_message)
         ENDIF
      RETURN
! ***********************************************************************
      END function VARIABLE_UNITS_OBJECT

