!      Last change: msg 11/11/2018 3:23:10 PM
      program MSGOLD95RW  ! SUBROUTINE RW_Main(cmdln)
      USE realwin
      use cl_data
!       USE realwin_types
      use ovls_data ! For printing out lists of overlay files registered
                    ! in the system. Ovls_data is in findovls.
      use debugtrace
      use energy_trace
      use offline_trace
	  IMPLICIT NONE
      INTEGER  win_handle, Arial_10_Bold
      CHARACTER (LEN=256):: WINDOW_NAME
      CHARACTER (LEN=25) :: BUILD_NUM
      TYPE (menu_t), DIMENSION (2), TARGET :: menu_bar
      INTEGER, PARAMETER :: idm_msg_file_exit=1, &
                            idm_mg_about = 20
      LOGICAL (KIND=1) :: SET_SP_CAPEX_ACTIVE_WO_DISPLAY, &
                          VOID_LOGICAL, &
                          SET_SP_CAPEX_ACTIVE_W_DISPLAY
      integer :: idx
      character :: charval
      character (len=256) left, dir
      integer :: dirindex_start, dirindex_end
      integer :: iy
      integer :: chdir ! External

      
      
      character (len=256) :: retrieved_env
          CALL set_environment_variable(name="FLIB_DVT_BUFFER",value="0")

      CALL GETENV("DEBUGDIR",cldata%command_line)


      IF(LEN(TRIM(cldata%command_line)) <= 1) THEN
         cldata%command_line = " "
         CALL GETCL(cldata%command_line)
      else
      
          dirindex_end=index(cldata%command_line,",")
          if(dirindex_end>0) then
          dirindex_start=1

            dir=cldata%command_line(dirindex_start:dirindex_end-1)
            dir=trim(dir)
            cldata%command_line=cldata%command_line( &
                dirindex_end+1:len_trim(cldata%command_line))
             ! Remove quotes
             cldata%command_line=&
                cldata%command_line(2:len_trim(cldata%command_line)-1)
            
            iy=chdir(dir)

         else
            call getcwd(dir)
            iy=chdir(dir)

          end if
      end if
      
      call write_command_line()
      call open_energy_trace()
      call open_offline_trace()


      IF(INDEX(cldata%command_line,'/SPCAPEX') == 0 .OR. &
                           INDEX(cldata%command_line,'/SPCAPEXDISP') /= 0) THEN
         Arial_10_Bold = &
                      select_font(typeface='ARIAL',point=10,bold=.true.)
         if(index(cldata%command_line, "/TRACE")>1) then
            call set_debug_tracing_on(.true.)
            if(index(cldata%command_line, "/ALLTRACE")>1) then
                call set_alltrace_on()
            end if
         else
            call set_debug_tracing_on(.false.)
         end if

         
         IF(INDEX(cldata%command_line,'/SPCAPEX') /= 0) THEN
            WINDOW_NAME = "GED CapEx powered by MIDAS Gold®"
            VOID_LOGICAL = SET_SP_CAPEX_ACTIVE_W_DISPLAY()
         ELSEIF(INDEX(cldata%command_line,'/ACTMONTHY') /= 0) THEN
            WINDOW_NAME = &
                "Monthly MIDAS® Corporate Finance"
         ELSE
            WINDOW_NAME = &
                      "MIDAS® Strategic Planning"
         ENDIF
         CALL set_menu_item(menu_item=menu_bar(1),text="E&xit", &
                 help="Terminate the program",command=idm_msg_file_exit)
         CALL set_menu_item(menu_item=menu_bar(2), &
                         text="&About", &
                         help="General information about this program.", &
                         command=idm_mg_about)
        WINDOW_NAME = trim(WINDOW_NAME)//' Build: '// &
            trim(BUILD_NUM())// &
            "                                     "// &
            "®Hitachi Energy Corporation"// &
            "                                     "// &
            "Runtime log"
         win_handle = create_main_window( &
              window_name = WINDOW_NAME, &
              menu=menu_bar, &
              paint_code=SCROLL_VERT_TEXT,x=.2,y=.2,width=.50, &
              height=.50, text_font=Arial_10_Bold, & !  , status_bar=status_bar)
              icon=load_icon(resource="MIDASicon"))
         Call STATUS_BAR_FUNCTIONS(win_handle)
         Call StoreWINDOWS_Info(win_handle)
      ELSE
         VOID_LOGICAL = SET_SP_CAPEX_ACTIVE_WO_DISPLAY()
      endif
      
      call MIDAS_XP()
      
      end program MSGOLD95RW
      subroutine write_command_line()
      use cl_data
      implicit none
      integer :: the_unit, ios

      
      ! write command line
       the_unit=get_new_unit()
      ! if(does_file_exist("command_line.txt")) then
        ! call delete_file("command_line.txt")
      ! end if
      open(unit=the_unit, iostat=ios, file="command_line.txt", &
                     action="write")
      if(ios/=0) then
        stop "Unable to open command-line file."
      end if
      write(the_unit,*) cldata%command_line

      close(the_unit)
      end subroutine write_command_line
! ***********************************************************************
      FUNCTION RW_control(command, window, wparam, lparam)
      use end_routine, only: end_program, er_message
! ***********************************************************************
!
      USE realwin
      IMPLICIT NONE
      INTEGER trash
      LOGICAL :: logical_trash
      LOGICAL :: RW_control
      INTEGER, INTENT(IN) :: command, window, wparam, lparam
      INTEGER, PARAMETER :: idm_msg_file_exit = 1, &
                            idm_mg_about = 20
      LOGICAL(kind=1) :: MESSAGE,DEBUG_ON,NO_WARNING_WINDOW,WARNING_MESSAGES
      INTEGER(kind=2) :: INVERSE_VIDEO
      LOGICAL(kind=1) :: USER_TERMINATED
       character(len=25) :: build_num
      COMMON /ERRORS/ INVERSE_VIDEO,MESSAGE,DEBUG_ON,USER_TERMINATED
!
      select case (command)
         case (idm_msg_file_exit,idm_rw_file_exit)
            USER_TERMINATED = .TRUE.
            CALL ERROR_MESSAGE()
            er_message='Stop requested from msgold95RW SIID245'
            call end_program(er_message)
         case (idm_mg_about)
            trash = message_box (title="About", &
              text="MIDAS " // trim(BUILD_NUM()) // &
              " (c) 2025 Hitachi Energy")
         case default
! In the default case, recognize commands that RealWin knows how to process
! and diagnose commands that have no case defined.
            IF (realwin_knows_command (command)) THEN
          ! RealWin knows how to process this command
               RW_control = .FALSE.  ! Let RealWin handle it
               return
            ELSE
          ! This command apparently hasn't been implemented
               trash = message_box (title="default", &
                                    text="Command not implemented")
            END IF
      end select

      RW_control = .true. ! Let RealWin know we handled it already

      END function RW_Control
! ***********************************************************************
      SUBROUTINE STATUS_BAR_FUNCTIONS(R_Win_handle)
! ***********************************************************************
      USE realwin
      use end_routine
      IMPLICIT NONE
      TYPE(status_t), SAVE, DIMENSION(8) :: status_bar
      INTEGER :: R_Win_handle
      CHARACTER(len=*) :: STATUS_TEXT, &
                    R_TOTAL_NUM_OF_END_POINTS_STR, &
                    STUDY_PERIOD_STR, &
                    SIM_MONTH_DAY
      CHARACTER (LEN=256) :: MIDAS_EXE_NAME
      CHARACTER (LEN=3) :: ENDPOINT_STR
      CHARACTER (LEN=5) :: YEAR_STR
      CHARACTER (LEN=4), SAVE :: TOTAL_NUM_OF_END_POINTS_STR
      INTEGER (KIND=4), SAVE :: status_bar_font,Win_handle
      INTEGER (KIND=2) :: END_POINTS,CURRENT_YEAR
      CHARACTER(len=25) :: BUILD_NUM
      REAL, DIMENSION(7) :: &
	  Status_Portion=(/.07,.20,.47,.045,.05,.12,.07/)
      LOGICAL (KIND=4) :: VOID_LOGICAL
      LOGICAL (KIND=1) :: SP_CAPEX_DISPLAY_ACTIVE
      CHARACTER (LEN=3) :: EXTENSION_PERIOD_STR
!
         Win_handle = R_Win_handle
      RETURN
      ENTRY RW_INITIALIZE_STATUS_LINE(STATUS_TEXT, &
                                      R_TOTAL_NUM_OF_END_POINTS_STR, &
                                      STUDY_PERIOD_STR, &
                                      SIM_MONTH_DAY)
         if(trim(study_period_str)=="2023-   0") then
           call end_program("msgold95rw:0002 - study period string " // &
           "is incorrect.")
         end if
         
         TOTAL_NUM_OF_END_POINTS_STR = R_TOTAL_NUM_OF_END_POINTS_STR
         call get_exe_name(MIDAS_EXE_NAME)
         CALL set_status_item (status_bar(1), portion=Status_Portion(1), &
                                                code = STATUS_TIME2)

         CALL set_status_item (status_bar(2), portion=Status_Portion(2), &
                 code = STATUS_USER_TEXT, text = trim(MIDAS_EXE_NAME))
         CALL set_status_item (status_bar(3), portion=Status_Portion(3), &
                code = STATUS_USER_TEXT)
         CALL set_status_item (status_bar(4), portion=Status_Portion(4), &
                code = STATUS_USER_TEXT, &
                text = '     ')
         CALL set_status_item (status_bar(5), portion=Status_Portion(5), &
                 code = STATUS_USER_TEXT, text = '     ')
         if (trim(STUDY_PERIOD_STR)=="") then
            call end_program("msgold95rw:0001 - study period string " &
             // " is empty.")
         end if

         CALL set_status_item (status_bar(6), portion=Status_Portion(6), &
                 code = STATUS_USER_TEXT, &
                 text = TRIM(STATUS_TEXT)//' '//TRIM(STUDY_PERIOD_STR))
                 
                 
         ! Year zero appears in STATUS_USER_TEXT here.
         CALL set_status_item (status_bar(7), portion=Status_Portion(7), &
                 code = STATUS_USER_TEXT)

         status_bar_font = &
                       select_font(typeface='ARIAL',point=8,bold=.true.)
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_UPDATE_RUNTIME_MESSAGES(STATUS_TEXT)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         CALL set_status_item (status_bar(3), portion=Status_Portion(3), &
                 code = STATUS_USER_TEXT, text = trim(STATUS_TEXT))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_SET_ENDPOINTS_ON_STATUS(END_POINTS)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         WRITE(ENDPOINT_STR,'(I3)') END_POINTS
         CALL set_status_item (status_bar(4), portion=Status_Portion(4), &
                     code = STATUS_USER_TEXT, &
                     text = ENDPOINT_STR//'/'// &
                                    trim(TOTAL_NUM_OF_END_POINTS_STR))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_SET_YEAR_ON_STATUS(CURRENT_YEAR)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         WRITE(YEAR_STR,'(I4)') CURRENT_YEAR
         CALL set_status_item (status_bar(5), portion=Status_Portion(5), &
                     code = STATUS_USER_TEXT, &
                     text = YEAR_STR)
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_UPDATE_STATUS_TIME(STATUS_TEXT)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         CALL set_status_item (status_bar(7), portion=Status_Portion(7), &
                 code = STATUS_USER_TEXT, text = trim(STATUS_TEXT))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_PROGRAM_LOCATION(STATUS_TEXT)
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         CALL set_status_item (status_bar(2), portion=Status_Portion(2), &
                 code = STATUS_USER_TEXT, text = trim(STATUS_TEXT))
         Call set_status_bar(status_bar=status_bar,update_flag=.true., &
                                                   font=status_bar_font)
      RETURN
      ENTRY RW_PROCESS_MESSAGES()
         IF(.NOT. SP_CAPEX_DISPLAY_ACTIVE()) RETURN
         VOID_LOGICAL = PROCESS_MESSAGES()
      RETURN
      END SUBROUTINE
