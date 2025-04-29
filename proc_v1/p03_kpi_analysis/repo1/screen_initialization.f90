module screen_initialization
USE realwin
use params
use cl_data

implicit none
contains

subroutine init_screen()
CHARACTER (LEN=256):: WINDOW_NAME
CHARACTER (LEN=25) :: BUILD_NUM
      INTEGER  win_handle, Arial_10_Bold
      TYPE (menu_t), DIMENSION (2), TARGET :: menu_bar
      INTEGER, PARAMETER :: idm_msg_file_exit=1, &
                            idm_mg_about = 20
      LOGICAL (KIND=1) :: SET_SP_CAPEX_ACTIVE_WO_DISPLAY, &
                          VOID_LOGICAL, &
                          SET_SP_CAPEX_ACTIVE_W_DISPLAY


      IF(INDEX(ClData%COMMAND_LINE,'/SPCAPEX') == 0 .OR. &
                   INDEX(cldata%COMMAND_LINE,'/SPCAPEXDISP') /= 0) THEN
         Arial_10_Bold = &
                      select_font(typeface='ARIAL',point=10,bold=.true.)
         IF(INDEX(cldata%COMMAND_LINE,'/SPCAPEX') /= 0) THEN
            WINDOW_NAME = "GED CapEx powered by MIDAS Gold"
            VOID_LOGICAL = SET_SP_CAPEX_ACTIVE_W_DISPLAY()
         ELSEIF(INDEX(cldata%COMMAND_LINE,'/ACTMONTHY') /= 0) THEN
            WINDOW_NAME = &
                "Hitachi Corporate Finance Powered by Monthly MIDAS Gold"
         ELSE
            WINDOW_NAME = &
                      "Hitachi Strategic Planning Powered by MIDAS Gold"
         ENDIF
		 
         CALL set_menu_item(menu_item=menu_bar(1),text="E&xit", &
                 help="Terminate the program",command=idm_msg_file_exit)
         CALL set_menu_item(menu_item=menu_bar(2), &
                         text="&About", &
                         help="General information about this program.", &
                         command=idm_mg_about)
        WINDOW_NAME = trim(WINDOW_NAME)//' '// &
            trim(BUILD_NUM())// &
            "                                     "// &
            "Copyright (c) 2025 Hitachi Energy"// &
            "                                     "// &
            "Runtime log"
         win_handle = create_main_window( &
              window_name = WINDOW_NAME, &
              menu=menu_bar, &
              paint_code=SCROLL_VERT_TEXT,x=.2,y=.2,width=.75, &
              height=.50,text_font=Arial_10_Bold, & !  , status_bar=status_bar)
              icon=load_icon(resource="MIDASicon"))
         Call STATUS_BAR_FUNCTIONS(win_handle)
         Call StoreWINDOWS_Info(win_handle)
      ELSE
         VOID_LOGICAL = SET_SP_CAPEX_ACTIVE_WO_DISPLAY()
      endif
end subroutine init_screen

end module screen_initialization
