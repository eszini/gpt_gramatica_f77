!
!
!	Change log
!
!	2024_04_18
!
!	changed module name size 
!	from  CAPACITY_OPTIONS_FIXED_VARIABLES
!	to    CAPACITY_OPTIONS_FIXED_VARS
!
!	removed/changed continuation lines
!
!	Files using MODULE:
!	CL_OBJT.FOR
!	grx_planning_routines.f90
!	capacity_options_alloc_vars.f90
!
!
      MODULE CAPACITY_OPTIONS_FIXED_VARS
!
! VARIABLES NEEDED ACROSS ROUTINES
!
      INTEGER (KIND=2) :: TOTAL_ALL_OPTIONS=0 
      INTEGER (KIND=2) :: BASE_OPTIONS=0 
      INTEGER (KIND=2) :: CYCLE_OPTIONS=0 
      INTEGER (KIND=2) :: PEAK_OPTIONS=0 
      INTEGER (KIND=2) :: PEAK_REDUC_OPTIONS=0
      INTEGER (KIND=2) :: FILL_OPTIONS=0
      INTEGER (KIND=2) :: HARD_WIRED_OPTIONS=0
      INTEGER (KIND=2) :: DSM_ADDITIONS=0
      INTEGER (KIND=2) :: DSM_OPTIONS=0
      INTEGER (KIND=2) :: CONTRACT_ADDITIONS=0
      INTEGER (KIND=2) :: CONTRACT_OPTIONS=0
      INTEGER (KIND=2) :: RENEWABLE_OPTIONS=0
      INTEGER (KIND=2) :: EFFICIENCY_OPTIONS=0
      INTEGER (KIND=2) :: CL_OPTIONS=0
      INTEGER (KIND=2) :: TOTAL_ACTIVE_OPTIONS=0
      
      ! John almost removed this declaration because it wasn't being
      ! used. Examination of code comments indicate that likely,
      ! developers thought they were using this declaration and forgot
      ! to remove the declaration in CN_OBJT(). 
      INTEGER (KIND=2) :: MAX_RESOURCE_ID=0
      INTEGER (KIND=2) :: TOTAL_POSSIBLE_OPTIONS=0
!
      INTEGER (KIND=2) :: BC_BASE_OPTIONS=0
      INTEGER(KIND=2)  :: BC_CYCLE_OPTIONS=0
      INTEGER(KIND=2)  :: BC_PEAK_OPTIONS=0
      INTEGER(KIND=2)  :: BC_PEAK_REDUC_OPTIONS=0
      INTEGER(KIND=2)  :: BC_FILL_OPTIONS=0
      INTEGER(KIND=2)  :: BC_HARD_WIRED_OPTIONS=0
      INTEGER(KIND=2)  :: BC_TOTAL_ACTIVE_OPTIONS=0
      INTEGER(KIND=2)  :: BC_TOTAL_ALL_OPTIONS=0
      INTEGER(KIND=2)  :: BC_MAX_RESOURCE_ID=0
      INTEGER(KIND=2)  :: BC_TOTAL_POSSIBLE_OPTIONS=0
!
      INTEGER (KIND=2) :: MAX_ID_FOR_ALL_OPTIONS=0
      INTEGER (KIND=2) :: ON_LINE_MONTH=1
      LOGICAL (KIND=1) :: ALL_OPTIONS_GT_ZERO=.FALSE.
      LOGICAL (KIND=1) :: OPTIONS_GT_ZERO=.FALSE.
!
      END MODULE CAPACITY_OPTIONS_FIXED_VARS
!--------
