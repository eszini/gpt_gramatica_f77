module msgoutpt_data
    real (kind=4), allocatable :: TRANS_ROR_CAPACITY_MO(:)
    real (kind=8), allocatable :: HOURLY_HYDRO_MSG(:,:,:)
    integer (kind=2) :: CURRENT_OUTPUT_RECORD=0
    integer (kind=2), allocatable :: VARIABLE_LIST(:)
    integer (kind=2) :: VARIABLES_USED=0
    INTEGER (kind=4) :: CURRENT_OUTPUT_RECORD_ms
    real, allocatable :: VARIABLE_VALUE(:)

    type t_msgoutpt_elements
        integer (kind=2) :: PLAN_NUMBER
        integer (kind=2) :: AN_DECOMP_VARIABLES_USED
        real, allocatable ::  OUTPUT_VARIABLE(:)
        logical (kind=1) :: short_form,short_form_active=.false.
        integer (kind=2) :: BIFP_VARIABLES_USED
        real :: SHFRM_VARIABLES_USED, &
                R_MTHLY_CAP_SALES_TO_LEVEL_RM(0:12), &
                R_MTHLY_CAP_PURCH_TO_LEVEL_RM(0:12)
        character (len=1) :: ADD_2_INCOME_STATEMENT=" "
        integer (kind=2) :: RM_ASSET_CLASS_ID=0
        REAL ::  CAPACITY_SALES_TO_LEVEL_RM, &
            CAPACITY_PURCHASES_TO_LEVEL_RM

   end type t_msgoutpt_elements
   
   type(t_msgoutpt_elements), save :: msgoutpt_elements
end module msgoutpt_data

