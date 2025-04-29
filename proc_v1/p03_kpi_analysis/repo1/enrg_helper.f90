module enrg_helper
use prod_arrays_dimensions
use forecast_decs
use paramscom
implicit none 
    INTEGER (kind=2), parameter:: el_capacity =  1, &
                     fixed_om =     2, &
                     Generation =   3, &
                     variable_om =  6, &
                     sulfur_o2 =    7, &
                     el_eqivalent_capacity=16

    integer (kind=2) :: asset_class
    
    real, allocatable :: EL_MON_MDS_VAR_COST(:,:,:)
    real, allocatable :: EL_MON_MDS_FIXED_COST(:,:,:)
    real, allocatable :: EL_MON_MDS_MARKET_PURCHASE(:,:)
    real, allocatable :: EL_MON_MDS_PURCHASES(:,:,:)
    real, allocatable :: EL_MON_MDS_REVENUE(:,:,:)
    real, allocatable :: EL_ANN_CLASS_MARKET_PURCHASE(:)
    real, allocatable :: EL_ANN_CLASS_MARKET_REVENUES(:)
    real, allocatable :: EL_MON_MDS_MARKET_REVENUES(:,:)
    real, allocatable :: EL_ANN_CLASS_REVENUE(:,:)
    real, allocatable :: INTRA_COMPANY_SALES_REVENUE(:)
    real, allocatable :: EL_ANN_CLASS_FIXED_COST(:,:)
    real, allocatable :: EL_ANN_CLASS_VAR_COST(:,:)
    character(len=30), allocatable :: class_name(:)
    logical (kind=1) :: class_exists(max_load_classes)=.false.
    real, allocatable :: el_ann_class_energy(:,:)
    real, allocatable :: EL_ANN_CLASS_CAPACITY(:,:)
    INTEGER (kind=2), allocatable :: ASSET_CLASS_POINTER(:)
    real, allocatable :: EL_ANN_CLASS_SELL_ENERGY(:,:)
    real, allocatable :: EL_ANN_CLASS_SELL_CAPACITY(:,:)
    REAL (kind=4), allocatable :: MONTHLY_EL_GROUP_REPORT(:,:,:)
    real, allocatable :: FISCAL_EL_ENERGY(:,:)
    real, allocatable :: FISCAL_EL_CAPACITY(:,:)
    real, allocatable :: FISCAL_EL_VAR_COST(:,:), &
        FISCAL_EL_FIXED_COST(:,:), FISCAL_EL_SO2_EMIS(:,:), &
        FISCAL_EL_MARKET_REVENUE(:,:)
    logical (kind=1) :: MONTHLY_EL_REPORT_ACTIVE
    logical (kind=1) :: ENRG_LIMIT_REPORT_ACTIVE=.false.
    logical (kind=1) :: mhydro_files_are_active
    logical (kind=1) :: YES_HYDRO_SALES_AS_NEGATIVE=.false.
    logical (kind=1) :: HYDRO_SALES_AS_NEGATIVE
    LOGICAL (kind=1) ::  HYDRO_FILES_ARE_ACTIVE_SAVED=.false.
    LOGICAL (kind=1) ::  mCLASS_IN_AREA(MAX_LOAD_CLASSES)
    real, allocatable :: INTRA_COMPANY_PURCHASE_EXPENSE(:)
    real, allocatable :: EL_ANN_CLASS_PURCHASES(:,:)
    real (kind=4) :: CAWCD_ENERGY=0.
    real, allocatable :: EL_ANN_CLASS_SO2(:)    
    real, allocatable :: EL_MON_MDS_ENERGY(:,:,:)
    real (kind=4) :: R_MONTHLY_GROUP_REPORT(0:12,0:99,17)
        
    
contains
      subroutine set_class_in_area(idx, val)
        logical (kind=1) :: val
        integer :: idx
        
        mCLASS_IN_AREA(idx)=val
        
      end subroutine set_class_in_area
      
      subroutine GET_CLASS_IN_AREA(R_CLASS_IN_AREA,R_AREA)
        integer (kind=2) :: classIdx
        integer (kind=2), intent(in) :: r_area
        logical (kind=1), intent(inout) :: &
            R_CLASS_IN_AREA(:)

         DO classidx = 1, MAX_LOAD_CLASSES 
            R_CLASS_IN_AREA(classIdx) = &
                WHICH_CLASSES_IN_AREA(R_AREA,classIdx)
         ENDDO
      end subroutine GET_CLASS_IN_AREA
      
      function HYDRO_FILES_ARE_ACTIVE()
        character (len=1) :: HYDRO_FILES_ARE_ACTIVE
        
         IF(mhydro_files_are_active) THEN
            HYDRO_FILES_ARE_ACTIVE =  'T'
         ELSE
            HYDRO_FILES_ARE_ACTIVE =  'F'
         ENDIF
      end function HYDRO_FILES_ARE_ACTIVE
      subroutine set_enrg_limit_report(val)
        logical (kind=1) :: val
        ENRG_LIMIT_REPORT_ACTIVE=val
      
      end subroutine set_enrg_limit_report
      function get_ENRG_LIMIT_REPORT()
         logical (kind=1) :: get_ENRG_LIMIT_REPORT, result_val
         result_val=ENRG_LIMIT_REPORT_ACTIVE .AND. &
              HYDRO_FILES_ARE_ACTIVE() == 'T'
                                       
         get_ENRG_LIMIT_REPORT = result_val
      end function get_ENRG_LIMIT_REPORT
      
      subroutine UPDATE_MONTHLY_EL_RPT_ACTIVE
         MONTHLY_EL_REPORT_ACTIVE = get_ENRG_LIMIT_REPORT()
         YES_HYDRO_SALES_AS_NEGATIVE = HYDRO_SALES_AS_NEGATIVE
      end subroutine UPDATE_MONTHLY_EL_RPT_ACTIVE
      
      subroutine get_class_name(r_area_name, idx_class)

         character(len=*) :: r_area_name(:)
         integer :: lenght
         integer (kind=2) :: idx_class
         
         lenght = min(len(r_area_name(1)),len(class_name(1)))
         do idx_class = 1, max_load_classes
            if(class_exists(idx_class)) then
               r_area_name(idx_class) = class_name(idx_class)(1:lenght)
            else
               r_area_name(idx_class) = ' '
            endif
         enddo
       end subroutine get_class_name
       
        subroutine get_class_exists(r_class_exists)
        logical(kind=1), dimension(max_load_classes) :: r_class_exists
        integer :: i
        
        do i = 1, max_load_classes
            r_class_exists(i) = class_exists(i)
        enddo
     end subroutine get_class_exists
     
      subroutine return_el_asset_class_prod(r_class,r_class_exists, &
         r_el_ann_class_capacity, &
          r_el_ann_class_energy, max_hydro_class_num,  &
          asset_class_pointer, el_ann_class_capacity)
          

         real, intent(in) :: el_ann_class_capacity(:,:)
         integer (kind=2), intent(in) ::  asset_class_pointer(:)
         integer(kind=2), intent(in) :: r_class
         integer(kind=2) :: max_hydro_class_num         
         integer(kind=2) :: asset_class ! John added "missing" 
         ! definition. See legacy codebase for answers.
         real, intent(out) :: r_el_ann_class_energy
         real :: r_el_ann_class_capacity
         logical(kind=1), intent(inout) :: r_class_exists
         
         r_class_exists = .false.
         r_el_ann_class_capacity = 0.
         r_el_ann_class_energy = 0.
         if(r_class <= max_hydro_class_num) then
            if(r_class ==0) then
               asset_class = 0
            else
               asset_class = asset_class_pointer(r_class)
            endif
            if(asset_class > 0 .or. r_class == 0) then
               r_class_exists = .true.
               r_el_ann_class_capacity = &
                           el_ann_class_capacity(asset_class,1) + &
                           el_ann_class_capacity(asset_class,2) + &
                           el_ann_class_capacity(asset_class,3)
!
               r_el_ann_class_energy = &
                           el_ann_class_energy(asset_class,1) + &
                           el_ann_class_energy(asset_class,2) + &
                           el_ann_class_energy(asset_class,3)
            endif
         endif
      end subroutine return_el_asset_class_prod
      ! ******************************************************************
      subroutine return_el_asset_class_sell(r_class,r_class_exists, &
        r_el_ann_class_capacity, &
       r_el_ann_class_energy, max_hydro_class_num, &
       asset_class_pointer, el_ann_class_sell_capacity, &
       el_ann_class_sell_energy)
                                       
         real, intent(in) :: el_ann_class_sell_energy(:,:)
         real, intent(out) :: r_el_ann_class_energy
         real, intent(out) :: r_el_ann_class_capacity
         logical(kind=1), intent(out) :: r_class_exists
         integer (kind=2), intent(in) :: r_class
         integer (kind=2), intent(in) :: max_hydro_class_num
         integer (kind=2) :: asset_class
         integer (kind=2), intent(in) ::  asset_class_pointer(:)
         real, intent(in) :: el_ann_class_sell_capacity(:,:)
         
         r_class_exists = .false.
         r_el_ann_class_capacity = 0.
         r_el_ann_class_energy = 0.
         if(r_class <= max_hydro_class_num) then
            if(r_class ==0) then
               asset_class = 0
            else
               asset_class = asset_class_pointer(r_class)
            endif
            if(asset_class > 0 .or. r_class == 0) then
               r_class_exists = .true.
               r_el_ann_class_capacity = &
                          el_ann_class_sell_capacity(asset_class,1) + &
                          el_ann_class_sell_capacity(asset_class,2) + &
                           el_ann_class_sell_capacity(asset_class,3)
!
               r_el_ann_class_energy = &
                           el_ann_class_sell_energy(asset_class,1) + &
                           el_ann_class_sell_energy(asset_class,2) + &
                           el_ann_class_sell_energy(asset_class,3)
            endif
         endif
      end subroutine return_el_asset_class_sell
end module enrg_helper