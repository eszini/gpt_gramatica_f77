module dsex_obj_interfaces
interface start_up
    FUNCTION START_UP(R_DEBUG_ON,R_BASE_FILE_DEFINITION, &
                       POINTS_IN_LOAD_CURVE,R_BASE_FILE_FAMILY)
     
     logical (kind=1) :: start_up, r_debug_on
     character (len=*) :: R_BASE_FILE_DEFINITION, R_BASE_FILE_FAMILY
     integer (kind=2) :: POINTS_IN_LOAD_CURVE
     end function start_up
     
end interface start_up
interface sp_capex_active
    function sp_capex_active()
    logical (kind=1) :: sp_capex_active
    end function sp_capex_active
end interface sp_capex_active
end module dsex_obj_interfaces