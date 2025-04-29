module cla2_decs
implicit none

    type t_cla_planning_area
    ! TG_2_PLANNING_AREA  comes from cl_units_read routine in cla_objt
    INTEGER (KIND=2), ALLOCATABLE :: TG_2_PLANNING_AREA(:)
    end type t_cla_planning_area
    type(t_cla_planning_area), save :: ns_cla_planning_area
end module cla2_decs