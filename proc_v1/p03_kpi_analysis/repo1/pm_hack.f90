
subroutine pmr(r_unit_str, return_value)
    use prim_mover_idx
    implicit none
    character(len=6) :: r_unit_str
    integer (kind=2) :: return_value
    return_value=primary_mover_index_db(r_unit_str)

end subroutine pmr
