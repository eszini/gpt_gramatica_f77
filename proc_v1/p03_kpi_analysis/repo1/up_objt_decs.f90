module up_objt_decs
implicit none
interface
    function GET_CL_MW_FROM_POINTR(GV, YR)
    real :: GV
    integer (kind=2) :: YR
    real :: GET_CL_MW_FROM_POINTR
    end function GET_CL_MW_FROM_POINTR
end interface
end module up_objt_decs