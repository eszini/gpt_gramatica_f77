module rollscom
    implicit none
    ! rollcum
    real(kind=8) :: LOAD_ACCUM(0:366)=0
    real(kind=4) :: ADDEND=0
    real(kind=4) :: LENGPM_DA=0
    
    ! rollsum_l4
    logical(kind=4) :: DERATABLE=.false.

    ! rollsum_i2
    
    integer(kind=2) :: IDA=0
    integer(kind=2) :: JMO=0
    integer(kind=2) :: JDA=0
    integer(kind=2) :: LENGYR_DA=0
    integer(kind=2) :: LENGYR_MO=0
    integer(kind=2) :: LAST_ACTU=0
    integer(kind=2) :: CUMUL_CM_DA(0:24)=0
    
end module rollscom

