module objtfunc_decs
implicit none
real, allocatable :: ACCUMLATOR_FOR(:)
integer (kind=2) :: PRESENT_VALUE

INTEGER(kind=2), parameter :: cMAX=0,cMIN=1,cAVERAGE=2,cSUM=3, &
    cPRESENT_VALUE=4,cLEVEL_VALUE=5

    integer (kind=2), allocatable :: FUNCTION_OPERATOR(:)
    real, allocatable :: SUM_FOR(:)
    real, allocatable :: NUM_PERIODS(:)
    real, allocatable :: DISCOUNT_FOR_YEAR(:,:), &
        SUM_OF_YEARLY_DISCOUNT(:,:)
    real :: VAR_VALUE
    integer (kind=2) :: LEVEL_VALUE
    integer (kind=2), allocatable :: VARIABLE_NUM(:)

contains

     
   subroutine FUNCTION_VALUE_CAL(VAR_POINTR,VARIABLE_VALUE,R_YR, &
    FUNCTION_VALUE)
    real :: function_value, var_pointr
    integer (kind=2) :: r_yr
    real (kind=4) :: variable_value

     IF(FUNCTION_OPERATOR(VAR_POINTR) == cMAX) THEN
        ACCUMLATOR_FOR(VAR_POINTR)=AMAX1(ACCUMLATOR_FOR(VAR_POINTR), &
                                         VARIABLE_VALUE)
        FUNCTION_VALUE = ACCUMLATOR_FOR(VAR_POINTR)
     ELSEIF(FUNCTION_OPERATOR(VAR_POINTR) == cMIN) THEN
        ACCUMLATOR_FOR(VAR_POINTR)=AMIN1(ACCUMLATOR_FOR(VAR_POINTR), &
                                         VARIABLE_VALUE)
        FUNCTION_VALUE = ACCUMLATOR_FOR(VAR_POINTR)
     ELSEIF(FUNCTION_OPERATOR(VAR_POINTR) == cAVERAGE) THEN
        SUM_FOR(VAR_POINTR) = SUM_FOR(VAR_POINTR) + VARIABLE_VALUE
        NUM_PERIODS(VAR_POINTR) = NUM_PERIODS(VAR_POINTR) + 1.
        ACCUMLATOR_FOR(VAR_POINTR) = SUM_FOR(VAR_POINTR)/ &
                                    NUM_PERIODS(VAR_POINTR)
        FUNCTION_VALUE = ACCUMLATOR_FOR(VAR_POINTR)
     ELSEIF(FUNCTION_OPERATOR(VAR_POINTR) == cSUM) THEN
        ACCUMLATOR_FOR(VAR_POINTR) = ACCUMLATOR_FOR(VAR_POINTR) + &
                                                      VARIABLE_VALUE
        FUNCTION_VALUE = ACCUMLATOR_FOR(VAR_POINTR)
     ELSE
        ACCUMLATOR_FOR(VAR_POINTR) = ACCUMLATOR_FOR(VAR_POINTR) + &
                   VARIABLE_VALUE*DISCOUNT_FOR_YEAR(VAR_POINTR,R_YR)
        IF(FUNCTION_OPERATOR(VAR_POINTR) == PRESENT_VALUE) THEN
           FUNCTION_VALUE = ACCUMLATOR_FOR(VAR_POINTR)
        ELSEIF(FUNCTION_OPERATOR(VAR_POINTR) == LEVEL_VALUE) THEN
           FUNCTION_VALUE = ACCUMLATOR_FOR(VAR_POINTR)/ &
                             SUM_OF_YEARLY_DISCOUNT(VAR_POINTR,R_YR)
            ENDIF
         ENDIF
    end subroutine FUNCTION_VALUE_CAL
end module objtfunc_decs
