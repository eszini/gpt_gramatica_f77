module esrn_decs
implicit none

    INTEGER (kind=2) :: MAXIMUM_VECTOR_NUM
    ! 101308. Extended due to EV018      
    INTEGER (kind=4) :: MAX_VECTORS,MAXIMUM_VECTORS,I4,I4_REF ! 093008.
    INTEGER (kind=4), allocatable :: I4_VECTOR_MAP(:),I4_MONTHLY_VECTOR_MAP(:,:),&
        I4_REFERENCE_VECTOR_MAP(:)
	LOGICAL (kind=1), allocatable :: VECTOR_TYPE_IS_VALUE(:)
	logical (kind=1) :: INTO_EXTENSION_PERIOD=.false.
	REAL, allocatable :: ACTIVE_VECTOR_VALUES(:)

contains
      function get_escalated_value(R_VALUE,R_VECTOR_NO)
! THIS ASSUMES THAT R_VALUE HAS BEEN ESCALATED TO THE PREVIOUS YEAR
! THE NEW VALUE IS RETURNED
real :: get_escalated_value !, i
integer (kind=2) :: r_vector_no
real (kind=4) ::  r_value
integer (kind=4) :: i
         get_escalated_value = R_VALUE 
         IF(R_VECTOR_NO < -1 .OR. ABS(R_VECTOR_NO) > &
            MAXIMUM_VECTOR_NUM) THEN

            I = 0
         ELSE
            I = I4_VECTOR_MAP(ABS(R_VECTOR_NO))
         ENDIF
         
         IF(VECTOR_TYPE_IS_VALUE(I)) THEN
            IF(INTO_EXTENSION_PERIOD) THEN
               get_escalated_value = R_VALUE * ACTIVE_VECTOR_VALUES(I)
            ELSE
               get_escalated_value = ACTIVE_VECTOR_VALUES(I)
            ENDIF
         ELSE
            get_escalated_value = R_VALUE * ACTIVE_VECTOR_VALUES(I)
         ENDIF
      RETURN
      end function get_escalated_value
end module esrn_decs