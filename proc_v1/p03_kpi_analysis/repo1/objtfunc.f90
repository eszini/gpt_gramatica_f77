!***********************************************************************
!
!   THE OBJECT FUNCTION IS CALCULATED IN THIS ROUTINE
!
!***********************************************************************
!
      RECURSIVE SUBROUTINE RECEIVE_POSTFIX_INFO(R_STACK_POINTR, &
                                                QUE_TYPE,QUE_INFO, &
                                                QUE_VECTOR,QUE_POINTR)
!
! PARAMETERS
!
      use spindriftlib
      use prod_arrays_dimensions
      use mod_base_year
      use msgoutpt
      SAVE
      INTEGER (KIND=2) :: VARIABLE_NUMBER,R_YEAR,R_YR
      REAL :: RETURN_VALUE
!     REAL RETURN_THE_VALUE_FOR
!
      INTEGER (KIND=2) :: ADD,MULTIPLY,SUBTRACT,DIVIDE,POWER
      PARAMETER(ADD=1,MULTIPLY=4,SUBTRACT=2,DIVIDE=3,POWER=5)
!
! VARIABLE OF THE CALCULATION OF THE OBJECTIVE FUNCTION
!

      REAL :: CAL_STACK(0:5)
      REAL :: VV_rpi ! variable_value argument
      real :: VAR_POINTR
      REAL :: FUNCTION_VALUE,R_OBJ_FUNC_VALUE
      INTEGER (KIND=1) :: POINTR
!
! VARIABLES FOR THE POSTFIX CALCULATIONS
!
      INTEGER (KIND=2) :: QUE_STACK_POINTR,R_STACK_POINTR
      CHARACTER (LEN=1) :: QUE_TYPE(*)
      REAL :: QUE_INFO(*)
      LOGICAL (KIND=1) :: QUE_VECTOR(*)
      INTEGER (KIND=2) :: QUE_POINTR(*)
      CHARACTER (LEN=1) :: OPERATION_TYPE(:)
      REAL :: OPERATION_INFO(:)
      LOGICAL (KIND=1) :: OPERATE_ON_VECTOR(:)
      ALLOCATABLE :: OPERATION_TYPE,OPERATION_INFO,OPERATE_ON_VECTOR
!
! VARIABLES OF THE DESCRIPTION OF THE OBJECTIVE FUNCTION VARIABLES
!
      INTEGER (KIND=2) :: L,I,R_NUM_OF_VARIABLES,NUM_OF_VARIABLES=0
      CHARACTER (LEN=70) :: R_PARTIAL_OBJFUNC_DES(*),R_GROUP_STRING(*), &
                   R_VARIABLE_STRING(*)
      CHARACTER (LEN=1) :: R_DISCOUNT_START_PERIOD(*)
      INTEGER (KIND=2) :: R_VARIABLE_NUM(*),R_FUNCTION_OPERATOR(*), &
                R_START_YEAR(*),R_END_YEAR(*), &
                R_GROUP_NUM(*)
      REAL :: R_DISCOUNT_RATE(*)
!
      INTEGER (KIND=2) :: MAX,MIN,AVERAGE,SUM,PRESENT_VALUE,LEVEL_VALUE
      PARAMETER(MAX=0,MIN=1,AVERAGE=2,SUM=3, &
                PRESENT_VALUE=4,LEVEL_VALUE=5)
      CHARACTER (LEN=70) :: PARTIAL_OBJFUNC_DES(:),GROUP_STRING(:), &
                   VARIABLE_STRING(:)
      CHARACTER (LEN=80) :: OBJECTIVE_FUNCTION_DESCRIPTION=' ', &
                   R_OBJECTIVE_FUNC_DESCRIPTION
      INTEGER (KIND=2) :: VARIABLE_NUM(:),FUNCTION_OPERATOR(:), &
                START_YEAR(:),END_YEAR(:), &
                GROUP_NUM(:),VAR_NUM,OPERATION_VARIABEL_POINTR(:)
      INTEGER (KIND=2) :: BASE_YR,BASE_YEAR
      REAL :: DISCOUNT_RATE
      REAL :: SUM_FOR(:), &
           NUM_PERIODS(:), &
           ACCUMLATOR_FOR(:),VAR_VALUE, &
           DISCOUNT_FOR_YEAR(:,:), &
           SUM_OF_YEARLY_DISCOUNT(:,:)
      ALLOCATABLE:: PARTIAL_OBJFUNC_DES,GROUP_STRING, &
                    VARIABLE_STRING, &
                    VARIABLE_NUM,FUNCTION_OPERATOR, &
                    START_YEAR,END_YEAR, &
                    GROUP_NUM, &
                    SUM_FOR, &
                    NUM_PERIODS, &
                    ACCUMLATOR_FOR, &
                    OPERATION_VARIABEL_POINTR, &
                    DISCOUNT_FOR_YEAR, &
                    SUM_OF_YEARLY_DISCOUNT
!
!      SAVE OPERATION_TYPE,OPERATION_INFO,QUE_STACK_POINTR,
!     +     OPERATE_ON_VECTOR
!      SAVE PARTIAL_OBJFUNC_DES,GROUP_STRING,
!     +     VARIABLE_STRING,
!     +     VARIABLE_NUM,FUNCTION_OPERATOR,
!     +     START_YEAR,END_YEAR,
!     +     GROUP_NUM,
!     +     SUM_FOR,
!     +     NUM_PERIODS,
!     +     ACCUMLATOR_FOR,
!     +     OPERATION_VARIABEL_POINTR,
!     +     DISCOUNT_FOR_YEAR,
!     +     SUM_OF_YEARLY_DISCOUNT,
!     +     BASE_YR
!
         QUE_STACK_POINTR = R_STACK_POINTR
         ALLOCATE(OPERATION_TYPE(QUE_STACK_POINTR), &
                  OPERATION_INFO(QUE_STACK_POINTR), &
                  OPERATE_ON_VECTOR(QUE_STACK_POINTR), &
                  OPERATION_VARIABEL_POINTR(QUE_STACK_POINTR))
         DO I = 1, QUE_STACK_POINTR
            OPERATION_TYPE(I) = QUE_TYPE(I)
            OPERATION_INFO(I) = QUE_INFO(I)
            OPERATE_ON_VECTOR(I) = QUE_VECTOR(I)
            OPERATION_VARIABEL_POINTR(I) = QUE_POINTR(I)
         ENDDO
      RETURN
!
!***********************************************************************
      ENTRY STORE_OBJECT_FUNCTION_VARIABLES(R_NUM_OF_VARIABLES, &
                                       R_OBJECTIVE_FUNC_DESCRIPTION, &
                                       R_PARTIAL_OBJFUNC_DES, &
                                       R_GROUP_STRING, &
                                       R_VARIABLE_STRING, &
                                       R_DISCOUNT_START_PERIOD, &
                                       R_VARIABLE_NUM, &
                                       R_FUNCTION_OPERATOR, &
                                       R_START_YEAR, &
                                       R_END_YEAR, &
                                       R_GROUP_NUM, &
                                       R_DISCOUNT_RATE)
!***********************************************************************
!
         OBJECTIVE_FUNCTION_DESCRIPTION = R_OBJECTIVE_FUNC_DESCRIPTION
!
         NUM_OF_VARIABLES = R_NUM_OF_VARIABLES
         ALLOCATE(PARTIAL_OBJFUNC_DES(NUM_OF_VARIABLES), &
                  GROUP_STRING(NUM_OF_VARIABLES), &
                  VARIABLE_STRING(NUM_OF_VARIABLES), &
                  VARIABLE_NUM(NUM_OF_VARIABLES), &
                  FUNCTION_OPERATOR(NUM_OF_VARIABLES), &
                  START_YEAR(NUM_OF_VARIABLES), &
                  END_YEAR(NUM_OF_VARIABLES), &
                  GROUP_NUM(NUM_OF_VARIABLES), &
                  SUM_FOR(NUM_OF_VARIABLES), &
                  NUM_PERIODS(NUM_OF_VARIABLES), &
                  ACCUMLATOR_FOR(NUM_OF_VARIABLES), &
                  DISCOUNT_FOR_YEAR(NUM_OF_VARIABLES,100), &
                  SUM_OF_YEARLY_DISCOUNT(NUM_OF_VARIABLES,100))
!
         DISCOUNT_FOR_YEAR = 1.
         SUM_OF_YEARLY_DISCOUNT = 0.
         BASE_YR = get_BASE_YEAR()
         DO L = 1, NUM_OF_VARIABLES
            PARTIAL_OBJFUNC_DES(L) = R_PARTIAL_OBJFUNC_DES(L)
            GROUP_STRING(L) = R_GROUP_STRING(L)
            VARIABLE_STRING(L) = R_VARIABLE_STRING(L)
            VARIABLE_NUM(L) = R_VARIABLE_NUM(L)
            FUNCTION_OPERATOR(L) = R_FUNCTION_OPERATOR(L)
            START_YEAR(L) = R_START_YEAR(L)
            END_YEAR(L) = R_END_YEAR(L)
            GROUP_NUM(L) = R_GROUP_NUM(L)
            IF(FUNCTION_OPERATOR(L) >= PRESENT_VALUE) THEN
               DISCOUNT_RATE = 1. + R_DISCOUNT_RATE(L)/100.
               IF(R_DISCOUNT_START_PERIOD(L) == 'B') THEN
                  DISCOUNT_FOR_YEAR(L,1) = 1.
                  SUM_OF_YEARLY_DISCOUNT(L,1) = 1.
               ELSEIF(R_DISCOUNT_START_PERIOD(L) == 'M') THEN
                  DISCOUNT_FOR_YEAR(L,1) = 1./SQRT(DISCOUNT_RATE)
                  SUM_OF_YEARLY_DISCOUNT(L,1) =1./DISCOUNT_FOR_YEAR(L,1)
               ELSE
                  DISCOUNT_FOR_YEAR(L,1) = 1./DISCOUNT_RATE
                  SUM_OF_YEARLY_DISCOUNT(L,1) = 1./DISCOUNT_RATE
               ENDIF
               DO I = 2, 100
                  DISCOUNT_FOR_YEAR(L,I) = DISCOUNT_FOR_YEAR(L,I-1) / &
                                               DISCOUNT_RATE
                  SUM_OF_YEARLY_DISCOUNT(L,I) = DISCOUNT_FOR_YEAR(L,I) + &
                                           SUM_OF_YEARLY_DISCOUNT(L,I-1)
               ENDDO
            ENDIF
         ENDDO
!
      RETURN
!***********************************************************************
      ENTRY CALCULATE_OBJECTIVE_FUNCTION(R_YEAR,R_OBJ_FUNC_VALUE)
!***********************************************************************
         POINTR = 0
         CAL_STACK(0) = 0.
         DO I = 1, QUE_STACK_POINTR
            IF(OPERATION_TYPE(I) == 'C') THEN
               POINTR = POINTR + 1
               CAL_STACK(POINTR) = OPERATION_INFO(I)
            ELSEIF(OPERATION_TYPE(I) == 'V') THEN
               POINTR = POINTR + 1
               VAR_NUM = VARIABLE_NUM(int(OPERATION_INFO(I),2))

               CALL GET_VARIABLE_VALUE(VAR_NUM,VAR_VALUE)
               IF(OPERATE_ON_VECTOR(I)) THEN
                  CAL_STACK(POINTR) = VAR_VALUE
               ELSE
                  IF(R_YEAR < START_YEAR(OPERATION_VARIABEL_POINTR(I)) &
                                .OR. &
                   R_YEAR > END_YEAR(OPERATION_VARIABEL_POINTR(I))) THEN
                     CAL_STACK(POINTR)=ACCUMLATOR_FOR(int(OPERATION_INFO(I),2))
                  ELSE
                     CALL FUNCTION_VALUE_CAL(OPERATION_INFO(I), &
                                          VAR_VALUE,R_YEAR-BASE_YR, &
                                          CAL_STACK(POINTR))
                  ENDIF
               ENDIF
            ELSEIF(OPERATION_TYPE(I) == 'F') THEN
               IF(R_YEAR < START_YEAR(OPERATION_VARIABEL_POINTR(I)) .OR. &
                   R_YEAR > END_YEAR(OPERATION_VARIABEL_POINTR(I))) THEN
                     CAL_STACK(POINTR)=ACCUMLATOR_FOR(int(OPERATION_INFO(I),2))
               ELSE
                  VAR_VALUE = CAL_STACK(POINTR)
                  CALL FUNCTION_VALUE_CAL(OPERATION_INFO(I), &
                                          VAR_VALUE,R_YEAR-BASE_YR, &
                                          CAL_STACK(POINTR))
               ENDIF
            ELSEIF(OPERATION_TYPE(I) == 'O') THEN
               IF(INT(OPERATION_INFO(I),2) == ADD) THEN
                  CAL_STACK(POINTR-1) = CAL_STACK(POINTR-1) + &
                                        CAL_STACK(POINTR)
               ELSEIF(INT(OPERATION_INFO(I),2) == SUBTRACT) THEN
                  CAL_STACK(POINTR-1) = CAL_STACK(POINTR-1) - &
                                        CAL_STACK(POINTR)
               ELSEIF(INT(OPERATION_INFO(I),2) == DIVIDE) THEN
                  CAL_STACK(POINTR-1) = CAL_STACK(POINTR-1)/ &
                                        CAL_STACK(POINTR)
               ELSEIF(INT(OPERATION_INFO(I),2) == MULTIPLY) THEN
                  CAL_STACK(POINTR-1) = CAL_STACK(POINTR-1) * &
                                        CAL_STACK(POINTR)
               ELSEIF(INT(OPERATION_INFO(I),2) == POWER) THEN
                  CAL_STACK(POINTR-1) = CAL_STACK(POINTR-1) ** &
                                        CAL_STACK(POINTR)
               ENDIF
               POINTR = POINTR - 1
            ENDIF
         ENDDO
         R_OBJ_FUNC_VALUE = CAL_STACK(POINTR)
      RETURN
!
!***********************************************************************
      ENTRY FUNCTION_VALUE_CAL(VAR_POINTR,VV_rpi,R_YR, &
                               FUNCTION_VALUE)
!***********************************************************************
!

!        VV_rpi = RETURN_THE_VALUE_FOR(VARIABLE_NUM(VAR_POINTR))
!
         IF(FUNCTION_OPERATOR(int(VAR_POINTR,2)) == MAX) THEN
            ACCUMLATOR_FOR(int(VAR_POINTR,2))=AMAX1(ACCUMLATOR_FOR(int(VAR_POINTR,2)), &
                                             VV_rpi)
            FUNCTION_VALUE = ACCUMLATOR_FOR(int(VAR_POINTR,2))
         ELSEIF(FUNCTION_OPERATOR(int(VAR_POINTR,2)) == MIN) THEN
            ACCUMLATOR_FOR(int(VAR_POINTR,2))=AMIN1(ACCUMLATOR_FOR(int(VAR_POINTR,2)), &
                                             VV_rpi)
            FUNCTION_VALUE = ACCUMLATOR_FOR(int(VAR_POINTR,2))
         ELSEIF(FUNCTION_OPERATOR(int(VAR_POINTR,2)) == AVERAGE) THEN
            SUM_FOR(int(VAR_POINTR,2)) = SUM_FOR(int(VAR_POINTR,2)) + VV_rpi
            NUM_PERIODS(int(VAR_POINTR,2)) = NUM_PERIODS(int(VAR_POINTR,2)) + 1.
            ACCUMLATOR_FOR(int(VAR_POINTR,2)) = SUM_FOR(int(VAR_POINTR,2))/ &
                                        NUM_PERIODS(int(VAR_POINTR,2))
            FUNCTION_VALUE = ACCUMLATOR_FOR(int(VAR_POINTR,2))
         ELSEIF(FUNCTION_OPERATOR(int(VAR_POINTR,2)) == SUM) THEN
            ACCUMLATOR_FOR(int(VAR_POINTR,2)) = ACCUMLATOR_FOR(int(VAR_POINTR,2)) + &
                                                          VV_rpi
            FUNCTION_VALUE = ACCUMLATOR_FOR(int(VAR_POINTR,2))
         ELSE
            ACCUMLATOR_FOR(int(VAR_POINTR,2)) = ACCUMLATOR_FOR(int(VAR_POINTR,2)) + &
                       VV_rpi*DISCOUNT_FOR_YEAR(int(VAR_POINTR,2),R_YR)
            IF(FUNCTION_OPERATOR(int(VAR_POINTR,2)) == PRESENT_VALUE) THEN
               FUNCTION_VALUE = ACCUMLATOR_FOR(int(VAR_POINTR,2))
            ELSEIF(FUNCTION_OPERATOR(int(VAR_POINTR,2)) == LEVEL_VALUE) THEN
               FUNCTION_VALUE = ACCUMLATOR_FOR(int(VAR_POINTR,2))/ &
                                 SUM_OF_YEARLY_DISCOUNT(int(VAR_POINTR,2),R_YR)
            ENDIF
         ENDIF
      RETURN

      ENTRY SET_OBJECT_FUNC_ACCUMULATORS

         DO L = 1, NUM_OF_VARIABLES
            IF(FUNCTION_OPERATOR(L) >= PRESENT_VALUE) THEN
               ACCUMLATOR_FOR(L) = 0.
               SUM_FOR(L) = 0.
               NUM_PERIODS(L) = 0.
            ELSE
               SUM_FOR(L) = 0.
               NUM_PERIODS(L) = 0.
               IF(FUNCTION_OPERATOR(L) == MAX) THEN
                  ACCUMLATOR_FOR(L) = -9999999.
               ELSEIF(FUNCTION_OPERATOR(L) == MIN) THEN
                  ACCUMLATOR_FOR(L) = 9999999.
               ELSE
                  ACCUMLATOR_FOR(L) = 0.
               ENDIF
            ENDIF
         ENDDO
      RETURN
      END

