MODULE conversion_routines
         INTERFACE CONVERT_2_STR
            MODULE PROCEDURE INT2_CONVER_2_STR
            MODULE PROCEDURE STR_CONVER_2_STR
            MODULE PROCEDURE INT4_CONVER_2_STR
            MODULE PROCEDURE INT8_CONVER_2_STR
            MODULE PROCEDURE REAL_CONVER_2_STR
            MODULE PROCEDURE REAL8_CONVER_2_STR
            MODULE PROCEDURE INT2_CONVER_2_STR_W_LEN
            MODULE PROCEDURE REAL_CONVER_2_STR_W_DEC
         END INTERFACE
      CONTAINS
!--------
         FUNCTION INT2_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=2) :: VALUE_IN
            WRITE(TEMP_STR,'(I6)') VALUE_IN
            VALUE_OUT = ADJUSTL(TEMP_STR)
         END FUNCTION
!--------
         FUNCTION INT2_CONVER_2_STR_W_LEN(VALUE_IN,STR_LEN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=2) :: VALUE_IN
         INTEGER (KIND=4) :: STR_LEN
            WRITE(TEMP_STR,'(I8.8)') VALUE_IN
            IF(STR_LEN <= 8) THEN
               VALUE_OUT = TEMP_STR(8-STR_LEN+1:8)
            ELSE
               VALUE_OUT = ADJUSTL(TEMP_STR)
            ENDIF
         END FUNCTION
!--------
         FUNCTION STR_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=256) :: VALUE_OUT
         CHARACTER (LEN=*) :: VALUE_IN
            IF(INDEX(TRIM(VALUE_IN),' ') == 0) THEN
                VALUE_OUT = VALUE_IN
            ELSE
                VALUE_OUT = '"'//TRIM(VALUE_IN)//'"'
            ENDIF
         END FUNCTION
!--------
         FUNCTION INT4_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=4) :: VALUE_IN
            WRITE(TEMP_STR,'(I11)') VALUE_IN
            VALUE_OUT = ADJUSTL(TEMP_STR)
         END FUNCTION
!--------
         FUNCTION INT8_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=30) :: VALUE_OUT,TEMP_STR
         INTEGER (KIND=8) :: VALUE_IN
            WRITE(TEMP_STR,'(I22)') VALUE_IN
            VALUE_OUT = ADJUSTL(TEMP_STR)
         END FUNCTION
!--------
         FUNCTION REAL_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR,TEMP_STR2
         REAL (KIND=4) :: VALUE_IN
         INTEGER (KIND=2) :: STR_LEN,I
            IF(ABS(VALUE_IN) < .00001) THEN
               WRITE(TEMP_STR,'(F19.11)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .0001) THEN
               WRITE(TEMP_STR,'(F19.10)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .001) THEN
               WRITE(TEMP_STR,'(F19.9)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .01) THEN
               WRITE(TEMP_STR,'(F19.8)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .1) THEN
               WRITE(TEMP_STR,'(F19.7)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 1.) THEN
               WRITE(TEMP_STR,'(F19.6)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 10.) THEN
               WRITE(TEMP_STR,'(F19.5)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 100.) THEN
               WRITE(TEMP_STR,'(F19.4)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 1000.) THEN
               WRITE(TEMP_STR,'(F19.3)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 10000.) THEN
               WRITE(TEMP_STR,'(F19.2)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 100000.) THEN
               WRITE(TEMP_STR,'(F19.1)') VALUE_IN
            ELSE
               WRITE(TEMP_STR,'(F19.0)') VALUE_IN
            ENDIF
            TEMP_STR2 = ADJUSTL(TEMP_STR)
            STR_LEN = LEN_TRIM(TEMP_STR2)
            DO I = STR_LEN,1,-1
               IF(TEMP_STR2(I:I) /= '0') EXIT
            ENDDO
            IF(TEMP_STR2(I:I) == '.') THEN
               VALUE_OUT = TEMP_STR2(1:I-1)
            ELSE
               VALUE_OUT = TEMP_STR2(1:I)
            ENDIF
         END FUNCTION
!--------
       FUNCTION REAL_CONVER_2_STR_W_DEC(VALUE_IN,DEC) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR,TEMP_STR2
         REAL (KIND=4) :: VALUE_IN
         INTEGER (KIND=2) :: STR_LEN,I
         INTEGER (KIND=4) :: DEC
            IF(DEC == 11) THEN
               WRITE(TEMP_STR,'(F19.11)') VALUE_IN
            ELSEIF(DEC == 10) THEN
               WRITE(TEMP_STR,'(F19.10)') VALUE_IN
            ELSEIF(DEC == 9) THEN
               WRITE(TEMP_STR,'(F19.9)') VALUE_IN
            ELSEIF(DEC == 8) THEN
               WRITE(TEMP_STR,'(F19.8)') VALUE_IN
            ELSEIF(DEC == 7) THEN
               WRITE(TEMP_STR,'(F19.7)') VALUE_IN
            ELSEIF(DEC == 6) THEN
               WRITE(TEMP_STR,'(F19.6)') VALUE_IN
            ELSEIF(DEC == 5) THEN
               WRITE(TEMP_STR,'(F19.5)') VALUE_IN
            ELSEIF(DEC == 4) THEN
               WRITE(TEMP_STR,'(F19.4)') VALUE_IN
            ELSEIF(DEC == 3) THEN
               WRITE(TEMP_STR,'(F19.3)') VALUE_IN
            ELSEIF(DEC == 2) THEN
               WRITE(TEMP_STR,'(F19.2)') VALUE_IN
            ELSEIF(DEC == 1) THEN
               WRITE(TEMP_STR,'(F19.1)') VALUE_IN
            ELSE
               WRITE(TEMP_STR,'(F19.0)') VALUE_IN
            ENDIF
            TEMP_STR2 = ADJUSTL(TEMP_STR)
            STR_LEN = LEN_TRIM(TEMP_STR2)
            DO I = STR_LEN,1,-1
               IF(TEMP_STR2(I:I) /= '0') EXIT
            ENDDO
            IF(TEMP_STR2(I:I) == '.') THEN
               VALUE_OUT = TEMP_STR2(1:I-1)
            ELSE
               VALUE_OUT = TEMP_STR2(1:I)
            ENDIF
         END FUNCTION
!--------
         FUNCTION REAL8_CONVER_2_STR(VALUE_IN) RESULT(VALUE_OUT)
         CHARACTER (LEN=20) :: VALUE_OUT,TEMP_STR,TEMP_STR2
         REAL (KIND=8) :: VALUE_IN
         INTEGER (KIND=2) :: STR_LEN,I
            IF(ABS(VALUE_IN) < .00001) THEN
               WRITE(TEMP_STR,'(F19.11)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .0001) THEN
               WRITE(TEMP_STR,'(F19.10)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .001) THEN
               WRITE(TEMP_STR,'(F19.9)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .01) THEN
               WRITE(TEMP_STR,'(F19.8)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < .1) THEN
               WRITE(TEMP_STR,'(F19.7)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 1.) THEN
               WRITE(TEMP_STR,'(F19.6)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 10.) THEN
               WRITE(TEMP_STR,'(F19.5)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 100.) THEN
               WRITE(TEMP_STR,'(F19.4)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 1000.) THEN
               WRITE(TEMP_STR,'(F19.3)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 10000.) THEN
               WRITE(TEMP_STR,'(F19.2)') VALUE_IN
            ELSEIF(ABS(VALUE_IN) < 100000.) THEN
               WRITE(TEMP_STR,'(F19.1)') VALUE_IN
            ELSE
               WRITE(TEMP_STR,'(F19.0)') VALUE_IN
            ENDIF
            TEMP_STR2 = ADJUSTL(TEMP_STR)
            STR_LEN = LEN_TRIM(TEMP_STR2)
            DO I = STR_LEN,1,-1
               IF(TEMP_STR2(I:I) /= '0') EXIT
            ENDDO
            IF(TEMP_STR2(I:I) == '.') THEN
               VALUE_OUT = TEMP_STR2(1:I-1)
            ELSE
               VALUE_OUT = TEMP_STR2(1:I)
            ENDIF
         END FUNCTION
      END MODULE conversion_routines