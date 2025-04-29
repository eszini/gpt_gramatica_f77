!     ******************************************************************
!     FN_OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 1/29/2007 3:53:38 PM
!     Author : MARK S GERBER
!     Last change: MSG 5/13/2010 2:28:54 PM
!     ******************************************************************

C***********************************************************************
      SUBROUTINE FN_OBJECT
      use end_routine, only: end_program, er_message
C***********************************************************************
C
       use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      INTEGER*2 IREC,INUNIT,DELETE,LRECL/337/,UNIT_NO/12/
      INTEGER IOS,IOS_BASE,ACCTNO
      CHARACTER*1 TAKE_BONUS_TAX_DEPRECIATION
      CHARACTER*5 OVERLAY_FAMILY_NAME,NWCAP_FIN_FIL
      CHARACTER*30 DESC,COMMENT
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL*4 FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
C DECLARATION FOR /FUTURE ASSETS FILE/
      REAL SALVAGE_VALUE,TXNORM,ADRLIFE,REGULATORY_ALLOCATOR
      CHARACTER*4 DEPMET
      REAL DBRATE,CWIPRB,BOKLF,TAXLF,
     +     OHRATE,PLANT(15),CASH(15)
      INTEGER*2 AFDCSW,AFDCPERIODS
C     ADDED MIDAS GOLD 7/24/91
      INTEGER*2 ASSET_CLASS_ID,ALLOCATION_VECTOR,CASH_VECT
C DECLARATION FOR LOCALS
      CHARACTER*28 FILE_TYPE/'Expansion-Financial Accounts'/
      CHARACTER*2 NEWPLT_FIN_OL/'BC'/
      LOGICAL*1 FINANCIAL_OPTIONS_OPEN/.FALSE./
      INTEGER*2 R_UNIT_NO
      REAL ACCELERATE_FIXED_CHARGE,
     +     ACCELERATE_CHARGE_PERCENT,
     +     DELAY_FIXED_CHARGE,
     +     DELAY_CHARGE_PERCENT,
     +     EXTRAORDINARY_EXPEN_ALLOCATION,
     +     WRITE_OFF_YEARS,
     +     ABANDONMENT_FIXED_CHARGE,
     +     ABANDONMENT_CHARGE_PERCENT
      INTEGER*2 WRITE_OFF_ABANDONMENT METHOD
C
C OPTIONS CHECKING
C
      INTEGER*2 I,J,RECORDS_IN_BASE_FILE/0/,R_RECORDS_IN_BASE_FILE
      LOGICAL*1 LAHEY_LF95
      CHARACTER*50 SCREEN_OUTPUT
C
C***********************************************************************
      ENTRY FN_MAKEBIN
C***********************************************************************
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                            "FNB"//trim(NWCAP_FIN_FIL())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//NWCAP_FIN_FIL()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,NWCAP_FIN_FIL(),ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCFNWCP.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         PLANT = 0.
         CASH = 0.
!
         IREC = 0
         READ(10,*) DELETE
         DO
            SALVAGE_VALUE = 0
            ACCELERATE_FIXED_CHARGE = 0.
            ACCELERATE_CHARGE_PERCENT = 0.
            DELAY_FIXED_CHARGE = 0.
            DELAY_CHARGE_PERCENT = 0.
            WRITE_OFF_ABANDONMENT METHOD = 0
            EXTRAORDINARY_EXPEN_ALLOCATION = 0.
            ABANDONMENT_FIXED_CHARGE = 0.
            ABANDONMENT_CHARGE_PERCENT = 0.
            WRITE_OFF_YEARS = 999.
            TAKE_BONUS_TAX_DEPRECIATION = 'N'
C
C THE FOLLOWING 2 LINES WHERE MOVED TO PROVIDE CORRECT RIPPLE
C IN WINEDIT 3/13/94
C
            BOKLF = -99999.
            TXNORM = -99999.
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               PLANT = -999999.
               CASH = -999999.
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,TAXLF,
     +            ADRLIFE,DEPMET,DBRATE,CWIPRB,AFDCSW,
     +            AFDCPERIODS,CASH_VECT,OHRATE,REGULATORY_ALLOCATOR,
     +            PLANT,CASH,DESC,COMMENT,BOKLF,
     +            TXNORM,SALVAGE_VALUE,
     +            ACCELERATE_FIXED_CHARGE,
     +            ACCELERATE_CHARGE_PERCENT,
     +            DELAY_FIXED_CHARGE,
     +            DELAY_CHARGE_PERCENT,
     +            WRITE_OFF_ABANDONMENT METHOD,
     +            EXTRAORDINARY_EXPEN_ALLOCATION,
     +            WRITE_OFF_YEARS,
     +            ABANDONMENT_FIXED_CHARGE,
     +            ABANDONMENT_CHARGE_PERCENT,
     +            TAKE_BONUS_TAX_DEPRECIATION   ! 56
C
C RIPPLE    DOWN PLANT AND CASH VALUES
C
               IF(PLANT(1) == -999999.) PLANT(1) = 0.
               DO I = 2, 15
                  IF(PLANT(I) == -999999.) PLANT(I) = PLANT(I-1)
               ENDDO
               IF(CASH(1) == -999999.) CASH(1) = 0.
               DO I = 2, 15
                  IF(CASH(I) == -999999.) CASH(I) = CASH(I-1)
               ENDDO
C
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,ACCTNO,TAXLF,ADRLIFE,DEPMET,
     +            DBRATE,CWIPRB,AFDCSW,AFDCPERIODS,CASH_VECT,OHRATE,
     +            REGULATORY_ALLOCATOR,PLANT,CASH,BOKLF,DESC,
     +            TXNORM,SALVAGE_VALUE,
     +            ACCELERATE_FIXED_CHARGE,
     +            ACCELERATE_CHARGE_PERCENT,
     +            DELAY_FIXED_CHARGE,
     +            DELAY_CHARGE_PERCENT,
     +            WRITE_OFF_ABANDONMENT METHOD,
     +            EXTRAORDINARY_EXPEN_ALLOCATION,
     +            WRITE_OFF_YEARS,
     +            ABANDONMENT_FIXED_CHARGE,
     +            ABANDONMENT_CHARGE_PERCENT,
     +            TAKE_BONUS_TAX_DEPRECIATION
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
C        ENDFILE(11)
         CLOSE(11)
      ELSE IF(INDEX(NWCAP_FIN_FIL(),'NONE') == 0) THEN
         ! 'Expansion Asset'
         
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "FN_OBJT:1111") 
      ENDIF
      RECORDS_IN_BASE_FILE = IREC
      RETURN
C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C OVERLAY THE NEW-PLANT FINANCIAL DATA
C***********************************************************************
      ENTRY FN_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"FNO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(NEWPLT_FIN_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCFNWCP.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLFNWCP.BIN"
      OPEN(12,FILE=FILE_NAME,
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,ACCTNO,TAXLF,
     +         ADRLIFE,DEPMET,DBRATE,CWIPRB,AFDCSW,
     +         AFDCPERIODS,CASH_VECT,OHRATE,REGULATORY_ALLOCATOR,
     +         PLANT,CASH,BOKLF,DESC,
     +         TXNORM,SALVAGE_VALUE,
     +         ACCELERATE_FIXED_CHARGE,
     +         ACCELERATE_CHARGE_PERCENT,
     +         DELAY_FIXED_CHARGE,
     +         DELAY_CHARGE_PERCENT,
     +         WRITE_OFF_ABANDONMENT METHOD,
     +         EXTRAORDINARY_EXPEN_ALLOCATION,
     +         WRITE_OFF_YEARS,
     +         ABANDONMENT_FIXED_CHARGE,
     +         ABANDONMENT_CHARGE_PERCENT,
     +         TAKE_BONUS_TAX_DEPRECIATION
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,TAXLF,
     +            ADRLIFE,DEPMET,DBRATE,CWIPRB,AFDCSW,
     +            AFDCPERIODS,CASH_VECT,OHRATE,REGULATORY_ALLOCATOR,
     +            PLANT,CASH,DESC,COMMENT,BOKLF,
     +            TXNORM,SALVAGE_VALUE,
     +            ACCELERATE_FIXED_CHARGE,
     +            ACCELERATE_CHARGE_PERCENT,
     +            DELAY_FIXED_CHARGE,
     +            DELAY_CHARGE_PERCENT,
     +            WRITE_OFF_ABANDONMENT METHOD,
     +            EXTRAORDINARY_EXPEN_ALLOCATION,
     +            WRITE_OFF_YEARS,
     +            ABANDONMENT_FIXED_CHARGE,
     +            ABANDONMENT_CHARGE_PERCENT,
     +            TAKE_BONUS_TAX_DEPRECIATION
            ENDIF
            WRITE(12,REC=IREC) DELETE,ACCTNO,TAXLF,
     +         ADRLIFE,DEPMET,DBRATE,CWIPRB,AFDCSW,
     +         AFDCPERIODS,CASH_VECT,OHRATE,REGULATORY_ALLOCATOR,
     +         PLANT,CASH,BOKLF,DESC,
     +         TXNORM,SALVAGE_VALUE,
     +         ACCELERATE_FIXED_CHARGE,
     +         ACCELERATE_CHARGE_PERCENT,
     +         DELAY_FIXED_CHARGE,
     +         DELAY_CHARGE_PERCENT,
     +         WRITE_OFF_ABANDONMENT METHOD,
     +         EXTRAORDINARY_EXPEN_ALLOCATION,
     +         WRITE_OFF_YEARS,
     +         ABANDONMENT_FIXED_CHARGE,
     +         ABANDONMENT_CHARGE_PERCENT,
     +         TAKE_BONUS_TAX_DEPRECIATION
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(NEWPLT_FIN_OL == 'BC') CLOSE(11)
      NEWPLT_FIN_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from FN_OBJT SIID130'
      call end_program(er_message)
C***********************************************************************
      ENTRY RESET_NEWPLT_FIN_OL
C***********************************************************************
         NEWPLT_FIN_OL = 'BC'
      RETURN
C***********************************************************************
      ENTRY OPEN_FINANCIAL_OPTIONS(R_UNIT_NO,R_RECORDS_IN_BASE_FILE)
C***********************************************************************
         UNIT_NO = R_UNIT_NO
         R_RECORDS_IN_BASE_FILE = RECORDS_IN_BASE_FILE
         FILE_NAME = trim(OUTPUT_DIRECTORY())//NEWPLT_FIN_OL//
     +                                                       "FNWCP.BIN"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS .AND. .NOT. FINANCIAL_OPTIONS_OPEN) THEN
            FINANCIAL_OPTIONS_OPEN = .TRUE.
            OPEN(UNIT_NO,FILE=FILE_NAME,ACCESS="DIRECT",RECL=LRECL)
         ELSE
            R_UNIT_NO = -1
         ENDIF
      RETURN
C***********************************************************************
      ENTRY CLOSE_FINANCIAL_OPTIONS
C***********************************************************************
         IF(FINANCIAL_OPTIONS_OPEN) CLOSE(UNIT_NO)
         FINANCIAL_OPTIONS_OPEN = .FALSE.
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
C***********************************************************************
      SUBROUTINE OPTIONS_FIN_CHECK(R_UNITS_2_CHECK,R_UNIT_NAME,
     +                       R_DATA_POINTER,R_LEAD_TIME,R_RESOURCE_TYPE)
      use end_routine, only: end_program, er_message
C***********************************************************************
C
      INTEGER*2 UNIT_NO,I,J,R_UNITS_2_CHECK,
     +          R_DATA_POINTER(*),R_LEAD_TIME(*),
     +          G_RECORDS_IN_BASE_FILE,IREC
      INTEGER*2 AFDCSW,AFDCPERIODS,CASH_VECT
      REAL TAXLF,DBRATE,CWIPRB,OHRATE,PLANT(15)
      REAL ADRLIFE,REGULATORY_ALLOCATOR
      CHARACTER*4 DEPMET
      INTEGER*2 DELETE
      INTEGER IOS,ACCTNO
      CHARACTER*(*) R_UNIT_NAME(*),R_RESOURCE_TYPE(*)
C
         UNIT_NO = 10
         CALL OPEN_FINANCIAL_OPTIONS(UNIT_NO,G_RECORDS_IN_BASE_FILE)
         DO I = 1, R_UNITS_2_CHECK 
            IF(R_RESOURCE_TYPE(I) /= 'CL' .AND. 
     +                                 R_RESOURCE_TYPE(I) /= 'EL') CYCLE
            IREC = R_DATA_POINTER(I) 
            IF(IREC <= 0) CYCLE
            IF(IREC > G_RECORDS_IN_BASE_FILE) THEN
               WRITE(4,*) '*** line 288 FN_OBJT.FOR ***'
               WRITE(4,*) "In the Capacity Options File, a reference"
               WRITE(4,*) "was made to Expansion Plant Financial"
               WRITE(4,*) "record ",IREC," which is greater"
               WRITE(4,*) "than the number of records in the"
               WRITE(4,*) "Expansion Plant Financial file."
               er_message='See WARNING MESSAGES -FN_OBJT.FOR-1'
               call end_program(er_message)
            ENDIF
            READ(UNIT_NO,REC=IREC,IOSTAT=IOS) DELETE,ACCTNO,TAXLF,
     +         ADRLIFE,DEPMET,DBRATE,CWIPRB,AFDCSW,
     +         AFDCPERIODS,CASH_VECT,OHRATE,REGULATORY_ALLOCATOR,
     +         PLANT
C    +         ,CASH,BOKLF,DESC,
C    +         TXNORM,SALVAGE_VALUE,
C    +         ACCELERATE_FIXED_CHARGE,
C    +         ACCELERATE_CHARGE_PERCENT,
C    +         DELAY_FIXED_CHARGE,
C    +         DELAY_CHARGE_PERCENT,
C    +         WRITE_OFF_ABANDONMENT METHOD,
C    +         EXTRAORDINARY_EXPEN_ALLOCATION,
C    +         WRITE_OFF_YEARS,
C    +         ABANDONMENT_FIXED_CHARGE,
C    +         ABANDONMENT_CHARGE_PERCENT,
C    +         TAKE_BONUS_TAX_DEPRECIATION
            DO J = 1, 15
               IF(PLANT(J) == 0.) CYCLE
               IF(R_LEAD_TIME(I) /= J-1) THEN
                  WRITE(4,*) 'For '//trim(R_UNIT_NAME(I))//
     +            ', an expansion option, the lead-time and '//
     +            'plant entering service information are inconsistent.'
               ENDIF
               EXIT
            ENDDO
         ENDDO
         CALL CLOSE_FINANCIAL_OPTIONS
      RETURN
      END

