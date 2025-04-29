      SUBROUTINE CREATE_REPORT_HEADER(SCENAME)
      use dreptcom
      implicit none
         
C CALCULATED ONCE, BEFORE THE END POINT OR YEAR LOOP

      INTEGER*2 LENGHT
      CHARACTER*5 VERSION,SCENAME
      CHARACTER*28 COMPANY_NAME,TITLE*40
C
C DETAILED REPORTS HEADER INFORMATION
C
      BANNER_LINE ='MIDAS Gold Analyst                        '//
     +      '           Detailed Report                   '//
     +      '                            Version '//trim(VERSION())
      UTIL_STUDY_LINE = 'Company: '//COMPANY_NAME()
      UTILITY_NAME = UTIL_STUDY_LINE
      LENGHT = LEN(trim(TITLE()))
      LENGHT = 61-LENGHT/2
      UTIL_STUDY_LINE(LENGHT:) = TITLE()
      STUDY = 'Study: '//SCENAME
      RETURN
C
      ENTRY DETAILED_REPORT_HEADER
C
         WRITE (9,1001) BANNER_LINE,trim(UTIL_STUDY_LINE),
     +                  FORECAST_YEAR,RUN_DATE,
     +                  trim(STUDY)//' '//RUN_END_POINT,
     +                  RUN_TIME
      RETURN
 1001 FORMAT('1',T5,A//T5,A/T5,A,T124,A/T5,A,T127,A/)
      END

