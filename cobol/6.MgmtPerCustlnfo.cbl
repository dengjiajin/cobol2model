IDENTIFICATION DIVISION.
PROGRAM-ID. MGMT-PER-CUST-INFO.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  SQLCA.
    05  SQLCODE            PIC S9(9) COMP-4.

01  WS-RESP-CODE           PIC X(06).
01  WS-RESP-MSG            PIC X(50).
01  CHG-FLAG-CUST          PIC X.
01  CHG-FLAG-PER           PIC X.

LINKAGE SECTION.
01  REQ-CUST-NO            PIC X(10).
01  REQ-CUST-NM            PIC X(50).
01  REQ-CUST-ENG-NM        PIC X(50).
01  REQ-CUST-LVL-CD        PIC X(02).
01  REQ-MOBILE-NO          PIC X(15).
01  REQ-E-MAIL             PIC X(50).
01  REQ-CRTF-TYP-CD        PIC X(02).
01  REQ-CRTF-NO            PIC X(20).
01  REQ-CRTF-MATR-DT       PIC X(08).

01  REQ-ADDR               PIC X(100).
01  REQ-HOUSDRGST-ADDR     PIC X(100).
01  REQ-GENDER-CD          PIC X(01).
01  REQ-MARRG-SITUATION-CD PIC X(01).
01  REQ-BIRTH-DT           PIC X(08).
01  REQ-CAREER-TYP-CD      PIC X(02).
01  REQ-STATE-RGN-CD       PIC X(02).
01  REQ-DOM-OVERS-FLG-CD   PIC X(01).
01  REQ-IDCARD-TYP-CD      PIC X(02).
01  REQ-EMPLY-FLG          PIC X(01).
01  REQ-SHRHD-FLG          PIC X(01).
01  REQ-SPS-NAME           PIC X(50).
01  REQ-SPS-ENG-NAME       PIC X(50).
01  REQ-SPS-CRTF-TYP-CD    PIC X(02).
01  REQ-SPS-CRTF-NO        PIC X(20).
01  REQ-SPS-TEL-NO         PIC X(15).
01  REQ-WORKUNIT-NM        PIC X(50).
01  REQ-WORKUNIT-ADDR      PIC X(100).
01  REQ-ADMIN-CMPRMNT-CD   PIC X(02).

01  RESP-CODE              PIC X(06).
01  RESP-MSG               PIC X(50).

PROCEDURE DIVISION 
    USING REQ-CUST-NO, REQ-CUST-NM, REQ-CUST-ENG-NM,
          REQ-CUST-LVL-CD, REQ-MOBILE-NO, REQ-E-MAIL,
          REQ-CRTF-TYP-CD, REQ-CRTF-NO, REQ-CRTF-MATR-DT,
          REQ-ADDR, REQ-HOUSDRGST-ADDR, REQ-GENDER-CD,
          REQ-MARRG-SITUATION-CD, REQ-BIRTH-DT, REQ-CAREER-TYP-CD,
          REQ-STATE-RGN-CD, REQ-DOM-OVERS-FLG-CD, REQ-IDCARD-TYP-CD,
          REQ-EMPLY-FLG, REQ-SHRHD-FLG, REQ-SPS-NAME,
          REQ-SPS-ENG-NAME, REQ-SPS-CRTF-TYP-CD, REQ-SPS-CRTF-NO,
          REQ-SPS-TEL-NO, REQ-WORKUNIT-NM, REQ-WORKUNIT-ADDR,
          REQ-ADMIN-CMPRMNT-CD, RESP-CODE, RESP-MSG.

MAIN-LOGIC.
    MOVE 'N' TO CHG-FLAG-CUST
    MOVE 'N' TO CHG-FLAG-PER
    MOVE 'E99999' TO WS-RESP-CODE
    MOVE 'PROCESSING ERROR' TO WS-RESP-MSG

    IF REQ-CUST-NO = SPACES OR REQ-CUST-NO = LOW-VALUES
       MOVE 'F20003' TO WS-RESP-CODE
       MOVE 'Customer number is required' TO WS-RESP-MSG
       GO TO EXIT-PROGRAM
    END-IF.

    *> 客户类型校验
    EXEC SQL
      SELECT CUST_TYP_CD INTO :WS-RESP-CODE
        FROM CUSTOMER_BASIC_INFO
       WHERE CUST_NO = :REQ-CUST-NO
    END-EXEC.

    EVALUATE TRUE
       WHEN SQLCODE = 0
          IF WS-RESP-CODE NOT = '0'
             MOVE 'F20002' TO WS-RESP-CODE
             MOVE 'Customer type is not personal' TO WS-RESP-MSG
             GO TO EXIT-PROGRAM
          END-IF
       WHEN SQLCODE = 100
          MOVE 'F20000' TO WS-RESP-CODE
          MOVE 'Customer not found' TO WS-RESP-MSG
          GO TO EXIT-PROGRAM
       WHEN OTHER
          MOVE 'E12001' TO WS-RESP-CODE
          MOVE 'Database error' TO WS-RESP-MSG
          GO TO EXIT-PROGRAM
    END-EVALUATE.

    EXEC SQL START TRANSACTION END-EXEC.

    *> 更新客户基础信息
    IF REQ-CUST-NM NOT = SPACES OR REQ-CUST-ENG-NM NOT = SPACES OR
       REQ-CUST-LVL-CD NOT = SPACES OR REQ-MOBILE-NO NOT = SPACES OR
       REQ-E-MAIL NOT = SPACES OR REQ-CRTF-TYP-CD NOT = SPACES OR
       REQ-CRTF-NO NOT = SPACES OR REQ-CRTF-MATR-DT NOT = SPACES
       
       EXEC SQL
          UPDATE CUSTOMER_BASIC_INFO
             SET CUST_NM       = CASE WHEN :REQ-CUST-NM != SPACES 
                                      THEN :REQ-CUST-NM ELSE CUST_NM END,
                 CUST_ENG_NM   = CASE WHEN :REQ-CUST-ENG-NM != SPACES 
                                      THEN :REQ-CUST-ENG-NM ELSE CUST_ENG_NM END,
                 CUST_LVL_CD   = CASE WHEN :REQ-CUST-LVL-CD != SPACES 
                                      THEN :REQ-CUST-LVL-CD ELSE CUST_LVL_CD END,
                 MOBILE_NO     = CASE WHEN :REQ-MOBILE-NO != SPACES 
                                      THEN :REQ-MOBILE-NO ELSE MOBILE_NO END,
                 E_MAIL        = CASE WHEN :REQ-E-MAIL != SPACES 
                                      THEN :REQ-E-MAIL ELSE E_MAIL END,
                 CRTF_TYP_CD   = CASE WHEN :REQ-CRTF-TYP-CD != SPACES 
                                      THEN :REQ-CRTF-TYP-CD ELSE CRTF_TYP_CD END,
                 CRTF_NO       = CASE WHEN :REQ-CRTF-NO != SPACES 
                                      THEN :REQ-CRTF-NO ELSE CRTF_NO END,
                 CRTF_MATR_DT  = CASE WHEN :REQ-CRTF-MATR-DT != SPACES 
                                      THEN :REQ-CRTF-MATR-DT ELSE CRTF_MATR_DT END,
                 LAST_UPD_DT   = CURRENT_DATE,
                 LAST_UPD_TM   = CURRENT_TIME
           WHERE CUST_NO = :REQ-CUST-NO
       END-EXEC

       IF SQLCODE = 0
          MOVE 'Y' TO CHG-FLAG-CUST
       ELSE
          MOVE 'E12004' TO WS-RESP-CODE
          MOVE 'Failed to update basic info' TO WS-RESP-MSG
          EXEC SQL ROLLBACK END-EXEC
          GO TO EXIT-PROGRAM
       END-IF
    END-IF.

    *> 更新个人信息
    IF REQ-ADDR NOT = SPACES OR REQ-HOUSDRGST-ADDR NOT = SPACES OR
       REQ-GENDER-CD NOT = SPACES OR REQ-MARRG-SITUATION-CD NOT = SPACES OR
       REQ-BIRTH-DT NOT = SPACES OR REQ-CAREER-TYP-CD NOT = SPACES OR
       REQ-STATE-RGN-CD NOT = SPACES OR REQ-DOM-OVERS-FLG-CD NOT = SPACES OR
       REQ-IDCARD-TYP-CD NOT = SPACES OR REQ-EMPLY-FLG NOT = SPACES OR
       REQ-SHRHD-FLG NOT = SPACES OR REQ-SPS-NAME NOT = SPACES OR
       REQ-SPS-ENG-NAME NOT = SPACES OR REQ-SPS-CRTF-TYP-CD NOT = SPACES OR
       REQ-SPS-CRTF-NO NOT = SPACES OR REQ-SPS-TEL-NO NOT = SPACES OR
       REQ-WORKUNIT-NM NOT = SPACES OR REQ-WORKUNIT-ADDR NOT = SPACES OR
       REQ-ADMIN-CMPRMNT-CD NOT = SPACES
       
       EXEC SQL
          UPDATE PERSONAL_CUSTOMER_INFO
             SET ADDR               = CASE WHEN :REQ-ADDR != SPACES 
                                          THEN :REQ-ADDR ELSE ADDR END,
                 HOUSDRGST_ADDR     = CASE WHEN :REQ-HOUSDRGST-ADDR != SPACES 
                                          THEN :REQ-HOUSDRGST-ADDR ELSE HOUSDRGST_ADDR END,
                 GENDER_CD          = CASE WHEN :REQ-GENDER-CD != SPACES 
                                          THEN :REQ-GENDER-CD ELSE GENDER_CD END,
                 MARRG_SITUATION_CD = CASE WHEN :REQ-MARRG-SITUATION-CD != SPACES 
                                          THEN :REQ-MARRG-SITUATION-CD ELSE MARRG_SITUATION_CD END,
                 BIRTH_DT           = CASE WHEN :REQ-BIRTH-DT != SPACES 
                                          THEN :REQ-BIRTH-DT ELSE BIRTH_DT END,
                 CAREER_TYP_CD      = CASE WHEN :REQ-CAREER-TYP-CD != SPACES 
                                          THEN :REQ-CAREER-TYP-CD ELSE CAREER_TYP_CD END,
                 STATE_AND_RGN_CD   = CASE WHEN :REQ-STATE-RGN-CD != SPACES 
                                          THEN :REQ-STATE-RGN-CD ELSE STATE_AND_RGN_CD END,
                 DOM_OVERS_FLG_CD   = CASE WHEN :REQ-DOM-OVERS-FLG-CD != SPACES 
                                          THEN :REQ-DOM-OVERS-FLG-CD ELSE DOM_OVERS_FLG_CD END,
                 IDCARD_TYP_CD      = CASE WHEN :REQ-IDCARD-TYP-CD != SPACES 
                                          THEN :REQ-IDCARD-TYP-CD ELSE IDCARD_TYP_CD END,
                 EMPLY_FLG          = CASE WHEN :REQ-EMPLY-FLG != SPACES 
                                          THEN :REQ-EMPLY-FLG ELSE EMPLY_FLG END,
                 SHRHD_FLG          = CASE WHEN :REQ-SHRHD-FLG != SPACES 
                                          THEN :REQ-SHRHD-FLG ELSE SHRHD_FLG END,
                 SPS_NAME           = CASE WHEN :REQ-SPS-NAME != SPACES 
                                          THEN :REQ-SPS-NAME ELSE SPS_NAME END,
                 SPS_ENG_NAME       = CASE WHEN :REQ-SPS-ENG-NAME != SPACES 
                                          THEN :REQ-SPS-ENG-NAME ELSE SPS_ENG_NAME END,
                 SPS_CRTF_TYP_CD    = CASE WHEN :REQ-SPS-CRTF-TYP-CD != SPACES 
                                          THEN :REQ-SPS-CRTF-TYP-CD ELSE SPS_CRTF_TYP_CD END,
                 SPS_CRTF_NO        = CASE WHEN :REQ-SPS-CRTF-NO != SPACES 
                                          THEN :REQ-SPS-CRTF-NO ELSE SPS_CRTF_NO END,
                 SPS_TEL_NO         = CASE WHEN :REQ-SPS-TEL-NO != SPACES 
                                          THEN :REQ-SPS-TEL-NO ELSE SPS_TEL_NO END,
                 WORK_UNIT_NM       = CASE WHEN :REQ-WORKUNIT-NM != SPACES 
                                          THEN :REQ-WORKUNIT-NM ELSE WORK_UNIT_NM END,
                 WORK_UNIT_ADDR     = CASE WHEN :REQ-WORKUNIT-ADDR != SPACES 
                                          THEN :REQ-WORKUNIT-ADDR ELSE WORK_UNIT_ADDR END,
                 ADMIN_CMPRMNT_CD   = CASE WHEN :REQ-ADMIN-CMPRMNT-CD != SPACES 
                                          THEN :REQ-ADMIN-CMPRMNT-CD ELSE ADMIN_CMPRMNT_CD END,
                 LAST_UPD_DT        = CURRENT_DATE,
                 LAST_UPD_TM        = CURRENT_TIME
           WHERE CUST_NO = :REQ-CUST-NO
       END-EXEC

       IF SQLCODE = 0
          MOVE 'Y' TO CHG-FLAG-PER
       ELSE
          MOVE 'E12005' TO WS-RESP-CODE
          MOVE 'Failed to update personal info' TO WS-RESP-MSG
          EXEC SQL ROLLBACK END-EXEC
          GO TO EXIT-PROGRAM
       END-IF
    END-IF.

    EXEC SQL COMMIT END-EXEC.

    MOVE '000000' TO WS-RESP-CODE
    EVALUATE TRUE
       WHEN CHG-FLAG-CUST = 'Y' AND CHG-FLAG-PER = 'Y'
          MOVE 'Both info updated successfully' TO WS-RESP-MSG
       WHEN CHG-FLAG-CUST = 'Y'
          MOVE 'Basic info updated successfully' TO WS-RESP-MSG
       WHEN CHG-FLAG-PER = 'Y'
          MOVE 'Personal info updated successfully' TO WS-RESP-MSG
       WHEN OTHER
          MOVE 'No changes detected' TO WS-RESP-MSG
    END-EVALUATE.

EXIT-PROGRAM.
    MOVE WS-RESP-CODE TO RESP-CODE
    MOVE WS-RESP-MSG TO RESP-MSG
    EXIT PROGRAM.