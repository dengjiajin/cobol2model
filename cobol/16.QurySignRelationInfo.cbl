      ******************************************************************
      * 程序名称：QURYSIGNRELATIONINFO
      * 程序功能：客户签约关系查询
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYSIGNRELATIONINFO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CRTF-NO                PIC X(20).
          05 WS-CRTF-TYP-CD            PIC X(2).
          05 WS-CUST-ACCT-NO           PIC X(20).
          05 WS-CUST-NM                PIC X(60).
          05 WS-SIGN-SMLTYP-TYP-CD     PIC X(4).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-SIGN-RELATION-COUNT    PIC 9(4).
          05 WS-SIGN-RELATION-TABLE OCCURS 100 
             DEPENDING ON WS-SIGN-RELATION-COUNT
             INDEXED BY WS-SIGN-RELATION-INDEX.
             10 WS-SIGN-RELATION-INFO.
                15 WS-SIGN-NO          PIC X(20).
                15 WS-SIGN-TYPE        PIC X(2).
                15 WS-SIGN-STATUS      PIC X(1).
                15 WS-SIGN-DATE        PIC X(8).
                15 WS-SIGN-AMOUNT      PIC 9(10)V99.
                15 WS-SIGN-DESC        PIC X(50).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-TEMP-COUNT             PIC 9(4).
       
      * 模拟数据表
       01 SIGN-RELATION-TABLE.
          05 FILLER PIC X(95) VALUE 
             'SR00101A202501011000.50电子银行签约'.
          05 FILLER PIC X(95) VALUE 
             'SR00202A202502022000.00手机银行签约'.
          05 FILLER PIC X(95) VALUE 
             'SR00303I202503033000.75网上支付签约'.
       
       01 SIGN-RELATION-RECORD REDEFINES SIGN-RELATION-TABLE.
          05 SIGN-RELATION-DATA OCCURS 3.
             10 SIGN-NO                PIC X(20).
             10 SIGN-TYPE              PIC X(2).
             10 SIGN-STATUS            PIC X(1).
             10 SIGN-DATE              PIC X(8).
             10 SIGN-AMOUNT            PIC 9(10)V99.
             10 SIGN-DESC              PIC X(50).
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CRTF-NO                PIC X(20).
          05 LK-CRTF-TYP-CD            PIC X(2).
          05 LK-CUST-ACCT-NO           PIC X(20).
          05 LK-CUST-NM                PIC X(60).
          05 LK-SIGN-SMLTYP-TYP-CD     PIC X(4).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-SIGN-RELATION-COUNT    PIC 9(4).
          05 LK-SIGN-RELATION-TABLE OCCURS 100 
             DEPENDING ON LK-SIGN-RELATION-COUNT
             INDEXED BY LK-SIGN-RELATION-INDEX.
             10 LK-SIGN-RELATION-INFO.
                15 LK-SIGN-NO          PIC X(20).
                15 LK-SIGN-TYPE        PIC X(2).
                15 LK-SIGN-STATUS      PIC X(1).
                15 LK-SIGN-DATE        PIC X(8).
                15 LK-SIGN-AMOUNT      PIC 9(10)V99.
                15 LK-SIGN-DESC        PIC X(50).
       
       PROCEDURE DIVISION USING LK-INPUT-DATA, LK-OUTPUT-DATA.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 输入参数验证
           PERFORM VALIDATE-INPUT
           
      * 如果验证通过，执行查询
           IF LK-RETURN-CODE = 0
              PERFORM QUERY-SIGN-RELATION
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE 0 TO LK-SIGN-RELATION-COUNT
           
      * 复制输入参数到工作存储区
           MOVE LK-CRTF-NO TO WS-CRTF-NO
           MOVE LK-CRTF-TYP-CD TO WS-CRTF-TYP-CD
           MOVE LK-CUST-ACCT-NO TO WS-CUST-ACCT-NO
           MOVE LK-CUST-NM TO WS-CUST-NM
           MOVE LK-SIGN-SMLTYP-TYP-CD TO WS-SIGN-SMLTYP-TYP-CD.
       
       VALIDATE-INPUT.
      * 检查必要输入参数
           IF WS-CRTF-NO = SPACES AND 
              WS-CUST-ACCT-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '证件号码和客户账号不能同时为空' 
                TO LK-RETURN-MESSAGE
           END-IF
           
      * 检查证件类型
           IF WS-CRTF-TYP-CD NOT = SPACES AND
              WS-CRTF-TYP-CD NOT = '01' AND
              WS-CRTF-TYP-CD NOT = '02' AND
              WS-CRTF-TYP-CD NOT = '03'
              MOVE 1002 TO LK-RETURN-CODE
              MOVE '证件类型代码不正确' 
                TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-SIGN-RELATION.
      * 初始化计数器
           MOVE 0 TO WS-TEMP-COUNT
           
      * 模拟查询逻辑
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 3
                   
      * 模拟查询条件匹配
              IF (WS-CRTF-NO = SPACES OR 
                  WS-CRTF-NO = '510123199001011234') AND
                 (WS-CUST-ACCT-NO = SPACES OR
                  WS-CUST-ACCT-NO = '6225880112345678')
                  
                 ADD 1 TO WS-TEMP-COUNT
                 MOVE SIGN-NO(WS-I) 
                   TO LK-SIGN-NO(WS-TEMP-COUNT)
                 MOVE SIGN-TYPE(WS-I) 
                   TO LK-SIGN-TYPE(WS-TEMP-COUNT)
                 MOVE SIGN-STATUS(WS-I) 
                   TO LK-SIGN-STATUS(WS-TEMP-COUNT)
                 MOVE SIGN-DATE(WS-I) 
                   TO LK-SIGN-DATE(WS-TEMP-COUNT)
                 MOVE SIGN-AMOUNT(WS-I) 
                   TO LK-SIGN-AMOUNT(WS-TEMP-COUNT)
                 MOVE SIGN-DESC(WS-I) 
                   TO LK-SIGN-DESC(WS-TEMP-COUNT)
              END-IF
           END-PERFORM
           
      * 设置返回的记录数
           MOVE WS-TEMP-COUNT TO LK-SIGN-RELATION-COUNT
           
           IF LK-SIGN-RELATION-COUNT = 0
              MOVE 1003 TO LK-RETURN-CODE
              MOVE '未找到匹配的签约关系' 
                TO LK-RETURN-MESSAGE
           ELSE
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
           END-IF.
       
       END PROGRAM QURYSIGNRELATIONINFO.