、      ******************************************************************
      * 程序名称：QURYPERCUSTCHNLTXNCOMMOND
      * 程序功能：对私客户交易渠道控制查询
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYPERCUSTCHNLTXNCOMMOND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CUST-NO                PIC X(20).
          05 WS-TENANT-NO              PIC X(10).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-CUST-TXN-CHNL-COUNT    PIC 9(4).
          05 WS-CUST-TXN-CHNL-TABLE OCCURS 10 
             DEPENDING ON WS-CUST-TXN-CHNL-COUNT
             INDEXED BY WS-CUST-TXN-CHNL-INDEX.
             10 WS-CUST-TXN-CHNL-INFO.
                15 WS-YR-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 WS-MON-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 WS-PMIT-TERMINAL-TYP-CD 
                                       PIC X(2).
                15 WS-LMT-TYP-CD       PIC X(2).
                15 WS-DAY-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 WS-MON-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 WS-DAY-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 WS-YR-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 WS-SGL-TX-HIGH-AMT  PIC 9(10)V99.
                15 WS-SGL-TX-LOWEST-AMT 
                                       PIC 9(10)V99.
                15 WS-QT-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 WS-QT-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 WS-CUST-NO-OUT      PIC X(20).
                15 WS-RSN              PIC X(100).
                15 WS-VALID-FLG        PIC X(1).
       
      * 客户交易渠道控制信息表结构
       01 CUST-CHNL-TXN-COMMOND-TABLE.
          05 FILLER PIC X(200) VALUE 
             '0000100000.0000020000.0001L1000000.000010005000200000'.
          05 FILLER PIC X(200) VALUE 
             '00005000.0000001000.0002L2000500.0000050020001000000'.
          05 FILLER PIC X(200) VALUE 
             '0000200000.0000010000.0003L3001000.00002001000500000'.
       
       01 CUST-CHNL-TXN-COMMOND-RECORD 
          REDEFINES CUST-CHNL-TXN-COMMOND-TABLE.
          05 CUST-CHNL-TXN-DATA OCCURS 3.
             10 CHNL-YR-ACCM-MAX-AMT   PIC 9(10)V99.
             10 CHNL-MON-ACCM-MAX-AMT  PIC 9(10)V99.
             10 CHNL-PMIT-TERMINAL-CD  PIC X(2).
             10 CHNL-LMT-TYP-CD        PIC X(2).
             10 CHNL-DAY-ACCM-MAX-AMT  PIC 9(10)V99.
             10 CHNL-MON-ACCM-MAX-CNT  PIC 9(5).
             10 CHNL-DAY-ACCM-MAX-CNT  PIC 9(5).
             10 CHNL-YR-ACCM-MAX-CNT   PIC 9(5).
             10 CHNL-SGL-TX-HIGH-AMT   PIC 9(10)V99.
             10 CHNL-SGL-TX-LOW-AMT    PIC 9(10)V99.
             10 CHNL-QT-ACCM-MAX-CNT   PIC 9(5).
             10 CHNL-QT-ACCM-MAX-AMT   PIC 9(10)V99.
             10 CHNL-CUST-NO           PIC X(20).
             10 CHNL-RSN               PIC X(100).
             10 CHNL-VALID-FLG         PIC X(1).
             10 FILLER                 PIC X(21).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-TEMP-COUNT             PIC 9(4).
          05 WS-DATA-FOUND             PIC X(1).
             88 WS-DATA-FOUND-Y        VALUE 'Y'.
             88 WS-DATA-FOUND-N        VALUE 'N'.
          05 WS-TENANT-MATCH           PIC X(1).
             88 WS-TENANT-MATCH-Y      VALUE 'Y'.
             88 WS-TENANT-MATCH-N      VALUE 'N'.
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CUST-NO                PIC X(20).
          05 LK-TENANT-NO              PIC X(10).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-CUST-TXN-CHNL-COUNT    PIC 9(4).
          05 LK-CUST-TXN-CHNL-TABLE OCCURS 10 
             DEPENDING ON LK-CUST-TXN-CHNL-COUNT
             INDEXED BY LK-CUST-TXN-CHNL-INDEX.
             10 LK-CUST-TXN-CHNL-INFO.
                15 LK-YR-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 LK-MON-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 LK-PMIT-TERMINAL-TYP-CD 
                                       PIC X(2).
                15 LK-LMT-TYP-CD       PIC X(2).
                15 LK-DAY-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 LK-MON-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 LK-DAY-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 LK-YR-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 LK-SGL-TX-HIGH-AMT  PIC 9(10)V99.
                15 LK-SGL-TX-LOWEST-AMT 
                                       PIC 9(10)V99.
                15 LK-QT-ACCM-MAX-TX-STKCNT 
                                       PIC 9(5).
                15 LK-QT-ACCM-MAX-TX-AMT 
                                       PIC 9(10)V99.
                15 LK-CUST-NO-OUT      PIC X(20).
                15 LK-RSN              PIC X(100).
                15 LK-VALID-FLG        PIC X(1).
       
       PROCEDURE DIVISION 
         USING LK-INPUT-DATA, LK-OUTPUT-DATA.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 输入参数验证
           PERFORM VALIDATE-INPUT
           
      * 如果验证通过，执行查询
           IF LK-RETURN-CODE = 0
              PERFORM QUERY-CUST-CHNL-TXN-COMMOND
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE 0 TO LK-CUST-TXN-CHNL-COUNT
           MOVE 'N' TO WS-DATA-FOUND
           
      * 复制输入参数到工作存储区
           MOVE LK-CUST-NO TO WS-CUST-NO
           MOVE LK-TENANT-NO TO WS-TENANT-NO.
       
       VALIDATE-INPUT.
      * 检查必要输入参数（对应Java的CheckParaUtil.checkInputForEmpty）
           IF WS-CUST-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '客户编号不能为空' 
                TO LK-RETURN-MESSAGE
           ELSE
              IF WS-TENANT-NO = SPACES
                 MOVE 1002 TO LK-RETURN-CODE
                 MOVE '租户号不能为空' 
                   TO LK-RETURN-MESSAGE
              END-IF
           END-IF.
       
       QUERY-CUST-CHNL-TXN-COMMOND.
           DISPLAY '开始查询客户交易渠道控制信息...'
           DISPLAY '查询客户编号: ' WS-CUST-NO
           DISPLAY '查询租户号: ' WS-TENANT-NO
           
      * 初始化计数器
           MOVE 0 TO WS-TEMP-COUNT
           
      * 查询客户交易渠道控制信息
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 3
                   
      * 模拟租户号验证（实际应用中应根据租户号过滤）
              MOVE 'Y' TO WS-TENANT-MATCH
              
              IF CHNL-CUST-NO(WS-I) = WS-CUST-NO AND
                 WS-TENANT-MATCH-Y
                 
                 MOVE 'Y' TO WS-DATA-FOUND
                 ADD 1 TO WS-TEMP-COUNT
                 
      * 复制交易渠道控制信息到输出表
                 MOVE CHNL-YR-ACCM-MAX-AMT(WS-I)
                   TO LK-YR-ACCM-MAX-TX-AMT(WS-TEMP-COUNT)
                 MOVE CHNL-MON-ACCM-MAX-AMT(WS-I)
                   TO LK-MON-ACCM-MAX-TX-AMT(WS-TEMP-COUNT)
                 MOVE CHNL-PMIT-TERMINAL-CD(WS-I)
                   TO LK-PMIT-TERMINAL-TYP-CD(WS-TEMP-COUNT)
                 MOVE CHNL-LMT-TYP-CD(WS-I)
                   TO LK-LMT-TYP-CD(WS-TEMP-COUNT)
                 MOVE CHNL-DAY-ACCM-MAX-AMT(WS-I)
                   TO LK-DAY-ACCM-MAX-TX-AMT(WS-TEMP-COUNT)
                 MOVE CHNL-MON-ACCM-MAX-CNT(WS-I)
                   TO LK-MON-ACCM-MAX-TX-STKCNT(WS-TEMP-COUNT)
                 MOVE CHNL-DAY-ACCM-MAX-CNT(WS-I)
                   TO LK-DAY-ACCM-MAX-TX-STKCNT(WS-TEMP-COUNT)
                 MOVE CHNL-YR-ACCM-MAX-CNT(WS-I)
                   TO LK-YR-ACCM-MAX-TX-STKCNT(WS-TEMP-COUNT)
                 MOVE CHNL-SGL-TX-HIGH-AMT(WS-I)
                   TO LK-SGL-TX-HIGH-AMT(WS-TEMP-COUNT)
                 MOVE CHNL-SGL-TX-LOW-AMT(WS-I)
                   TO LK-SGL-TX-LOWEST-AMT(WS-TEMP-COUNT)
                 MOVE CHNL-QT-ACCM-MAX-CNT(WS-I)
                   TO LK-QT-ACCM-MAX-TX-STKCNT(WS-TEMP-COUNT)
                 MOVE CHNL-QT-ACCM-MAX-AMT(WS-I)
                   TO LK-QT-ACCM-MAX-TX-AMT(WS-TEMP-COUNT)
                 MOVE CHNL-CUST-NO(WS-I)
                   TO LK-CUST-NO-OUT(WS-TEMP-COUNT)
                 MOVE CHNL-RSN(WS-I)
                   TO LK-RSN(WS-TEMP-COUNT)
                 MOVE CHNL-VALID-FLG(WS-I)
                   TO LK-VALID-FLG(WS-TEMP-COUNT)
                 
                 DISPLAY '找到交易渠道控制记录 ' WS-TEMP-COUNT ':'
                 DISPLAY '  允许终端类型: ' 
                         LK-PMIT-TERMINAL-TYP-CD(WS-TEMP-COUNT)
                 DISPLAY '  限额类型: ' 
                         LK-LMT-TYP-CD(WS-TEMP-COUNT)
                 DISPLAY '  单笔最高金额: ' 
                         LK-SGL-TX-HIGH-AMT(WS-TEMP-COUNT)
                 DISPLAY '  日累计最大金额: ' 
                         LK-DAY-ACCM-MAX-TX-AMT(WS-TEMP-COUNT)
              END-IF
           END-PERFORM
           
      * 设置返回的记录数
           MOVE WS-TEMP-COUNT TO LK-CUST-TXN-CHNL-COUNT
           
      * 检查查询结果
           IF WS-DATA-FOUND-N
              MOVE 20000 TO LK-RETURN-CODE
              MOVE '未找到客户交易渠道控制信息' 
                TO LK-RETURN-MESSAGE
           ELSE
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
              DISPLAY '共找到 ' LK-CUST-TXN-CHNL-COUNT 
                      ' 条交易渠道控制记录'
           END-IF.
       
       END PROGRAM QURYPERCUSTCHNLTXNCOMMOND.