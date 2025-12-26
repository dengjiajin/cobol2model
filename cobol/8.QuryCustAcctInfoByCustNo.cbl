      ******************************************************************
      * 程序名称：QURYCUSTACCTINFOBYCUSTNO
      * 程序功能：通过客户号查询客户账户路由信息
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYCUSTACCTINFOBYCUSTNO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-TENANT-NO              PIC X(10).
          05 WS-CUST-NO                PIC X(20).
          05 WS-ROUTE-TYP-CD           PIC X(2).
          05 WS-STUS-CD                PIC X(1).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-CUST-ACCT-ROUTE-COUNT  PIC 9(4).
          05 WS-CUST-ACCT-ROUTE-TABLE OCCURS 10 
             DEPENDING ON WS-CUST-ACCT-ROUTE-COUNT
             INDEXED BY WS-CUST-ACCT-ROUTE-INDEX.
             10 WS-CUST-ACCT-ROUTE-INFO.
                15 WS-CUST-NO-OUT      PIC X(20).
                15 WS-AFS-PRODT-NO     PIC X(10).
                15 WS-BASE-PRODT-NO    PIC X(10).
                15 WS-MAIN-ACCT-NO     PIC X(20).
                15 WS-OPER-TYP-CD      PIC X(2).
                15 WS-RELA-SEQ-NO      PIC X(5).
                15 WS-ROUTE-TYP-CD-OUT PIC X(2).
                15 WS-ROUTE-VAL        PIC X(30).
                15 WS-VALID-FLG        PIC X(1).
       
      * 客户账户路由信息表结构
       01 CUST-ACCT-INFO-TABLE.
          05 FILLER PIC X(100) VALUE 
             'C001PROD001BASE001622588011234567801001ROUTE10001'.
          05 FILLER PIC X(100) VALUE 
             'C001PROD002BASE002622588011234567802002ROUTE20001'.
          05 FILLER PIC X(100) VALUE 
             'C002PROD001BASE001622588022234567801001ROUTE30001'.
          05 FILLER PIC X(100) VALUE 
             'C003PROD003BASE003622588033234567803003ROUTE40001'.
          05 FILLER PIC X(100) VALUE 
             'C001PROD004BASE004622588044234567804004ROUTE10002'.
       
       01 CUST-ACCT-INFO-RECORD 
          REDEFINES CUST-ACCT-INFO-TABLE.
          05 CUST-ACCT-DATA OCCURS 5.
             10 ACCT-CUST-NO           PIC X(20).
             10 ACCT-AFS-PRODT-NO      PIC X(10).
             10 ACCT-BASE-PRODT-NO     PIC X(10).
             10 ACCT-MAIN-ACCT-NO      PIC X(20).
             10 ACCT-OPER-TYP-CD       PIC X(2).
             10 ACCT-RELA-SEQ-NO       PIC X(5).
             10 ACCT-ROUTE-TYP-CD      PIC X(2).
             10 ACCT-ROUTE-VAL         PIC X(30).
             10 ACCT-VALID-FLG         PIC X(1).
             10 FILLER                 PIC X(10).
       
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
          05 LK-TENANT-NO              PIC X(10).
          05 LK-CUST-NO                PIC X(20).
          05 LK-ROUTE-TYP-CD           PIC X(2).
          05 LK-STUS-CD                PIC X(1).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-CUST-ACCT-ROUTE-COUNT  PIC 9(4).
          05 LK-CUST-ACCT-ROUTE-TABLE OCCURS 10 
             DEPENDING ON LK-CUST-ACCT-ROUTE-COUNT
             INDEXED BY LK-CUST-ACCT-ROUTE-INDEX.
             10 LK-CUST-ACCT-ROUTE-INFO.
                15 LK-CUST-NO-OUT      PIC X(20).
                15 LK-AFS-PRODT-NO     PIC X(10).
                15 LK-BASE-PRODT-NO    PIC X(10).
                15 LK-MAIN-ACCT-NO     PIC X(20).
                15 LK-OPER-TYP-CD      PIC X(2).
                15 LK-RELA-SEQ-NO      PIC X(5).
                15 LK-ROUTE-TYP-CD-OUT PIC X(2).
                15 LK-ROUTE-VAL        PIC X(30).
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
              PERFORM QUERY-CUST-ACCT-INFO
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE 0 TO LK-CUST-ACCT-ROUTE-COUNT
           MOVE 'N' TO WS-DATA-FOUND
           MOVE 'Y' TO WS-TENANT-MATCH
           
      * 复制输入参数到工作存储区
           MOVE LK-TENANT-NO TO WS-TENANT-NO
           MOVE LK-CUST-NO TO WS-CUST-NO
           MOVE LK-ROUTE-TYP-CD TO WS-ROUTE-TYP-CD
           MOVE LK-STUS-CD TO WS-STUS-CD.
       
       VALIDATE-INPUT.
      * 检查必要输入参数（对应Java的CheckParaUtil.checkInputForEmpty）
           IF WS-TENANT-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '租户号不能为空' 
                TO LK-RETURN-MESSAGE
           ELSE
              IF WS-CUST-NO = SPACES
                 MOVE 1002 TO LK-RETURN-CODE
                 MOVE '客户号不能为空' 
                   TO LK-RETURN-MESSAGE
              ELSE
                 IF WS-ROUTE-TYP-CD = SPACES
                    MOVE 1003 TO LK-RETURN-CODE
                    MOVE '路由值类型不能为空' 
                      TO LK-RETURN-MESSAGE
                 END-IF
              END-IF
           END-IF.
       
       QUERY-CUST-ACCT-INFO.
           DISPLAY '开始查询客户账户路由信息...'
           DISPLAY '查询条件:'
           DISPLAY '  租户号: ' WS-TENANT-NO
           DISPLAY '  客户号: ' WS-CUST-NO
           DISPLAY '  路由类型: ' WS-ROUTE-TYP-CD
           DISPLAY '  状态代码: ' WS-STUS-CD
           
      * 初始化计数器
           MOVE 0 TO WS-TEMP-COUNT
           
      * 查询客户账户路由信息
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 5
                   
      * 模拟租户号验证（实际应用中应根据租户号过滤）
              MOVE 'Y' TO WS-TENANT-MATCH
              
              IF ACCT-CUST-NO(WS-I) = WS-CUST-NO AND
                 (WS-ROUTE-TYP-CD = SPACES OR
                  ACCT-ROUTE-TYP-CD(WS-I) = WS-ROUTE-TYP-CD) AND
                 (WS-STUS-CD = SPACES OR
                  ACCT-VALID-FLG(WS-I) = WS-STUS-CD) AND
                 WS-TENANT-MATCH-Y
                 
                 MOVE 'Y' TO WS-DATA-FOUND
                 ADD 1 TO WS-TEMP-COUNT
                 
      * 复制账户路由信息到输出表
                 MOVE ACCT-CUST-NO(WS-I)
                   TO LK-CUST-NO-OUT(WS-TEMP-COUNT)
                 MOVE ACCT-AFS-PRODT-NO(WS-I)
                   TO LK-AFS-PRODT-NO(WS-TEMP-COUNT)
                 MOVE ACCT-BASE-PRODT-NO(WS-I)
                   TO LK-BASE-PRODT-NO(WS-TEMP-COUNT)
                 MOVE ACCT-MAIN-ACCT-NO(WS-I)
                   TO LK-MAIN-ACCT-NO(WS-TEMP-COUNT)
                 MOVE ACCT-OPER-TYP-CD(WS-I)
                   TO LK-OPER-TYP-CD(WS-TEMP-COUNT)
                 MOVE ACCT-RELA-SEQ-NO(WS-I)
                   TO LK-RELA-SEQ-NO(WS-TEMP-COUNT)
                 MOVE ACCT-ROUTE-TYP-CD(WS-I)
                   TO LK-ROUTE-TYP-CD-OUT(WS-TEMP-COUNT)
                 MOVE ACCT-ROUTE-VAL(WS-I)
                   TO LK-ROUTE-VAL(WS-TEMP-COUNT)
                 MOVE ACCT-VALID-FLG(WS-I)
                   TO LK-VALID-FLG(WS-TEMP-COUNT)
                 
                 DISPLAY '找到账户路由记录 ' WS-TEMP-COUNT ':'
                 DISPLAY '  主账号: ' 
                         LK-MAIN-ACCT-NO(WS-TEMP-COUNT)
                 DISPLAY '  可售产品: ' 
                         LK-AFS-PRODT-NO(WS-TEMP-COUNT)
                 DISPLAY '  基础产品: ' 
                         LK-BASE-PRODT-NO(WS-TEMP-COUNT)
                 DISPLAY '  路由值: ' 
                         LK-ROUTE-VAL(WS-TEMP-COUNT)
                 DISPLAY '  路由类型: ' 
                         LK-ROUTE-TYP-CD-OUT(WS-TEMP-COUNT)
                 DISPLAY '  有效标志: ' 
                         LK-VALID-FLG(WS-TEMP-COUNT)
              END-IF
           END-PERFORM
           
      * 设置返回的记录数
           MOVE WS-TEMP-COUNT TO LK-CUST-ACCT-ROUTE-COUNT
           
      * 检查查询结果
           IF WS-DATA-FOUND-N
              MOVE 20000 TO LK-RETURN-CODE
              MOVE '未找到客户账户路由信息' 
                TO LK-RETURN-MESSAGE
           ELSE
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
              DISPLAY '共找到 ' LK-CUST-ACCT-ROUTE-COUNT 
                      ' 条账户路由记录'
           END-IF.
       
       END PROGRAM QURYCUSTACCTINFOBYCUSTNO.