       IDENTIFICATION DIVISION.
       PROGRAM-ID. QRYCUSTACCTINFO.
       AUTHOR.     SYSTEM.
       DATE-COMPILED.
      ******************************************************************
      * 程序功能：通过账号查询客户账户路由信息
      * 根据路由值、关联序号、路由类型查询客户账户路由信息
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * 输入参数
       01 WS-INPUT-AREA.
          05 WS-TENANT-NO          PIC X(20).
          05 WS-ROUTE-VAL          PIC X(50).
          05 WS-ROUTE-TYP-CD       PIC X(10).
          05 WS-RELA-SEQ-NO        PIC X(20).
          05 WS-FILLER             PIC X(80).
          
      * 输出参数
       01 WS-OUTPUT-AREA.
          05 WS-RETURN-CODE        PIC 9(4).
          05 WS-RETURN-MESSAGE     PIC X(100).
          05 WS-RECORD-COUNT       PIC 9(4).
          05 WS-FILLER             PIC X(100).
          
      * 客户账户路由信息记录
       01 WS-CUST-ACCT-ROUTE-TABLE.
          05 WS-CUST-ACCT-ROUTE-INFO 
             OCCURS 100 TIMES
             DEPENDING ON WS-RECORD-COUNT
             INDEXED BY IDX.
             10 WS-ID               PIC 9(18).
             10 WS-TENANT-NO-O      PIC X(20).
             10 WS-CUST-NO          PIC X(20).
             10 WS-AFS-PRODT-NO     PIC X(20).
             10 WS-BASE-PRODT-NO    PIC X(20).
             10 WS-MAIN-ACCT-NO     PIC X(30).
             10 WS-OPER-TYP-CD      PIC X(10).
             10 WS-RELA-SEQ-NO-O    PIC X(20).
             10 WS-ROUTE-TYP-CD-O   PIC X(10).
             10 WS-ROUTE-VAL-O      PIC X(50).
             10 WS-VALID-FLG        PIC X(1).
             10 WS-CRT-TELR-NO      PIC X(20).
             10 WS-UPD-TELR-NO      PIC X(20).
             10 WS-UPD-TM.
                15 WS-UPD-TM-DATE   PIC 9(8).
                15 WS-UPD-TM-TIME   PIC 9(6).
             10 WS-CRT-TM.
                15 WS-CRT-TM-DATE   PIC 9(8).
                15 WS-CRT-TM-TIME   PIC 9(6).

      * 数据库连接和工作变量
       01 WS-DB-CONNECTION.
          05 WS-DB-NAME            PIC X(30) 
             VALUE 'HSBCECIFDB'.
          05 WS-USER-ID            PIC X(20) 
             VALUE 'ECIFUSER'.
          05 WS-PASSWORD           PIC X(20) 
             VALUE 'ECIFPASS'.
           
       01 WS-SQL-STATEMENT         PIC X(500).
       01 WS-SQLCODE               PIC S9(9) COMP.
       01 WS-ERROR-MSG             PIC X(100).
       01 WS-CURRENT-DATE          PIC 9(8).
       01 WS-CURRENT-TIME          PIC 9(6).
       
       LINKAGE SECTION.
      * 调用参数区
       01 LK-INPUT-AREA.
          05 LK-TENANT-NO          PIC X(20).
          05 LK-ROUTE-VAL          PIC X(50).
          05 LK-ROUTE-TYP-CD       PIC X(10).
          05 LK-RELA-SEQ-NO        PIC X(20).
          05 LK-FILLER             PIC X(80).
          
      * 返回参数区  
       01 LK-OUTPUT-AREA.
          05 LK-RETURN-CODE        PIC 9(4).
          05 LK-RETURN-MESSAGE     PIC X(100).
          05 LK-RECORD-COUNT       PIC 9(4).
          05 LK-FILLER             PIC X(100).
           
      * 返回数据区
       01 LK-CUST-ACCT-ROUTE-TABLE.
          05 LK-CUST-ACCT-ROUTE-INFO 
             OCCURS 100 TIMES
             DEPENDING ON LK-RECORD-COUNT.
             10 LK-ID               PIC 9(18).
             10 LK-TENANT-NO-O      PIC X(20).
             10 LK-CUST-NO          PIC X(20).
             10 LK-AFS-PRODT-NO     PIC X(20).
             10 LK-BASE-PRODT-NO    PIC X(20).
             10 LK-MAIN-ACCT-NO     PIC X(30).
             10 LK-OPER-TYP-CD      PIC X(10).
             10 LK-RELA-SEQ-NO-O    PIC X(20).
             10 LK-ROUTE-TYP-CD-O   PIC X(10).
             10 LK-ROUTE-VAL-O      PIC X(50).
             10 LK-VALID-FLG        PIC X(1).
             10 LK-CRT-TELR-NO      PIC X(20).
             10 LK-UPD-TELR-NO      PIC X(20).
             10 LK-UPD-TM.
                15 LK-UPD-TM-DATE   PIC 9(8).
                15 LK-UPD-TM-TIME   PIC 9(6).
             10 LK-CRT-TM.
                15 LK-CRT-TM-DATE   PIC 9(8).
                15 LK-CRT-TM-TIME   PIC 9(6).

       PROCEDURE DIVISION 
         USING LK-INPUT-AREA, LK-OUTPUT-AREA, 
               LK-CUST-ACCT-ROUTE-TABLE.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 参数校验
           PERFORM VALIDATE-INPUT-PARAMS
           IF WS-RETURN-CODE NOT = ZERO
              PERFORM RETURN-ERROR
              GOBACK
           END-IF
           
      * 查询客户账户路由信息
           PERFORM QUERY-CUST-ACCT-INFO
           
      * 返回结果
           PERFORM RETURN-RESULT
           
           GOBACK.
           
       INITIALIZE-PROGRAM.
           MOVE ZERO TO WS-RETURN-CODE
           MOVE SPACES TO WS-RETURN-MESSAGE
           MOVE ZERO TO WS-RECORD-COUNT
           MOVE LK-INPUT-AREA TO WS-INPUT-AREA
           EXIT.
           
       VALIDATE-INPUT-PARAMS.
      * 检查租户号
           IF WS-TENANT-NO = SPACES OR LOW-VALUES
              MOVE 1001 TO WS-RETURN-CODE
              MOVE '租户号不能为空' TO WS-RETURN-MESSAGE
              EXIT
           END-IF
           
      * 检查路由值
           IF WS-ROUTE-VAL = SPACES OR LOW-VALUES
              MOVE 1002 TO WS-RETURN-CODE
              MOVE '路由值不能为空' TO WS-RETURN-MESSAGE
              EXIT
           END-IF
           
      * 检查路由类型
           IF WS-ROUTE-TYP-CD = SPACES OR LOW-VALUES
              MOVE 1003 TO WS-RETURN-CODE
              MOVE '路由类型不能为空' TO WS-RETURN-MESSAGE
              EXIT
           END-IF
           
           MOVE 0 TO WS-RETURN-CODE
           EXIT.
           
       QUERY-CUST-ACCT-INFO.
      * 构建SQL查询语句
           STRING 
             'SELECT ID, TENANT_NO, CUST_NO, AFS_PRODT_NO, '
             'BASE_PRODT_NO, MAIN_ACCT_NO, OPER_TYP_CD, '
             'RELA_SEQ_NO, ROUTE_TYP_CD, ROUTE_VAL, VALID_FLG, '
             'CRT_TELR_NO, UPD_TELR_NO, UPD_TM, CRT_TM '
             'FROM THSBCECIF_CUST_ACCT_INFO '
             'WHERE TENANT_NO = ? '
             'AND ROUTE_VAL = ? '
             'AND ROUTE_TYP_CD = ? '
             'AND VALID_FLG = ''1'' '
             DELIMITED BY SIZE
             INTO WS-SQL-STATEMENT
           END-STRING
           
      * 如果关联序号不为空，添加到查询条件
           IF WS-RELA-SEQ-NO NOT = SPACES AND 
              WS-RELA-SEQ-NO NOT = LOW-VALUES
              STRING 
                FUNCTION TRIM(WS-SQL-STATEMENT)
                ' AND RELA_SEQ_NO = ?'
                DELIMITED BY SIZE
                INTO WS-SQL-STATEMENT
              END-STRING
           END-IF
           
      * 执行数据库查询 (这里简化了数据库操作)
           PERFORM EXECUTE-DB-QUERY
           EXIT.
           
       EXECUTE-DB-QUERY.
      * 模拟数据库查询结果
           MOVE 2 TO WS-RECORD-COUNT
           
      * 第一条记录
           MOVE 100001 TO WS-ID(1)
           MOVE WS-TENANT-NO TO WS-TENANT-NO-O(1)
           MOVE 'CUST0000001' TO WS-CUST-NO(1)
           MOVE 'AFS001' TO WS-AFS-PRODT-NO(1)
           MOVE 'BASE001' TO WS-BASE-PRODT-NO(1)
           MOVE '6228480018888888888' TO WS-MAIN-ACCT-NO(1)
           MOVE 'OPER001' TO WS-OPER-TYP-CD(1)
           MOVE WS-RELA-SEQ-NO TO WS-RELA-SEQ-NO-O(1)
           MOVE WS-ROUTE-TYP-CD TO WS-ROUTE-TYP-CD-O(1)
           MOVE WS-ROUTE-VAL TO WS-ROUTE-VAL-O(1)
           MOVE '1' TO WS-VALID-FLG(1)
           MOVE 'TELR001' TO WS-CRT-TELR-NO(1)
           MOVE 'TELR002' TO WS-UPD-TELR-NO(1)
           MOVE 20250919 TO WS-UPD-TM-DATE(1)
           MOVE 143052 TO WS-UPD-TM-TIME(1)
           MOVE 20250919 TO WS-CRT-TM-DATE(1)
           MOVE 143052 TO WS-CRT-TM-TIME(1)
           
      * 第二条记录
           MOVE 100002 TO WS-ID(2)
           MOVE WS-TENANT-NO TO WS-TENANT-NO-O(2)
           MOVE 'CUST0000002' TO WS-CUST-NO(2)
           MOVE 'AFS002' TO WS-AFS-PRODT-NO(2)
           MOVE 'BASE002' TO WS-BASE-PRODT-NO(2)
           MOVE '6228480029999999999' TO WS-MAIN-ACCT-NO(2)
           MOVE 'OPER002' TO WS-OPER-TYP-CD(2)
           MOVE WS-RELA-SEQ-NO TO WS-RELA-SEQ-NO-O(2)
           MOVE WS-ROUTE-TYP-CD TO WS-ROUTE-TYP-CD-O(2)
           MOVE WS-ROUTE-VAL TO WS-ROUTE-VAL-O(2)
           MOVE '1' TO WS-VALID-FLG(2)
           MOVE 'TELR003' TO WS-CRT-TELR-NO(2)
           MOVE 'TELR004' TO WS-UPD-TELR-NO(2)
           MOVE 20250919 TO WS-UPD-TM-DATE(2)
           MOVE 153052 TO WS-UPD-TM-TIME(2)
           MOVE 20250919 TO WS-CRT-TM-DATE(2)
           MOVE 153052 TO WS-CRT-TM-TIME(2)
           
           MOVE 0 TO WS-RETURN-CODE
           MOVE '查询成功' TO WS-RETURN-MESSAGE
           EXIT.
           
       RETURN-ERROR.
           MOVE WS-RETURN-CODE TO LK-RETURN-CODE
           MOVE WS-RETURN-MESSAGE TO LK-RETURN-MESSAGE
           MOVE ZERO TO LK-RECORD-COUNT
           EXIT.
           
       RETURN-RESULT.
           MOVE WS-RETURN-CODE TO LK-RETURN-CODE
           MOVE WS-RETURN-MESSAGE TO LK-RETURN-MESSAGE
           MOVE WS-RECORD-COUNT TO LK-RECORD-COUNT
           
      * 复制查询结果到返回区
           PERFORM VARYING IDX FROM 1 BY 1 
             UNTIL IDX > WS-RECORD-COUNT
             MOVE WS-ID(IDX) TO LK-ID(IDX)
             MOVE WS-TENANT-NO-O(IDX) TO LK-TENANT-NO-O(IDX)
             MOVE WS-CUST-NO(IDX) TO LK-CUST-NO(IDX)
             MOVE WS-AFS-PRODT-NO(IDX) TO LK-AFS-PRODT-NO(IDX)
             MOVE WS-BASE-PRODT-NO(IDX) TO LK-BASE-PRODT-NO(IDX)
             MOVE WS-MAIN-ACCT-NO(IDX) TO LK-MAIN-ACCT-NO(IDX)
             MOVE WS-OPER-TYP-CD(IDX) TO LK-OPER-TYP-CD(IDX)
             MOVE WS-RELA-SEQ-NO-O(IDX) TO LK-RELA-SEQ-NO-O(IDX)
             MOVE WS-ROUTE-TYP-CD-O(IDX) TO LK-ROUTE-TYP-CD-O(IDX)
             MOVE WS-ROUTE-VAL-O(IDX) TO LK-ROUTE-VAL-O(IDX)
             MOVE WS-VALID-FLG(IDX) TO LK-VALID-FLG(IDX)
             MOVE WS-CRT-TELR-NO(IDX) TO LK-CRT-TELR-NO(IDX)
             MOVE WS-UPD-TELR-NO(IDX) TO LK-UPD-TELR-NO(IDX)
             MOVE WS-UPD-TM-DATE(IDX) TO LK-UPD-TM-DATE(IDX)
             MOVE WS-UPD-TM-TIME(IDX) TO LK-UPD-TM-TIME(IDX)
             MOVE WS-CRT-TM-DATE(IDX) TO LK-CRT-TM-DATE(IDX)
             MOVE WS-CRT-TM-TIME(IDX) TO LK-CRT-TM-TIME(IDX)
           END-PERFORM
           EXIT.
           
       END PROGRAM QRYCUSTACCTINFO.