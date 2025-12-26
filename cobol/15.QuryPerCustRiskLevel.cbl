      ******************************************************************
      * 程序名称：QURYPERCUSTRISKLEVEL
      * 程序功能：对私客户风险等级信息查询
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYPERCUSTRISKLEVEL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CUST-NO                PIC X(20).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-CUST-ATTN-EXTT-CD      PIC X(2).
          05 WS-CUST-NO-OUT            PIC X(20).
          05 WS-CUST-TYP-CD            PIC X(2).
          05 WS-EVALT-ACRDGAS-COMNT    PIC X(100).
          05 WS-EVALT-DT               PIC X(8).
          05 WS-RELS-DT                PIC X(8).
          05 WS-RELS-OR-ISU-ORG-NO     PIC X(20).
       
      * 客户基本信息表结构
       01 CUSTOMER-BASIC-INFO-TABLE.
          05 FILLER PIC X(150) VALUE 
             'C00101P510123199001011234张三'.
          05 FILLER PIC X(150) VALUE 
             'C00202C914400001123456789有限公司'.
          05 FILLER PIC X(150) VALUE 
             'C00301P510123199002022345李四'.
       
       01 CUSTOMER-BASIC-RECORD 
          REDEFINES CUSTOMER-BASIC-INFO-TABLE.
          05 CUSTOMER-BASIC-DATA OCCURS 3.
             10 CUST-BASIC-NO          PIC X(20).
             10 CUST-BASIC-TYP-CD      PIC X(2).
             10 CUST-BASIC-CRTF-TYP-CD PIC X(2).
             10 CUST-BASIC-CRTF-NO     PIC X(20).
             10 CUST-BASIC-NM          PIC X(60).
             10 FILLER                 PIC X(46).
       
      * 客户风险等级信息表结构
       01 CUSTOMER-RISK-INFO-TABLE.
          05 FILLER PIC X(180) VALUE 
             'C00101H2025010120250101ORG001高度风险客户'.
          05 FILLER PIC X(180) VALUE 
             'C00202M2025020220250202ORG002中等风险客户'.
          05 FILLER PIC X(180) VALUE 
             'C00301L2025030320250303ORG003低风险客户'.
       
       01 CUSTOMER-RISK-RECORD 
          REDEFINES CUSTOMER-RISK-INFO-TABLE.
          05 CUSTOMER-RISK-DATA OCCURS 3.
             10 CUST-RISK-NO           PIC X(20).
             10 CUST-RISK-TYP-CD       PIC X(2).
             10 CUST-RISK-ATTN-CD      PIC X(1).
             10 CUST-RISK-EVALT-DT     PIC X(8).
             10 CUST-RISK-RELS-DT      PIC X(8).
             10 CUST-RISK-ORG-NO       PIC X(20).
             10 CUST-RISK-COMNT        PIC X(100).
             10 FILLER                 PIC X(21).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-J                      PIC 9(4).
          05 WS-CUST-BASIC-FOUND       PIC X(1).
             88 WS-CUST-BASIC-FOUND-Y  VALUE 'Y'.
             88 WS-CUST-BASIC-FOUND-N  VALUE 'N'.
          05 WS-CUST-RISK-FOUND        PIC X(1).
             88 WS-CUST-RISK-FOUND-Y   VALUE 'Y'.
             88 WS-CUST-RISK-FOUND-N   VALUE 'N'.
          05 WS-TEMP-CUST-NO           PIC X(20).
          05 WS-TEMP-CUST-TYP-CD       PIC X(2).
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CUST-NO                PIC X(20).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-CUST-ATTN-EXTT-CD      PIC X(2).
          05 LK-CUST-NO-OUT            PIC X(20).
          05 LK-CUST-TYP-CD            PIC X(2).
          05 LK-EVALT-ACRDGAS-COMNT    PIC X(100).
          05 LK-EVALT-DT               PIC X(8).
          05 LK-RELS-DT                PIC X(8).
          05 LK-RELS-OR-ISU-ORG-NO     PIC X(20).
       
       PROCEDURE DIVISION 
         USING LK-INPUT-DATA, LK-OUTPUT-DATA.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 输入参数验证
           PERFORM VALIDATE-INPUT
           
      * 如果验证通过，执行查询
           IF LK-RETURN-CODE = 0
              PERFORM QUERY-CUST-RISK-LEVEL
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE SPACES TO LK-CUST-ATTN-EXTT-CD
           MOVE SPACES TO LK-CUST-NO-OUT
           MOVE SPACES TO LK-CUST-TYP-CD
           MOVE SPACES TO LK-EVALT-ACRDGAS-COMNT
           MOVE SPACES TO LK-EVALT-DT
           MOVE SPACES TO LK-RELS-DT
           MOVE SPACES TO LK-RELS-OR-ISU-ORG-NO
           
      * 复制输入参数到工作存储区
           MOVE LK-CUST-NO TO WS-CUST-NO
           MOVE 'N' TO WS-CUST-BASIC-FOUND
           MOVE 'N' TO WS-CUST-RISK-FOUND.
       
       VALIDATE-INPUT.
      * 检查必要输入参数
           IF WS-CUST-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '客户编号不能为空' 
                TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-CUST-RISK-LEVEL.
           DISPLAY '开始查询客户风险等级信息...'
           DISPLAY '查询客户编号: ' WS-CUST-NO
           
      * 第一步：查询客户基本信息
           PERFORM QUERY-CUSTOMER-BASIC-INFO
           
      * 第二步：如果找到客户基本信息，查询风险等级信息
           IF WS-CUST-BASIC-FOUND-Y
              PERFORM QUERY-CUSTOMER-RISK-INFO
           ELSE
              MOVE 1002 TO LK-RETURN-CODE
              MOVE '未找到客户基本信息' 
                TO LK-RETURN-MESSAGE
           END-IF
           
      * 第三步：设置返回结果
           IF WS-CUST-RISK-FOUND-Y
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
           ELSE
              IF LK-RETURN-CODE = 0
                 MOVE 1003 TO LK-RETURN-CODE
                 MOVE '未找到客户风险等级信息' 
                   TO LK-RETURN-MESSAGE
              END-IF
           END-IF.
       
       QUERY-CUSTOMER-BASIC-INFO.
           DISPLAY '查询客户基本信息...'
           
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 3
                   
              IF CUST-BASIC-NO(WS-I) = WS-CUST-NO
                 MOVE 'Y' TO WS-CUST-BASIC-FOUND
                 MOVE CUST-BASIC-NO(WS-I) 
                   TO WS-TEMP-CUST-NO
                 MOVE CUST-BASIC-TYP-CD(WS-I) 
                   TO WS-TEMP-CUST-TYP-CD
                 
                 DISPLAY '找到客户基本信息:'
                 DISPLAY '  客户编号: ' CUST-BASIC-NO(WS-I)
                 DISPLAY '  客户类型: ' CUST-BASIC-TYP-CD(WS-I)
                 DISPLAY '  客户名称: ' CUST-BASIC-NM(WS-I)
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       QUERY-CUSTOMER-RISK-INFO.
           DISPLAY '查询客户风险等级信息...'
           DISPLAY '查询条件 - 客户编号: ' WS-TEMP-CUST-NO
                   ', 客户类型: ' WS-TEMP-CUST-TYP-CD
           
           PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > 3
                   
              IF CUST-RISK-NO(WS-J) = WS-TEMP-CUST-NO AND
                 CUST-RISK-TYP-CD(WS-J) = WS-TEMP-CUST-TYP-CD
                 
                 MOVE 'Y' TO WS-CUST-RISK-FOUND
                 
      * 设置输出参数
                 MOVE CUST-RISK-ATTN-CD(WS-J) 
                   TO LK-CUST-ATTN-EXTT-CD
                 MOVE CUST-RISK-NO(WS-J) 
                   TO LK-CUST-NO-OUT
                 MOVE CUST-RISK-TYP-CD(WS-J) 
                   TO LK-CUST-TYP-CD
                 MOVE CUST-RISK-COMNT(WS-J) 
                   TO LK-EVALT-ACRDGAS-COMNT
                 MOVE CUST-RISK-EVALT-DT(WS-J) 
                   TO LK-EVALT-DT
                 MOVE CUST-RISK-RELS-DT(WS-J) 
                   TO LK-RELS-DT
                 MOVE CUST-RISK-ORG-NO(WS-J) 
                   TO LK-RELS-OR-ISU-ORG-NO
                 
                 DISPLAY '找到客户风险等级信息:'
                 DISPLAY '  客户关注程度: ' 
                         LK-CUST-ATTN-EXTT-CD
                 DISPLAY '  评定日期: ' LK-EVALT-DT
                 DISPLAY '  评定说明: ' 
                         LK-EVALT-ACRDGAS-COMNT
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       END PROGRAM QURYPERCUSTRISKLEVEL.