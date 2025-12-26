      ******************************************************************
      * 程序名称：QURYCUSTINFO
      * 程序功能：客户基本信息查询
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYCUSTINFO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CUST-NO                PIC X(20).
          05 WS-CUST-NM                PIC X(60).
          05 WS-CRTF-NO                PIC X(20).
          05 WS-CRTF-TYP-CD            PIC X(2).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-CRTF-MATR-DT           PIC X(8).
          05 WS-CRTF-NO-OUT            PIC X(20).
          05 WS-CRTF-TYP-CD-OUT        PIC X(2).
          05 WS-CUST-ATTN-EXTT-CD      PIC X(2).
          05 WS-CUST-NM-OUT            PIC X(60).
          05 WS-CUST-NO-OUT            PIC X(20).
          05 WS-CUST-TYP-CD            PIC X(2).
       
      * 客户基本信息表结构
       01 CUSTOMER-BASIC-INFO-TABLE.
          05 FILLER PIC X(200) VALUE 
             'C00101张三0151012319900101123420251231'.
          05 FILLER PIC X(200) VALUE 
             'C00202有限公司0251012319900202234520261231'.
          05 FILLER PIC X(200) VALUE 
             'C00301李四0151012319900303345620271231'.
          05 FILLER PIC X(200) VALUE 
             'C00401王五0151012319900404456720281231'.
          05 FILLER PIC X(200) VALUE 
             'C00502科技公司0251012319900505567820291231'.
       
       01 CUSTOMER-BASIC-RECORD 
          REDEFINES CUSTOMER-BASIC-INFO-TABLE.
          05 CUSTOMER-BASIC-DATA OCCURS 5.
             10 BASIC-CUST-NO          PIC X(20).
             10 BASIC-CUST-TYP-CD      PIC X(2).
             10 BASIC-CUST-NM          PIC X(60).
             10 BASIC-GENDER-CD        PIC X(1).
             10 BASIC-CRTF-NO          PIC X(20).
             10 BASIC-CRTF-TYP-CD      PIC X(2).
             10 BASIC-CRTF-MATR-DT     PIC X(8).
             10 FILLER                 PIC X(87).
       
      * 客户风险等级信息表结构
       01 CUSTOMER-RISK-INFO-TABLE.
          05 FILLER PIC X(100) VALUE 
             'C00101H'.
          05 FILLER PIC X(100) VALUE 
             'C00202M'.
          05 FILLER PIC X(100) VALUE 
             'C00301L'.
          05 FILLER PIC X(100) VALUE 
             'C00401M'.
          05 FILLER PIC X(100) VALUE 
             'C00502H'.
       
       01 CUSTOMER-RISK-RECORD 
          REDEFINES CUSTOMER-RISK-INFO-TABLE.
          05 CUSTOMER-RISK-DATA OCCURS 5.
             10 RISK-CUST-NO           PIC X(20).
             10 RISK-CUST-TYP-CD       PIC X(2).
             10 RISK-CUST-ATTN-CD      PIC X(1).
             10 FILLER                 PIC X(77).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-J                      PIC 9(4).
          05 WS-CUST-FOUND             PIC X(1).
             88 WS-CUST-FOUND-Y        VALUE 'Y'.
             88 WS-CUST-FOUND-N        VALUE 'N'.
          05 WS-RISK-FOUND             PIC X(1).
             88 WS-RISK-FOUND-Y        VALUE 'Y'.
             88 WS-RISK-FOUND-N        VALUE 'N'.
          05 WS-TEMP-CUST-NO           PIC X(20).
          05 WS-TEMP-CUST-TYP-CD       PIC X(2).
          05 WS-INPUT-VALID            PIC X(1).
             88 WS-INPUT-VALID-Y       VALUE 'Y'.
             88 WS-INPUT-VALID-N       VALUE 'N'.
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CUST-NO                PIC X(20).
          05 LK-CUST-NM                PIC X(60).
          05 LK-CRTF-NO                PIC X(20).
          05 LK-CRTF-TYP-CD            PIC X(2).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-CRTF-MATR-DT           PIC X(8).
          05 LK-CRTF-NO                PIC X(20).
          05 LK-CRTF-TYP-CD            PIC X(2).
          05 LK-CUST-ATTN-EXTT-CD      PIC X(2).
          05 LK-CUST-NM                PIC X(60).
          05 LK-CUST-NO                PIC X(20).
          05 LK-CUST-TYP-CD            PIC X(2).
       
       PROCEDURE DIVISION 
         USING LK-INPUT-DATA, LK-OUTPUT-DATA.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 输入参数验证
           PERFORM VALIDATE-INPUT
           
      * 如果验证通过，执行查询
           IF WS-INPUT-VALID-Y
              PERFORM QUERY-CUST-INFO
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE SPACES TO LK-CRTF-MATR-DT
           MOVE SPACES TO LK-CRTF-NO
           MOVE SPACES TO LK-CRTF-TYP-CD
           MOVE SPACES TO LK-CUST-ATTN-EXTT-CD
           MOVE SPACES TO LK-CUST-NM
           MOVE SPACES TO LK-CUST-NO
           MOVE SPACES TO LK-CUST-TYP-CD
           MOVE 'N' TO WS-CUST-FOUND
           MOVE 'N' TO WS-RISK-FOUND
           MOVE 'Y' TO WS-INPUT-VALID
           
      * 复制输入参数到工作存储区
           MOVE LK-CUST-NO TO WS-CUST-NO
           MOVE LK-CUST-NM TO WS-CUST-NM
           MOVE LK-CRTF-NO TO WS-CRTF-NO
           MOVE LK-CRTF-TYP-CD TO WS-CRTF-TYP-CD.
       
       VALIDATE-INPUT.
      * 检查输入参数（对应Java的验证逻辑）
      * 1. 客户号为空时，检查是否录入证件类型
           IF WS-CUST-NO = SPACES AND 
              WS-CRTF-TYP-CD = SPACES
      * 对应Java的F20004异常
              MOVE 20004 TO LK-RETURN-CODE
              MOVE '证件类型输入为空' 
                TO LK-RETURN-MESSAGE
              MOVE 'N' TO WS-INPUT-VALID
           ELSE
      * 2. 客户号为空时，检查是否录入证件号码
              IF WS-CUST-NO = SPACES AND 
                 WS-CRTF-NO = SPACES
      * 对应Java的F20005异常
                 MOVE 20005 TO LK-RETURN-CODE
                 MOVE '证件号码输入为空' 
                   TO LK-RETURN-MESSAGE
                 MOVE 'N' TO WS-INPUT-VALID
              END-IF
           END-IF.
       
       QUERY-CUST-INFO.
           DISPLAY '开始查询客户基本信息...'
           DISPLAY '查询条件:'
           DISPLAY '  客户编号: ' WS-CUST-NO
           DISPLAY '  客户名称: ' WS-CUST-NM
           DISPLAY '  证件号码: ' WS-CRTF-NO
           DISPLAY '  证件类型: ' WS-CRTF-TYP-CD
           
      * 第一步：查询客户基本信息
           PERFORM QUERY-CUSTOMER-BASIC-INFO
           
      * 第二步：如果找到客户基本信息，查询风险等级信息
           IF WS-CUST-FOUND-Y
              PERFORM QUERY-CUSTOMER-RISK-INFO
           ELSE
      * 对应Java的F20000异常
              MOVE 20000 TO LK-RETURN-CODE
              MOVE '未找到客户基本信息' 
                TO LK-RETURN-MESSAGE
           END-IF
           
      * 第三步：设置成功返回
           IF WS-CUST-FOUND-Y AND WS-RISK-FOUND-Y
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
              DISPLAY '客户信息查询完成'
           END-IF.
       
       QUERY-CUSTOMER-BASIC-INFO.
           DISPLAY '查询客户基本信息...'
           
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 5
                   
      * 模拟多条件查询逻辑
              IF (WS-CUST-NO = SPACES OR 
                  BASIC-CUST-NO(WS-I) = WS-CUST-NO) AND
                 (WS-CUST-NM = SPACES OR 
                  BASIC-CUST-NM(WS-I) = WS-CUST-NM) AND
                 (WS-CRTF-NO = SPACES OR 
                  BASIC-CRTF-NO(WS-I) = WS-CRTF-NO) AND
                 (WS-CRTF-TYP-CD = SPACES OR 
                  BASIC-CRTF-TYP-CD(WS-I) = WS-CRTF-TYP-CD)
                 
                 MOVE 'Y' TO WS-CUST-FOUND
                 MOVE BASIC-CUST-NO(WS-I) 
                   TO WS-TEMP-CUST-NO
                 MOVE BASIC-CUST-TYP-CD(WS-I) 
                   TO WS-TEMP-CUST-TYP-CD
                 
      * 复制基本信息到输出参数
                 MOVE BASIC-CUST-NO(WS-I) 
                   TO LK-CUST-NO
                 MOVE BASIC-CUST-NM(WS-I) 
                   TO LK-CUST-NM
                 MOVE BASIC-CRTF-NO(WS-I) 
                   TO LK-CRTF-NO
                 MOVE BASIC-CRTF-TYP-CD(WS-I) 
                   TO LK-CRTF-TYP-CD
                 MOVE BASIC-CRTF-MATR-DT(WS-I) 
                   TO LK-CRTF-MATR-DT
                 MOVE BASIC-CUST-TYP-CD(WS-I) 
                   TO LK-CUST-TYP-CD
                 
                 DISPLAY '找到客户基本信息:'
                 DISPLAY '  客户编号: ' LK-CUST-NO
                 DISPLAY '  客户名称: ' LK-CUST-NM
                 DISPLAY '  证件号码: ' LK-CRTF-NO
                 DISPLAY '  客户类型: ' LK-CUST-TYP-CD
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       QUERY-CUSTOMER-RISK-INFO.
           DISPLAY '查询客户风险等级信息...'
           DISPLAY '查询条件 - 客户编号: ' WS-TEMP-CUST-NO
                   ', 客户类型: ' WS-TEMP-CUST-TYP-CD
           
           PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > 5
                   
              IF RISK-CUST-NO(WS-J) = WS-TEMP-CUST-NO AND
                 RISK-CUST-TYP-CD(WS-J) = WS-TEMP-CUST-TYP-CD
                 
                 MOVE 'Y' TO WS-RISK-FOUND
                 
      * 复制风险等级信息到输出参数
                 MOVE RISK-CUST-ATTN-CD(WS-J) 
                   TO LK-CUST-ATTN-EXTT-CD
                 
                 DISPLAY '找到客户风险等级信息:'
                 DISPLAY '  客户关注程度: ' 
                         LK-CUST-ATTN-EXTT-CD
                 
      * 显示关注程度描述
                 EVALUATE LK-CUST-ATTN-EXTT-CD
                    WHEN 'H'
                       DISPLAY '  关注程度描述: 高度关注'
                    WHEN 'M'
                       DISPLAY '  关注程度描述: 中度关注'
                    WHEN 'L'
                       DISPLAY '  关注程度描述: 低度关注'
                    WHEN OTHER
                       DISPLAY '  关注程度描述: 正常客户'
                 END-EVALUATE
                 
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       END PROGRAM QURYCUSTINFO.