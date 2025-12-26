      ******************************************************************
      * 程序名称：QURYCUSTTYPE
      * 程序功能：客户类型查询
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYCUSTTYPE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CUST-NO                PIC X(20).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-CUST-TYP-CD            PIC X(2).
       
      * 客户基本信息表结构
       01 CUSTOMER-BASIC-INFO-TABLE.
          05 FILLER PIC X(150) VALUE 
             'C00101P510123199001011234张三'.
          05 FILLER PIC X(150) VALUE 
             'C00202C914400001123456789有限公司'.
          05 FILLER PIC X(150) VALUE 
             'C00301P510123199002022345李四'.
          05 FILLER PIC X(150) VALUE 
             'C00402C914400002234567890科技公司'.
          05 FILLER PIC X(150) VALUE 
             'C00501P510123199003033456王五'.
       
       01 CUSTOMER-BASIC-RECORD 
          REDEFINES CUSTOMER-BASIC-INFO-TABLE.
          05 CUSTOMER-BASIC-DATA OCCURS 5.
             10 BASIC-CUST-NO          PIC X(20).
             10 BASIC-CUST-TYP-CD      PIC X(2).
             10 BASIC-CRTF-TYP-CD      PIC X(2).
             10 BASIC-CRTF-NO          PIC X(20).
             10 BASIC-CUST-NM          PIC X(60).
             10 FILLER                 PIC X(46).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-CUST-FOUND             PIC X(1).
             88 WS-CUST-FOUND-Y        VALUE 'Y'.
             88 WS-CUST-FOUND-N        VALUE 'N'.
          05 WS-TEMP-CUST-TYP-CD       PIC X(2).
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CUST-NO                PIC X(20).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-CUST-TYP-CD            PIC X(2).
       
       PROCEDURE DIVISION 
         USING LK-INPUT-DATA, LK-OUTPUT-DATA.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 输入参数验证
           PERFORM VALIDATE-INPUT
           
      * 如果验证通过，执行查询
           IF LK-RETURN-CODE = 0
              PERFORM QUERY-CUST-TYPE
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE SPACES TO LK-CUST-TYP-CD
           MOVE 'N' TO WS-CUST-FOUND
           
      * 复制输入参数到工作存储区
           MOVE LK-CUST-NO TO WS-CUST-NO.
       
       VALIDATE-INPUT.
      * 检查必要输入参数
           IF WS-CUST-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '客户编号不能为空' 
                TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-CUST-TYPE.
           DISPLAY '开始查询客户类型...'
           DISPLAY '查询客户编号: ' WS-CUST-NO
           
      * 查询客户基本信息
           PERFORM QUERY-CUSTOMER-BASIC-INFO
           
      * 检查查询结果并设置返回
           IF WS-CUST-FOUND-Y
      * 设置客户类型代码到输出参数
              MOVE WS-TEMP-CUST-TYP-CD TO LK-CUST-TYP-CD
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
              DISPLAY '客户类型查询完成: ' LK-CUST-TYP-CD
           ELSE
      * 对应Java的F20000异常
              MOVE 20000 TO LK-RETURN-CODE
              MOVE '未找到客户基本信息' 
                TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-CUSTOMER-BASIC-INFO.
           DISPLAY '查询客户基本信息...'
           
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 5
                   
              IF BASIC-CUST-NO(WS-I) = WS-CUST-NO
                 MOVE 'Y' TO WS-CUST-FOUND
                 MOVE BASIC-CUST-TYP-CD(WS-I) 
                   TO WS-TEMP-CUST-TYP-CD
                 
                 DISPLAY '找到客户基本信息:'
                 DISPLAY '  客户编号: ' BASIC-CUST-NO(WS-I)
                 DISPLAY '  客户名称: ' BASIC-CUST-NM(WS-I)
                 DISPLAY '  客户类型: ' WS-TEMP-CUST-TYP-CD
                 
      * 显示客户类型描述
                 EVALUATE WS-TEMP-CUST-TYP-CD
                    WHEN '01'
                       DISPLAY '  类型描述: 个人客户'
                    WHEN '02' 
                       DISPLAY '  类型描述: 企业客户'
                    WHEN '03'
                       DISPLAY '  类型描述: 机构客户'
                    WHEN OTHER
                       DISPLAY '  类型描述: 其他类型'
                 END-EVALUATE
                 
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       END PROGRAM QURYCUSTTYPE.