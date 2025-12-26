      ******************************************************************
      * 程序名称：QURYOVSCASHWITHDRRECTRFLG
      * 程序功能：查询境外取现控制标志
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYOVSCASHWITHDRRECTRFLG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CUST-NO                PIC X(20).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-BLKLIST-CUST-FLG       PIC X(1).
       
      * 客户基本信息表结构
       01 CUSTOMER-BASIC-INFO-TABLE.
          05 FILLER PIC X(150) VALUE 
             'C00101510123199001011234张三'.
          05 FILLER PIC X(150) VALUE 
             'C00202510123199002022345李四'.
          05 FILLER PIC X(150) VALUE 
             'C00301510123199003033456王五'.
       
       01 CUSTOMER-BASIC-RECORD 
          REDEFINES CUSTOMER-BASIC-INFO-TABLE.
          05 CUSTOMER-BASIC-DATA OCCURS 3.
             10 BASIC-CUST-NO          PIC X(20).
             10 BASIC-CRTF-TYP-CD      PIC X(2).
             10 BASIC-CRTF-NO          PIC X(20).
             10 BASIC-CUST-NM          PIC X(60).
             10 FILLER                 PIC X(48).
       
      * 境外取现黑名单信息表结构
       01 OVS-CASH-WITHDR-BLK-TABLE.
          05 FILLER PIC X(80) VALUE 
             '51012319900101123401'.
          05 FILLER PIC X(80) VALUE 
             '51012319900303345601'.
          05 FILLER PIC X(80) VALUE 
             '51012319900505456701'.
       
       01 OVS-CASH-WITHDR-BLK-RECORD 
          REDEFINES OVS-CASH-WITHDR-BLK-TABLE.
          05 OVS-CASH-WITHDR-DATA OCCURS 3.
             10 OVS-CRTF-NO            PIC X(20).
             10 OVS-CRTF-TYP-CD        PIC X(2).
             10 OVS-VALID-FLG          PIC X(1).
             10 FILLER                 PIC X(57).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-J                      PIC 9(4).
          05 WS-CUST-BASIC-FOUND       PIC X(1).
             88 WS-CUST-BASIC-FOUND-Y  VALUE 'Y'.
             88 WS-CUST-BASIC-FOUND-N  VALUE 'N'.
          05 WS-OVS-BLK-FOUND          PIC X(1).
             88 WS-OVS-BLK-FOUND-Y     VALUE 'Y'.
             88 WS-OVS-BLK-FOUND-N     VALUE 'N'.
          05 WS-TEMP-CRTF-NO           PIC X(20).
          05 WS-TEMP-CRTF-TYP-CD       PIC X(2).
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CUST-NO                PIC X(20).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-BLKLIST-CUST-FLG       PIC X(1).
       
       PROCEDURE DIVISION 
         USING LK-INPUT-DATA, LK-OUTPUT-DATA.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 输入参数验证
           PERFORM VALIDATE-INPUT
           
      * 如果验证通过，执行查询
           IF LK-RETURN-CODE = 0
              PERFORM QUERY-OVS-CASH-WITHDR-CTRL
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE 'N' TO LK-BLKLIST-CUST-FLG
           MOVE 'N' TO WS-CUST-BASIC-FOUND
           MOVE 'N' TO WS-OVS-BLK-FOUND
           
      * 复制输入参数到工作存储区
           MOVE LK-CUST-NO TO WS-CUST-NO.
       
       VALIDATE-INPUT.
      * 检查必要输入参数
           IF WS-CUST-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '客户编号不能为空' 
                TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-OVS-CASH-WITHDR-CTRL.
           DISPLAY '开始查询境外取现控制标志...'
           DISPLAY '查询客户编号: ' WS-CUST-NO
           
      * 第一步：查询客户基本信息
           PERFORM QUERY-CUSTOMER-BASIC-INFO
           
      * 第二步：如果找到客户基本信息，查询境外取现黑名单
           IF WS-CUST-BASIC-FOUND-Y
              PERFORM QUERY-OVS-CASH-WITHDR-BLK
           ELSE
      * 对应Java的F20000异常
              MOVE 20000 TO LK-RETURN-CODE
              MOVE '未找到客户基本信息' 
                TO LK-RETURN-MESSAGE
           END-IF
           
      * 第三步：设置黑名单标志
           IF WS-OVS-BLK-FOUND-Y
              MOVE '0' TO LK-BLKLIST-CUST-FLG
              DISPLAY '客户存在境外取现黑名单记录'
           ELSE
              MOVE 'N' TO LK-BLKLIST-CUST-FLG
              DISPLAY '客户不存在境外取现黑名单记录'
           END-IF
           
      * 设置成功返回
           IF LK-RETURN-CODE = 0
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-CUSTOMER-BASIC-INFO.
           DISPLAY '查询客户基本信息...'
           
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 3
                   
              IF BASIC-CUST-NO(WS-I) = WS-CUST-NO
                 MOVE 'Y' TO WS-CUST-BASIC-FOUND
                 MOVE BASIC-CRTF-NO(WS-I) 
                   TO WS-TEMP-CRTF-NO
                 MOVE BASIC-CRTF-TYP-CD(WS-I) 
                   TO WS-TEMP-CRTF-TYP-CD
                 
                 DISPLAY '找到客户基本信息:'
                 DISPLAY '  客户名称: ' BASIC-CUST-NM(WS-I)
                 DISPLAY '  证件号码: ' WS-TEMP-CRTF-NO
                 DISPLAY '  证件类型: ' WS-TEMP-CRTF-TYP-CD
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       QUERY-OVS-CASH-WITHDR-BLK.
           DISPLAY '查询境外取现黑名单信息...'
           DISPLAY '查询条件 - 证件号码: ' WS-TEMP-CRTF-NO
                   ', 证件类型: ' WS-TEMP-CRTF-TYP-CD
           
           PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > 3
                   
              IF OVS-CRTF-NO(WS-J) = WS-TEMP-CRTF-NO AND
                 OVS-CRTF-TYP-CD(WS-J) = WS-TEMP-CRTF-TYP-CD AND
                 OVS-VALID-FLG(WS-J) = '1'
                 
                 MOVE 'Y' TO WS-OVS-BLK-FOUND
                 DISPLAY '找到境外取现黑名单记录'
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       END PROGRAM QURYOVSCASHWITHDRRECTRFLG.