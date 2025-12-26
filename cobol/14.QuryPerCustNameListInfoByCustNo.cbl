      ******************************************************************
      * 程序名称：QURYPERCUSTNAMELIST
      * 程序功能：对私客户名单信息查询
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYPERCUSTNAMELIST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CUST-NO                PIC X(20).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
          05 WS-NAME-LIST-COUNT        PIC 9(4).
          05 WS-NAME-LIST-TABLE OCCURS 10 
             DEPENDING ON WS-NAME-LIST-COUNT
             INDEXED BY WS-NAME-LIST-INDEX.
             10 WS-NAME-LIST-INFO.
                15 WS-CUST-NO-OUT      PIC X(20).
                15 WS-CRTF-TYP-CD      PIC X(2).
                15 WS-CRTF-NO          PIC X(20).
                15 WS-NM-SNGL-TYP-CD   PIC X(2).
                15 WS-DATA-SORC-CD     PIC X(2).
                15 WS-ORG-DISMN-CD     PIC X(2).
                15 WS-CTRL-FLG         PIC X(1).
                15 WS-CHK-FLG-CD       PIC X(1).
                15 WS-EFFT-DT          PIC X(8).
                15 WS-EFFT-TM          PIC X(6).
                15 WS-INVALID-DT       PIC X(8).
                15 WS-INVALID-TM       PIC X(6).
                15 WS-VALID-FLG        PIC X(1).
       
      * 个人客户名单信息表结构
       01 PERSONAL-CUSTOMER-LIST-TABLE.
          05 FILLER PIC X(100) VALUE 
             'C00101510123199001011234BL01Y20250101090000'.
          05 FILLER PIC X(100) VALUE 
             'C00102510123199001011234WL02N20250201100000'.
          05 FILLER PIC X(100) VALUE 
             'C00201510123199002022345GL01Y20250301110000'.
          05 FILLER PIC X(100) VALUE 
             'C00301510123199003033456BL01Y20250401120000'.
       
       01 PERSONAL-CUSTOMER-LIST-RECORD 
          REDEFINES PERSONAL-CUSTOMER-LIST-TABLE.
          05 CUSTOMER-LIST-DATA OCCURS 4.
             10 LIST-CUST-NO           PIC X(20).
             10 LIST-CRTF-TYP-CD       PIC X(2).
             10 LIST-CRTF-NO           PIC X(20).
             10 LIST-NM-SNGL-TYP-CD    PIC X(2).
             10 LIST-DATA-SORC-CD      PIC X(2).
             10 LIST-ORG-DISMN-CD      PIC X(2).
             10 LIST-CTRL-FLG          PIC X(1).
             10 LIST-CHK-FLG-CD        PIC X(1).
             10 LIST-EFFT-DT           PIC X(8).
             10 LIST-EFFT-TM           PIC X(6).
             10 LIST-INVALID-DT        PIC X(8).
             10 LIST-INVALID-TM        PIC X(6).
             10 LIST-VALID-FLG         PIC X(1).
             10 FILLER                 PIC X(16).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-TEMP-COUNT             PIC 9(4).
          05 WS-DATA-FOUND             PIC X(1).
             88 WS-DATA-FOUND-Y        VALUE 'Y'.
             88 WS-DATA-FOUND-N        VALUE 'N'.
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CUST-NO                PIC X(20).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
          05 LK-NAME-LIST-COUNT        PIC 9(4).
          05 LK-NAME-LIST-TABLE OCCURS 10 
             DEPENDING ON LK-NAME-LIST-COUNT
             INDEXED BY LK-NAME-LIST-INDEX.
             10 LK-NAME-LIST-INFO.
                15 LK-CUST-NO-OUT      PIC X(20).
                15 LK-CRTF-TYP-CD      PIC X(2).
                15 LK-CRTF-NO          PIC X(20).
                15 LK-NM-SNGL-TYP-CD   PIC X(2).
                15 LK-DATA-SORC-CD     PIC X(2).
                15 LK-ORG-DISMN-CD     PIC X(2).
                15 LK-CTRL-FLG         PIC X(1).
                15 LK-CHK-FLG-CD       PIC X(1).
                15 LK-EFFT-DT          PIC X(8).
                15 LK-EFFT-TM          PIC X(6).
                15 LK-INVALID-DT       PIC X(8).
                15 LK-INVALID-TM       PIC X(6).
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
              PERFORM QUERY-CUST-NAME-LIST
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE 0 TO LK-NAME-LIST-COUNT
           MOVE 'N' TO WS-DATA-FOUND
           
      * 复制输入参数到工作存储区
           MOVE LK-CUST-NO TO WS-CUST-NO.
       
       VALIDATE-INPUT.
      * 检查必要输入参数
           IF WS-CUST-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '客户编号不能为空' 
                TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-CUST-NAME-LIST.
           DISPLAY '开始查询对私客户名单信息...'
           DISPLAY '查询客户编号: ' WS-CUST-NO
           
      * 初始化计数器
           MOVE 0 TO WS-TEMP-COUNT
           
      * 查询个人客户名单信息
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 4
                   
              IF LIST-CUST-NO(WS-I) = WS-CUST-NO
                 MOVE 'Y' TO WS-DATA-FOUND
                 ADD 1 TO WS-TEMP-COUNT
                 
      * 复制数据到输出表
                 MOVE LIST-CUST-NO(WS-I)
                   TO LK-CUST-NO-OUT(WS-TEMP-COUNT)
                 MOVE LIST-CRTF-TYP-CD(WS-I)
                   TO LK-CRTF-TYP-CD(WS-TEMP-COUNT)
                 MOVE LIST-CRTF-NO(WS-I)
                   TO LK-CRTF-NO(WS-TEMP-COUNT)
                 MOVE LIST-NM-SNGL-TYP-CD(WS-I)
                   TO LK-NM-SNGL-TYP-CD(WS-TEMP-COUNT)
                 MOVE LIST-DATA-SORC-CD(WS-I)
                   TO LK-DATA-SORC-CD(WS-TEMP-COUNT)
                 MOVE LIST-ORG-DISMN-CD(WS-I)
                   TO LK-ORG-DISMN-CD(WS-TEMP-COUNT)
                 MOVE LIST-CTRL-FLG(WS-I)
                   TO LK-CTRL-FLG(WS-TEMP-COUNT)
                 MOVE LIST-CHK-FLG-CD(WS-I)
                   TO LK-CHK-FLG-CD(WS-TEMP-COUNT)
                 MOVE LIST-EFFT-DT(WS-I)
                   TO LK-EFFT-DT(WS-TEMP-COUNT)
                 MOVE LIST-EFFT-TM(WS-I)
                   TO LK-EFFT-TM(WS-TEMP-COUNT)
                 MOVE LIST-INVALID-DT(WS-I)
                   TO LK-INVALID-DT(WS-TEMP-COUNT)
                 MOVE LIST-INVALID-TM(WS-I)
                   TO LK-INVALID-TM(WS-TEMP-COUNT)
                 MOVE LIST-VALID-FLG(WS-I)
                   TO LK-VALID-FLG(WS-TEMP-COUNT)
                 
                 DISPLAY '找到名单信息记录 ' WS-TEMP-COUNT ':'
                 DISPLAY '  证件类型: ' 
                         LK-CRTF-TYP-CD(WS-TEMP-COUNT)
                 DISPLAY '  证件号码: ' 
                         LK-CRTF-NO(WS-TEMP-COUNT)
                 DISPLAY '  名单类型: ' 
                         LK-NM-SNGL-TYP-CD(WS-TEMP-COUNT)
                 DISPLAY '  控制标志: ' 
                         LK-CTRL-FLG(WS-TEMP-COUNT)
              END-IF
           END-PERFORM
           
      * 设置返回的记录数
           MOVE WS-TEMP-COUNT TO LK-NAME-LIST-COUNT
           
      * 检查查询结果
           IF WS-DATA-FOUND-N
      * 对应Java中的F20003异常
              MOVE 20003 TO LK-RETURN-CODE
              MOVE '未找到客户名单信息' 
                TO LK-RETURN-MESSAGE
           ELSE
              MOVE 0 TO LK-RETURN-CODE
              MOVE '查询成功' TO LK-RETURN-MESSAGE
              DISPLAY '共找到 ' LK-NAME-LIST-COUNT 
                      ' 条名单信息记录'
           END-IF.
       
       END PROGRAM QURYPERCUSTNAMELIST.