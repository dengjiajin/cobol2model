      ******************************************************************
      * 程序名称：QURYPERCUSTINFOBYCUSTNO
      * 程序功能：根据客户编号查询个人客户信息
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QURYPERCUSTINFOBYCUSTNO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * 输入参数结构
       01 WS-INPUT-DATA.
          05 WS-CUST-NO                PIC X(20).
          
      * 输出参数结构
       01 WS-OUTPUT-DATA.
          05 WS-RETURN-CODE            PIC 9(4).
          05 WS-RETURN-MESSAGE         PIC X(50).
      * 客户基本信息字段
          05 WS-ADDR                   PIC X(100).
          05 WS-ADMIN-CMPRMNT-CD       PIC X(6).
          05 WS-CAREER-TYP-CD          PIC X(2).
          05 WS-CRTF-MATR-DT           PIC X(8).
          05 WS-CRTF-NO                PIC X(20).
          05 WS-CRTF-TYP-CD            PIC X(2).
          05 WS-CUST-ATTN-EXTT-CD      PIC X(2).
          05 WS-CUST-NM                PIC X(60).
          05 WS-CUST-NO-OUT            PIC X(20).
          05 WS-DOM-OVERS-FLG-CD       PIC X(1).
          05 WS-EMPLY-FLG              PIC X(1).
          05 WS-ETHNIC-CD              PIC X(2).
          05 WS-GENDER-CD              PIC X(1).
          05 WS-ID-CARD-TYP-CD         PIC X(2).
          05 WS-RSVD-MOBILE-NO         PIC X(11).
          05 WS-SPS-CRTF-NO            PIC X(20).
          05 WS-SPS-CRTF-TYP-CD        PIC X(2).
          05 WS-SPS-NAME               PIC X(60).
          05 WS-SPS-TEL-NO             PIC X(20).
          05 WS-STATE-AND-RGN-CD       PIC X(3).
       
      * 客户基本信息表结构
       01 CUSTOMER-BASIC-INFO-TABLE.
          05 FILLER PIC X(200) VALUE 
             'C00101张三0151012319900101123420251231DCN86地址1'.
          05 FILLER PIC X(200) VALUE 
             'C00201李四0251012319900202234520261231FCN86地址2'.
          05 FILLER PIC X(200) VALUE 
             'C00301王五0151012319900303345620271231DCN86地址3'.
       
       01 CUSTOMER-BASIC-RECORD 
          REDEFINES CUSTOMER-BASIC-INFO-TABLE.
          05 CUSTOMER-BASIC-DATA OCCURS 3.
             10 BASIC-CUST-NO          PIC X(20).
             10 BASIC-CUST-TYP-CD      PIC X(2).
             10 BASIC-CUST-NM          PIC X(60).
             10 BASIC-GENDER-CD        PIC X(1).
             10 BASIC-CRTF-NO          PIC X(20).
             10 BASIC-CRTF-TYP-CD      PIC X(2).
             10 BASIC-CRTF-MATR-DT     PIC X(8).
             10 BASIC-DOM-OVERS-FLG-CD PIC X(1).
             10 BASIC-STATE-RGN-CD     PIC X(3).
             10 BASIC-ADDR             PIC X(100).
             10 BASIC-RSVD-MOBILE-NO   PIC X(11).
             10 BASIC-EMPLY-FLG        PIC X(1).
       
      * 个人客户信息表结构
       01 PERSONAL-CUSTOMER-INFO-TABLE.
          05 FILLER PIC X(200) VALUE 
             'C00186教师01汉01地址1配偶1配偶101510123199001011234'.
          05 FILLER PIC X(200) VALUE 
             'C00286工程师02汉01地址2配偶2配偶202510123199002022345'.
          05 FILLER PIC X(200) VALUE 
             'C00386医生03汉01地址3配偶3配偶303510123199003033456'.
       
       01 PERSONAL-CUSTOMER-RECORD 
          REDEFINES PERSONAL-CUSTOMER-INFO-TABLE.
          05 PERSONAL-CUSTOMER-DATA OCCURS 3.
             10 PERSONAL-CUST-NO       PIC X(20).
             10 PERSONAL-ADMIN-CD      PIC X(6).
             10 PERSONAL-CAREER-CD     PIC X(2).
             10 PERSONAL-ETHNIC-CD     PIC X(2).
             10 PERSONAL-GENDER-CD     PIC X(1).
             10 PERSONAL-ADDR          PIC X(100).
             10 PERSONAL-SPS-NAME      PIC X(60).
             10 PERSONAL-SPS-CRTF-TYP  PIC X(2).
             10 PERSONAL-SPS-CRTF-NO   PIC X(20).
             10 PERSONAL-SPS-TEL-NO    PIC X(20).
             10 FILLER                 PIC X(5).
       
      * 临时工作变量
       01 WS-WORK-VARIABLES.
          05 WS-I                      PIC 9(4).
          05 WS-J                      PIC 9(4).
          05 WS-BASIC-FOUND            PIC X(1).
             88 WS-BASIC-FOUND-Y       VALUE 'Y'.
             88 WS-BASIC-FOUND-N       VALUE 'N'.
          05 WS-PERSONAL-FOUND         PIC X(1).
             88 WS-PERSONAL-FOUND-Y    VALUE 'Y'.
             88 WS-PERSONAL-FOUND-N    VALUE 'N'.
       
       LINKAGE SECTION.
      * 输入参数链接节
       01 LK-INPUT-DATA.
          05 LK-CUST-NO                PIC X(20).
           
      * 输出参数链接节
       01 LK-OUTPUT-DATA.
          05 LK-RETURN-CODE            PIC 9(4).
          05 LK-RETURN-MESSAGE         PIC X(50).
      * 客户基本信息字段
          05 LK-ADDR                   PIC X(100).
          05 LK-ADMIN-CMPRMNT-CD       PIC X(6).
          05 LK-CAREER-TYP-CD          PIC X(2).
          05 LK-CRTF-MATR-DT           PIC X(8).
          05 LK-CRTF-NO                PIC X(20).
          05 LK-CRTF-TYP-CD            PIC X(2).
          05 LK-CUST-ATTN-EXTT-CD      PIC X(2).
          05 LK-CUST-NM                PIC X(60).
          05 LK-CUST-NO-OUT            PIC X(20).
          05 LK-DOM-OVERS-FLG-CD       PIC X(1).
          05 LK-EMPLY-FLG              PIC X(1).
          05 LK-ETHNIC-CD              PIC X(2).
          05 LK-GENDER-CD              PIC X(1).
          05 LK-ID-CARD-TYP-CD         PIC X(2).
          05 LK-RSVD-MOBILE-NO         PIC X(11).
          05 LK-SPS-CRTF-NO            PIC X(20).
          05 LK-SPS-CRTF-TYP-CD        PIC X(2).
          05 LK-SPS-NAME               PIC X(60).
          05 LK-SPS-TEL-NO             PIC X(20).
          05 LK-STATE-AND-RGN-CD       PIC X(3).
       
       PROCEDURE DIVISION 
         USING LK-INPUT-DATA, LK-OUTPUT-DATA.
       
       MAIN-PROCESS.
      * 初始化
           PERFORM INITIALIZE-PROGRAM
           
      * 输入参数验证
           PERFORM VALIDATE-INPUT
           
      * 如果验证通过，执行查询
           IF LK-RETURN-CODE = 0
              PERFORM QUERY-CUST-INFO
           END-IF
           
           GOBACK.
       
       INITIALIZE-PROGRAM.
      * 初始化输出参数
           MOVE 0 TO LK-RETURN-CODE
           MOVE SPACES TO LK-RETURN-MESSAGE
           MOVE SPACES TO LK-ADDR
           MOVE SPACES TO LK-ADMIN-CMPRMNT-CD
           MOVE SPACES TO LK-CAREER-TYP-CD
           MOVE SPACES TO LK-CRTF-MATR-DT
           MOVE SPACES TO LK-CRTF-NO
           MOVE SPACES TO LK-CRTF-TYP-CD
           MOVE SPACES TO LK-CUST-ATTN-EXTT-CD
           MOVE SPACES TO LK-CUST-NM
           MOVE SPACES TO LK-CUST-NO-OUT
           MOVE SPACES TO LK-DOM-OVERS-FLG-CD
           MOVE SPACES TO LK-EMPLY-FLG
           MOVE SPACES TO LK-ETHNIC-CD
           MOVE SPACES TO LK-GENDER-CD
           MOVE SPACES TO LK-ID-CARD-TYP-CD
           MOVE SPACES TO LK-RSVD-MOBILE-NO
           MOVE SPACES TO LK-SPS-CRTF-NO
           MOVE SPACES TO LK-SPS-CRTF-TYP-CD
           MOVE SPACES TO LK-SPS-NAME
           MOVE SPACES TO LK-SPS-TEL-NO
           MOVE SPACES TO LK-STATE-AND-RGN-CD
           
      * 复制输入参数到工作存储区
           MOVE LK-CUST-NO TO WS-CUST-NO
           MOVE 'N' TO WS-BASIC-FOUND
           MOVE 'N' TO WS-PERSONAL-FOUND.
       
       VALIDATE-INPUT.
      * 检查必要输入参数（对应Java的CheckParaUtil.checkInputForEmpty）
           IF WS-CUST-NO = SPACES
              MOVE 1001 TO LK-RETURN-CODE
              MOVE '客户编号不能为空' 
                TO LK-RETURN-MESSAGE
           END-IF.
       
       QUERY-CUST-INFO.
           DISPLAY '开始查询个人客户信息...'
           DISPLAY '查询客户编号: ' WS-CUST-NO
           
      * 第一步：查询客户基本信息
           PERFORM QUERY-CUSTOMER-BASIC-INFO
           
      * 第二步：查询个人客户信息
           IF WS-BASIC-FOUND-Y
              PERFORM QUERY-PERSONAL-CUSTOMER-INFO
           END-IF
           
      * 第三步：检查查询结果并设置返回
           IF WS-BASIC-FOUND-N
      * 对应Java的F20000异常
              MOVE 20000 TO LK-RETURN-CODE
              MOVE '未找到客户基本信息' 
                TO LK-RETURN-MESSAGE
           ELSE
              IF WS-PERSONAL-FOUND-N
      * 对应Java的F20001异常
                 MOVE 20001 TO LK-RETURN-CODE
                 MOVE '未找到个人客户信息' 
                   TO LK-RETURN-MESSAGE
              ELSE
                 MOVE 0 TO LK-RETURN-CODE
                 MOVE '查询成功' TO LK-RETURN-MESSAGE
                 DISPLAY '客户信息查询完成'
              END-IF
           END-IF.
       
       QUERY-CUSTOMER-BASIC-INFO.
           DISPLAY '查询客户基本信息...'
           
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 3
                   
              IF BASIC-CUST-NO(WS-I) = WS-CUST-NO
                 MOVE 'Y' TO WS-BASIC-FOUND
                 
      * 复制基本信息到输出参数
                 MOVE BASIC-CUST-NO(WS-I) 
                   TO LK-CUST-NO-OUT
                 MOVE BASIC-CUST-NM(WS-I) 
                   TO LK-CUST-NM
                 MOVE BASIC-GENDER-CD(WS-I) 
                   TO LK-GENDER-CD
                 MOVE BASIC-CRTF-NO(WS-I) 
                   TO LK-CRTF-NO
                 MOVE BASIC-CRTF-TYP-CD(WS-I) 
                   TO LK-CRTF-TYP-CD
                 MOVE BASIC-CRTF-MATR-DT(WS-I) 
                   TO LK-CRTF-MATR-DT
                 MOVE BASIC-DOM-OVERS-FLG-CD(WS-I) 
                   TO LK-DOM-OVERS-FLG-CD
                 MOVE BASIC-STATE-RGN-CD(WS-I) 
                   TO LK-STATE-AND-RGN-CD
                 MOVE BASIC-ADDR(WS-I) 
                   TO LK-ADDR
                 MOVE BASIC-RSVD-MOBILE-NO(WS-I) 
                   TO LK-RSVD-MOBILE-NO
                 MOVE BASIC-EMPLY-FLG(WS-I) 
                   TO LK-EMPLY-FLG
                 
                 DISPLAY '找到客户基本信息:'
                 DISPLAY '  客户名称: ' LK-CUST-NM
                 DISPLAY '  证件号码: ' LK-CRTF-NO
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       QUERY-PERSONAL-CUSTOMER-INFO.
           DISPLAY '查询个人客户信息...'
           
           PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > 3
                   
              IF PERSONAL-CUST-NO(WS-J) = WS-CUST-NO
                 MOVE 'Y' TO WS-PERSONAL-FOUND
                 
      * 复制个人客户信息到输出参数
                 MOVE PERSONAL-ADMIN-CD(WS-J) 
                   TO LK-ADMIN-CMPRMNT-CD
                 MOVE PERSONAL-CAREER-CD(WS-J) 
                   TO LK-CAREER-TYP-CD
                 MOVE PERSONAL-ETHNIC-CD(WS-J) 
                   TO LK-ETHNIC-CD
                 MOVE PERSONAL-SPS-NAME(WS-J) 
                   TO LK-SPS-NAME
                 MOVE PERSONAL-SPS-CRTF-TYP(WS-J) 
                   TO LK-SPS-CRTF-TYP-CD
                 MOVE PERSONAL-SPS-CRTF-NO(WS-J) 
                   TO LK-SPS-CRTF-NO
                 MOVE PERSONAL-SPS-TEL-NO(WS-J) 
                   TO LK-SPS-TEL-NO
                 
      * 身份证类型代码（从证件类型代码映射）
                 MOVE LK-CRTF-TYP-CD TO LK-ID-CARD-TYP-CD
                 
                 DISPLAY '找到个人客户信息:'
                 DISPLAY '  职业类型: ' LK-CAREER-TYP-CD
                 DISPLAY '  配偶姓名: ' LK-SPS-NAME
                 EXIT PERFORM
              END-IF
           END-PERFORM.
       
       END PROGRAM QURYPERCUSTINFOBYCUSTNO.