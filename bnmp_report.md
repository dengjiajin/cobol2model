# BNMP 2.0 业务模型分析报告

基于HSBC ECIF COBOL代码分析，按照BNMP 2.0规范提取的业务模型。

## 概述统计

| 元素类型 | 数量 |
|---------|------|
| 实体属性 | 111 |
| 业务实体 | 8 |
| 业务流程 | 16 |
| 任务步骤 | 77 |
| 流程任务 | 45 |

## 一、数据模型（业务实体）

### CUSTOMER_BASIC_INFO (客户基本信息)

存储客户的基本信息，包括客户号、客户名称、证件信息、联系方式、地址等核心客户数据，支持个人客户和企业客户类型

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| TENANT_NO | 租户编号 | VARCHAR | 10 | 必填，多租户标识 |
| CUST_NO | 客户编号 | VARCHAR | 20 | 主键，必填 |
| CUST_TYP_CD | 客户类型代码 | VARCHAR | 2 | 01-个人客户，02-企业客户，03-机构客户 |
| CUST_LVL_CD | 客户等级代码 | VARCHAR | 2 | 客户分级标识 |
| CUST_NM | 客户名称 | VARCHAR | 60 | 必填 |
| CUST_ENG_NM | 客户英文名称 | VARCHAR | 60 | 可选 |
| CRTF_TYP_CD | 证件类型代码 | VARCHAR | 2 | 01-身份证，02-护照等 |
| CRTF_NO | 证件号码 | VARCHAR | 20 | 必填 |
| CRTF_GRANT_DT | 证件发放日期 | DATE | 10 | 格式yyyy-mm-dd |
| CRTF_MATR_DT | 证件到期日期 | DATE | 10 | 格式yyyy-mm-dd |
| STATE_AND_RGN_CD | 国家和地区代码 | VARCHAR | 3 | 国际标准代码 |
| ADDR | 地址 | VARCHAR | 100 | 联系地址 |
| RSVD_MOBILE_NO | 预留手机号码 | VARCHAR | 15 | 主要联系电话 |
| MOBILE_NO | 手机号码 | VARCHAR | 15 | 备用联系电话 |
| E_MAIL | 电子邮箱 | VARCHAR | 50 | 邮箱格式验证 |
| EMPLY_FLG | 员工标志 | VARCHAR | 1 | 1-是员工，0-非员工 |
| VALID_FLG | 有效标志 | VARCHAR | 1 | 1-有效，0-无效 |
| CRT_TELR_NO | 创建柜员号 | VARCHAR | 10 | 创建人标识 |
| UPD_TELR_NO | 更新柜员号 | VARCHAR | 10 | 最后更新人 |
| CRT_TM | 创建时间 | TIMESTAMP | 26 | 系统自动生成 |
| UPD_TM | 更新时间 | TIMESTAMP | 26 | 系统自动更新 |

### PERSONAL_CUSTOMER_INFO (个人客户信息)

存储个人客户的扩展信息，包括性别、出生日期、职业、婚姻状况、配偶信息、工作单位等个人专属数据，与客户基本信息表关联

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| TENANT_NO | 租户编号 | VARCHAR | 10 | 必填 |
| CUST_NO | 客户编号 | VARCHAR | 20 | 主键，外键关联CUSTOMER_BASIC_INFO [FK] |
| GENDER_CD | 性别代码 | VARCHAR | 1 | 1-男，2-女 |
| BIRTH_DT | 出生日期 | DATE | 8 | 格式yyyymmdd |
| CAREER_TYP_CD | 职业类型代码 | VARCHAR | 2 | 职业分类代码 |
| ETHNIC_CD | 民族代码 | VARCHAR | 2 | 民族标识 |
| MARRG_SITUATION_CD | 婚姻状况代码 | VARCHAR | 1 | 1-已婚，2-未婚，3-离异 |
| ADMIN_CMPRMNT_CD | 行政区划代码 | VARCHAR | 6 | 标准行政区划 |
| DOM_OVERS_FLG_CD | 境内外标志代码 | VARCHAR | 1 | D-境内，F-境外 |
| IDCARD_TYP_CD | 身份证类型代码 | VARCHAR | 2 | 身份证件类型 |
| ADDR | 地址 | VARCHAR | 100 | 居住地址 |
| HOUSDRGST_ADDR | 户籍地址 | VARCHAR | 100 | 户口所在地 |
| SPS_NAME | 配偶姓名 | VARCHAR | 60 | 可选 |
| SPS_ENG_NAME | 配偶英文姓名 | VARCHAR | 60 | 可选 |
| SPS_CRTF_TYP_CD | 配偶证件类型代码 | VARCHAR | 2 | 可选 |
| SPS_CRTF_NO | 配偶证件号码 | VARCHAR | 20 | 可选 |
| SPS_TEL_NO | 配偶电话号码 | VARCHAR | 20 | 可选 |
| WORK_UNIT_NM | 工作单位名称 | VARCHAR | 50 | 可选 |
| WORK_UNIT_ADDR | 工作单位地址 | VARCHAR | 100 | 可选 |
| SHRHD_FLG | 股东标志 | VARCHAR | 1 | 1-是股东，0-非股东 |
| VALID_FLG | 有效标志 | VARCHAR | 1 | 1-有效，0-无效 |
| CRT_TELR_NO | 创建柜员号 | VARCHAR | 10 | 创建人标识 |
| UPD_TELR_NO | 更新柜员号 | VARCHAR | 10 | 最后更新人 |
| CRT_TM | 创建时间 | TIMESTAMP | 26 | 系统自动生成 |
| UPD_TM | 更新时间 | TIMESTAMP | 26 | 系统自动更新 |

### CUST_ACCT_INFO (客户账户路由信息)

存储客户账户与产品的路由关系信息，用于客户归并、账户关联查询，支持多种路由类型和产品映射

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| ID | 主键ID | BIGINT | 18 | 自增主键 |
| TENANT_NO | 租户编号 | VARCHAR | 20 | 必填 |
| CUST_NO | 客户编号 | VARCHAR | 20 | 必填，外键关联CUSTOMER_BASIC_INFO [FK] |
| AFS_PRODT_NO | 可售产品编号 | VARCHAR | 20 | 产品标识 |
| BASE_PRODT_NO | 基础产品编号 | VARCHAR | 20 | 基础产品标识 |
| MAIN_ACCT_NO | 主账号 | VARCHAR | 30 | 账户主键 |
| OPER_TYP_CD | 操作类型代码 | VARCHAR | 10 | 01-新增，02-修改，03-删除 |
| RELA_SEQ_NO | 关联序号 | VARCHAR | 20 | 关系排序 |
| ROUTE_TYP_CD | 路由类型代码 | VARCHAR | 10 | 路由分类 |
| ROUTE_VAL | 路由值 | VARCHAR | 50 | 路由键值 |
| VALID_FLG | 有效标志 | VARCHAR | 1 | 1-有效，0-无效 |
| CRT_TELR_NO | 创建柜员号 | VARCHAR | 20 | 创建人 |
| UPD_TELR_NO | 更新柜员号 | VARCHAR | 20 | 更新人 |
| CRT_TM | 创建时间 | TIMESTAMP | 26 | 系统生成 |
| UPD_TM | 更新时间 | TIMESTAMP | 26 | 系统更新 |

### CUSTOMER_RISK_INFO (客户风险等级信息)

存储客户的风险等级评定信息，包括关注程度、评定日期、评定机构、评定说明等风控数据

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| CUST_NO | 客户编号 | VARCHAR | 20 | 主键，外键关联CUSTOMER_BASIC_INFO [FK] |
| CUST_TYP_CD | 客户类型代码 | VARCHAR | 2 | 客户类型 |
| CUST_ATTN_EXTT_CD | 客户关注程度代码 | VARCHAR | 2 | H-高，M-中，L-低 |
| EVALT_DT | 评定日期 | DATE | 8 | 格式yyyymmdd |
| RELS_DT | 发布日期 | DATE | 8 | 格式yyyymmdd |
| RELS_OR_ISU_ORG_NO | 发布或签发机构编号 | VARCHAR | 20 | 机构标识 |
| EVALT_ACRDGAS_COMNT | 评定依据说明 | VARCHAR | 100 | 评定理由 |

### OVS_CASH_WITHDR_BLK (境外取现黑名单)

存储境外取现受限客户的黑名单信息，用于境外现金提取控制标志查询

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| CRTF_NO | 证件号码 | VARCHAR | 20 | 主键 |
| CRTF_TYP_CD | 证件类型代码 | VARCHAR | 2 | 证件类型 |
| VALID_FLG | 有效标志 | VARCHAR | 1 | 1-有效，0-无效 |

### CUST_CHNL_TXN_COMMOND (客户交易渠道控制信息)

存储客户在各交易渠道的限额控制信息，包括日/月/季/年累计限额、单笔限额、允许终端类型等

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| CUST_NO | 客户编号 | VARCHAR | 20 | 外键关联CUSTOMER_BASIC_INFO [FK] |
| TENANT_NO | 租户编号 | VARCHAR | 10 | 多租户标识 |
| PMIT_TERMINAL_TYP_CD | 允许终端类型代码 | VARCHAR | 2 | 终端类型 |
| LMT_TYP_CD | 限额类型代码 | VARCHAR | 2 | 限额分类 |
| SGL_TX_HIGH_AMT | 单笔最高金额 | DECIMAL | 12,2 | 单笔上限 |
| SGL_TX_LOWEST_AMT | 单笔最低金额 | DECIMAL | 12,2 | 单笔下限 |
| DAY_ACCM_MAX_TX_AMT | 日累计最大交易金额 | DECIMAL | 12,2 | 日限额 |
| DAY_ACCM_MAX_TX_STKCNT | 日累计最大交易笔数 | INTEGER | 5 | 日笔数限制 |
| MON_ACCM_MAX_TX_AMT | 月累计最大交易金额 | DECIMAL | 12,2 | 月限额 |
| MON_ACCM_MAX_TX_STKCNT | 月累计最大交易笔数 | INTEGER | 5 | 月笔数限制 |
| QT_ACCM_MAX_TX_AMT | 季累计最大交易金额 | DECIMAL | 12,2 | 季限额 |
| QT_ACCM_MAX_TX_STKCNT | 季累计最大交易笔数 | INTEGER | 5 | 季笔数限制 |
| YR_ACCM_MAX_TX_AMT | 年累计最大交易金额 | DECIMAL | 12,2 | 年限额 |
| YR_ACCM_MAX_TX_STKCNT | 年累计最大交易笔数 | INTEGER | 5 | 年笔数限制 |
| RSN | 原因说明 | VARCHAR | 100 | 限额设置原因 |
| VALID_FLG | 有效标志 | VARCHAR | 1 | 1-有效，0-无效 |

### PERSONAL_CUSTOMER_LIST (个人客户名单信息)

存储个人客户的名单分类信息，支持黑名单、白名单、灰名单等多种名单类型管理

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| CUST_NO | 客户编号 | VARCHAR | 20 | 外键关联CUSTOMER_BASIC_INFO [FK] |
| CRTF_TYP_CD | 证件类型代码 | VARCHAR | 2 | 证件类型 |
| CRTF_NO | 证件号码 | VARCHAR | 20 | 证件号 |
| NM_SNGL_TYP_CD | 名单类型代码 | VARCHAR | 2 | BL-黑名单，WL-白名单，GL-灰名单 |
| DATA_SORC_CD | 数据来源代码 | VARCHAR | 2 | 数据来源标识 |
| ORG_DISMN_CD | 机构维度代码 | VARCHAR | 2 | 机构维度 |
| CTRL_FLG | 控制标志 | VARCHAR | 1 | Y-控制，N-不控制 |
| CHK_FLG_CD | 核查标志代码 | VARCHAR | 1 | 核查状态 |
| EFFT_DT | 生效日期 | DATE | 8 | 格式yyyymmdd |
| EFFT_TM | 生效时间 | TIME | 6 | 格式hhmmss |
| INVALID_DT | 失效日期 | DATE | 8 | 格式yyyymmdd |
| INVALID_TM | 失效时间 | TIME | 6 | 格式hhmmss |
| VALID_FLG | 有效标志 | VARCHAR | 1 | 1-有效，0-无效 |

### SIGN_RELATION_INFO (签约关系信息)

存储客户的各类签约关系信息，包括电子银行签约、手机银行签约、网上支付签约等

| 字段名 | 中文名 | 类型 | 长度 | 规则 |
|-------|-------|------|-----|------|
| SIGN_NO | 签约编号 | VARCHAR | 20 | 主键 |
| CRTF_NO | 证件号码 | VARCHAR | 20 | 客户证件号 |
| CRTF_TYP_CD | 证件类型代码 | VARCHAR | 2 | 证件类型 |
| CUST_ACCT_NO | 客户账号 | VARCHAR | 20 | 签约账号 |
| CUST_NM | 客户名称 | VARCHAR | 60 | 签约客户名 |
| SIGN_SMLTYP_TYP_CD | 签约小类类型代码 | VARCHAR | 4 | 签约分类 |
| SIGN_TYPE | 签约类型 | VARCHAR | 2 | 签约大类 |
| SIGN_STATUS | 签约状态 | VARCHAR | 1 | A-有效，I-无效 |
| SIGN_DATE | 签约日期 | DATE | 8 | 格式yyyymmdd |
| SIGN_AMOUNT | 签约金额 | DECIMAL | 12,2 | 签约额度 |
| SIGN_DESC | 签约描述 | VARCHAR | 50 | 签约说明 |

## 二、流程模型（业务流程）

### 客户归并处理

- **流程名称**: 客户归并
- **COBOL文件**: 1.ConsolidationCust.cbl
- **程序ID**: CUSTMRG01
- **描述**: 将并出客户的账户路由信息归并到并入客户名下，实现客户信息合并

#### 任务1: 参数基础校验

业务逻辑: 校验并入客户号和并出客户号是否为空

- **步骤1.1**: 校验并入客户号
  - 逻辑: 检查REQ-CUST-NO是否为空
- **步骤1.2**: 校验并出客户号
  - 逻辑: 检查REQ-MERGE-CUST-NO是否为空

#### 任务2: 验证客户是否存在

业务逻辑: 验证并入客户和并出客户是否在系统中存在且有效

- **步骤2.1**: 检查并入客户
  - 逻辑: 查询CUST_ACCT_INFO表验证并入客户是否存在
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: CUST_NO, TENANT_NO, VALID_FLG
- **步骤2.2**: 检查并出客户
  - 逻辑: 查询CUST_ACCT_INFO表验证并出客户是否存在
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: CUST_NO, TENANT_NO, VALID_FLG

#### 任务3: 执行客户归并操作

业务逻辑: 将并出客户的账户路由信息更新为并入客户号

- **步骤3.1**: 更新账户路由
  - 逻辑: 将并出客户的CUST_NO更新为并入客户号
  - 操作类型: UPDATE
  - 数据操作: UPDATE on CUST_ACCT_INFO: CUST_NO, UPD_TELR_NO, UPD_TM

#### 任务4: 检查路由冲突

业务逻辑: 检查归并后是否存在路由值冲突的记录

- **步骤4.1**: 检测路由冲突
  - 逻辑: 查询是否存在相同路由值的记录
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: ROUTE_VAL, ROUTE_TYP_CD, RELA_SEQ_NO

### 按账号列表客户归并

- **流程名称**: 按账号客户归并
- **COBOL文件**: 2.ConsolidationCustByAcctNo.cbl
- **程序ID**: CUSTMRG02
- **描述**: 根据指定的账号列表，将并出客户的特定账户归并到并入客户名下

#### 任务1: 参数基础校验

业务逻辑: 校验必填参数是否完整

- **步骤1.1**: 校验并入客户号
  - 逻辑: 检查REQ-CUST-NO是否为空
- **步骤1.2**: 校验并出客户号
  - 逻辑: 检查REQ-MERGE-CUST-NO是否为空
- **步骤1.3**: 校验路由类型
  - 逻辑: 检查REQ-ROUTE-TYP-CD是否为空
- **步骤1.4**: 校验账号列表
  - 逻辑: 检查REQ-ACCT-COUNT是否大于0

#### 任务2: 按账号批量归并

业务逻辑: 遍历账号列表，逐个更新账户路由信息

- **步骤2.1**: 更新账户路由
  - 逻辑: 按ROUTE_VAL和ROUTE_TYP_CD更新CUST_NO
  - 操作类型: UPDATE
  - 数据操作: UPDATE on CUST_ACCT_INFO: CUST_NO, UPD_TELR_NO, UPD_TM

### 创建个人客户信息（简版）

- **流程名称**: 创建个人客户（简版）
- **COBOL文件**: 3.CreatePerCustInfo.cbl
- **程序ID**: CRTPERC01
- **描述**: 根据证件信息创建个人客户，支持身份证自动提取性别和出生日期

#### 任务1: 参数基础校验

业务逻辑: 校验证件号码、证件类型、客户名称是否为空

- **步骤1.1**: 校验证件号码
  - 逻辑: 检查REQ-CRTF-NO是否为空
- **步骤1.2**: 校验证件类型
  - 逻辑: 检查REQ-CRTF-TYP-CD是否为空
- **步骤1.3**: 校验客户名称
  - 逻辑: 检查REQ-CUST-NM是否为空

#### 任务2: 检查客户是否已存在

业务逻辑: 根据证件类型和证件号码检查客户是否已存在

- **步骤2.1**: 查询现有客户
  - 逻辑: 按证件类型和证件号查询CUSTOMER_BASIC_INFO
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CUST_NO, TENANT_NO, CUST_NM, CRTF_TYP_CD, CRTF_NO

#### 任务3: 生成客户编号

业务逻辑: 使用序列或时间戳生成唯一客户号

- **步骤3.1**: 获取序列值
  - 逻辑: 从CUST_NO_SEQ获取下一个序列值

#### 任务4: 从身份证提取信息

业务逻辑: 如果是身份证，自动提取出生日期和性别

- **步骤4.1**: 解析身份证
  - 逻辑: 解析18位或15位身份证获取出生日期和性别

#### 任务5: 插入客户信息

业务逻辑: 插入客户基本信息和个人客户信息

- **步骤5.1**: 插入基本信息
  - 逻辑: 向CUSTOMER_BASIC_INFO插入客户基本数据
  - 操作类型: INSERT
  - 数据操作: INSERT on CUSTOMER_BASIC_INFO: TENANT_NO, CUST_NO, CUST_TYP_CD, CUST_LVL_CD, CRTF_TYP_CD, CRTF_NO, CUST_NM, VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM
- **步骤5.2**: 插入个人信息
  - 逻辑: 向PERSONAL_CUSTOMER_INFO插入个人客户数据
  - 操作类型: INSERT
  - 数据操作: INSERT on PERSONAL_CUSTOMER_INFO: TENANT_NO, CUST_NO, GENDER_CD, BIRTH_DT, VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM

### 创建个人客户信息（完整版）

- **流程名称**: 创建个人客户（完整版）
- **COBOL文件**: 4.CrtPerCustInfo.cbl
- **程序ID**: CRTPERC02
- **描述**: 创建个人客户的完整信息，包含所有必填字段验证

#### 任务1: 完整参数校验

业务逻辑: 校验所有必填字段

- **步骤1.1**: 校验证件类型
  - 逻辑: 检查REQ-CRTF-TYP-CD是否为空
- **步骤1.2**: 校验证件号码
  - 逻辑: 检查REQ-CRTF-NO是否为空
- **步骤1.3**: 校验客户名称
  - 逻辑: 检查REQ-CUST-NM是否为空
- **步骤1.4**: 校验性别代码
  - 逻辑: 检查REQ-GENDER-CD是否为空
- **步骤1.5**: 校验国家地区
  - 逻辑: 检查REQ-STATE-RGN-CD是否为空
- **步骤1.6**: 校验证件发放日期
  - 逻辑: 检查REQ-CRTF-GRANT-DT是否为空
- **步骤1.7**: 校验证件到期日期
  - 逻辑: 检查REQ-CRTF-MATR-DT是否为空
- **步骤1.8**: 校验职业类型
  - 逻辑: 检查REQ-CAREER-TYP-CD是否为空
- **步骤1.9**: 校验地址
  - 逻辑: 检查REQ-ADDR是否为空
- **步骤1.10**: 校验手机号
  - 逻辑: 检查REQ-RSVD-MOBILE-NO是否为空

#### 任务2: 检查客户是否已存在

业务逻辑: 根据证件类型和证件号码检查客户是否已存在

- **步骤2.1**: 查询现有客户
  - 逻辑: 按证件类型和证件号查询CUSTOMER_BASIC_INFO
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CRTF_TYP_CD, CRTF_NO, VALID_FLG

#### 任务3: 插入完整客户信息

业务逻辑: 插入客户基本信息和个人客户信息

- **步骤3.1**: 插入基本信息
  - 逻辑: 向CUSTOMER_BASIC_INFO插入完整客户基本数据
  - 操作类型: INSERT
  - 数据操作: INSERT on CUSTOMER_BASIC_INFO: TENANT_NO, CUST_NO, CUST_TYP_CD, CUST_LVL_CD, CRTF_TYP_CD, CRTF_NO, CUST_NM, CRTF_GRANT_DT, CRTF_MATR_DT, STATE_AND_RGN_CD, ADDR, RSVD_MOBILE_NO, EMPLY_FLG, VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM
- **步骤3.2**: 插入个人信息
  - 逻辑: 向PERSONAL_CUSTOMER_INFO插入完整个人客户数据
  - 操作类型: INSERT
  - 数据操作: INSERT on PERSONAL_CUSTOMER_INFO: TENANT_NO, CUST_NO, GENDER_CD, CAREER_TYP_CD, ADMIN_CMPRMNT_CD, VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM

### 客户账户路由信息管理

- **流程名称**: 管理客户账户信息
- **COBOL文件**: 5.MgmtCustAcctInfo.cbl
- **程序ID**: MGMTCRT01
- **描述**: 对客户账户路由信息进行新增、修改、删除操作

#### 任务1: 参数基础校验

业务逻辑: 校验必填参数和操作类型有效性

- **步骤1.1**: 校验租户号
  - 逻辑: 检查REQ-TENANT-NO是否为空
- **步骤1.2**: 校验客户编号
  - 逻辑: 检查REQ-CUST-NO是否为空
- **步骤1.3**: 校验路由值
  - 逻辑: 检查REQ-ROUTE-VAL是否为空
- **步骤1.4**: 校验路由类型
  - 逻辑: 检查REQ-ROUTE-TYP-CD是否为空
- **步骤1.5**: 校验操作类型
  - 逻辑: 验证REQ-OPER-TYP-CD为01/02/03

#### 任务2: 新增客户账户路由信息

业务逻辑: 检查记录不存在后插入新记录

- **步骤2.1**: 检查记录存在
  - 逻辑: 查询是否已存在相同路由记录
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: TENANT_NO, CUST_NO, ROUTE_TYP_CD, ROUTE_VAL, RELA_SEQ_NO, VALID_FLG
- **步骤2.2**: 插入路由记录
  - 逻辑: 插入新的客户账户路由信息
  - 操作类型: INSERT
  - 数据操作: INSERT on CUST_ACCT_INFO: TENANT_NO, CUST_NO, AFS_PRODT_NO, BASE_PRODT_NO, MAIN_ACCT_NO, OPER_TYP_CD, RELA_SEQ_NO, ROUTE_TYP_CD, ROUTE_VAL, VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM

#### 任务3: 修改客户账户路由信息

业务逻辑: 检查记录存在后更新记录

- **步骤3.1**: 检查记录存在
  - 逻辑: 查询是否存在要修改的路由记录
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: TENANT_NO, CUST_NO, ROUTE_TYP_CD, ROUTE_VAL, RELA_SEQ_NO, VALID_FLG
- **步骤3.2**: 更新路由记录
  - 逻辑: 更新客户账户路由信息
  - 操作类型: UPDATE
  - 数据操作: UPDATE on CUST_ACCT_INFO: AFS_PRODT_NO, BASE_PRODT_NO, MAIN_ACCT_NO, OPER_TYP_CD, UPD_TELR_NO, UPD_TM

#### 任务4: 删除客户账户路由信息

业务逻辑: 逻辑删除，将VALID_FLG设置为0

- **步骤4.1**: 检查记录存在
  - 逻辑: 查询是否存在要删除的路由记录
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: TENANT_NO, CUST_NO, ROUTE_TYP_CD, ROUTE_VAL, RELA_SEQ_NO, VALID_FLG
- **步骤4.2**: 逻辑删除记录
  - 逻辑: 将VALID_FLG更新为0实现逻辑删除
  - 操作类型: UPDATE
  - 数据操作: UPDATE on CUST_ACCT_INFO: VALID_FLG, UPD_TELR_NO, UPD_TM

### 个人客户信息维护

- **流程名称**: 管理个人客户信息
- **COBOL文件**: 6.MgmtPerCustlnfo.cbl
- **程序ID**: MGMT-PER-CUST-INFO
- **描述**: 维护个人客户的基本信息和扩展信息

#### 任务1: 验证客户类型

业务逻辑: 验证客户号是否存在且为个人客户类型

- **步骤1.1**: 查询客户类型
  - 逻辑: 查询CUSTOMER_BASIC_INFO获取客户类型
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CUST_TYP_CD, CUST_NO

#### 任务2: 更新客户基本信息

业务逻辑: 根据传入参数更新客户基本信息表

- **步骤2.1**: 更新基本信息
  - 逻辑: 条件更新CUSTOMER_BASIC_INFO中的非空字段
  - 操作类型: UPDATE
  - 数据操作: UPDATE on CUSTOMER_BASIC_INFO: CUST_NM, CUST_ENG_NM, CUST_LVL_CD, MOBILE_NO, E_MAIL, CRTF_TYP_CD, CRTF_NO, CRTF_MATR_DT

#### 任务3: 更新个人客户信息

业务逻辑: 根据传入参数更新个人客户信息表

- **步骤3.1**: 更新个人信息
  - 逻辑: 条件更新PERSONAL_CUSTOMER_INFO中的非空字段
  - 操作类型: UPDATE
  - 数据操作: UPDATE on PERSONAL_CUSTOMER_INFO: ADDR, HOUSDRGST_ADDR, GENDER_CD, MARRG_SITUATION_CD, BIRTH_DT, CAREER_TYP_CD, STATE_AND_RGN_CD, DOM_OVERS_FLG_CD, IDCARD_TYP_CD, EMPLY_FLG, SHRHD_FLG, SPS_NAME, SPS_ENG_NAME, SPS_CRTF_TYP_CD, SPS_CRTF_NO, SPS_TEL_NO, WORK_UNIT_NM, WORK_UNIT_ADDR, ADMIN_CMPRMNT_CD

### 通过账号查询客户账户路由信息

- **流程名称**: 按账号查询账户信息
- **COBOL文件**: 7.QuryCustAcctInfoByCustAcct.cbl
- **程序ID**: QRYCUSTACCTINFO
- **描述**: 根据路由值、关联序号、路由类型查询客户账户路由信息

#### 任务1: 输入参数校验

业务逻辑: 校验租户号、路由值、路由类型是否为空

- **步骤1.1**: 校验租户号
  - 逻辑: 检查WS-TENANT-NO是否为空
- **步骤1.2**: 校验路由值
  - 逻辑: 检查WS-ROUTE-VAL是否为空
- **步骤1.3**: 校验路由类型
  - 逻辑: 检查WS-ROUTE-TYP-CD是否为空

#### 任务2: 查询客户账户路由信息

业务逻辑: 根据条件查询THSBCECIF_CUST_ACCT_INFO表

- **步骤2.1**: 执行查询
  - 逻辑: 按租户号、路由值、路由类型查询账户路由信息
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: ID, TENANT_NO, CUST_NO, AFS_PRODT_NO, BASE_PRODT_NO, MAIN_ACCT_NO, OPER_TYP_CD, RELA_SEQ_NO, ROUTE_TYP_CD, ROUTE_VAL, VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, UPD_TM, CRT_TM

### 通过客户号查询客户账户路由信息

- **流程名称**: 按客户号查询账户信息
- **COBOL文件**: 8.QuryCustAcctInfoByCustNo.cbl
- **程序ID**: QURYCUSTACCTINFOBYCUSTNO
- **描述**: 根据客户号、路由类型、状态查询客户账户路由信息

#### 任务1: 输入参数校验

业务逻辑: 校验租户号、客户号、路由类型是否为空

- **步骤1.1**: 校验租户号
  - 逻辑: 检查WS-TENANT-NO是否为空
- **步骤1.2**: 校验客户号
  - 逻辑: 检查WS-CUST-NO是否为空
- **步骤1.3**: 校验路由类型
  - 逻辑: 检查WS-ROUTE-TYP-CD是否为空

#### 任务2: 查询客户账户路由信息

业务逻辑: 根据客户号等条件查询账户路由信息

- **步骤2.1**: 执行查询
  - 逻辑: 按客户号、路由类型、状态查询账户路由信息
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_ACCT_INFO: CUST_NO, AFS_PRODT_NO, BASE_PRODT_NO, MAIN_ACCT_NO, OPER_TYP_CD, RELA_SEQ_NO, ROUTE_TYP_CD, ROUTE_VAL, VALID_FLG

### 客户基本信息查询

- **流程名称**: 查询客户信息
- **COBOL文件**: 9.QuryCustInfo.cbl
- **程序ID**: QURYCUSTINFO
- **描述**: 根据客户号或证件信息查询客户基本信息和风险等级

#### 任务1: 输入参数校验

业务逻辑: 客户号为空时必须提供证件类型和证件号码

- **步骤1.1**: 校验证件类型
  - 逻辑: 客户号为空时检查证件类型是否为空
- **步骤1.2**: 校验证件号码
  - 逻辑: 客户号为空时检查证件号码是否为空

#### 任务2: 查询客户基本信息

业务逻辑: 根据查询条件查询CUSTOMER_BASIC_INFO表

- **步骤2.1**: 查询客户基本信息
  - 逻辑: 按客户号或证件信息查询客户基本信息
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CUST_NO, CUST_TYP_CD, CUST_NM, CRTF_NO, CRTF_TYP_CD, CRTF_MATR_DT

#### 任务3: 查询客户风险等级信息

业务逻辑: 根据客户号和客户类型查询风险等级

- **步骤3.1**: 查询风险等级
  - 逻辑: 查询CUSTOMER_RISK_INFO获取客户关注程度
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_RISK_INFO: CUST_NO, CUST_TYP_CD, CUST_ATTN_EXTT_CD

### 客户类型查询

- **流程名称**: 查询客户类型
- **COBOL文件**: 10.QuryCustType.cbl
- **程序ID**: QURYCUSTTYPE
- **描述**: 根据客户号查询客户类型代码

#### 任务1: 输入参数校验

业务逻辑: 校验客户编号是否为空

- **步骤1.1**: 校验客户编号
  - 逻辑: 检查WS-CUST-NO是否为空

#### 任务2: 查询客户基本信息获取类型

业务逻辑: 从客户基本信息表获取客户类型代码

- **步骤2.1**: 查询客户类型
  - 逻辑: 按客户号查询CUSTOMER_BASIC_INFO获取类型
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CUST_NO, CUST_TYP_CD, CUST_NM

### 境外取现控制标志查询

- **流程名称**: 查询境外取现控制标志
- **COBOL文件**: 11.QuryOvsCashWithdrReCtrFlg.cbl
- **程序ID**: QURYOVSCASHWITHDRRECTRFLG
- **描述**: 查询客户是否在境外取现黑名单中

#### 任务1: 输入参数校验

业务逻辑: 校验客户编号是否为空

- **步骤1.1**: 校验客户编号
  - 逻辑: 检查WS-CUST-NO是否为空

#### 任务2: 获取客户证件信息

业务逻辑: 查询客户基本信息获取证件号码和类型

- **步骤2.1**: 查询基本信息
  - 逻辑: 按客户号查询CUSTOMER_BASIC_INFO获取证件信息
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CUST_NO, CRTF_NO, CRTF_TYP_CD, CUST_NM

#### 任务3: 查询境外取现黑名单

业务逻辑: 根据证件信息查询是否在黑名单中

- **步骤3.1**: 查询黑名单
  - 逻辑: 按证件号和类型查询OVS_CASH_WITHDR_BLK表
  - 操作类型: SELECT
  - 数据操作: SELECT on OVS_CASH_WITHDR_BLK: CRTF_NO, CRTF_TYP_CD, VALID_FLG

### 对私客户交易渠道控制查询

- **流程名称**: 查询客户渠道交易控制
- **COBOL文件**: 12.QuryPerCustChnlTxnCommond.cbl
- **程序ID**: QURYPERCUSTCHNLTXNCOMMOND
- **描述**: 查询个人客户的各渠道交易限额控制信息

#### 任务1: 输入参数校验

业务逻辑: 校验客户编号和租户号是否为空

- **步骤1.1**: 校验客户编号
  - 逻辑: 检查WS-CUST-NO是否为空
- **步骤1.2**: 校验租户号
  - 逻辑: 检查WS-TENANT-NO是否为空

#### 任务2: 查询交易渠道控制信息

业务逻辑: 查询客户的渠道交易限额配置

- **步骤2.1**: 查询限额信息
  - 逻辑: 按客户号和租户号查询CUST_CHNL_TXN_COMMOND表
  - 操作类型: SELECT
  - 数据操作: SELECT on CUST_CHNL_TXN_COMMOND: CUST_NO, YR_ACCM_MAX_TX_AMT, MON_ACCM_MAX_TX_AMT, PMIT_TERMINAL_TYP_CD, LMT_TYP_CD, DAY_ACCM_MAX_TX_AMT, MON_ACCM_MAX_TX_STKCNT, DAY_ACCM_MAX_TX_STKCNT, YR_ACCM_MAX_TX_STKCNT, SGL_TX_HIGH_AMT, SGL_TX_LOWEST_AMT, QT_ACCM_MAX_TX_STKCNT, QT_ACCM_MAX_TX_AMT, RSN, VALID_FLG

### 根据客户编号查询个人客户信息

- **流程名称**: 按客户号查询个人信息
- **COBOL文件**: 13.QuryPerCustInfoByCustNo.cbl
- **程序ID**: QURYPERCUSTINFOBYCUSTNO
- **描述**: 查询个人客户的完整信息，包括基本信息和个人扩展信息

#### 任务1: 输入参数校验

业务逻辑: 校验客户编号是否为空

- **步骤1.1**: 校验客户编号
  - 逻辑: 检查WS-CUST-NO是否为空

#### 任务2: 查询客户基本信息

业务逻辑: 查询CUSTOMER_BASIC_INFO获取基本信息

- **步骤2.1**: 查询基本信息
  - 逻辑: 按客户号查询客户基本信息
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CUST_NO, CUST_NM, GENDER_CD, CRTF_NO, CRTF_TYP_CD, CRTF_MATR_DT, DOM_OVERS_FLG_CD, STATE_AND_RGN_CD, ADDR, RSVD_MOBILE_NO, EMPLY_FLG

#### 任务3: 查询个人客户扩展信息

业务逻辑: 查询PERSONAL_CUSTOMER_INFO获取个人扩展信息

- **步骤3.1**: 查询个人信息
  - 逻辑: 按客户号查询个人客户扩展信息
  - 操作类型: SELECT
  - 数据操作: SELECT on PERSONAL_CUSTOMER_INFO: CUST_NO, ADMIN_CMPRMNT_CD, CAREER_TYP_CD, ETHNIC_CD, GENDER_CD, ADDR, SPS_NAME, SPS_CRTF_TYP_CD, SPS_CRTF_NO, SPS_TEL_NO

### 对私客户名单信息查询

- **流程名称**: 查询客户名单信息
- **COBOL文件**: 14.QuryPerCustNameListInfoByCustNo.cbl
- **程序ID**: QURYPERCUSTNAMELIST
- **描述**: 查询个人客户的名单分类信息（黑名单/白名单/灰名单）

#### 任务1: 输入参数校验

业务逻辑: 校验客户编号是否为空

- **步骤1.1**: 校验客户编号
  - 逻辑: 检查WS-CUST-NO是否为空

#### 任务2: 查询客户名单信息

业务逻辑: 查询PERSONAL_CUSTOMER_LIST获取名单分类

- **步骤2.1**: 查询名单
  - 逻辑: 按客户号查询个人客户名单信息
  - 操作类型: SELECT
  - 数据操作: SELECT on PERSONAL_CUSTOMER_LIST: CUST_NO, CRTF_TYP_CD, CRTF_NO, NM_SNGL_TYP_CD, DATA_SORC_CD, ORG_DISMN_CD, CTRL_FLG, CHK_FLG_CD, EFFT_DT, EFFT_TM, INVALID_DT, INVALID_TM, VALID_FLG

### 对私客户风险等级信息查询

- **流程名称**: 查询客户风险等级
- **COBOL文件**: 15.QuryPerCustRiskLevel.cbl
- **程序ID**: QURYPERCUSTRISKLEVEL
- **描述**: 查询个人客户的风险等级评定信息

#### 任务1: 输入参数校验

业务逻辑: 校验客户编号是否为空

- **步骤1.1**: 校验客户编号
  - 逻辑: 检查WS-CUST-NO是否为空

#### 任务2: 查询客户基本信息

业务逻辑: 查询CUSTOMER_BASIC_INFO获取客户类型

- **步骤2.1**: 查询基本信息
  - 逻辑: 按客户号查询客户基本信息获取客户类型
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_BASIC_INFO: CUST_NO, CUST_TYP_CD, CUST_NM

#### 任务3: 查询客户风险等级信息

业务逻辑: 查询CUSTOMER_RISK_INFO获取风险等级详情

- **步骤3.1**: 查询风险等级
  - 逻辑: 按客户号和类型查询风险等级信息
  - 操作类型: SELECT
  - 数据操作: SELECT on CUSTOMER_RISK_INFO: CUST_NO, CUST_TYP_CD, CUST_ATTN_EXTT_CD, EVALT_DT, RELS_DT, RELS_OR_ISU_ORG_NO, EVALT_ACRDGAS_COMNT

### 客户签约关系查询

- **流程名称**: 查询签约关系
- **COBOL文件**: 16.QurySignRelationInfo.cbl
- **程序ID**: QURYSIGNRELATIONINFO
- **描述**: 根据证件信息或账号查询客户的签约关系

#### 任务1: 输入参数校验

业务逻辑: 证件号码和客户账号不能同时为空

- **步骤1.1**: 校验查询条件
  - 逻辑: 检查CRTF-NO和CUST-ACCT-NO不能同时为空
- **步骤1.2**: 校验证件类型
  - 逻辑: 验证证件类型代码有效性

#### 任务2: 查询客户签约关系

业务逻辑: 查询SIGN_RELATION_INFO获取签约信息

- **步骤2.1**: 查询签约
  - 逻辑: 按证件信息或账号查询签约关系
  - 操作类型: SELECT
  - 数据操作: SELECT on SIGN_RELATION_INFO: SIGN_NO, SIGN_TYPE, SIGN_STATUS, SIGN_DATE, SIGN_AMOUNT, SIGN_DESC

