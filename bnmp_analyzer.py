#!/usr/bin/env python3
"""
BNMP 2.0 业务模型分析器
分析COBOL代码和数据库信息，提取业务模型并存储到本地数据库
"""

import sqlite3
import uuid
import os
import re
from datetime import datetime


# ==================== 数据库初始化 ====================
def init_database(db_path):
    """初始化数据库，创建元素表和元素关系表"""
    try:
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
    except Exception as e:
        print(f"错误: 无法连接数据库 {db_path}")
        print(f"详细错误: {e}")
        print(f"工作目录: {os.getcwd()}")
        print(f"数据库文件所在目录: {os.path.dirname(db_path)}")
        print(f"数据库文件目录可写: {os.access(os.path.dirname(db_path), os.W_OK)}")
        raise

    # 创建元素表 - 存储所有类型的识别元素
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS bnmp_elements (
        element_id TEXT PRIMARY KEY,
        element_type TEXT NOT NULL,  -- ENTITY/ATTRIBUTE/PROCESS/TASK/STEP
        element_name TEXT NOT NULL,
        element_name_cn TEXT,        -- 中文名称
        description TEXT,
        description_cn TEXT,         -- 中文描述
        
        -- 业务实体(ENTITY)专用字段
        table_name TEXT,
        entity_summary TEXT,
        
        -- 实体属性(ATTRIBUTE)专用字段
        field_name TEXT,
        field_type TEXT,
        field_length TEXT,
        field_rules TEXT,
        foreign_key_ref TEXT,
        parent_entity_id TEXT,
        
        -- 流程(PROCESS)专用字段
        cobol_file_name TEXT,
        program_id TEXT,
        
        -- 任务(TASK)专用字段
        task_logic TEXT,
        cobol_snippet TEXT,
        
        -- 步骤(STEP)专用字段
        step_logic TEXT,
        data_operation_type TEXT,    -- SELECT/INSERT/UPDATE/DELETE
        data_operation_desc TEXT,
        step_cobol_snippet TEXT,
        
        -- 元数据
        created_at TEXT DEFAULT CURRENT_TIMESTAMP,
        updated_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
    """)

    # 创建元素关系表
    cursor.execute("""
    CREATE TABLE IF NOT EXISTS bnmp_element_relations (
        relation_id TEXT PRIMARY KEY,
        source_element_id TEXT NOT NULL,
        target_element_id TEXT NOT NULL,
        relation_type TEXT NOT NULL,  -- PROCESS_TASK/TASK_STEP/TASK_TASK/STEP_STEP/STEP_ENTITY/STEP_ATTRIBUTE/ENTITY_ATTRIBUTE
        relation_order INTEGER,       -- 用于排序（前置关系）
        description TEXT,
        created_at TEXT DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (source_element_id) REFERENCES bnmp_elements(element_id),
        FOREIGN KEY (target_element_id) REFERENCES bnmp_elements(element_id)
    )
    """)

    # 创建索引
    cursor.execute(
        "CREATE INDEX IF NOT EXISTS idx_element_type ON bnmp_elements(element_type)"
    )
    cursor.execute(
        "CREATE INDEX IF NOT EXISTS idx_relation_type ON bnmp_element_relations(relation_type)"
    )
    cursor.execute(
        "CREATE INDEX IF NOT EXISTS idx_source_element ON bnmp_element_relations(source_element_id)"
    )
    cursor.execute(
        "CREATE INDEX IF NOT EXISTS idx_target_element ON bnmp_element_relations(target_element_id)"
    )

    conn.commit()
    return conn


def generate_id():
    """生成唯一ID"""
    return str(uuid.uuid4())


# ==================== 数据模型定义 ====================

# 业务实体定义（从COBOL代码分析得出）
BUSINESS_ENTITIES = [
    {
        "table_name": "CUSTOMER_BASIC_INFO",
        "name_cn": "客户基本信息",
        "summary": "存储客户的基本信息，包括客户号、客户名称、证件信息、联系方式、地址等核心客户数据，支持个人客户和企业客户类型",
        "attributes": [
            {
                "name": "TENANT_NO",
                "name_cn": "租户编号",
                "type": "VARCHAR",
                "length": "10",
                "rules": "必填，多租户标识",
            },
            {
                "name": "CUST_NO",
                "name_cn": "客户编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "主键，必填",
            },
            {
                "name": "CUST_TYP_CD",
                "name_cn": "客户类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "01-个人客户，02-企业客户，03-机构客户",
            },
            {
                "name": "CUST_LVL_CD",
                "name_cn": "客户等级代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "客户分级标识",
            },
            {
                "name": "CUST_NM",
                "name_cn": "客户名称",
                "type": "VARCHAR",
                "length": "60",
                "rules": "必填",
            },
            {
                "name": "CUST_ENG_NM",
                "name_cn": "客户英文名称",
                "type": "VARCHAR",
                "length": "60",
                "rules": "可选",
            },
            {
                "name": "CRTF_TYP_CD",
                "name_cn": "证件类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "01-身份证，02-护照等",
            },
            {
                "name": "CRTF_NO",
                "name_cn": "证件号码",
                "type": "VARCHAR",
                "length": "20",
                "rules": "必填",
            },
            {
                "name": "CRTF_GRANT_DT",
                "name_cn": "证件发放日期",
                "type": "DATE",
                "length": "10",
                "rules": "格式yyyy-mm-dd",
            },
            {
                "name": "CRTF_MATR_DT",
                "name_cn": "证件到期日期",
                "type": "DATE",
                "length": "10",
                "rules": "格式yyyy-mm-dd",
            },
            {
                "name": "STATE_AND_RGN_CD",
                "name_cn": "国家和地区代码",
                "type": "VARCHAR",
                "length": "3",
                "rules": "国际标准代码",
            },
            {
                "name": "ADDR",
                "name_cn": "地址",
                "type": "VARCHAR",
                "length": "100",
                "rules": "联系地址",
            },
            {
                "name": "RSVD_MOBILE_NO",
                "name_cn": "预留手机号码",
                "type": "VARCHAR",
                "length": "15",
                "rules": "主要联系电话",
            },
            {
                "name": "MOBILE_NO",
                "name_cn": "手机号码",
                "type": "VARCHAR",
                "length": "15",
                "rules": "备用联系电话",
            },
            {
                "name": "E_MAIL",
                "name_cn": "电子邮箱",
                "type": "VARCHAR",
                "length": "50",
                "rules": "邮箱格式验证",
            },
            {
                "name": "EMPLY_FLG",
                "name_cn": "员工标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-是员工，0-非员工",
            },
            {
                "name": "VALID_FLG",
                "name_cn": "有效标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-有效，0-无效",
            },
            {
                "name": "CRT_TELR_NO",
                "name_cn": "创建柜员号",
                "type": "VARCHAR",
                "length": "10",
                "rules": "创建人标识",
            },
            {
                "name": "UPD_TELR_NO",
                "name_cn": "更新柜员号",
                "type": "VARCHAR",
                "length": "10",
                "rules": "最后更新人",
            },
            {
                "name": "CRT_TM",
                "name_cn": "创建时间",
                "type": "TIMESTAMP",
                "length": "26",
                "rules": "系统自动生成",
            },
            {
                "name": "UPD_TM",
                "name_cn": "更新时间",
                "type": "TIMESTAMP",
                "length": "26",
                "rules": "系统自动更新",
            },
        ],
    },
    {
        "table_name": "PERSONAL_CUSTOMER_INFO",
        "name_cn": "个人客户信息",
        "summary": "存储个人客户的扩展信息，包括性别、出生日期、职业、婚姻状况、配偶信息、工作单位等个人专属数据，与客户基本信息表关联",
        "attributes": [
            {
                "name": "TENANT_NO",
                "name_cn": "租户编号",
                "type": "VARCHAR",
                "length": "10",
                "rules": "必填",
            },
            {
                "name": "CUST_NO",
                "name_cn": "客户编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "主键，外键关联CUSTOMER_BASIC_INFO",
                "fk": "CUSTOMER_BASIC_INFO.CUST_NO",
            },
            {
                "name": "GENDER_CD",
                "name_cn": "性别代码",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-男，2-女",
            },
            {
                "name": "BIRTH_DT",
                "name_cn": "出生日期",
                "type": "DATE",
                "length": "8",
                "rules": "格式yyyymmdd",
            },
            {
                "name": "CAREER_TYP_CD",
                "name_cn": "职业类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "职业分类代码",
            },
            {
                "name": "ETHNIC_CD",
                "name_cn": "民族代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "民族标识",
            },
            {
                "name": "MARRG_SITUATION_CD",
                "name_cn": "婚姻状况代码",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-已婚，2-未婚，3-离异",
            },
            {
                "name": "ADMIN_CMPRMNT_CD",
                "name_cn": "行政区划代码",
                "type": "VARCHAR",
                "length": "6",
                "rules": "标准行政区划",
            },
            {
                "name": "DOM_OVERS_FLG_CD",
                "name_cn": "境内外标志代码",
                "type": "VARCHAR",
                "length": "1",
                "rules": "D-境内，F-境外",
            },
            {
                "name": "IDCARD_TYP_CD",
                "name_cn": "身份证类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "身份证件类型",
            },
            {
                "name": "ADDR",
                "name_cn": "地址",
                "type": "VARCHAR",
                "length": "100",
                "rules": "居住地址",
            },
            {
                "name": "HOUSDRGST_ADDR",
                "name_cn": "户籍地址",
                "type": "VARCHAR",
                "length": "100",
                "rules": "户口所在地",
            },
            {
                "name": "SPS_NAME",
                "name_cn": "配偶姓名",
                "type": "VARCHAR",
                "length": "60",
                "rules": "可选",
            },
            {
                "name": "SPS_ENG_NAME",
                "name_cn": "配偶英文姓名",
                "type": "VARCHAR",
                "length": "60",
                "rules": "可选",
            },
            {
                "name": "SPS_CRTF_TYP_CD",
                "name_cn": "配偶证件类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "可选",
            },
            {
                "name": "SPS_CRTF_NO",
                "name_cn": "配偶证件号码",
                "type": "VARCHAR",
                "length": "20",
                "rules": "可选",
            },
            {
                "name": "SPS_TEL_NO",
                "name_cn": "配偶电话号码",
                "type": "VARCHAR",
                "length": "20",
                "rules": "可选",
            },
            {
                "name": "WORK_UNIT_NM",
                "name_cn": "工作单位名称",
                "type": "VARCHAR",
                "length": "50",
                "rules": "可选",
            },
            {
                "name": "WORK_UNIT_ADDR",
                "name_cn": "工作单位地址",
                "type": "VARCHAR",
                "length": "100",
                "rules": "可选",
            },
            {
                "name": "SHRHD_FLG",
                "name_cn": "股东标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-是股东，0-非股东",
            },
            {
                "name": "VALID_FLG",
                "name_cn": "有效标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-有效，0-无效",
            },
            {
                "name": "CRT_TELR_NO",
                "name_cn": "创建柜员号",
                "type": "VARCHAR",
                "length": "10",
                "rules": "创建人标识",
            },
            {
                "name": "UPD_TELR_NO",
                "name_cn": "更新柜员号",
                "type": "VARCHAR",
                "length": "10",
                "rules": "最后更新人",
            },
            {
                "name": "CRT_TM",
                "name_cn": "创建时间",
                "type": "TIMESTAMP",
                "length": "26",
                "rules": "系统自动生成",
            },
            {
                "name": "UPD_TM",
                "name_cn": "更新时间",
                "type": "TIMESTAMP",
                "length": "26",
                "rules": "系统自动更新",
            },
        ],
    },
    {
        "table_name": "CUST_ACCT_INFO",
        "name_cn": "客户账户路由信息",
        "summary": "存储客户账户与产品的路由关系信息，用于客户归并、账户关联查询，支持多种路由类型和产品映射",
        "attributes": [
            {
                "name": "ID",
                "name_cn": "主键ID",
                "type": "BIGINT",
                "length": "18",
                "rules": "自增主键",
            },
            {
                "name": "TENANT_NO",
                "name_cn": "租户编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "必填",
            },
            {
                "name": "CUST_NO",
                "name_cn": "客户编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "必填，外键关联CUSTOMER_BASIC_INFO",
                "fk": "CUSTOMER_BASIC_INFO.CUST_NO",
            },
            {
                "name": "AFS_PRODT_NO",
                "name_cn": "可售产品编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "产品标识",
            },
            {
                "name": "BASE_PRODT_NO",
                "name_cn": "基础产品编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "基础产品标识",
            },
            {
                "name": "MAIN_ACCT_NO",
                "name_cn": "主账号",
                "type": "VARCHAR",
                "length": "30",
                "rules": "账户主键",
            },
            {
                "name": "OPER_TYP_CD",
                "name_cn": "操作类型代码",
                "type": "VARCHAR",
                "length": "10",
                "rules": "01-新增，02-修改，03-删除",
            },
            {
                "name": "RELA_SEQ_NO",
                "name_cn": "关联序号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "关系排序",
            },
            {
                "name": "ROUTE_TYP_CD",
                "name_cn": "路由类型代码",
                "type": "VARCHAR",
                "length": "10",
                "rules": "路由分类",
            },
            {
                "name": "ROUTE_VAL",
                "name_cn": "路由值",
                "type": "VARCHAR",
                "length": "50",
                "rules": "路由键值",
            },
            {
                "name": "VALID_FLG",
                "name_cn": "有效标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-有效，0-无效",
            },
            {
                "name": "CRT_TELR_NO",
                "name_cn": "创建柜员号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "创建人",
            },
            {
                "name": "UPD_TELR_NO",
                "name_cn": "更新柜员号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "更新人",
            },
            {
                "name": "CRT_TM",
                "name_cn": "创建时间",
                "type": "TIMESTAMP",
                "length": "26",
                "rules": "系统生成",
            },
            {
                "name": "UPD_TM",
                "name_cn": "更新时间",
                "type": "TIMESTAMP",
                "length": "26",
                "rules": "系统更新",
            },
        ],
    },
    {
        "table_name": "CUSTOMER_RISK_INFO",
        "name_cn": "客户风险等级信息",
        "summary": "存储客户的风险等级评定信息，包括关注程度、评定日期、评定机构、评定说明等风控数据",
        "attributes": [
            {
                "name": "CUST_NO",
                "name_cn": "客户编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "主键，外键关联CUSTOMER_BASIC_INFO",
                "fk": "CUSTOMER_BASIC_INFO.CUST_NO",
            },
            {
                "name": "CUST_TYP_CD",
                "name_cn": "客户类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "客户类型",
            },
            {
                "name": "CUST_ATTN_EXTT_CD",
                "name_cn": "客户关注程度代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "H-高，M-中，L-低",
            },
            {
                "name": "EVALT_DT",
                "name_cn": "评定日期",
                "type": "DATE",
                "length": "8",
                "rules": "格式yyyymmdd",
            },
            {
                "name": "RELS_DT",
                "name_cn": "发布日期",
                "type": "DATE",
                "length": "8",
                "rules": "格式yyyymmdd",
            },
            {
                "name": "RELS_OR_ISU_ORG_NO",
                "name_cn": "发布或签发机构编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "机构标识",
            },
            {
                "name": "EVALT_ACRDGAS_COMNT",
                "name_cn": "评定依据说明",
                "type": "VARCHAR",
                "length": "100",
                "rules": "评定理由",
            },
        ],
    },
    {
        "table_name": "OVS_CASH_WITHDR_BLK",
        "name_cn": "境外取现黑名单",
        "summary": "存储境外取现受限客户的黑名单信息，用于境外现金提取控制标志查询",
        "attributes": [
            {
                "name": "CRTF_NO",
                "name_cn": "证件号码",
                "type": "VARCHAR",
                "length": "20",
                "rules": "主键",
            },
            {
                "name": "CRTF_TYP_CD",
                "name_cn": "证件类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "证件类型",
            },
            {
                "name": "VALID_FLG",
                "name_cn": "有效标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-有效，0-无效",
            },
        ],
    },
    {
        "table_name": "CUST_CHNL_TXN_COMMOND",
        "name_cn": "客户交易渠道控制信息",
        "summary": "存储客户在各交易渠道的限额控制信息，包括日/月/季/年累计限额、单笔限额、允许终端类型等",
        "attributes": [
            {
                "name": "CUST_NO",
                "name_cn": "客户编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "外键关联CUSTOMER_BASIC_INFO",
                "fk": "CUSTOMER_BASIC_INFO.CUST_NO",
            },
            {
                "name": "TENANT_NO",
                "name_cn": "租户编号",
                "type": "VARCHAR",
                "length": "10",
                "rules": "多租户标识",
            },
            {
                "name": "PMIT_TERMINAL_TYP_CD",
                "name_cn": "允许终端类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "终端类型",
            },
            {
                "name": "LMT_TYP_CD",
                "name_cn": "限额类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "限额分类",
            },
            {
                "name": "SGL_TX_HIGH_AMT",
                "name_cn": "单笔最高金额",
                "type": "DECIMAL",
                "length": "12,2",
                "rules": "单笔上限",
            },
            {
                "name": "SGL_TX_LOWEST_AMT",
                "name_cn": "单笔最低金额",
                "type": "DECIMAL",
                "length": "12,2",
                "rules": "单笔下限",
            },
            {
                "name": "DAY_ACCM_MAX_TX_AMT",
                "name_cn": "日累计最大交易金额",
                "type": "DECIMAL",
                "length": "12,2",
                "rules": "日限额",
            },
            {
                "name": "DAY_ACCM_MAX_TX_STKCNT",
                "name_cn": "日累计最大交易笔数",
                "type": "INTEGER",
                "length": "5",
                "rules": "日笔数限制",
            },
            {
                "name": "MON_ACCM_MAX_TX_AMT",
                "name_cn": "月累计最大交易金额",
                "type": "DECIMAL",
                "length": "12,2",
                "rules": "月限额",
            },
            {
                "name": "MON_ACCM_MAX_TX_STKCNT",
                "name_cn": "月累计最大交易笔数",
                "type": "INTEGER",
                "length": "5",
                "rules": "月笔数限制",
            },
            {
                "name": "QT_ACCM_MAX_TX_AMT",
                "name_cn": "季累计最大交易金额",
                "type": "DECIMAL",
                "length": "12,2",
                "rules": "季限额",
            },
            {
                "name": "QT_ACCM_MAX_TX_STKCNT",
                "name_cn": "季累计最大交易笔数",
                "type": "INTEGER",
                "length": "5",
                "rules": "季笔数限制",
            },
            {
                "name": "YR_ACCM_MAX_TX_AMT",
                "name_cn": "年累计最大交易金额",
                "type": "DECIMAL",
                "length": "12,2",
                "rules": "年限额",
            },
            {
                "name": "YR_ACCM_MAX_TX_STKCNT",
                "name_cn": "年累计最大交易笔数",
                "type": "INTEGER",
                "length": "5",
                "rules": "年笔数限制",
            },
            {
                "name": "RSN",
                "name_cn": "原因说明",
                "type": "VARCHAR",
                "length": "100",
                "rules": "限额设置原因",
            },
            {
                "name": "VALID_FLG",
                "name_cn": "有效标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-有效，0-无效",
            },
        ],
    },
    {
        "table_name": "PERSONAL_CUSTOMER_LIST",
        "name_cn": "个人客户名单信息",
        "summary": "存储个人客户的名单分类信息，支持黑名单、白名单、灰名单等多种名单类型管理",
        "attributes": [
            {
                "name": "CUST_NO",
                "name_cn": "客户编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "外键关联CUSTOMER_BASIC_INFO",
                "fk": "CUSTOMER_BASIC_INFO.CUST_NO",
            },
            {
                "name": "CRTF_TYP_CD",
                "name_cn": "证件类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "证件类型",
            },
            {
                "name": "CRTF_NO",
                "name_cn": "证件号码",
                "type": "VARCHAR",
                "length": "20",
                "rules": "证件号",
            },
            {
                "name": "NM_SNGL_TYP_CD",
                "name_cn": "名单类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "BL-黑名单，WL-白名单，GL-灰名单",
            },
            {
                "name": "DATA_SORC_CD",
                "name_cn": "数据来源代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "数据来源标识",
            },
            {
                "name": "ORG_DISMN_CD",
                "name_cn": "机构维度代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "机构维度",
            },
            {
                "name": "CTRL_FLG",
                "name_cn": "控制标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "Y-控制，N-不控制",
            },
            {
                "name": "CHK_FLG_CD",
                "name_cn": "核查标志代码",
                "type": "VARCHAR",
                "length": "1",
                "rules": "核查状态",
            },
            {
                "name": "EFFT_DT",
                "name_cn": "生效日期",
                "type": "DATE",
                "length": "8",
                "rules": "格式yyyymmdd",
            },
            {
                "name": "EFFT_TM",
                "name_cn": "生效时间",
                "type": "TIME",
                "length": "6",
                "rules": "格式hhmmss",
            },
            {
                "name": "INVALID_DT",
                "name_cn": "失效日期",
                "type": "DATE",
                "length": "8",
                "rules": "格式yyyymmdd",
            },
            {
                "name": "INVALID_TM",
                "name_cn": "失效时间",
                "type": "TIME",
                "length": "6",
                "rules": "格式hhmmss",
            },
            {
                "name": "VALID_FLG",
                "name_cn": "有效标志",
                "type": "VARCHAR",
                "length": "1",
                "rules": "1-有效，0-无效",
            },
        ],
    },
    {
        "table_name": "SIGN_RELATION_INFO",
        "name_cn": "签约关系信息",
        "summary": "存储客户的各类签约关系信息，包括电子银行签约、手机银行签约、网上支付签约等",
        "attributes": [
            {
                "name": "SIGN_NO",
                "name_cn": "签约编号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "主键",
            },
            {
                "name": "CRTF_NO",
                "name_cn": "证件号码",
                "type": "VARCHAR",
                "length": "20",
                "rules": "客户证件号",
            },
            {
                "name": "CRTF_TYP_CD",
                "name_cn": "证件类型代码",
                "type": "VARCHAR",
                "length": "2",
                "rules": "证件类型",
            },
            {
                "name": "CUST_ACCT_NO",
                "name_cn": "客户账号",
                "type": "VARCHAR",
                "length": "20",
                "rules": "签约账号",
            },
            {
                "name": "CUST_NM",
                "name_cn": "客户名称",
                "type": "VARCHAR",
                "length": "60",
                "rules": "签约客户名",
            },
            {
                "name": "SIGN_SMLTYP_TYP_CD",
                "name_cn": "签约小类类型代码",
                "type": "VARCHAR",
                "length": "4",
                "rules": "签约分类",
            },
            {
                "name": "SIGN_TYPE",
                "name_cn": "签约类型",
                "type": "VARCHAR",
                "length": "2",
                "rules": "签约大类",
            },
            {
                "name": "SIGN_STATUS",
                "name_cn": "签约状态",
                "type": "VARCHAR",
                "length": "1",
                "rules": "A-有效，I-无效",
            },
            {
                "name": "SIGN_DATE",
                "name_cn": "签约日期",
                "type": "DATE",
                "length": "8",
                "rules": "格式yyyymmdd",
            },
            {
                "name": "SIGN_AMOUNT",
                "name_cn": "签约金额",
                "type": "DECIMAL",
                "length": "12,2",
                "rules": "签约额度",
            },
            {
                "name": "SIGN_DESC",
                "name_cn": "签约描述",
                "type": "VARCHAR",
                "length": "50",
                "rules": "签约说明",
            },
        ],
    },
]

# ==================== 流程模型定义 ====================

BUSINESS_PROCESSES = [
    {
        "cobol_file": "1.ConsolidationCust.cbl",
        "program_id": "CUSTMRG01",
        "name": "客户归并",
        "name_cn": "客户归并处理",
        "description": "将并出客户的账户路由信息归并到并入客户名下，实现客户信息合并",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "参数基础校验",
                "logic": "校验并入客户号和并出客户号是否为空",
                "steps": [
                    {
                        "name": "校验并入客户号",
                        "logic": "检查REQ-CUST-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验并出客户号",
                        "logic": "检查REQ-MERGE-CUST-NO是否为空",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "客户存在性验证",
                "name_cn": "验证客户是否存在",
                "logic": "验证并入客户和并出客户是否在系统中存在且有效",
                "steps": [
                    {
                        "name": "检查并入客户",
                        "logic": "查询CUST_ACCT_INFO表验证并入客户是否存在",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": ["CUST_NO", "TENANT_NO", "VALID_FLG"],
                        },
                    },
                    {
                        "name": "检查并出客户",
                        "logic": "查询CUST_ACCT_INFO表验证并出客户是否存在",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": ["CUST_NO", "TENANT_NO", "VALID_FLG"],
                        },
                    },
                ],
            },
            {
                "name": "执行归并",
                "name_cn": "执行客户归并操作",
                "logic": "将并出客户的账户路由信息更新为并入客户号",
                "steps": [
                    {
                        "name": "更新账户路由",
                        "logic": "将并出客户的CUST_NO更新为并入客户号",
                        "operation": {
                            "type": "UPDATE",
                            "table": "CUST_ACCT_INFO",
                            "fields": ["CUST_NO", "UPD_TELR_NO", "UPD_TM"],
                        },
                    }
                ],
            },
            {
                "name": "冲突检测",
                "name_cn": "检查路由冲突",
                "logic": "检查归并后是否存在路由值冲突的记录",
                "steps": [
                    {
                        "name": "检测路由冲突",
                        "logic": "查询是否存在相同路由值的记录",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": ["ROUTE_VAL", "ROUTE_TYP_CD", "RELA_SEQ_NO"],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "2.ConsolidationCustByAcctNo.cbl",
        "program_id": "CUSTMRG02",
        "name": "按账号客户归并",
        "name_cn": "按账号列表客户归并",
        "description": "根据指定的账号列表，将并出客户的特定账户归并到并入客户名下",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "参数基础校验",
                "logic": "校验必填参数是否完整",
                "steps": [
                    {
                        "name": "校验并入客户号",
                        "logic": "检查REQ-CUST-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验并出客户号",
                        "logic": "检查REQ-MERGE-CUST-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验路由类型",
                        "logic": "检查REQ-ROUTE-TYP-CD是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验账号列表",
                        "logic": "检查REQ-ACCT-COUNT是否大于0",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "批量归并",
                "name_cn": "按账号批量归并",
                "logic": "遍历账号列表，逐个更新账户路由信息",
                "steps": [
                    {
                        "name": "更新账户路由",
                        "logic": "按ROUTE_VAL和ROUTE_TYP_CD更新CUST_NO",
                        "operation": {
                            "type": "UPDATE",
                            "table": "CUST_ACCT_INFO",
                            "fields": ["CUST_NO", "UPD_TELR_NO", "UPD_TM"],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "3.CreatePerCustInfo.cbl",
        "program_id": "CRTPERC01",
        "name": "创建个人客户（简版）",
        "name_cn": "创建个人客户信息（简版）",
        "description": "根据证件信息创建个人客户，支持身份证自动提取性别和出生日期",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "参数基础校验",
                "logic": "校验证件号码、证件类型、客户名称是否为空",
                "steps": [
                    {
                        "name": "校验证件号码",
                        "logic": "检查REQ-CRTF-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验证件类型",
                        "logic": "检查REQ-CRTF-TYP-CD是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验客户名称",
                        "logic": "检查REQ-CUST-NM是否为空",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "客户查重",
                "name_cn": "检查客户是否已存在",
                "logic": "根据证件类型和证件号码检查客户是否已存在",
                "steps": [
                    {
                        "name": "查询现有客户",
                        "logic": "按证件类型和证件号查询CUSTOMER_BASIC_INFO",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": [
                                "CUST_NO",
                                "TENANT_NO",
                                "CUST_NM",
                                "CRTF_TYP_CD",
                                "CRTF_NO",
                            ],
                        },
                    }
                ],
            },
            {
                "name": "生成客户号",
                "name_cn": "生成客户编号",
                "logic": "使用序列或时间戳生成唯一客户号",
                "steps": [
                    {
                        "name": "获取序列值",
                        "logic": "从CUST_NO_SEQ获取下一个序列值",
                        "operation": None,
                    }
                ],
            },
            {
                "name": "提取身份证信息",
                "name_cn": "从身份证提取信息",
                "logic": "如果是身份证，自动提取出生日期和性别",
                "steps": [
                    {
                        "name": "解析身份证",
                        "logic": "解析18位或15位身份证获取出生日期和性别",
                        "operation": None,
                    }
                ],
            },
            {
                "name": "创建客户记录",
                "name_cn": "插入客户信息",
                "logic": "插入客户基本信息和个人客户信息",
                "steps": [
                    {
                        "name": "插入基本信息",
                        "logic": "向CUSTOMER_BASIC_INFO插入客户基本数据",
                        "operation": {
                            "type": "INSERT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "CUST_TYP_CD",
                                "CUST_LVL_CD",
                                "CRTF_TYP_CD",
                                "CRTF_NO",
                                "CUST_NM",
                                "VALID_FLG",
                                "CRT_TELR_NO",
                                "UPD_TELR_NO",
                                "CRT_TM",
                                "UPD_TM",
                            ],
                        },
                    },
                    {
                        "name": "插入个人信息",
                        "logic": "向PERSONAL_CUSTOMER_INFO插入个人客户数据",
                        "operation": {
                            "type": "INSERT",
                            "table": "PERSONAL_CUSTOMER_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "GENDER_CD",
                                "BIRTH_DT",
                                "VALID_FLG",
                                "CRT_TELR_NO",
                                "UPD_TELR_NO",
                                "CRT_TM",
                                "UPD_TM",
                            ],
                        },
                    },
                ],
            },
        ],
    },
    {
        "cobol_file": "4.CrtPerCustInfo.cbl",
        "program_id": "CRTPERC02",
        "name": "创建个人客户（完整版）",
        "name_cn": "创建个人客户信息（完整版）",
        "description": "创建个人客户的完整信息，包含所有必填字段验证",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "完整参数校验",
                "logic": "校验所有必填字段",
                "steps": [
                    {
                        "name": "校验证件类型",
                        "logic": "检查REQ-CRTF-TYP-CD是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验证件号码",
                        "logic": "检查REQ-CRTF-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验客户名称",
                        "logic": "检查REQ-CUST-NM是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验性别代码",
                        "logic": "检查REQ-GENDER-CD是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验国家地区",
                        "logic": "检查REQ-STATE-RGN-CD是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验证件发放日期",
                        "logic": "检查REQ-CRTF-GRANT-DT是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验证件到期日期",
                        "logic": "检查REQ-CRTF-MATR-DT是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验职业类型",
                        "logic": "检查REQ-CAREER-TYP-CD是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验地址",
                        "logic": "检查REQ-ADDR是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验手机号",
                        "logic": "检查REQ-RSVD-MOBILE-NO是否为空",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "客户查重",
                "name_cn": "检查客户是否已存在",
                "logic": "根据证件类型和证件号码检查客户是否已存在",
                "steps": [
                    {
                        "name": "查询现有客户",
                        "logic": "按证件类型和证件号查询CUSTOMER_BASIC_INFO",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": ["CRTF_TYP_CD", "CRTF_NO", "VALID_FLG"],
                        },
                    }
                ],
            },
            {
                "name": "创建客户记录",
                "name_cn": "插入完整客户信息",
                "logic": "插入客户基本信息和个人客户信息",
                "steps": [
                    {
                        "name": "插入基本信息",
                        "logic": "向CUSTOMER_BASIC_INFO插入完整客户基本数据",
                        "operation": {
                            "type": "INSERT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "CUST_TYP_CD",
                                "CUST_LVL_CD",
                                "CRTF_TYP_CD",
                                "CRTF_NO",
                                "CUST_NM",
                                "CRTF_GRANT_DT",
                                "CRTF_MATR_DT",
                                "STATE_AND_RGN_CD",
                                "ADDR",
                                "RSVD_MOBILE_NO",
                                "EMPLY_FLG",
                                "VALID_FLG",
                                "CRT_TELR_NO",
                                "UPD_TELR_NO",
                                "CRT_TM",
                                "UPD_TM",
                            ],
                        },
                    },
                    {
                        "name": "插入个人信息",
                        "logic": "向PERSONAL_CUSTOMER_INFO插入完整个人客户数据",
                        "operation": {
                            "type": "INSERT",
                            "table": "PERSONAL_CUSTOMER_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "GENDER_CD",
                                "CAREER_TYP_CD",
                                "ADMIN_CMPRMNT_CD",
                                "VALID_FLG",
                                "CRT_TELR_NO",
                                "UPD_TELR_NO",
                                "CRT_TM",
                                "UPD_TM",
                            ],
                        },
                    },
                ],
            },
        ],
    },
    {
        "cobol_file": "5.MgmtCustAcctInfo.cbl",
        "program_id": "MGMTCRT01",
        "name": "管理客户账户信息",
        "name_cn": "客户账户路由信息管理",
        "description": "对客户账户路由信息进行新增、修改、删除操作",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "参数基础校验",
                "logic": "校验必填参数和操作类型有效性",
                "steps": [
                    {
                        "name": "校验租户号",
                        "logic": "检查REQ-TENANT-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验客户编号",
                        "logic": "检查REQ-CUST-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验路由值",
                        "logic": "检查REQ-ROUTE-VAL是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验路由类型",
                        "logic": "检查REQ-ROUTE-TYP-CD是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验操作类型",
                        "logic": "验证REQ-OPER-TYP-CD为01/02/03",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "新增账户路由",
                "name_cn": "新增客户账户路由信息",
                "logic": "检查记录不存在后插入新记录",
                "steps": [
                    {
                        "name": "检查记录存在",
                        "logic": "查询是否已存在相同路由记录",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "ROUTE_TYP_CD",
                                "ROUTE_VAL",
                                "RELA_SEQ_NO",
                                "VALID_FLG",
                            ],
                        },
                    },
                    {
                        "name": "插入路由记录",
                        "logic": "插入新的客户账户路由信息",
                        "operation": {
                            "type": "INSERT",
                            "table": "CUST_ACCT_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "AFS_PRODT_NO",
                                "BASE_PRODT_NO",
                                "MAIN_ACCT_NO",
                                "OPER_TYP_CD",
                                "RELA_SEQ_NO",
                                "ROUTE_TYP_CD",
                                "ROUTE_VAL",
                                "VALID_FLG",
                                "CRT_TELR_NO",
                                "UPD_TELR_NO",
                                "CRT_TM",
                                "UPD_TM",
                            ],
                        },
                    },
                ],
            },
            {
                "name": "修改账户路由",
                "name_cn": "修改客户账户路由信息",
                "logic": "检查记录存在后更新记录",
                "steps": [
                    {
                        "name": "检查记录存在",
                        "logic": "查询是否存在要修改的路由记录",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "ROUTE_TYP_CD",
                                "ROUTE_VAL",
                                "RELA_SEQ_NO",
                                "VALID_FLG",
                            ],
                        },
                    },
                    {
                        "name": "更新路由记录",
                        "logic": "更新客户账户路由信息",
                        "operation": {
                            "type": "UPDATE",
                            "table": "CUST_ACCT_INFO",
                            "fields": [
                                "AFS_PRODT_NO",
                                "BASE_PRODT_NO",
                                "MAIN_ACCT_NO",
                                "OPER_TYP_CD",
                                "UPD_TELR_NO",
                                "UPD_TM",
                            ],
                        },
                    },
                ],
            },
            {
                "name": "删除账户路由",
                "name_cn": "删除客户账户路由信息",
                "logic": "逻辑删除，将VALID_FLG设置为0",
                "steps": [
                    {
                        "name": "检查记录存在",
                        "logic": "查询是否存在要删除的路由记录",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": [
                                "TENANT_NO",
                                "CUST_NO",
                                "ROUTE_TYP_CD",
                                "ROUTE_VAL",
                                "RELA_SEQ_NO",
                                "VALID_FLG",
                            ],
                        },
                    },
                    {
                        "name": "逻辑删除记录",
                        "logic": "将VALID_FLG更新为0实现逻辑删除",
                        "operation": {
                            "type": "UPDATE",
                            "table": "CUST_ACCT_INFO",
                            "fields": ["VALID_FLG", "UPD_TELR_NO", "UPD_TM"],
                        },
                    },
                ],
            },
        ],
    },
    {
        "cobol_file": "6.MgmtPerCustlnfo.cbl",
        "program_id": "MGMT-PER-CUST-INFO",
        "name": "管理个人客户信息",
        "name_cn": "个人客户信息维护",
        "description": "维护个人客户的基本信息和扩展信息",
        "tasks": [
            {
                "name": "客户类型校验",
                "name_cn": "验证客户类型",
                "logic": "验证客户号是否存在且为个人客户类型",
                "steps": [
                    {
                        "name": "查询客户类型",
                        "logic": "查询CUSTOMER_BASIC_INFO获取客户类型",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": ["CUST_TYP_CD", "CUST_NO"],
                        },
                    }
                ],
            },
            {
                "name": "更新基本信息",
                "name_cn": "更新客户基本信息",
                "logic": "根据传入参数更新客户基本信息表",
                "steps": [
                    {
                        "name": "更新基本信息",
                        "logic": "条件更新CUSTOMER_BASIC_INFO中的非空字段",
                        "operation": {
                            "type": "UPDATE",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": [
                                "CUST_NM",
                                "CUST_ENG_NM",
                                "CUST_LVL_CD",
                                "MOBILE_NO",
                                "E_MAIL",
                                "CRTF_TYP_CD",
                                "CRTF_NO",
                                "CRTF_MATR_DT",
                            ],
                        },
                    }
                ],
            },
            {
                "name": "更新个人信息",
                "name_cn": "更新个人客户信息",
                "logic": "根据传入参数更新个人客户信息表",
                "steps": [
                    {
                        "name": "更新个人信息",
                        "logic": "条件更新PERSONAL_CUSTOMER_INFO中的非空字段",
                        "operation": {
                            "type": "UPDATE",
                            "table": "PERSONAL_CUSTOMER_INFO",
                            "fields": [
                                "ADDR",
                                "HOUSDRGST_ADDR",
                                "GENDER_CD",
                                "MARRG_SITUATION_CD",
                                "BIRTH_DT",
                                "CAREER_TYP_CD",
                                "STATE_AND_RGN_CD",
                                "DOM_OVERS_FLG_CD",
                                "IDCARD_TYP_CD",
                                "EMPLY_FLG",
                                "SHRHD_FLG",
                                "SPS_NAME",
                                "SPS_ENG_NAME",
                                "SPS_CRTF_TYP_CD",
                                "SPS_CRTF_NO",
                                "SPS_TEL_NO",
                                "WORK_UNIT_NM",
                                "WORK_UNIT_ADDR",
                                "ADMIN_CMPRMNT_CD",
                            ],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "7.QuryCustAcctInfoByCustAcct.cbl",
        "program_id": "QRYCUSTACCTINFO",
        "name": "按账号查询账户信息",
        "name_cn": "通过账号查询客户账户路由信息",
        "description": "根据路由值、关联序号、路由类型查询客户账户路由信息",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验租户号、路由值、路由类型是否为空",
                "steps": [
                    {
                        "name": "校验租户号",
                        "logic": "检查WS-TENANT-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验路由值",
                        "logic": "检查WS-ROUTE-VAL是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验路由类型",
                        "logic": "检查WS-ROUTE-TYP-CD是否为空",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "查询账户路由",
                "name_cn": "查询客户账户路由信息",
                "logic": "根据条件查询THSBCECIF_CUST_ACCT_INFO表",
                "steps": [
                    {
                        "name": "执行查询",
                        "logic": "按租户号、路由值、路由类型查询账户路由信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": [
                                "ID",
                                "TENANT_NO",
                                "CUST_NO",
                                "AFS_PRODT_NO",
                                "BASE_PRODT_NO",
                                "MAIN_ACCT_NO",
                                "OPER_TYP_CD",
                                "RELA_SEQ_NO",
                                "ROUTE_TYP_CD",
                                "ROUTE_VAL",
                                "VALID_FLG",
                                "CRT_TELR_NO",
                                "UPD_TELR_NO",
                                "UPD_TM",
                                "CRT_TM",
                            ],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "8.QuryCustAcctInfoByCustNo.cbl",
        "program_id": "QURYCUSTACCTINFOBYCUSTNO",
        "name": "按客户号查询账户信息",
        "name_cn": "通过客户号查询客户账户路由信息",
        "description": "根据客户号、路由类型、状态查询客户账户路由信息",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验租户号、客户号、路由类型是否为空",
                "steps": [
                    {
                        "name": "校验租户号",
                        "logic": "检查WS-TENANT-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验客户号",
                        "logic": "检查WS-CUST-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验路由类型",
                        "logic": "检查WS-ROUTE-TYP-CD是否为空",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "查询账户路由",
                "name_cn": "查询客户账户路由信息",
                "logic": "根据客户号等条件查询账户路由信息",
                "steps": [
                    {
                        "name": "执行查询",
                        "logic": "按客户号、路由类型、状态查询账户路由信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_ACCT_INFO",
                            "fields": [
                                "CUST_NO",
                                "AFS_PRODT_NO",
                                "BASE_PRODT_NO",
                                "MAIN_ACCT_NO",
                                "OPER_TYP_CD",
                                "RELA_SEQ_NO",
                                "ROUTE_TYP_CD",
                                "ROUTE_VAL",
                                "VALID_FLG",
                            ],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "9.QuryCustInfo.cbl",
        "program_id": "QURYCUSTINFO",
        "name": "查询客户信息",
        "name_cn": "客户基本信息查询",
        "description": "根据客户号或证件信息查询客户基本信息和风险等级",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "客户号为空时必须提供证件类型和证件号码",
                "steps": [
                    {
                        "name": "校验证件类型",
                        "logic": "客户号为空时检查证件类型是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验证件号码",
                        "logic": "客户号为空时检查证件号码是否为空",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "查询基本信息",
                "name_cn": "查询客户基本信息",
                "logic": "根据查询条件查询CUSTOMER_BASIC_INFO表",
                "steps": [
                    {
                        "name": "查询客户基本信息",
                        "logic": "按客户号或证件信息查询客户基本信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": [
                                "CUST_NO",
                                "CUST_TYP_CD",
                                "CUST_NM",
                                "CRTF_NO",
                                "CRTF_TYP_CD",
                                "CRTF_MATR_DT",
                            ],
                        },
                    }
                ],
            },
            {
                "name": "查询风险信息",
                "name_cn": "查询客户风险等级信息",
                "logic": "根据客户号和客户类型查询风险等级",
                "steps": [
                    {
                        "name": "查询风险等级",
                        "logic": "查询CUSTOMER_RISK_INFO获取客户关注程度",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_RISK_INFO",
                            "fields": ["CUST_NO", "CUST_TYP_CD", "CUST_ATTN_EXTT_CD"],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "10.QuryCustType.cbl",
        "program_id": "QURYCUSTTYPE",
        "name": "查询客户类型",
        "name_cn": "客户类型查询",
        "description": "根据客户号查询客户类型代码",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验客户编号是否为空",
                "steps": [
                    {
                        "name": "校验客户编号",
                        "logic": "检查WS-CUST-NO是否为空",
                        "operation": None,
                    }
                ],
            },
            {
                "name": "查询客户类型",
                "name_cn": "查询客户基本信息获取类型",
                "logic": "从客户基本信息表获取客户类型代码",
                "steps": [
                    {
                        "name": "查询客户类型",
                        "logic": "按客户号查询CUSTOMER_BASIC_INFO获取类型",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": ["CUST_NO", "CUST_TYP_CD", "CUST_NM"],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "11.QuryOvsCashWithdrReCtrFlg.cbl",
        "program_id": "QURYOVSCASHWITHDRRECTRFLG",
        "name": "查询境外取现控制标志",
        "name_cn": "境外取现控制标志查询",
        "description": "查询客户是否在境外取现黑名单中",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验客户编号是否为空",
                "steps": [
                    {
                        "name": "校验客户编号",
                        "logic": "检查WS-CUST-NO是否为空",
                        "operation": None,
                    }
                ],
            },
            {
                "name": "查询客户基本信息",
                "name_cn": "获取客户证件信息",
                "logic": "查询客户基本信息获取证件号码和类型",
                "steps": [
                    {
                        "name": "查询基本信息",
                        "logic": "按客户号查询CUSTOMER_BASIC_INFO获取证件信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": ["CUST_NO", "CRTF_NO", "CRTF_TYP_CD", "CUST_NM"],
                        },
                    }
                ],
            },
            {
                "name": "查询黑名单",
                "name_cn": "查询境外取现黑名单",
                "logic": "根据证件信息查询是否在黑名单中",
                "steps": [
                    {
                        "name": "查询黑名单",
                        "logic": "按证件号和类型查询OVS_CASH_WITHDR_BLK表",
                        "operation": {
                            "type": "SELECT",
                            "table": "OVS_CASH_WITHDR_BLK",
                            "fields": ["CRTF_NO", "CRTF_TYP_CD", "VALID_FLG"],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "12.QuryPerCustChnlTxnCommond.cbl",
        "program_id": "QURYPERCUSTCHNLTXNCOMMOND",
        "name": "查询客户渠道交易控制",
        "name_cn": "对私客户交易渠道控制查询",
        "description": "查询个人客户的各渠道交易限额控制信息",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验客户编号和租户号是否为空",
                "steps": [
                    {
                        "name": "校验客户编号",
                        "logic": "检查WS-CUST-NO是否为空",
                        "operation": None,
                    },
                    {
                        "name": "校验租户号",
                        "logic": "检查WS-TENANT-NO是否为空",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "查询渠道控制",
                "name_cn": "查询交易渠道控制信息",
                "logic": "查询客户的渠道交易限额配置",
                "steps": [
                    {
                        "name": "查询限额信息",
                        "logic": "按客户号和租户号查询CUST_CHNL_TXN_COMMOND表",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUST_CHNL_TXN_COMMOND",
                            "fields": [
                                "CUST_NO",
                                "YR_ACCM_MAX_TX_AMT",
                                "MON_ACCM_MAX_TX_AMT",
                                "PMIT_TERMINAL_TYP_CD",
                                "LMT_TYP_CD",
                                "DAY_ACCM_MAX_TX_AMT",
                                "MON_ACCM_MAX_TX_STKCNT",
                                "DAY_ACCM_MAX_TX_STKCNT",
                                "YR_ACCM_MAX_TX_STKCNT",
                                "SGL_TX_HIGH_AMT",
                                "SGL_TX_LOWEST_AMT",
                                "QT_ACCM_MAX_TX_STKCNT",
                                "QT_ACCM_MAX_TX_AMT",
                                "RSN",
                                "VALID_FLG",
                            ],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "13.QuryPerCustInfoByCustNo.cbl",
        "program_id": "QURYPERCUSTINFOBYCUSTNO",
        "name": "按客户号查询个人信息",
        "name_cn": "根据客户编号查询个人客户信息",
        "description": "查询个人客户的完整信息，包括基本信息和个人扩展信息",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验客户编号是否为空",
                "steps": [
                    {
                        "name": "校验客户编号",
                        "logic": "检查WS-CUST-NO是否为空",
                        "operation": None,
                    }
                ],
            },
            {
                "name": "查询基本信息",
                "name_cn": "查询客户基本信息",
                "logic": "查询CUSTOMER_BASIC_INFO获取基本信息",
                "steps": [
                    {
                        "name": "查询基本信息",
                        "logic": "按客户号查询客户基本信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": [
                                "CUST_NO",
                                "CUST_NM",
                                "GENDER_CD",
                                "CRTF_NO",
                                "CRTF_TYP_CD",
                                "CRTF_MATR_DT",
                                "DOM_OVERS_FLG_CD",
                                "STATE_AND_RGN_CD",
                                "ADDR",
                                "RSVD_MOBILE_NO",
                                "EMPLY_FLG",
                            ],
                        },
                    }
                ],
            },
            {
                "name": "查询个人信息",
                "name_cn": "查询个人客户扩展信息",
                "logic": "查询PERSONAL_CUSTOMER_INFO获取个人扩展信息",
                "steps": [
                    {
                        "name": "查询个人信息",
                        "logic": "按客户号查询个人客户扩展信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "PERSONAL_CUSTOMER_INFO",
                            "fields": [
                                "CUST_NO",
                                "ADMIN_CMPRMNT_CD",
                                "CAREER_TYP_CD",
                                "ETHNIC_CD",
                                "GENDER_CD",
                                "ADDR",
                                "SPS_NAME",
                                "SPS_CRTF_TYP_CD",
                                "SPS_CRTF_NO",
                                "SPS_TEL_NO",
                            ],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "14.QuryPerCustNameListInfoByCustNo.cbl",
        "program_id": "QURYPERCUSTNAMELIST",
        "name": "查询客户名单信息",
        "name_cn": "对私客户名单信息查询",
        "description": "查询个人客户的名单分类信息（黑名单/白名单/灰名单）",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验客户编号是否为空",
                "steps": [
                    {
                        "name": "校验客户编号",
                        "logic": "检查WS-CUST-NO是否为空",
                        "operation": None,
                    }
                ],
            },
            {
                "name": "查询名单信息",
                "name_cn": "查询客户名单信息",
                "logic": "查询PERSONAL_CUSTOMER_LIST获取名单分类",
                "steps": [
                    {
                        "name": "查询名单",
                        "logic": "按客户号查询个人客户名单信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "PERSONAL_CUSTOMER_LIST",
                            "fields": [
                                "CUST_NO",
                                "CRTF_TYP_CD",
                                "CRTF_NO",
                                "NM_SNGL_TYP_CD",
                                "DATA_SORC_CD",
                                "ORG_DISMN_CD",
                                "CTRL_FLG",
                                "CHK_FLG_CD",
                                "EFFT_DT",
                                "EFFT_TM",
                                "INVALID_DT",
                                "INVALID_TM",
                                "VALID_FLG",
                            ],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "15.QuryPerCustRiskLevel.cbl",
        "program_id": "QURYPERCUSTRISKLEVEL",
        "name": "查询客户风险等级",
        "name_cn": "对私客户风险等级信息查询",
        "description": "查询个人客户的风险等级评定信息",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "校验客户编号是否为空",
                "steps": [
                    {
                        "name": "校验客户编号",
                        "logic": "检查WS-CUST-NO是否为空",
                        "operation": None,
                    }
                ],
            },
            {
                "name": "查询基本信息",
                "name_cn": "查询客户基本信息",
                "logic": "查询CUSTOMER_BASIC_INFO获取客户类型",
                "steps": [
                    {
                        "name": "查询基本信息",
                        "logic": "按客户号查询客户基本信息获取客户类型",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_BASIC_INFO",
                            "fields": ["CUST_NO", "CUST_TYP_CD", "CUST_NM"],
                        },
                    }
                ],
            },
            {
                "name": "查询风险等级",
                "name_cn": "查询客户风险等级信息",
                "logic": "查询CUSTOMER_RISK_INFO获取风险等级详情",
                "steps": [
                    {
                        "name": "查询风险等级",
                        "logic": "按客户号和类型查询风险等级信息",
                        "operation": {
                            "type": "SELECT",
                            "table": "CUSTOMER_RISK_INFO",
                            "fields": [
                                "CUST_NO",
                                "CUST_TYP_CD",
                                "CUST_ATTN_EXTT_CD",
                                "EVALT_DT",
                                "RELS_DT",
                                "RELS_OR_ISU_ORG_NO",
                                "EVALT_ACRDGAS_COMNT",
                            ],
                        },
                    }
                ],
            },
        ],
    },
    {
        "cobol_file": "16.QurySignRelationInfo.cbl",
        "program_id": "QURYSIGNRELATIONINFO",
        "name": "查询签约关系",
        "name_cn": "客户签约关系查询",
        "description": "根据证件信息或账号查询客户的签约关系",
        "tasks": [
            {
                "name": "参数校验",
                "name_cn": "输入参数校验",
                "logic": "证件号码和客户账号不能同时为空",
                "steps": [
                    {
                        "name": "校验查询条件",
                        "logic": "检查CRTF-NO和CUST-ACCT-NO不能同时为空",
                        "operation": None,
                    },
                    {
                        "name": "校验证件类型",
                        "logic": "验证证件类型代码有效性",
                        "operation": None,
                    },
                ],
            },
            {
                "name": "查询签约关系",
                "name_cn": "查询客户签约关系",
                "logic": "查询SIGN_RELATION_INFO获取签约信息",
                "steps": [
                    {
                        "name": "查询签约",
                        "logic": "按证件信息或账号查询签约关系",
                        "operation": {
                            "type": "SELECT",
                            "table": "SIGN_RELATION_INFO",
                            "fields": [
                                "SIGN_NO",
                                "SIGN_TYPE",
                                "SIGN_STATUS",
                                "SIGN_DATE",
                                "SIGN_AMOUNT",
                                "SIGN_DESC",
                            ],
                        },
                    }
                ],
            },
        ],
    },
]

# ==================== COBOL代码片段提取函数 ====================


def read_cobol_file(cobol_file_path: str) -> str:
    """读取COBOL文件内容"""
    try:
        with open(cobol_file_path, "r", encoding="utf-8", errors="ignore") as f:
            return f.read()
    except Exception as e:
        print(f"警告: 无法读取COBOL文件 {cobol_file_path}: {e}")
        return ""



def append_cobol_snippet(cursor, element_id: str, snippet: str) -> None:
    """为元素追加COBOL代码片段（去重）"""
    if not snippet:
        return

    cursor.execute(
        "SELECT cobol_snippet FROM bnmp_elements WHERE element_id = ?",
        (element_id,),
    )
    row = cursor.fetchone()
    current = row[0] if row else None
    if not current:
        cursor.execute(
            "UPDATE bnmp_elements SET cobol_snippet = ? WHERE element_id = ?",
            (snippet, element_id),
        )
        return

    if snippet in current:
        return

    cursor.execute(
        "UPDATE bnmp_elements SET cobol_snippet = ? WHERE element_id = ?",
        (f"{current}\n\n{snippet}", element_id),
    )


def extract_sql_blocks(cobol_content: str) -> list:
    """提取COBOL中的 EXEC SQL 代码块"""
    if not cobol_content:
        return []

    lines = cobol_content.split("\n")
    blocks = []
    block = []
    in_block = False

    for line in lines:
        if not in_block and re.search(r"\bEXEC\s+SQL\b", line, re.IGNORECASE):
            in_block = True
            block = [line]
            if re.search(r"\bEND-EXEC\b", line, re.IGNORECASE):
                blocks.append(block)
                in_block = False
            continue

        if in_block:
            block.append(line)
            if re.search(r"\bEND-EXEC\b", line, re.IGNORECASE):
                blocks.append(block)
                in_block = False

    return ["\n".join(b).strip() for b in blocks if b]


def build_table_snippet_map(cobol_content: str, table_names: list) -> dict:
    """按表名收集SQL块，返回表名->代码片段"""
    blocks = extract_sql_blocks(cobol_content)
    if not blocks or not table_names:
        return {}

    table_map = {}
    for block in blocks:
        block_upper = block.upper()
        for table_name in table_names:
            if re.search(rf"\b{re.escape(table_name.upper())}\b", block_upper):
                table_map.setdefault(table_name, []).append(block)

    return {k: "\n\n".join(v) for k, v in table_map.items()}


def extract_task_code_snippet(
    cobol_content: str, task_name: str, task_index: int
) -> str:
    """从COBOL代码中提取任务的代码片段

    支持多种代码结构：
    1. 直接代码：*> 1) 参数校验 后面直接写代码
    2. PERFORM调用：*> 参数校验 PERFORM XXX
       需要提取PERFORM调用的段落代码
    """
    if not cobol_content:
        return ""

    lines = cobol_content.split("\n")

    # 构建段落名称到行号的映射
    paragraph_map = {}
    for i, line in enumerate(lines):
        # 匹配段落定义：在行首，以非空格开头，后面跟.
        if re.match(r"^[A-Z][A-Z0-9-]+\.", line.strip()):
            para_name = line.strip().split(".")[0]
            paragraph_map[para_name] = i

    # 找到主段落（MAIN-LOGIC, MAIN-PROCESS等）
    main_para_name = None
    main_para_start = -1
    for para_name in ["MAIN-LOGIC", "MAIN-PROCESS"]:
        if para_name in paragraph_map:
            main_para_name = para_name
            main_para_start = paragraph_map[para_name]
            break

    if main_para_start == -1:
        return ""

    # 找到任务注释 - 按优先级尝试不同的匹配方式
    task_comment_line = -1

    # 方法1：带编号的注释（*> 1) xxx）
    task_pattern_numbered = rf"^\s*\*>\s*{task_index}\)[.\s]"
    for i, line in enumerate(lines):
        if i < main_para_start:
            continue
        if re.search(task_pattern_numbered, line):
            task_comment_line = i
            break

    # 方法2：不带编号的注释，使用task_name匹配
    if task_comment_line == -1 and task_name:
        # 尝试匹配注释内容中的关键词
        task_name_keywords = (
            task_name.replace("校验", "")
            .replace("查询", "")
            .replace("归并", "")
            .replace("创建", "")
            .replace("管理", "")
            .replace("更新", "")
            .replace("参数", "")
            .replace("账户", "")
            .replace("路由", "")
            .replace("客户", "")
            .strip()
        )
        if task_name_keywords:
            for i, line in enumerate(lines):
                if i < main_para_start:
                    continue
                if re.search(r"^\s*\*[> ]\s*", line) and task_name_keywords in line:
                    task_comment_line = i
                    break

    # 方法3：不带编号的注释，使用段落号（对于task_index=2，找第3个注释，因为第1个是"初始化"）
    if task_comment_line == -1:
        comment_count = 0
        target_index = task_index
        # 如果task_index=2，说明前面有"初始化"注释，找第3个注释
        # 检查第1个注释是否是"初始化"
        for i, line in enumerate(lines):
            if i < main_para_start:
                continue
            if re.search(r"^\s*\*[> ]\s*初始化", line):
                # 找到"初始化"注释，task_index需要+1
                target_index = task_index + 1
                break

        # 使用调整后的target_index查找注释
        comment_count = 0
        for i, line in enumerate(lines):
            if i < main_para_start:
                continue
            if re.search(r"^\s*\*[> ]\s*[^\*\s]", line):
                comment_count += 1
                if comment_count == target_index:
                    task_comment_line = i
                    break

    if task_comment_line == -1:
        return ""

    # 从注释后开始，找到PERFORM语句或直接代码
    snippet_lines = []
    found_perform = False
    perform_target = ""

    for i in range(task_comment_line + 1, len(lines)):
        line = lines[i]
        stripped = line.strip()

        # 检查PERFORM语句
        if not found_perform and re.search(r"^\s*PERFORM\s+", line, re.IGNORECASE):
            match = re.search(r"^\s*PERFORM\s+([A-Z0-9-]+)", line, re.IGNORECASE)
            if match:
                perform_target = match.group(1)
                found_perform = True
                # 跳过PERFORM行，查找被调用的段落
                continue

        # 如果找到PERFORM，提取被调用的段落
        if found_perform and perform_target:
            if perform_target in paragraph_map:
                para_start = paragraph_map[perform_target]
                # 提取段落代码直到下一个段落定义
                for j in range(para_start + 1, len(lines)):
                    para_line = lines[j]
                    if re.match(r"^[A-Z][A-Z0-9-]+\.", para_line.strip()):
                        break
                    snippet_lines.append(para_line)
                break
            else:
                # 找不到段落，返回空
                return ""

        # 如果不是PERFORM调用，收集直接代码
        if not found_perform:
            # 收集代码行（跳过空行和注释）
            if stripped and not stripped.startswith("*"):
                snippet_lines.append(line)
                continue

            # 检查段落定义（只在行首）
            if re.match(r"^[A-Z][A-Z0-9-]+\.", stripped):
                if snippet_lines:
                    break
                continue

            # 检查下一个任务注释（只在收集了代码后才检查）
            if snippet_lines and re.search(r"^\s*\*[> ]\s*\d+\)[.\s]", line):
                break

            # 检查结束标记（只在行首）
            if snippet_lines and (
                "EXIT-PROGRAM" in stripped
                or "GOBACK" in stripped
                or "STOP RUN" in stripped
            ):
                # 检查是否是段落定义
                if "EXIT-PROGRAM." in stripped or "GOBACK." in stripped:
                    if snippet_lines:
                        break
                # 如果不是段落定义（如 GO TO EXIT-PROGRAM），不停止
                continue

    if not snippet_lines:
        return ""

    return "\n".join(snippet_lines).strip()


def extract_step_code_snippet(
    cobol_content: str,
    step_name: str,
    operation_type: str = None,
    operation_table: str = None,
    operation_fields: list = None,
) -> str:
    """从COBOL代码中提取步骤的代码片段

    优先按 EXEC SQL 块匹配操作类型和表名，必要时回退到字段命中段落。
    """
    if not cobol_content:
        return ""

    lines = cobol_content.split("\n")

    sql_blocks = []
    block = []
    in_block = False

    for line in lines:
        if not in_block and re.search(r"\bEXEC\s+SQL\b", line, re.IGNORECASE):
            in_block = True
            block = [line]
            if re.search(r"\bEND-EXEC\b", line, re.IGNORECASE):
                sql_blocks.append(block)
                in_block = False
            continue

        if in_block:
            block.append(line)
            if re.search(r"\bEND-EXEC\b", line, re.IGNORECASE):
                sql_blocks.append(block)
                in_block = False

    def block_matches(block_lines):
        if not operation_type and not operation_table:
            return False
        block_text = "\n".join(block_lines)
        op_ok = True
        table_ok = True
        if operation_type:
            op_ok = re.search(
                rf"\b{re.escape(operation_type)}\b", block_text, re.IGNORECASE
            )
        if operation_table:
            table_ok = re.search(
                rf"\b{re.escape(operation_table)}\b", block_text, re.IGNORECASE
            )
        return bool(op_ok and table_ok)

    if sql_blocks and (operation_type or operation_table):
        for block_lines in sql_blocks:
            if block_matches(block_lines):
                return "\n".join(block_lines).strip()

        for block_lines in sql_blocks:
            block_text = "\n".join(block_lines)
            if operation_type and re.search(
                rf"\b{re.escape(operation_type)}\b", block_text, re.IGNORECASE
            ):
                return "\n".join(block_lines).strip()
            if operation_table and re.search(
                rf"\b{re.escape(operation_table)}\b", block_text, re.IGNORECASE
            ):
                return "\n".join(block_lines).strip()

    if step_name:
        snippet_lines = []
        in_section = False
        for line in lines:
            stripped = line.strip()
            if not in_section and step_name in stripped:
                in_section = True
            if in_section:
                if stripped and not stripped.startswith("*"):
                    snippet_lines.append(line)
                if re.search(r"\bEND-IF\b|\bEND-EXEC\b", stripped):
                    break
        if snippet_lines:
            return "\n".join(snippet_lines).strip()

    if operation_fields:
        field_tokens = []
        for field in operation_fields:
            if not field:
                continue
            token = field.replace("_", "-")
            if len(token) < 3:
                continue
            field_tokens.append(token)
        if field_tokens:
            paragraph_starts = []
            for i, line in enumerate(lines):
                if re.match(r"^[A-Z][A-Z0-9-]+\.", line.strip()):
                    paragraph_starts.append(i)

            best_start = None
            best_end = None
            best_score = 0
            for idx, start in enumerate(paragraph_starts):
                end = (
                    paragraph_starts[idx + 1]
                    if idx + 1 < len(paragraph_starts)
                    else len(lines)
                )
                block_text = "\n".join(lines[start + 1 : end])
                score = sum(1 for token in field_tokens if token in block_text)
                if score > best_score:
                    best_score = score
                    best_start = start
                    best_end = end
            if best_start is not None and best_score > 0:
                snippet = "\n".join(lines[best_start + 1 : best_end]).strip()
                if snippet:
                    return snippet

    return ""

def extract_process_code_snippet(cobol_content: str) -> str:
    """提取整个流程的代码片段（从 PROCEDURE DIVISION 开始）"""
    if not cobol_content:
        return ""

    lines = cobol_content.split("\n")
    snippet_lines = []
    in_procedure = False

    for line in lines:
        if "PROCEDURE DIVISION" in line:
            in_procedure = True
            snippet_lines.append(line)
            continue

        if in_procedure:
            snippet_lines.append(line)

    return "\n".join(snippet_lines).strip()


# ==================== 数据存储函数 ====================


def save_entities(conn):
    """保存业务实体和属性到数据库"""
    cursor = conn.cursor()
    entity_ids = {}  # 存储实体ID用于后续关联

    for entity in BUSINESS_ENTITIES:
        entity_id = generate_id()
        entity_ids[entity["table_name"]] = entity_id

        # 插入业务实体
        cursor.execute(
            """
        INSERT INTO bnmp_elements (
            element_id, element_type, element_name, element_name_cn,
            description, description_cn, table_name, entity_summary
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        """,
            (
                entity_id,
                "ENTITY",
                entity["table_name"],
                entity["name_cn"],
                f"Business entity for {entity['table_name']}",
                entity["summary"],
                entity["table_name"],
                entity["summary"],
            ),
        )

        # 插入实体属性
        for attr in entity["attributes"]:
            attr_id = generate_id()
            fk_ref = attr.get("fk", None)

            cursor.execute(
                """
            INSERT INTO bnmp_elements (
                element_id, element_type, element_name, element_name_cn,
                description, field_name, field_type, field_length,
                field_rules, foreign_key_ref, parent_entity_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
                (
                    attr_id,
                    "ATTRIBUTE",
                    attr["name"],
                    attr["name_cn"],
                    attr["name_cn"],
                    attr["name"],
                    attr["type"],
                    attr["length"],
                    attr["rules"],
                    fk_ref,
                    entity_id,
                ),
            )

            # 创建实体与属性的关联关系
            cursor.execute(
                """
            INSERT INTO bnmp_element_relations (
                relation_id, source_element_id, target_element_id,
                relation_type, description
            ) VALUES (?, ?, ?, ?, ?)
            """,
                (
                    generate_id(),
                    entity_id,
                    attr_id,
                    "ENTITY_ATTRIBUTE",
                    f"Entity {entity['table_name']} contains attribute {attr['name']}",
                ),
            )

    conn.commit()
    return entity_ids


def save_processes(conn, entity_ids):
    """保存流程、任务、步骤到数据库"""
    cursor = conn.cursor()

    script_dir = os.path.dirname(os.path.abspath(__file__))
    cobol_dir = os.path.join(script_dir, "cobol")

    if not os.path.exists(cobol_dir):
        print(f"警告: COBOL目录不存在: {cobol_dir}")
        print("将不提取代码片段，继续处理...")

    for process_idx, process in enumerate(BUSINESS_PROCESSES):
        process_id = generate_id()

        cobol_file_path = os.path.join(cobol_dir, process["cobol_file"])
        cobol_content = read_cobol_file(cobol_file_path)
        process_snippet = extract_process_code_snippet(cobol_content)
        sql_blocks = extract_sql_blocks(cobol_content)
        table_snippet_map = build_table_snippet_map(
            cobol_content, list(entity_ids.keys())
        )

        # 插入流程
        cursor.execute(
            """
        INSERT INTO bnmp_elements (
            element_id, element_type, element_name, element_name_cn,
            description, description_cn, cobol_file_name, program_id, cobol_snippet
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
            (
                process_id,
                "PROCESS",
                process["name"],
                process["name_cn"],
                process["description"],
                process["description"],
                process["cobol_file"],
                process["program_id"],
                process_snippet,
            ),
        )

        prev_task_id = None
        task_order = 0

        for task_idx, task in enumerate(process["tasks"], 1):
            task_id = generate_id()
            task_order += 1

            task_snippet = extract_task_code_snippet(
                cobol_content, task["name_cn"], task_idx
            )

            # 插入任务
            cursor.execute(
                """
            INSERT INTO bnmp_elements (
                element_id, element_type, element_name, element_name_cn,
                description, task_logic, cobol_file_name, cobol_snippet
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """,
                (
                    task_id,
                    "TASK",
                    task["name"],
                    task["name_cn"],
                    task["logic"],
                    task["logic"],
                    process["cobol_file"],
                    task_snippet,
                ),
            )

            # 创建流程与任务的关联
            cursor.execute(
                """
            INSERT INTO bnmp_element_relations (
                relation_id, source_element_id, target_element_id,
                relation_type, relation_order, description
            ) VALUES (?, ?, ?, ?, ?, ?)
            """,
                (
                    generate_id(),
                    process_id,
                    task_id,
                    "PROCESS_TASK",
                    task_order,
                    f"Process {process['name']} contains task {task['name']}",
                ),
            )

            # 创建任务之间的前后关系
            if prev_task_id:
                cursor.execute(
                    """
                INSERT INTO bnmp_element_relations (
                    relation_id, source_element_id, target_element_id,
                    relation_type, relation_order, description
                ) VALUES (?, ?, ?, ?, ?, ?)
                """,
                    (
                        generate_id(),
                        prev_task_id,
                        task_id,
                        "TASK_TASK",
                        task_order,
                        f"Task {task['name']} follows previous task",
                    ),
                )

            prev_step_id = None
            step_order = 0

            for step in task["steps"]:
                step_id = generate_id()
                step_order += 1

                operation = step.get("operation")
                operation_type = operation["type"] if operation else None
                operation_table = operation["table"] if operation else None
                operation_fields = operation["fields"] if operation else None
                operation_desc = None
                if operation:
                    operation_desc = f"{operation['type']} on {operation['table']}: {', '.join(operation['fields'])}"

                step_snippet = extract_step_code_snippet(
                    cobol_content,
                    step["name"],
                    operation_type,
                    operation_table,
                    operation_fields,
                )
                if not step_snippet and operation_table:
                    step_snippet = table_snippet_map.get(operation_table, "")
                if not step_snippet and operation_type:
                    for block in sql_blocks:
                        if re.search(
                            rf"\b{re.escape(operation_type)}\b", block, re.IGNORECASE
                        ):
                            step_snippet = block
                            break

                # 插入步骤
                cursor.execute(
                    """
                INSERT INTO bnmp_elements (
                    element_id, element_type, element_name, element_name_cn,
                    description, step_logic, data_operation_type,
                    data_operation_desc, cobol_file_name, step_cobol_snippet
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                    (
                        step_id,
                        "STEP",
                        step["name"],
                        step["name"],
                        step["logic"],
                        step["logic"],
                        operation_type,
                        operation_desc,
                        process["cobol_file"],
                        step_snippet,
                    ),
                )

                # 创建任务与步骤的关联
                cursor.execute(
                    """
                INSERT INTO bnmp_element_relations (
                    relation_id, source_element_id, target_element_id,
                    relation_type, relation_order, description
                ) VALUES (?, ?, ?, ?, ?, ?)
                """,
                    (
                        generate_id(),
                        task_id,
                        step_id,
                        "TASK_STEP",
                        step_order,
                        f"Task {task['name']} contains step {step['name']}",
                    ),
                )

                # 创建步骤之间的前后关系
                if prev_step_id:
                    cursor.execute(
                        """
                    INSERT INTO bnmp_element_relations (
                        relation_id, source_element_id, target_element_id,
                        relation_type, relation_order, description
                    ) VALUES (?, ?, ?, ?, ?, ?)
                    """,
                        (
                            generate_id(),
                            prev_step_id,
                            step_id,
                            "STEP_STEP",
                            step_order,
                            f"Step {step['name']} follows previous step",
                        ),
                    )

                # 创建步骤与业务实体的关联
                if operation:
                    table_name = operation["table"]
                    if table_name in entity_ids:
                        entity_id = entity_ids[table_name]
                        cursor.execute(
                            """
                        INSERT INTO bnmp_element_relations (
                            relation_id, source_element_id, target_element_id,
                            relation_type, description
                        ) VALUES (?, ?, ?, ?, ?)
                        """,
                            (
                                generate_id(),
                                step_id,
                                entity_id,
                                "STEP_ENTITY",
                                f"Step {step['name']} operates on entity {table_name}",
                            ),
                        )


                        append_cobol_snippet(cursor, entity_id, step_snippet)

                        # 创建步骤与属性的关联
                        for field in operation["fields"]:
                            cursor.execute(
                                """
                            SELECT element_id FROM bnmp_elements
                            WHERE element_type = 'ATTRIBUTE'
                            AND parent_entity_id = ?
                            AND field_name = ?
                            """,
                                (entity_id, field),
                            )
                            result = cursor.fetchone()
                            if result:
                                cursor.execute(
                                    """
                                INSERT INTO bnmp_element_relations (
                                    relation_id, source_element_id, target_element_id,
                                    relation_type, description
                                ) VALUES (?, ?, ?, ?, ?)
                                """,
                                    (
                                        generate_id(),
                                        step_id,
                                        result[0],
                                        "STEP_ATTRIBUTE",
                                        f"Step {step['name']} operates on field {field}",
                                    ),
                                )


                                append_cobol_snippet(cursor, result[0], step_snippet)

                prev_step_id = step_id

            prev_task_id = task_id

    conn.commit()


def print_statistics(conn):
    """打印统计信息"""
    cursor = conn.cursor()

    print("\n" + "=" * 60)
    print("BNMP 2.0 业务模型分析统计")
    print("=" * 60)

    # 元素统计
    cursor.execute(
        "SELECT element_type, COUNT(*) FROM bnmp_elements GROUP BY element_type"
    )
    print("\n【元素统计】")
    for row in cursor.fetchall():
        type_names = {
            "ENTITY": "业务实体",
            "ATTRIBUTE": "实体属性",
            "PROCESS": "业务流程",
            "TASK": "流程任务",
            "STEP": "任务步骤",
        }
        print(f"  {type_names.get(row[0], row[0])}: {row[1]}个")

    # 关系统计
    cursor.execute(
        "SELECT relation_type, COUNT(*) FROM bnmp_element_relations GROUP BY relation_type"
    )
    print("\n【关系统计】")
    for row in cursor.fetchall():
        type_names = {
            "ENTITY_ATTRIBUTE": "实体-属性关联",
            "PROCESS_TASK": "流程-任务关联",
            "TASK_STEP": "任务-步骤关联",
            "TASK_TASK": "任务前后关联",
            "STEP_STEP": "步骤前后关联",
            "STEP_ENTITY": "步骤-实体关联",
            "STEP_ATTRIBUTE": "步骤-属性关联",
        }
        print(f"  {type_names.get(row[0], row[0])}: {row[1]}个")

    # 业务实体列表
    cursor.execute(
        "SELECT element_name, element_name_cn, entity_summary FROM bnmp_elements WHERE element_type = 'ENTITY'"
    )
    print("\n【业务实体清单】")
    for i, row in enumerate(cursor.fetchall(), 1):
        print(f"  {i}. {row[0]} ({row[1]})")
        print(f"     描述: {row[2][:60]}...")

    # 业务流程列表
    cursor.execute(
        "SELECT element_name, element_name_cn, cobol_file_name, program_id FROM bnmp_elements WHERE element_type = 'PROCESS'"
    )
    print("\n【业务流程清单】")
    for i, row in enumerate(cursor.fetchall(), 1):
        print(f"  {i}. {row[0]} ({row[1]})")
        print(f"     COBOL文件: {row[2]}, 程序ID: {row[3]}")

    print("\n" + "=" * 60)


def main():
    """主函数"""
    script_dir = os.path.dirname(os.path.abspath(__file__))
    db_path = os.path.join(script_dir, "bnmp_model_v3_new.db")

    print(f"脚本所在目录: {script_dir}")
    print(f"数据库路径: {db_path}")
    print(f"工作目录: {os.getcwd()}")

    # 删除已存在的数据库
    if os.path.exists(db_path):
        print(f"删除已存在的数据库: {db_path}")
        try:
            os.remove(db_path)
        except:
            print(f"警告: 无法删除旧数据库，继续生成新数据库")

    print("初始化BNMP 2.0业务模型数据库...")
    try:
        conn = init_database(db_path)
    except Exception as e:
        print(f"数据库初始化失败: {e}")
        raise

    print("分析并保存业务实体...")
    entity_ids = save_entities(conn)

    print("分析并保存业务流程...")
    save_processes(conn, entity_ids)

    print_statistics(conn)

    conn.close()
    print(f"\n数据库已保存到: {db_path}")

    return db_path


if __name__ == "__main__":
    main()










