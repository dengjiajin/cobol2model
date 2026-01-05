#!/usr/bin/env python3
"""
BNMP 2.0 业务模型分析服务端
提供从数据库读取业务建模元素和元素关系的RESTful API接口
"""

import os
import json
import sqlite3
import logging
from typing import List, Dict, Any, Optional, Set
from contextlib import contextmanager
from urllib.parse import unquote

from fastapi import FastAPI, HTTPException, Form, Query, Request, UploadFile, File
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse, Response
from pydantic import BaseModel
import uvicorn
import re

# 配置日志
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# 配置
DATABASE_PATH = os.environ.get("BNMP_DB_PATH", "./bnmp_model.db")

# FastAPI应用
app = FastAPI(
    title="BNMP 2.0 业务模型分析服务",
    description="提供从数据库读取业务建模元素和元素关系的RESTful API接口",
    version="1.0.0",
    swagger_ui_parameters={"defaultModelsExpandDepth": -1},  # 可选：优化显示
)

# 👇 关键：替换 Swagger UI 的 CDN 为国内更稳定的源
from fastapi.openapi.docs import (
    get_swagger_ui_html,
    get_swagger_ui_oauth2_redirect_html,
)

@app.get("/docs", include_in_schema=False)
async def custom_swagger_ui_html():
    return get_swagger_ui_html(
        openapi_url=app.openapi_url,
        title=app.title + " - Swagger UI",
        swagger_js_url="https://unpkg.com/swagger-ui-dist@5.17.14/swagger-ui-bundle.js",
        swagger_css_url="https://unpkg.com/swagger-ui-dist@5.17.14/swagger-ui.css",
    )

@app.get(app.swagger_ui_oauth2_redirect_url, include_in_schema=False)
async def swagger_ui_redirect():
    return get_swagger_ui_oauth2_redirect_html()

# CORS配置
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ==================== 类型映射 ====================

# 选项类型到数据库element_type的映射
OPTION_TYPE_MAP = {
    "业务实体": "ENTITY",
    "实体属性": "ATTRIBUTE",
    "流程": "PROCESS",
    "工作事项": "TASK",
    "任务": "TASK",
    "步骤": "STEP",
}

# 关系类型映射
RELATION_TYPE_MAP = {
    "业务实体(A)-实体属性(B)": "ENTITY_ATTRIBUTE",
    "业务实体-实体属性": "ENTITY_ATTRIBUTE",
    "流程-任务": "PROCESS_TASK",
    "任务-步骤": "TASK_STEP",
    "任务-任务": "TASK_TASK",
    "步骤-步骤": "STEP_STEP",
    "步骤-业务实体": "STEP_ENTITY",
    "步骤-实体属性": "STEP_ATTRIBUTE",
}


# ==================== 数据库操作 ====================


@contextmanager
def get_db_connection():
    """获取数据库连接的上下文管理器"""
    conn = sqlite3.connect(DATABASE_PATH)
    conn.row_factory = sqlite3.Row
    try:
        yield conn
    finally:
        conn.close()


def get_all_elements() -> List[Dict]:
    """获取所有元素"""
    with get_db_connection() as conn:
        cursor = conn.cursor()
        cursor.execute("""
                       SELECT *
                       FROM bnmp_elements
                       ORDER BY element_type, element_name
                       """)
        rows = cursor.fetchall()
        return [dict(row) for row in rows]


def get_elements_by_type(element_type: str) -> List[Dict]:
    """根据类型获取元素"""
    with get_db_connection() as conn:
        cursor = conn.cursor()
        cursor.execute(
            """
                       SELECT *
                       FROM bnmp_elements
                       WHERE element_type = ?
                       ORDER BY element_name
                       """,
            (element_type,),
        )
        rows = cursor.fetchall()
        return [dict(row) for row in rows]


def get_elements_by_cobol_and_type(
    cobol_file_name: Optional[str], element_type: Optional[str]
) -> List[Dict]:
    """按 cobol_file_name + element_type 组合查询元素；参数任意为空则不作为过滤条件（查全量/单条件/双条件）"""
    with get_db_connection() as conn:
        cursor = conn.cursor()
        cursor.execute(
            """
                       SELECT *
                       FROM bnmp_elements
                       WHERE (? IS NULL OR cobol_file_name = ?)
                         AND (? IS NULL OR element_type = ?)
                       ORDER BY element_type, element_name
                       """,
            (cobol_file_name, cobol_file_name, element_type, element_type),
        )
        rows = cursor.fetchall()
        return [dict(row) for row in rows]


def get_element_by_id(element_id: str) -> Optional[Dict]:
    """根据ID获取元素"""
    with get_db_connection() as conn:
        cursor = conn.cursor()
        cursor.execute(
            """
                       SELECT *
                       FROM bnmp_elements
                       WHERE element_id = ?
                       """,
            (element_id,),
        )
        row = cursor.fetchone()
        return dict(row) if row else None


def get_relations_by_type(relation_type: str) -> List[Dict]:
    """根据类型获取关系"""
    with get_db_connection() as conn:
        cursor = conn.cursor()
        cursor.execute(
            """
                       SELECT r.*,
                              s.element_name    as source_name,
                              s.element_name_cn as source_name_cn,
                              s.element_type    as source_type,
                              t.element_name    as target_name,
                              t.element_name_cn as target_name_cn,
                              t.element_type    as target_type
                       FROM bnmp_element_relations r
                                LEFT JOIN bnmp_elements s ON r.source_element_id = s.element_id
                                LEFT JOIN bnmp_elements t ON r.target_element_id = t.element_id
                       WHERE r.relation_type = ?
                       ORDER BY r.relation_order
                       """,
            (relation_type,),
        )
        rows = cursor.fetchall()
        return [dict(row) for row in rows]


def get_all_relations() -> List[Dict]:
    """获取所有关系"""
    with get_db_connection() as conn:
        cursor = conn.cursor()
        cursor.execute("""
                       SELECT r.*,
                              s.element_name    as source_name,
                              s.element_name_cn as source_name_cn,
                              s.element_type    as source_type,
                              t.element_name    as target_name,
                              t.element_name_cn as target_name_cn,
                              t.element_type    as target_type
                       FROM bnmp_element_relations r
                                LEFT JOIN bnmp_elements s ON r.source_element_id = s.element_id
                                LEFT JOIN bnmp_elements t ON r.target_element_id = t.element_id
                       ORDER BY r.relation_type, r.relation_order
                       """)
        rows = cursor.fetchall()
        return [dict(row) for row in rows]


def resolve_major_element_ids(major_list: List[Dict]) -> Set[str]:
    """
    根据 majorElements 中的 type + name + description，
    从 bnmp_elements 表中解析出唯一的 element_id 集合。

    示例：
    [
        {
            "type": "实体属性",
            "name": "TENANT_NO",
            "description": "检查记录存在后更新记录"
        }
    ]
    """
    if not major_list:
        return set()

    ids: Set[str] = set()

    with get_db_connection() as conn:
        cursor = conn.cursor()

        for item in major_list:
            opt_type = item.get("type")
            name = item.get("name")
            desc = item.get("description")

            # 严格按 type + name + description 三个字段来定位
            if not opt_type or not name or not desc:
                continue

            # “实体属性”、“流程”等中文 → element_type（ATTRIBUTE / ENTITY / PROCESS / TASK / STEP）
            element_type = OPTION_TYPE_MAP.get(opt_type)
            if not element_type:
                continue

            if element_type == "ATTRIBUTE":
                # 属性：可能字段名在 field_name，也可能在 element_name，这里都尝试匹配
                cursor.execute(
                    """
                    SELECT element_id
                    FROM bnmp_elements
                    WHERE element_type = ?
                      AND (element_name = ? OR field_name = ?)
                      AND description = ?
                    """,
                    (element_type, name, name, desc),
                )
            else:
                # 其他类型：用 element_name + description 精确定位
                cursor.execute(
                    """
                    SELECT element_id
                    FROM bnmp_elements
                    WHERE element_type = ?
                      AND element_name = ?
                      AND description = ?
                    """,
                    (element_type, name, desc),
                )

            rows = cursor.fetchall()
            for row in rows:
                ids.add(str(row["element_id"]))

    return ids


# ==================== 数据转换 ====================


def format_element_for_response(elem: Dict, option: str = None) -> Dict:
    """将数据库元素格式化为API响应格式"""

    # 基础字段
    result = {
        "type": option or get_option_from_type(elem.get("element_type")),
        "id": elem.get("element_id"),
        "name": elem.get("element_name"),
        "name_cn": elem.get("element_name_cn"),
        "description": elem.get("description") or elem.get("description_cn"),
    }

    element_type = elem.get("element_type")

    # 根据类型添加特定字段
    if element_type == "ENTITY":
        result["table_name"] = elem.get("table_name")
        result["entity_summary"] = elem.get("entity_summary")

    elif element_type == "ATTRIBUTE":
        result["field_name"] = elem.get("field_name")
        result["field_type"] = elem.get("field_type")
        result["field_length"] = elem.get("field_length")
        result["field_rules"] = elem.get("field_rules")
        result["foreign_key_ref"] = elem.get("foreign_key_ref")
        result["parent_entity_id"] = elem.get("parent_entity_id")

    elif element_type == "PROCESS":
        result["program_id"] = elem.get("program_id")
        result["cobol_file_name"] = elem.get("cobol_file_name")

    elif element_type == "TASK":
        result["task_logic"] = elem.get("task_logic")
        result["cobol_snippet"] = elem.get("cobol_snippet")

    elif element_type == "STEP":
        result["step_logic"] = elem.get("step_logic")
        result["data_operation_type"] = elem.get("data_operation_type")
        result["data_operation_desc"] = elem.get("data_operation_desc")
        result["cobol_snippet"] = elem.get("step_cobol_snippet")

    # 添加source字段（模拟从COBOL提取的来源信息）
    result["source"] = [
        {
            "extracted_source": elem.get("cobol_snippet")
            or elem.get("step_cobol_snippet")
            or "",
            "type": "COBOL",
            "name": elem.get("cobol_file_name") or "",
        }
    ]

    return result


def get_option_from_type(element_type: str) -> str:
    """从element_type获取option名称"""
    reverse_map = {v: k for k, v in OPTION_TYPE_MAP.items()}
    return reverse_map.get(element_type, element_type)


def get_option_from_relation_type(relation_type: str) -> str:
    """从relation_type获取option名称"""
    reverse_map = {v: k for k, v in RELATION_TYPE_MAP.items()}
    return reverse_map.get(relation_type, relation_type)


# ==================== 请求模型 ====================


class ConnectRequest(BaseModel):
    majorElements: str
    minorElements: Optional[str] = "[]"
    option: Optional[str] = None


# ==================== API接口 ====================


@app.get("/")
async def root():
    """服务根路径 - 返回前端页面"""
    script_dir = os.path.dirname(os.path.abspath(__file__))
    # 尝试多个可能的HTML文件名
    html_files = ["index.html", "bnmp_frontend.html", "frontend.html"]
    for html_name in html_files:
        html_path = os.path.join(script_dir, html_name)
        if os.path.exists(html_path):
            return FileResponse(html_path, media_type="text/html")
    return {
        "service": "BNMP 2.0 业务模型分析服务",
        "version": "1.0.0",
        "message": "前端页面未找到，请将 bnmp_frontend.html 重命名为 index.html 并放在同一目录",
        "endpoints": [
            {
                "path": "/api/py/extract/keyword",
                "method": "GET",
                "description": "元素提取接口 - 从数据库读取业务建模元素",
            },
            {
                "path": "/api/py/extract/keyword2",
                "method": "GET",
                "description": "元素提取接口2 - cobol_file_name + option 组合查询",
            },
            {
                "path": "/api/py/generate/connect",
                "method": "POST",
                "description": "关系生成接口 - 从数据库读取元素关系",
            },
            {
                "path": "/api/cobol/to-markdown",
                "method": "POST",
                "description": "COBOL转Markdown接口 - 上传COBOL文件返回Markdown文档",
            },
        ],
        "supported_options": {
            "element_types": list(OPTION_TYPE_MAP.keys()),
            "relation_types": list(RELATION_TYPE_MAP.keys()),
        },
    }


@app.get("/api")
async def api_info():
    """服务根路径"""
    return {
        "service": "BNMP 2.0 业务模型分析服务",
        "version": "1.0.0",
        "endpoints": [
            {
                "path": "/api/py/extract/keyword",
                "method": "GET",
                "description": "元素提取接口 - 从数据库读取业务建模元素",
            },
            {
                "path": "/api/py/extract/keyword2",
                "method": "GET",
                "description": "元素提取接口2 - cobol_file_name + option 组合查询",
            },
            {
                "path": "/api/py/generate/connect",
                "method": "POST",
                "description": "关系生成接口 - 从数据库读取元素关系",
            },
            {
                "path": "/api/cobol/to-markdown",
                "method": "POST",
                "description": "COBOL转Markdown接口 - 上传COBOL文件返回Markdown文档",
            },
        ],
        "supported_options": {
            "element_types": list(OPTION_TYPE_MAP.keys()),
            "relation_types": list(RELATION_TYPE_MAP.keys()),
        },
    }


@app.get("/api/py/extract/keyword")
async def extract_keyword(
    filePath: str = Query(
        None, description="COBOL代码文件路径（可选，仅用于日志记录）"
    ),
    option: str = Query(
        None,
        description="支持的选项类型：业务实体、实体属性、流程、任务、步骤。为空则返回所有类型",
    ),
):
    """
    元素提取接口（GET）

    从bnmp_model.db数据库读取业务建模元素

    - **filePath**: COBOL代码文件路径（可选，仅用于日志记录）
    - **option**: 选项类型（业务实体、实体属性、流程、任务、步骤），为空返回所有类型

    返回：提取的关键词JSON数组
    """
    logger.info(f"元素提取请求: filePath={filePath}, option={option}")

    # 检查数据库是否存在
    if not os.path.exists(DATABASE_PATH):
        raise HTTPException(
            status_code=500, detail=f"数据库文件不存在: {DATABASE_PATH}"
        )

    try:
        if option:
            # 验证选项类型
            if option not in OPTION_TYPE_MAP:
                raise HTTPException(
                    status_code=400,
                    detail=f"不支持的选项类型: {option}。支持的类型: {list(OPTION_TYPE_MAP.keys())}",
                )

            # 获取指定类型的元素
            element_type = OPTION_TYPE_MAP[option]
            elements = get_elements_by_type(element_type)

            # 格式化结果
            result = [format_element_for_response(elem, option) for elem in elements]
        else:
            # 获取所有元素
            elements = get_all_elements()

            # 格式化结果
            result = [format_element_for_response(elem) for elem in elements]

        logger.info(f"元素提取完成: 共{len(result)}个元素")
        return result

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"元素提取失败: {e}")
        raise HTTPException(status_code=500, detail=f"元素提取失败: {str(e)}")


@app.get("/api/py/extract/keyword2")
async def extract_keyword2(
    fileName: str = Query(None, description="COBOL文件名（可选）。不传则不按文件过滤"),
    option: str = Query(
        None,
        description="选项（可选）。支持：业务实体、实体属性、流程、工作事项、步骤。不传则不按类型过滤",
    ),
):
    """
    元素提取接口（GET）- keyword2

    查询规则：
    1) 两个参数都非必传
    2) 全没传 -> 查全部
    3) 传了一个 -> where 一个
    4) 两个都传 -> where 两个（AND：cobol_file_name + element_type）
    5) 当查询业务实体或实体属性且提供COBOL文件名时，使用间接关联查询（文件→流程→任务→步骤→实体/属性）
    """
    logger.info(f"元素提取请求(keyword2): fileName={fileName}, option={option}")

    # 检查数据库是否存在
    if not os.path.exists(DATABASE_PATH):
        raise HTTPException(
            status_code=500, detail=f"数据库文件不存在: {DATABASE_PATH}"
        )

    try:
        # 检查是否需要使用间接关联查询
        use_indirect_query = False
        if fileName and option and option in ["业务实体", "实体属性"]:
            use_indirect_query = True
        elif fileName and not option:
            # 如果只传了fileName，先尝试直接查询流程
            elements = get_elements_by_cobol_and_type(fileName, "PROCESS")
            if not elements:
                # 如果没有直接匹配的流程，尝试间接关联查询
                use_indirect_query = True

        if use_indirect_query:
            # 使用间接关联查询：COBOL文件 → 流程 → 任务 → 步骤 → 实体/属性
            with get_db_connection() as conn:
                cursor = conn.cursor()

                # 构建查询条件
                if option:
                    target_element_type = OPTION_TYPE_MAP[option]
                    cursor.execute(
                        """
                                     SELECT DISTINCT e4.element_id, e4.element_type, e4.element_name, e4.element_name_cn, 
                                            e4.description, e4.description_cn, e4.entity_summary, e4.table_name, e4.field_name, 
                                            e4.field_type, e4.field_length, e4.field_rules, e4.foreign_key_ref,
                                            e4.parent_entity_id, COALESCE(efs.cobol_snippet, e4.cobol_snippet) AS cobol_snippet, e4.step_cobol_snippet, e1.cobol_file_name
                                   FROM bnmp_elements e1
                                   JOIN bnmp_element_relations r1 ON e1.element_id = r1.source_element_id AND r1.relation_type = 'PROCESS_TASK'
                                   JOIN bnmp_elements e2 ON r1.target_element_id = e2.element_id
                                   JOIN bnmp_element_relations r2 ON e2.element_id = r2.source_element_id AND r2.relation_type = 'TASK_STEP'
                                   JOIN bnmp_elements e3 ON r2.target_element_id = e3.element_id
                                   JOIN bnmp_element_relations r3 ON e3.element_id = r3.source_element_id AND r3.relation_type IN ('STEP_ENTITY', 'STEP_ATTRIBUTE')
                                   JOIN bnmp_elements e4 ON r3.target_element_id = e4.element_id
                                     LEFT JOIN bnmp_element_file_snippets efs ON efs.element_id = e4.element_id AND efs.cobol_file_name = e1.cobol_file_name
                                   WHERE e1.cobol_file_name = ? AND e4.element_type = ?
                                   ORDER BY e4.element_type, e4.element_name
                                   """,
                        (fileName, target_element_type),
                    )
                else:
                    # 查询所有相关实体和属性
                    cursor.execute(
                        """
                                     SELECT DISTINCT e4.element_id, e4.element_type, e4.element_name, e4.element_name_cn, 
                                            e4.description, e4.description_cn, e4.entity_summary, e4.table_name, e4.field_name, 
                                            e4.field_type, e4.field_length, e4.field_rules, e4.foreign_key_ref,
                                            e4.parent_entity_id, COALESCE(efs.cobol_snippet, e4.cobol_snippet) AS cobol_snippet, e4.step_cobol_snippet, e1.cobol_file_name
                                   FROM bnmp_elements e1
                                   JOIN bnmp_element_relations r1 ON e1.element_id = r1.source_element_id AND r1.relation_type = 'PROCESS_TASK'
                                   JOIN bnmp_elements e2 ON r1.target_element_id = e2.element_id
                                   JOIN bnmp_element_relations r2 ON e2.element_id = r2.source_element_id AND r2.relation_type = 'TASK_STEP'
                                   JOIN bnmp_elements e3 ON r2.target_element_id = e3.element_id
                                   JOIN bnmp_element_relations r3 ON e3.element_id = r3.source_element_id AND r3.relation_type IN ('STEP_ENTITY', 'STEP_ATTRIBUTE')
                                   JOIN bnmp_elements e4 ON r3.target_element_id = e4.element_id
                                     LEFT JOIN bnmp_element_file_snippets efs ON efs.element_id = e4.element_id AND efs.cobol_file_name = e1.cobol_file_name
                                   WHERE e1.cobol_file_name = ?
                                   ORDER BY e4.element_type, e4.element_name
                                   """,
                        (fileName,),
                    )

                rows = cursor.fetchall()
                result = []

                for row in rows:
                    elem_dict = dict(row)
                    # 使用format_element_for_response格式化结果
                    if option:
                        formatted = format_element_for_response(elem_dict, option)
                    else:
                        formatted = format_element_for_response(elem_dict)
                    result.append(formatted)

                logger.info(f"间接关联查询完成(keyword2): 共{len(result)}个元素")
                return result
        else:
            # 使用原有查询逻辑
            element_type = None
            if option:
                if option not in OPTION_TYPE_MAP:
                    raise HTTPException(
                        status_code=400,
                        detail=f"不支持的选项类型: {option}。支持的类型: {list(OPTION_TYPE_MAP.keys())}",
                    )
                element_type = OPTION_TYPE_MAP[option]

            elements = get_elements_by_cobol_and_type(fileName, element_type)

            # 格式化结果：如果传了 option，用它作为 type；否则按 element_type 反推
            if option:
                result = [
                    format_element_for_response(elem, option) for elem in elements
                ]
            else:
                result = [format_element_for_response(elem) for elem in elements]

            logger.info(f"直接查询完成(keyword2): 共{len(result)}个元素")
            return result

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"元素提取失败(keyword2): {e}")
        raise HTTPException(status_code=500, detail=f"元素提取失败: {str(e)}")


@app.post("/api/py/generate/connect")
async def generate_connect(payload: ConnectRequest):
    """
    关系生成接口

    从bnmp_model.db数据库读取元素关系

    - **majorElements**: 主元素JSON字符串（可为空数组），例如：
        [
            {
                "type": "实体属性",
                "name": "TENANT_NO",
                "description": "检查记录存在后更新记录"
            }
        ]
      注意：不再需要 id，后端使用 type + name + description 解析 element_id

    - **minorElements**: 次元素JSON字符串（可为空数组）
    - **option**: 连接选项（业务实体-实体属性、流程-任务、任务-步骤等），为空返回所有类型关系

    返回：子级元素列表（仅 target_element），包含 type, id, code, name
    """
    majorElements = payload.majorElements
    minorElements = payload.minorElements or "[]"
    option = payload.option

    logger.info(f"关系生成请求: option={option}")

    # 检查数据库是否存在
    if not os.path.exists(DATABASE_PATH):
        raise HTTPException(
            status_code=500, detail=f"数据库文件不存在: {DATABASE_PATH}"
        )

    # 验证连接选项（如果提供了option）
    if option and option not in RELATION_TYPE_MAP:
        raise HTTPException(
            status_code=400,
            detail=f"不支持的连接选项: {option}。支持的选项: {list(RELATION_TYPE_MAP.keys())}",
        )

    # 解析JSON参数
    try:
        major_list = json.loads(unquote(majorElements))
        minor_list = json.loads(unquote(minorElements))
    except json.JSONDecodeError as e:
        raise HTTPException(status_code=400, detail=f"JSON解析失败: {str(e)}")

    if not isinstance(major_list, list):
        raise HTTPException(status_code=400, detail="majorElements必须是JSON数组")
    if not isinstance(minor_list, list):
        raise HTTPException(status_code=400, detail="minorElements必须是JSON数组")

    try:
        # 获取关系数据
        if option:
            relation_type = RELATION_TYPE_MAP[option]
            relations = get_relations_by_type(relation_type)
        else:
            relations = get_all_relations()

        # 关键变化：根据 type + name + description 解析出 source 的 element_id 集合
        if major_list:
            major_ids = resolve_major_element_ids(major_list)  # Set[str]
        else:
            major_ids = None  # None 表示不启用过滤（保持“传空数组 = 不过滤”的语义）

        # 存储去重后的 target 元素（使用 id 做 key 避免重复）
        target_elements_set: Dict[str, Dict[str, Any]] = {}

        for rel in relations:
            source_id = rel.get("source_element_id")
            target_id = rel.get("target_element_id")

            # 如果 major_ids 不为 None，则仅保留 source 在 major_ids 中的关系
            if major_ids is not None and source_id not in major_ids:
                continue

            target_elem = {
                "type": get_option_from_type(rel.get("target_type")),
                "id": target_id,
                "code": rel.get("target_name"),  # 新增字段，值等于 name
                "name": rel.get("target_name"),
            }
            if target_id:
                target_elements_set[target_id] = target_elem  # 自动去重

        # 转为列表
        result = list(target_elements_set.values())

        logger.info(f"关系生成完成: 共{len(result)}个子元素")
        return result

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"关系生成失败: {e}")
        raise HTTPException(status_code=500, detail=f"关系生成失败: {str(e)}")


# ==================== COBOL转Markdown功能 ====================


def cobol_to_markdown(cobol_content: str) -> str:
    """将COBOL代码原样返回为Markdown内容（不添加额外文本）"""
    return cobol_content


@app.post("/api/cobol/to-markdown")
async def cobol_to_markdown_upload(file: UploadFile = File(...)):
    """
    COBOL转Markdown接口

    上传COBOL文件，返回Markdown格式文档供下载

    - **file**: COBOL文件（.cbl或.cob）

    返回：Markdown文件
    """
    try:
        content = await file.read()
        cobol_text = content.decode("utf-8", errors="ignore")

        markdown_content = cobol_to_markdown(cobol_text)

        filename = file.filename.rsplit(".", 1)[0] if file.filename else "output"

        return Response(
            content=markdown_content,
            media_type="text/markdown",
            headers={"Content-Disposition": f"attachment; filename={filename}.md"},
        )
    except Exception as e:
        logger.error(f"COBOL转Markdown失败: {e}")
        raise HTTPException(status_code=500, detail=f"转换失败: {str(e)}")


# ==================== 辅助API接口 ====================


@app.get("/api/db/elements")
async def get_db_elements(
    element_type: str = Query(
        None, description="元素类型：ENTITY、ATTRIBUTE、PROCESS、TASK、STEP"
    ),
):
    """
    直接查询数据库元素（辅助接口）
    """
    try:
        if element_type:
            elements = get_elements_by_type(element_type)
        else:
            elements = get_all_elements()
        return elements
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/db/relations")
async def get_db_relations(relation_type: str = Query(None, description="关系类型")):
    """
    直接查询数据库关系（辅助接口）
    """
    try:
        if relation_type:
            relations = get_relations_by_type(relation_type)
        else:
            relations = get_all_relations()
        return relations
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/db/stats")
async def get_db_stats():
    """
    获取数据库统计信息（辅助接口）
    """
    try:
        with get_db_connection() as conn:
            cursor = conn.cursor()

            # 统计元素数量
            cursor.execute("""
                           SELECT element_type, COUNT(*) as count
                           FROM bnmp_elements
                           GROUP BY element_type
                           """)
            element_stats = {
                row["element_type"]: row["count"] for row in cursor.fetchall()
            }

            # 统计关系数量
            cursor.execute("""
                           SELECT relation_type, COUNT(*) as count
                           FROM bnmp_element_relations
                           GROUP BY relation_type
                           """)
            relation_stats = {
                row["relation_type"]: row["count"] for row in cursor.fetchall()
            }

            return {
                "database_path": DATABASE_PATH,
                "elements": element_stats,
                "relations": relation_stats,
                "total_elements": sum(element_stats.values()),
                "total_relations": sum(relation_stats.values()),
            }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# ==================== 启动入口 ====================

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="BNMP 2.0 业务模型分析服务")
    parser.add_argument("--host", default="0.0.0.0", help="服务监听地址")
    parser.add_argument("--port", type=int, default=8000, help="服务监听端口")
    parser.add_argument("--db", default="./bnmp_model.db", help="数据库文件路径")
    parser.add_argument("--reload", action="store_true", help="开发模式，自动重载")

    args = parser.parse_args()

    # 设置数据库路径
    DATABASE_PATH = args.db
    os.environ["BNMP_DB_PATH"] = args.db

    print(f"""
 ╔══════════════════════════════════════════════════════════════╗
 ║           BNMP 2.0 业务模型分析服务                          ║
 ╠══════════════════════════════════════════════════════════════╣
 ║  服务地址: http://{args.host}:{args.port}                           ║
 ║  数据库: {args.db:<50} ║
 ╠══════════════════════════════════════════════════════════════╣
 ║  API接口:                                                    ║
 ║  GET  /api/py/extract/keyword  - 元素提取                    ║
 ║  GET  /api/py/extract/keyword2 - 元素提取2                   ║
 ║  POST /api/py/generate/connect - 关系生成                    ║
 ║  POST /api/cobol/to-markdown   - COBOL转Markdown             ║
 ║  GET  /api/db/elements         - 查询元素                    ║
 ║  GET  /api/db/relations        - 查询关系                    ║
 ║  GET  /api/db/stats            - 数据库统计                  ║
 ╚══════════════════════════════════════════════════════════════╝
    """)

    uvicorn.run(app, host=args.host, port=args.port)
