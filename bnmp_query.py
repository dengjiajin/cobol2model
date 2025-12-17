#!/usr/bin/env python3
"""
BNMP 2.0 业务模型查询工具
用于查询和展示业务模型的详细信息
"""

import sqlite3

def query_entities_detail(conn):
    """查询业务实体详情"""
    cursor = conn.cursor()
    
    print("\n" + "="*80)
    print("一、数据模型 - 业务实体详情")
    print("="*80)
    
    cursor.execute("""
        SELECT element_id, element_name, element_name_cn, entity_summary 
        FROM bnmp_elements 
        WHERE element_type = 'ENTITY'
        ORDER BY element_name
    """)
    
    entities = cursor.fetchall()
    
    for entity in entities:
        entity_id, table_name, name_cn, summary = entity
        
        print(f"\n【{table_name}】- {name_cn}")
        print("-"*60)
        print(f"描述: {summary}")
        print("\n字段列表:")
        
        cursor.execute("""
            SELECT field_name, element_name_cn, field_type, field_length, 
                   field_rules, foreign_key_ref
            FROM bnmp_elements
            WHERE element_type = 'ATTRIBUTE' AND parent_entity_id = ?
            ORDER BY element_id
        """, (entity_id,))
        
        print(f"{'序号':<4} {'字段名':<25} {'中文名':<20} {'类型':<12} {'长度':<8} {'规则'}")
        print("-"*120)
        
        for i, attr in enumerate(cursor.fetchall(), 1):
            field_name, name_cn, field_type, length, rules, fk = attr
            fk_info = f" [FK→{fk}]" if fk else ""
            rules_short = (rules[:30] + "...") if len(rules) > 30 else rules
            print(f"{i:<4} {field_name:<25} {name_cn:<20} {field_type:<12} {length:<8} {rules_short}{fk_info}")

def query_processes_detail(conn):
    """查询业务流程详情"""
    cursor = conn.cursor()
    
    print("\n\n" + "="*80)
    print("二、流程模型 - 业务流程详情")
    print("="*80)
    
    cursor.execute("""
        SELECT element_id, element_name, element_name_cn, description, 
               cobol_file_name, program_id
        FROM bnmp_elements 
        WHERE element_type = 'PROCESS'
        ORDER BY cobol_file_name
    """)
    
    processes = cursor.fetchall()
    
    for process in processes:
        process_id, name, name_cn, desc, cobol_file, program_id = process
        
        print(f"\n\n{'='*80}")
        print(f"【流程】{name_cn}")
        print(f"{'='*80}")
        print(f"流程名称: {name}")
        print(f"COBOL文件: {cobol_file}")
        print(f"程序ID: {program_id}")
        print(f"流程描述: {desc}")
        
        # 查询任务
        cursor.execute("""
            SELECT e.element_id, e.element_name, e.element_name_cn, e.task_logic
            FROM bnmp_elements e
            JOIN bnmp_element_relations r ON e.element_id = r.target_element_id
            WHERE r.source_element_id = ? AND r.relation_type = 'PROCESS_TASK'
            ORDER BY r.relation_order
        """, (process_id,))
        
        tasks = cursor.fetchall()
        
        for task_idx, task in enumerate(tasks, 1):
            task_id, task_name, task_name_cn, task_logic = task
            
            print(f"\n  【任务{task_idx}】{task_name_cn}")
            print(f"  {'-'*60}")
            print(f"  任务名称: {task_name}")
            print(f"  业务逻辑: {task_logic}")
            
            # 查询步骤
            cursor.execute("""
                SELECT e.element_id, e.element_name, e.step_logic, 
                       e.data_operation_type, e.data_operation_desc
                FROM bnmp_elements e
                JOIN bnmp_element_relations r ON e.element_id = r.target_element_id
                WHERE r.source_element_id = ? AND r.relation_type = 'TASK_STEP'
                ORDER BY r.relation_order
            """, (task_id,))
            
            steps = cursor.fetchall()
            
            for step_idx, step in enumerate(steps, 1):
                step_id, step_name, step_logic, op_type, op_desc = step
                
                print(f"\n    【步骤{task_idx}.{step_idx}】{step_name}")
                print(f"    逻辑描述: {step_logic}")
                
                if op_type:
                    print(f"    操作类型: {op_type}")
                    print(f"    数据操作: {op_desc}")
                    
                    # 查询关联的实体
                    cursor.execute("""
                        SELECT e.element_name, e.element_name_cn
                        FROM bnmp_elements e
                        JOIN bnmp_element_relations r ON e.element_id = r.target_element_id
                        WHERE r.source_element_id = ? AND r.relation_type = 'STEP_ENTITY'
                    """, (step_id,))
                    
                    entities = cursor.fetchall()
                    if entities:
                        entity_list = ", ".join([f"{e[0]}({e[1]})" for e in entities])
                        print(f"    关联实体: {entity_list}")

def query_relations_summary(conn):
    """查询关系汇总"""
    cursor = conn.cursor()
    
    print("\n\n" + "="*80)
    print("三、元素关系汇总")
    print("="*80)
    
    cursor.execute("""
        SELECT relation_type, COUNT(*) as cnt
        FROM bnmp_element_relations
        GROUP BY relation_type
        ORDER BY cnt DESC
    """)
    
    type_names = {
        "ENTITY_ATTRIBUTE": "实体包含属性",
        "PROCESS_TASK": "流程包含任务",
        "TASK_STEP": "任务包含步骤",
        "TASK_TASK": "任务执行顺序",
        "STEP_STEP": "步骤执行顺序",
        "STEP_ENTITY": "步骤操作实体",
        "STEP_ATTRIBUTE": "步骤操作字段"
    }
    
    print(f"\n{'关系类型':<20} {'中文描述':<20} {'数量'}")
    print("-"*60)
    for row in cursor.fetchall():
        rel_type, cnt = row
        print(f"{rel_type:<20} {type_names.get(rel_type, ''):<20} {cnt}")

def query_data_operation_matrix(conn):
    """查询数据操作矩阵"""
    cursor = conn.cursor()
    
    print("\n\n" + "="*80)
    print("四、数据操作矩阵（步骤-实体操作关系）")
    print("="*80)
    
    cursor.execute("""
        SELECT 
            p.element_name as process_name,
            p.element_name_cn as process_name_cn,
            s.element_name as step_name,
            s.data_operation_type,
            e.element_name as entity_name,
            e.element_name_cn as entity_name_cn
        FROM bnmp_elements s
        JOIN bnmp_element_relations r1 ON s.element_id = r1.source_element_id
        JOIN bnmp_elements e ON r1.target_element_id = e.element_id
        JOIN bnmp_element_relations r2 ON s.element_id = r2.target_element_id
        JOIN bnmp_elements t ON r2.source_element_id = t.element_id
        JOIN bnmp_element_relations r3 ON t.element_id = r3.target_element_id
        JOIN bnmp_elements p ON r3.source_element_id = p.element_id
        WHERE s.element_type = 'STEP' 
        AND r1.relation_type = 'STEP_ENTITY'
        AND r2.relation_type = 'TASK_STEP'
        AND r3.relation_type = 'PROCESS_TASK'
        AND s.data_operation_type IS NOT NULL
        ORDER BY p.element_name, s.data_operation_type, e.element_name
    """)
    
    results = cursor.fetchall()
    
    current_process = None
    print(f"\n{'流程':<25} {'步骤':<25} {'操作':<8} {'实体'}")
    print("-"*100)
    
    for row in results:
        process, process_cn, step, op_type, entity, entity_cn = row
        if current_process != process:
            if current_process:
                print()
            current_process = process
            print(f"\n◆ {process_cn}")
        print(f"  {step:<25} {op_type:<8} {entity}({entity_cn})")

def export_summary_report(conn, output_file):
    """导出汇总报告"""
    cursor = conn.cursor()
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("# BNMP 2.0 业务模型分析报告\n\n")
        f.write("## 概述\n\n")
        f.write("本报告基于COBOL代码分析，按照BNMP 2.0规范提取的业务模型。\n\n")
        
        # 统计信息
        cursor.execute("SELECT element_type, COUNT(*) FROM bnmp_elements GROUP BY element_type")
        f.write("### 元素统计\n\n")
        f.write("| 元素类型 | 数量 |\n")
        f.write("|---------|------|\n")
        type_names = {"ENTITY": "业务实体", "ATTRIBUTE": "实体属性", "PROCESS": "业务流程", "TASK": "流程任务", "STEP": "任务步骤"}
        for row in cursor.fetchall():
            f.write(f"| {type_names.get(row[0], row[0])} | {row[1]} |\n")
        
        # 业务实体
        f.write("\n## 一、数据模型\n\n")
        cursor.execute("SELECT element_name, element_name_cn, entity_summary FROM bnmp_elements WHERE element_type = 'ENTITY'")
        for entity in cursor.fetchall():
            f.write(f"### {entity[0]} ({entity[1]})\n\n")
            f.write(f"{entity[2]}\n\n")
        
        # 业务流程
        f.write("\n## 二、流程模型\n\n")
        cursor.execute("SELECT element_name, element_name_cn, description, cobol_file_name, program_id FROM bnmp_elements WHERE element_type = 'PROCESS'")
        for proc in cursor.fetchall():
            f.write(f"### {proc[1]}\n\n")
            f.write(f"- **流程名称**: {proc[0]}\n")
            f.write(f"- **COBOL文件**: {proc[3]}\n")
            f.write(f"- **程序ID**: {proc[4]}\n")
            f.write(f"- **描述**: {proc[2]}\n\n")
    
    print(f"\n汇总报告已导出到: {output_file}")

def main():
    db_path = "/home/claude/cobol_analysis/bnmp_model.db"
    conn = sqlite3.connect(db_path)
    
    query_entities_detail(conn)
    query_processes_detail(conn)
    query_relations_summary(conn)
    query_data_operation_matrix(conn)
    
    # 导出报告
    report_path = "/home/claude/cobol_analysis/bnmp_report.md"
    export_summary_report(conn, report_path)
    
    conn.close()

if __name__ == "__main__":
    main()
