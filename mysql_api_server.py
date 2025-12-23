from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
import pymysql
import os
from dotenv import load_dotenv

# 加载环境变量
load_dotenv()

# 创建FastAPI应用
app = FastAPI(
    title="MySQL Customer Info API",
    description="用于查询customer_info表的API服务",
    version="1.0.0",
    docs_url="/api/docs",
    redoc_url="/api/redoc"
)

# 配置CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# MySQL数据库配置
DB_CONFIG = {
    "host": os.getenv("MYSQL_HOST", "8.129.10.47"),
    "port": int(os.getenv("MYSQL_PORT", "3306")),
    "user": os.getenv("MYSQL_USER", "coreapp"),
    "password": os.getenv("MYSQL_PASSWORD", "Gientech@123"),
    "database": os.getenv("MYSQL_DATABASE", "mcore_mp_cust_mgmt_db"),
    "charset": "utf8mb4"
}

# 获取数据库连接
def get_db_connection():
    """
    获取MySQL数据库连接
    
    返回：
        pymysql.Connection: 数据库连接对象
    
    异常：
        pymysql.MySQLError: 数据库连接错误
    """
    try:
        return pymysql.connect(**DB_CONFIG)
    except pymysql.MySQLError as e:
        raise Exception(f"数据库连接失败: {str(e)}")

# 根据客户编号查询客户信息
def get_customer_by_cust_no(cust_no):
    """
    根据客户编号查询客户信息
    
    参数：
        cust_no (str): 客户编号，对应数据库表中的CUST_NO_OR_ACCT_NO字段
    
    返回：
        dict: 客户信息字典，如果未找到返回None
    
    异常：
        pymysql.MySQLError: 数据库查询错误
    """
    connection = None
    cursor = None
    try:
        connection = get_db_connection()
        cursor = connection.cursor(pymysql.cursors.DictCursor)
        
        # 查询语句
        sql = "SELECT * FROM customer_info WHERE CUST_NO_OR_ACCT_NO = %s"
        cursor.execute(sql, (cust_no,))
        
        # 获取结果
        result = cursor.fetchone()
        return result
    finally:
        # 关闭游标和连接
        if cursor:
            cursor.close()
        if connection:
            connection.close()

# API端点：根据客户编号查询客户信息
@app.get("/api/customer/by-cust-no")
async def get_customer(
    custBi: str = Query(..., description="客户编号（对应表字段CUST_NO_OR_ACCT_NO）")
):
    """
    根据客户编号查询客户信息
    
    - **custBi**: 客户编号，对应数据库表中的CUST_NO_OR_ACCT_NO字段
    
    返回：客户信息JSON对象
    """
    try:
        customer = get_customer_by_cust_no(custBi)
        if not customer:
            raise HTTPException(status_code=404, detail=f"未找到客户信息: custBi={custBi}")
        return customer
    except pymysql.MySQLError as e:
        raise HTTPException(status_code=500, detail=f"数据库查询错误: {str(e)}")

# 主函数
if __name__ == "__main__":
    import uvicorn
    import argparse
    
    parser = argparse.ArgumentParser(description="MySQL Customer Info API 服务")
    parser.add_argument("--host", default="0.0.0.0", help="服务监听地址")
    parser.add_argument("--port", type=int, default=8000, help="服务监听端口")
    args = parser.parse_args()
    
    uvicorn.run(app, host=args.host, port=args.port)