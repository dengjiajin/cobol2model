# Repository Guidelines

## 快速须知
- 核心脚本：`bnmp_analyzer.py`（COBOL 解析与 BNMP 抽取）、`bnmp_query.py`（查询/校验）、`bnmp_api_server.py`（API 服务，依赖 SQLite `bnmp_model.db`）、`bnmp_frontend.html`（UI）。
- 数据与文档：`bnmp_model.db`、提示/说明 `1.*.txt`、`2.*.txt`、`3.*.txt`、`bnmp_report.md`。
- 环境与配置：可选虚拟环境 `bpmn_env/`；依赖 `req.txt`；`.gitignore`、`.vscode/`。

## 运行与开发
- 安装依赖：`pip install -r req.txt`（建议先激活 `bpmn_env`）。
- 启动 API：`python bnmp_api_server.py --host 127.0.0.1 --port 5666 --db ./bnmp_model.db`。
- 前端预览：本地打开 `bnmp_frontend.html`，将接口地址指向运行中的 API。
- 工具/检查：`python bnmp_query.py --db ./bnmp_model.db --help`；写库前先备份数据库。

## 编码规范
- Python 3，PEP 8，缩进 4 空格；函数/变量 snake_case，常量全大写。
- 文件命名沿用 `bnmp_*`；新模块提供 `if __name__ == "__main__":` 入口示例。
- 依赖集中在 `req.txt`；新增模块写精简 docstring，复杂逻辑前加简要注释。

## 测试
- 当前无自动化测试；新增功能补最小脚本或 `unittest`，命名 `test_<module>.py`。
- API 变更至少手测：启动后 `curl http://127.0.0.1:5666/health`（如有）或核心端点并记录 I/O。
- 涉及数据库写入先复制 `bnmp_model.db` 备份，确认无污染后再提交。

## 提交与 PR
- 提交信息用简洁祈使句（如 `feat: add merge route validator`），按功能拆分。
- PR 说明写明变更摘要、动机/关联 Issue、测试方式与结果；前端改动附截图或录屏。
- 保持依赖和结构兼容；有迁移风险在 PR 中标注步骤。

## 安全与配置
- 不提交含敏感数据的数据库或日志；示例库仅供开发验证。
- 网络/端口用于本地或受控环境；敏感凭据用环境变量，避免硬编码。
