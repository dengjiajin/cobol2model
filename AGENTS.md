# Repository Guidelines

## 项目结构与模块组织
- 根目录包含 Python 工具：`bnmp_analyzer.py`（COBOL 解析与 BNMP 模型抽取）、`bnmp_query.py`（查询/校验）、`bnmp_api_server.py`（API 服务，依赖 SQLite `bnmp_model.db`）、`bnmp_frontend.html`（简单 UI）。
- 数据与文档：`bnmp_model.db` 为示例数据库；提示与说明文件 `1.*.txt`、`2.*.txt`、`3.*.txt`、`bnmp_report.md` 提供业务背景。
- 环境与配置：可选虚拟环境目录 `bpmn_env/`；依赖列表 `req.txt`；`.gitignore` 管理忽略项。

## 构建、测试与本地运行
- 安装依赖：`pip install -r req.txt`（建议激活 `bpmn_env` 后执行）。
- 启动接口：`python bnmp_api_server.py --host 127.0.0.1 --port 5666 --db ./bnmp_model.db`（加载示例库，提供 API）。
- 前端预览：本地打开 `bnmp_frontend.html`，将接口地址指向运行中的 API 主机/端口。
- 数据检查：`python bnmp_query.py --db ./bnmp_model.db --help` 查看可用查询入口；操作前可备份数据库防止污染。

## 编码风格与命名约定
- 语言：Python 3，遵循 PEP 8，缩进 4 空格；函数/变量用 snake_case，常量用全大写。
- 文件命名沿用 `bnmp_*` 前缀；新模块提供 `if __name__ == "__main__":` 入口示例，便于脚本直接运行。
- 依赖收敛在 `req.txt`；新增模块添加精简 docstring，复杂逻辑前加简要注释说明意图。

## 测试指引
- 当前无自动化测试；新增功能请补充最小化脚本或 `unittest` 文件，命名为 `test_<module>.py`。
- API 变更至少自测：启动服务后用 `curl http://127.0.0.1:5666/health`（如存在）或对核心端点发请求，记录输入/输出示例。
- 涉及数据库写入时先复制 `bnmp_model.db` 备份，确认无数据污染后再提交。

## 提交与 PR 规范
- 提交信息建议简洁祈使句（如 `feat: add merge route validator`），按功能拆分提交。
- PR 说明包含：变更摘要、动机/关联 Issue、测试方式与结果；前端改动附关键截图或录屏。
- 保持与现有依赖、文件结构兼容；有兼容性风险时在 PR 中标注迁移步骤。

## 安全与配置提示
- 避免提交含敏感客户数据的数据库或日志；示例库仅供开发验证。
- 网络与端口配置在本地或受控环境使用，敏感凭据通过环境变量传递，避免硬编码。
