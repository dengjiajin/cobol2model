

cd /my/cobol2model/

/usr/local/bin/python3.11 -m venv bnmp_env

source  /my/cobol2model/bnmp_env/bin/activat

pip install -r req.txt

ps aux | grep bnmp_api_server.py

nohup python bnmp_api_server.py --host 0.0.0.0 --port 5666 --db ./bnmp_model.db > ./bnmp.log 2>&1


# 创建虚拟环境 （Windows）
python -m venv bnmp_env

# 激活（Windows）
bnmp-env\Scripts\activate

pip install req.txt

