# test-ecr

## Gerando requirements.txt

```bash
pip install pipreqs
python3 -m pipreqs.pipreqs .
```

## Executando

```bash
# Instale uvicorn
sudo apt install python3-uvicorn
uvicorn app.main:app --port 9000
```
