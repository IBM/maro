# MARO: ML Automated Remediation Oracle
AutoML debugging and remediation tool

Installation:
- Requires Python3 and Docker
- Run `pip install -e .`
- Run `docker build -t marosolver .`
- Run `docker run -p 8000:8000 -t marosolver`
- Try out Maro in our example notebook in examples/example.ipynb

Notes:
- Solver requires a modified version of Rosette from https://github.com/Quetzal-RDF/rosette
