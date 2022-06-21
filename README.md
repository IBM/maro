# MARO: ML Automated Remediation Oracle
AutoML debugging and remediation tool

We presented this work at MAPS 2022 in the paper entitled "Automatically Debugging AutoML Pipelines using Maro: ML Automated Remediation Oracle." Julian Dolby, Jason Tsay, and Martin Hirzel. 
The extended version of our paper is on arXiv: https://arxiv.org/abs/2205.01311

Installation:
- Requires Python3 and Docker
- Run `pip install -e .`
- Run `docker build -t marosolver .`
- Run `docker run -p 8000:8000 -t marosolver`
- Try out Maro in our example notebook in examples/example.ipynb

Notes:
- Solver requires a modified version of Rosette from https://github.com/Quetzal-RDF/rosette
