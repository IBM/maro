# MARO: ML Automated Remediation Oracle

AutoML debugging and remediation tool.

Installation:
- Requires Python3 and Docker
- Run `pip install -e .`
- Run `docker build -t marosolver .`
- Run `docker run -p 8000:8000 -t marosolver`
- Try out Maro in our example notebook in examples/example.ipynb

Notes:
- Solver requires a modified version of Rosette from https://github.com/Quetzal-RDF/rosette

To cite:

```
@InProceedings{dolby_tsay_hirzel_2022,
  title = "Automatically Debugging {AutoML} Pipelines Using {Maro}: {ML} Automated Remediation Oracle",
  author = "Dolby, Julian and Tsay, Jason and Hirzel, Martin",
  booktitle = "Symposium on Machine Programming (MAPS)",
  year = 2022,
  month = jun,
  pages = "60--69",
  url = "https://dl.acm.org/doi/10.1145/3520312.3534868" }
```

The extended version of our paper is on arXiv: https://arxiv.org/abs/2205.01311
