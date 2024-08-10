## Evaluating the feasibility of automating dataset retrieval for biodiversity monitoring

### Alexandre Fuster-Calvo\*, Sarah Valentin , William C Tamayo, Dominique Gravel

\*Contact author: alexfuster7@gmail.com

ABSTRACT

_Aim_

Effective management strategies for conserving biodiversity and mitigating the impacts of Global Change rely on access to comprehensive and up-to-date biodiversity data. However, manual search, retrieval, evaluation, and integration of this information into databases presents a significant challenge to keep pace with the rapid influx of large amounts of data, hindering its utility in contemporary decision-making processes. The automation of these tasks through advanced algorithms holds immense potential to revolutionize biodiversity monitoring.

_Innovation_

In this study, we investigate the potential for automating the retrieval and evaluation of biodiversity data from Dryad and Zenodo repositories. We employ automated algorithms to identify potentially relevant datasets and perform a manual assessment to gauge the feasibility of automatically ranking their relevance. We have designed an evaluation system based on various criteria. Additionally, we compare our results with those obtained from a scientific literature source, using data from Semantic Scholar for reference. Our evaluation centers on the database utilized by a national biodiversity monitoring system in Quebec, Canada.

_Main clonclusions_

The algorithms retrieved 91 (57%) relevant datasets for our database, showing the value of automated dataset search in repositories. Additionally, we find that scientific publication sources offer broader temporal coverage and can serve as conduits guiding researchers toward other valuable data sources. However, our manual evaluation highlights a significant challenge to distinguish datasets by their relevance—scarcity and non-uniform distribution of metadata, especially pertaining to spatial and temporal extents. We present an evaluative framework based on predefined criteria that can be adopted by automated algorithms for streamlined prioritization, and we make our manually evaluated data publicly available, serving as a benchmark for improving classification techniques. Finally, our study advocates for the implementation of metadata standards tailored for automated retrieval systems by repositories and sources of scientific literature. This, coupled with the rapid evolution of classification algorithms, holds transformative potential to advance in biodiversity monitoring and decisively steering the course of well-informed decision-making processes.


FOLDERS:

- **data** contains the raw data used for the analyses
- **figures** figures of the manuscript produced in the code 
- **scripts** R script to run analyses
- **scripts_retrieval** Python scripts to run dataset retrieval

_Run the code_

- install python
- clone the repository by:
  Via ssh: `git clone git@github.com:Alex-Fuster/automated_datset_retrieval.git`
  Via https: `git clone https://github.com/Alex-Fuster/automated_datset_retrieval.git`

- (optional) create a virtual environment in case you do not want to keep the libraries in your global library storage. `python -m venv /path/to/new/virtual/environment`.[find doc here](https://docs.python.org/3/library/venv.html)

- run the following line to install all necessary libraries: `pip install -r requirements.txt`

- start retrieval case zenodo: `python scripts_retrieval/zenodo.py`

- start retrieval case semantic_scolar: `python scripts_retrieval/semantic_scolar.py`
