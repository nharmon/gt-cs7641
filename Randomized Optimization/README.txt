.---------------------------------------------------------------------.
| Nathan A. Harmon         |  Machine Learning (CS 7641), Spring 2015 |
| nharmon@gatech.edu       |  Randomized Optimization Assignment      |
`---------------------------------------------------------------------'

=== File Manifest ===

  README.txt                            This file
  pima-indians
   |- pima-indians-diabetes.data        Pima Indians Dataset
   |- pima_analysis_ga.R                Genetic Algoritm Neural Net
   |- pima_analysis_rhc.R               Rand Hill Climb Neural Net
   `- pima_analysis_sa.R                Sim Annealing Neural Net
  problems
   |- four_peaks.R                      Four Peaks Problem
   |- four_peaks_mimic.py               Four Peaks via MIMIC
   |- knapsack.R                        Knapsack Problem
   |- knapsack_mimic.R                  Knapsack via MIMIC
   |- mimic.py                          mimicry Python Library
   |- traveling_salesman.R              Traveling Salesman Problem
   `- traveling_salesman_mimic.py       Traveling Salesman via MIMIC

=== Instructions  ===

  The following R packages are needed to run the learning programs:
      "GA", "GenSA", "gmodels", "neuralnet"

  Each .R file can be run from RStudio or from the command line. If 
  you need to install the previously mentioned packages, it may be 
  easier to load the programs into RStudio as there are commands at 
  the beginning of each program to install the packages it needs. You 
  just need to uncomment them.

  To run from the command line, simply:

      $ Rscript <program>
  
  Python scripts require the mimicry python library available from:
   https://pypi.python.org/pypi/mimicry

  Scripts can be run from the command line:

      $ python <script name>
