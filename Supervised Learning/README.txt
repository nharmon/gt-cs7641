.---------------------------------------------------------------------.
| Nathan A. Harmon         |  Machine Learning (CS 7641), Spring 2015 |
| nharmon@gatech.edu       |  Supervised Learning Assignment          |
`---------------------------------------------------------------------'

=== File Manifest ===

  README.txt                            This file
  diabetic_rehosp/                
   |- diabetic_analysis_dtree.R         Diabetes Decision Tree Program
   |- diabetic_analysis_dtreeboosted.R  Diabetes Boosted Decision tree
   |- diabetic_analysis_knn.R           Diabetes kNN Program
   |- diabetic_analysis_neuralnet.R     Diabetes ANN Program
   |- diabetic_analysis_svm.R           Diabetes SVM Program
   `- diabetic_data.csv                 Diabetes Dataset
  pima-indians
   |- pima-indians-diabetes.data        Pima Indians Dataset
   |- pima_analysis_dtree.R             Pima Indians Decision Tree Program
   |- pima_analysis_dtreeboosted.R      Pima Indians Boosted Decision Tree
   |- pima_analysis_knn.R               Pima Indians kNN Program
   |- pima_analysis_neuralnet.R         Pima Indians ANN Program
   `- pima_analysis_svm.R               Pima Indians SVM Program

=== Instructions  ===

  The following R packages are needed to run the learning programs:
      "C50", "class", "dummies", "gmodels", "kernlab", "neuralnet"

  Each .R file can be run from RStudio or from the command line. If 
  you need to install the previously mentioned packages, it may be 
  easier to load the programs into RStudio as there are commands at 
  the beginning of each program to install the packages it needs. You 
  just need to uncomment them.

  To run from the command line, simply:

      $ Rscript <program>
  
