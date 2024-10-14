# ECO3080 Machine Learning for Business
This is the coding session for ECO3080 Machine Learning for Business.

## 2024-2025 Fall
Students in 2024-2025 term 1 are required to learn Python instead of R as an accompanying programming language. Thus, I directly borrow the materials (especially Jupytor notebook files) from ISLP for the tutorial sessions. Students can access ISLP (www.statlearning.com/resources-python) for data, sample codes, and other instructions. The following topics are covered:
* **Introduction to Python**
  * Students should know some basic commands in Python and be familiar with different data types (especially lists, tuples, and strings); they should know how to load a dataframe and manage it as well (e.g., selecting rows and columns, dealing with missing values, ...); they are also supposed to generate proper graphs (or tables) for different types of variables (e.g., quantitative or qualitative).
* **Linear Regression**
  * Students should know how to load data from outside; they need to clearly set up a linear regression model by `pd.DataFrame()` or `MS()` from ISLP, together with `sm.OLS`; they are also supposed to use `.fit()` to  estimate the parameters and use `.get_prediction()` to make predictions based on new observed features. Diagnostic analysis is not required. 
* **Classification**
  * Students should know how to implement `sm.GLM` and `.fit()` to construct a binary logistic regression model and estimate the parameters; they also need to know how to use `LinearDiscriminantAnalysis`, `QuadraticDiscriminantAnalysis`, `GaussianNB` and `KNeighborsClassifier` from sklearn package to build up LDA, QDA, Naive Bayes, and KNN models; they should use these classification models to make predictions on the test set by `.predict()` and assess the test accuracy by using `confusion_table` from ISLP. In addition, students should be able to adjust the threshold values to generate different confusion matrices and hence manully draw the ROC plot. Poisson regression model is not required. 
* **Resampling Methods**
* **Linear Model Selection and Regularization**
* **Moving Beyond Linearity**
* **Tree-based Methods**
* **Deep Learning**
* **Survival Analysis**
* **Unsupervised Learning**

## 2023-2024 Fall
According to the feedback from students in 2022-2023, I rewrite the materials for 2023-2024 Term 1. The contents are simplified and all compiled in only one PDF file which includes 5 chapters:
* Chapter 1: Starting with R
* Chapter 2: Basic Classification Methods
* Chapter 3: Introduction to Model Selection
* Chapter 4: Basic Tree-based Methods
* Chapter 5: Introduction to Deep Learning

*Students are highly encouraged to type the codes by themselves rather than copying mine so that they can quickly get familiar with R. But those who have difficulties in learning R coding can still refer to the old version of my tutorials.* 

## 2022-2023 Fall
Files with name "Tutorial 1"-"Tutorial 8" include R scripts, slides and supplementary data, covering topics:
* Tutorial 1: Basic Functions and Data Management 
* Tutorial 2: Plots, Charts, Graphs and Maps
* Tutorial 3: Linear Regression
* Tutorial 4: Logistic/Probit, LDA/QDA and KNN 
* Tutorial 5: Resampling and Model Selection
* Tutorial 6: Nonlinear Models
* Tutorial 7: Tree-based Methods
* Tutorial 8: Introductory Deep Learning
