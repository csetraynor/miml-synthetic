This directory includes R scripts to reproduce the synthetic data experiments and tested imputation methods.
Scripts to generate the synthetic dataset:
01_ampute.R and 01_simulate.R
Scripts to perform the imputation:
02_<imputation>.R
To use MITABNET, add the mice.impute.tabnetRegressor.R in the mice package R folder: https://github.com/amices/mice and run 02_tabnet.R 