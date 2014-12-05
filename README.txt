## ##############################################################
## README, 12.5.2014
## Optimal detection of weak positive dependence between two
## mixture distributions
## Sihai D. Zhao, T. Tony Cai, and Hongzhe Li
## ##############################################################
## ==============================================================
## Description
## ==============================================================
C implementation of methods, along with R wrapper function and example usage.

## ==============================================================
## Contents
## ==============================================================
dd.c
- C code for proposed test statistic
- Linux: compile it using R CMD SHLIB dpm.c
- Windows: see http://mcglinn.web.unc.edu/blog/linking-c-with-r-in-windows/

dd.R
- R wrapper function for dd.c

example.R
- example usage

fxns.R
- other tests for dependency considered in the paper
