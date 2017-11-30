/*
####################################################
## Stat 202A - Homework 2
## Author: Aashna AGARWAL
## Date : Nov 14
## Description: This script implements linear regression 
## using the sweep operator
####################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
*/ 


# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;



/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Problem 1: Sweep operator 
   ~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
mat mySweepC(const mat A, int m){
  
  /*
  Perform a SWEEP operation on A with the pivot element A[m,m].
  
  A: a square matrix (mat).
  m: the pivot element is A[m, m]. 
  Returns a swept matrix B (which is m by m).
  
  Note the "const" in front of mat A; this is so you
  don't accidentally change A inside your code.
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  */
  
  mat B = A;
  int n = B.n_rows;
  
  for(int k = 0; k < m; k++){
    for(int i = 0; i < n; i++){
      for(int j = 0; j < n; j++){
        if((i != k) & (j != k))
          B(i, j) = B(i, j) - B(i, k) * B(k, j) / B(k, k);
      }
    }
    
    for(int i = 0; i < n; i++){
      if(i != k)
        B(i, k) = B(i, k) / B(k, k);
    }
    
    for(int j = 0; j < n; j++){
      if(j != k)
        B(k, j) = B(k, j) / B(k, k);
    }
    
    B(k, k) = - 1 / B(k, k);
    
  }
  
  // Return swept matrix B
  return(B);
    
}

