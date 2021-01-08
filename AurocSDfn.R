################################
# Function to calculate 
# AUROC confidence intervals
# 17 June 2020
# Mayhew
################################

#method comes from: https://cs.nyu.edu/~mohri/pub/area.pdf
#m is # positive examples
#n is # negative examples
#k is # incorrect predictions (k/N = error rate)


#function breaks is m + n > ~45
#need to fix combinations parameters... something with options(expressions=1e5) 

#preliminary functions calculates Z parameter
calcZ <- function(m,n,k,i){
  Znum <- 0
  Zdenom <- 0
  #if(k-i > 0){
    if(k-i > -1){
    
    for(x in 0:(k-i)){
      Z <- factorial(m+n+1-i)/(factorial(x)*(factorial(m+n+1-i-x)))
      Znum <- Znum+Z
    }
    for(x in 0:k){
      Z2 <- factorial(m+n+1)/(factorial(x)*(factorial(m+n+1-x)))
      Zdenom <- Zdenom + Z2
    }
    return(Znum/Zdenom)
    
  } else {
   
    return(0)
  
  }
  
}


#main function estimates standard deviation
AUROC.CI <- function(m, n, k){

  Z1 <- calcZ(m,n,k,1)
  Z2 <- calcZ(m,n,k,2)
  Z3 <- calcZ(m,n,k,3)
  Z4 <- calcZ(m,n,k,4)
  
  Tee <- 3*((m-n)^2 + m + n) + 2
    
  Q0 <- (m + n + 1)*(Tee*k^2) + ((-3*n^2 + 3*m*n + 3*m + 1)*Tee - 12*(3*m*n + m + n) - 8)*k + (-3*m^2 + 7*m + 10*n + 3*n*m + 10)*Tee - 4*(3*m*n + m + n + 1) 
    
  Q1 <- Tee*k^3 + 3*(m - 1)*Tee*k^2 + ((-3*n^2 + 3*m*n - 3*m + 8)*Tee - 6*(6*m*n + m + n))*k + (-3*m^2 + 7*(m + n) + 3*m*n)*Tee - 2*(6*m*n + m + n)
    
  sigma2 <- ((m+n+1)*(m+n)*(m+n-1)*Tee*((m+n-2)*Z4 - (2*m - n + 3*k - 10)*Z3))/(72*(m^2)*(n^2)) +
            ((m+n+1)*(m+n)*Tee*(m^2 - n*m + 3*k*m - 5*m + 2*k^2 - n*k + 12 - 9*k)*Z2)/(48*(m^2)*(n^2)) -
            ( ((m+n+1)^2) * ((m-n)^4) * (Z1^2) )/(16*(m^2)*(n^2)) -
            ((m+n+1)*Q1*Z1)/(72*(m^2)*(n^2)) +
            (k*Q0)/(144*(m^2)*(n^2))
            
  return(sqrt(sigma2))  
}


