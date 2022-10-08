set.seed(NULL)
n = 500
a1 = rnorm(n)
a2 = a1 + 2* runif(n)
b1 = rnorm(n)
b2 = 0.5 + b1 - 2*runif(n)
XXX = rbind(matrix(cbind(a1,a2),,2),matrix(cbind(b1,b2),,2))
YYY <- matrix(c(rep(1,n),rep(-1,n)))
plot(XXX,col=ifelse(YYY>0,4,2),pch=".",cex=7,xlab = "x1",ylab = "x2")



svm_gradient_descent <- function (x,y,C,S = 10,t = 1000,bias = TRUE) {
  #B <- coeffiient matrix
  #t <- number of interations
  #nn <- learning rate
  #H <- vector of h hinge loss value for each iteration
  #G <- gradient
  #b <- whether to include bias term (also called constant)
  #V <- vector of saved values y or 0 for max(0,1-Yi(BXi)
  #S <- Sample size for SGD
  #XS, YS <- Sample of XS,YS of size S
  
  
  
  
  
  
  
  
  #Adds a row of 1's if constant is true
  if (bias == TRUE) {
    Dummy1 <- matrix(1,nrow(x),1)
    x <- cbind(Dummy1,x)
  }
  
  #Changes TRUE / FALSE to -1 and 1 and turns y into a data type matrix
  Y <- data.matrix(ifelse(y == TRUE,1,-1))
  
  #Makes sure x is of data type matrix
  X <- data.matrix(x)
  
  #Initializes coefficient & gradient matrices
  B <- matrix(0,ncol(x),1)
  G <- matrix(0,ncol(x),1)
  
  
  for (i in 1:t) {
    
    
    
    #Sets previous gradient to be current gradient
    Gp <- G
    
    #Sampling without replacement
    Sample <- sample(nrow(X),S)
    XS <- as.matrix(X[Sample,])
    YS <- as.matrix(Y[Sample,])
    V <- matrix(0,nrow(YS),1)
    
    #Part(1)
    #max(0,1-yi(xiB)) part of the algorithm, saving output in V
    for (n in c(1:nrow(XS))) {
      #Main part of the max(0,1-yi(Bxi) equation
      V[n] <- ifelse((1 - (XS[n,] %*% B)*YS[n])>0,YS[n],0)
    }
    
    #Part(2)
    #updating the gradient using V,B and C
    for (j in c(1:ncol(XS))) {
      G[j] <- B[j]- (C*((t(V)%*%XS[,j])/nrow(XS)))
    }
    
    #Part(3)
    #Barzilai-Borwein Step Size for i> iterations (fixed for i==1)
    if (i== 1) {
      nn = 1/1000
    } else {
      numerator = abs(t(B-Bp)%*%(G-Gp))
      denominator = (t(G-Gp)%*%(G-Gp))
      nn = as.vector(numerator/denominator)
    }
    
    #Part(4)
    #Checks whether the value of B is NaN and prevents further iterations if TRUE
    if (is.na(nn)) {
      warning(c("Terminated after ",i," iterations"))
      predicted <- ifelse(X%*%Bp<0,-1,1)
      error_rate <- sum(ifelse(predicted ==Y,0,1))/nrow(Y)
      M <- 1/(sqrt(t(Bp)%*%Bp))
      
      svm_output <- list(coef = Bp, error_rate = error_rate,margin = M)
      class(svm_output) <- "svm_sgd_bb"
      return(svm_output)
    }
    
    #Sets previous coefficients to the current coefficients, before updating
    Bp <- B
    
    #Takes the step in the direction of minimum
    B <- B-nn*G
    
  }


  predicted <- ifelse(X%*%B<0,-1,1)
  error_rate <- sum(ifelse(predicted ==Y,0,1))/nrow(Y)
  M <- 1/(sqrt(t(B)%*%B))
  
  svm_output <- list(coef = B, error_rate = error_rate,margin = M)
  class(svm_output) <- "svm_sgd_bb"
  return(svm_output)
}



w_answer <- svm_gradient_descent(XXX,YYY,C=100,t=2000,S=300)
w_answer


plot(XXX,col=ifelse(YYY>0,4,2),pch=".",cex=7,xlab = "x1",ylab = "x2")
abline(-w_answer$coef[1]/w_answer$coef[3],-w_answer$coef[2]/w_answer$coef[3])
abline((w_answer$margin-w_answer$coef[1])/w_answer$coef[3],-w_answer$coef[2]/w_answer$coef[3],lty=2)
abline(-w_answer$margin-(w_answer$coef[1]/w_answer$coef[3]),-w_answer$coef[2]/w_answer$coef[3],lty=2)






