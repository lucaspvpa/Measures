library(princurve)
library(KernSmooth)
library(stfit)
# CovrGC
# Computing Generalized Covariance and Correlation along a Principal Curve
# CovGC CorGC
# using the "principal.curve" function (Hastie-Stuetzle Principal Curve)
# from library "princurve"
# or using the "pcop" function (Principal Curve of Oriented Points),
# depending on the argument "method".
#
# Arguments:
#         x: a matrix of n points in dimension 2
#    method: "HS" for using Hastie-Stuetzle principal curve, 
#            "PCOP" for using PCOP
#  h.method: method to choose the smoothing parameter in the internal local regression calls
# plot.true: if TRUE, the function produce a plot
#       ...: Additional parameters passed to principal curve functions
# 
# Value: A list with elements
#      PC: Pricipal curve fitted to points in x
#   CovGC: Generalized Covariance along a Principal Curve
#   CorGC: Generalized Correlation along a Principal Curve
#  LCovGC: Local Generalized Covariance along a Principal Curve
#  LCorGC: Local Generalized Correlation along a Principal Curve
#   
# Require: princurve, KernSmooth, functions "pcop", "localpolreg_no_plot"
Covr <- function(x, method="HS", h.method="dpill", plot.true=FALSE, ...){
  
  require(princurve)
  require(KernSmooth)
  
  # Computing the Principal Curve
  PC <- switch(method,
               hs=, HS = principal_curve(x,...),
               pcop=, PCOP = pcop(x,plot.true=plot.true,...)$pcop.f2
  )
  # Calculating plug-in bandwidths   
  ##   h1 <- dpill(PC$lambda,PC$s[,1]) # dpill does not work for x and y
  ##   h2 <- dpill(PC$lambda,PC$s[,2]) # arrays equally ordered
  
  
  # Estimating derivatives of the PC
  #   "locpoly(...)" does not admit a xgrid array of points
  #   where evaluating the regression function.
  #   I use my "locpolreg(...)" function instead.
  h1 <- diff(range(PC$lambda))/10
  h2 <- 2*max(diff(sort(PC$lambda)))
  hlambda <- max(h1,h2) 
  lpr1 <- locpolreg_no_plot(PC$lambda,PC$s[,1],p=2,r=1,h=hlambda)
  lpr2 <- locpolreg_no_plot(PC$lambda,PC$s[,2],p=2,r=1,h=hlambda)
  # (Note: lpr*%x and lpr*$y are sorted according to lpr*$x)
  
  
  # Computing angles alpha(s)
  alpha <- atan2(lpr1$mxgr, lpr2$mxgr)
  aux <- sort(PC$ord, ind=T)
  # recovering original ordering
  #points(PC$lambda, lpr2$mxgr[aux$ix],col=2)
  
  alpha <- alpha[aux$ix]
  
  # Estimating conditional orthogonal variance.
  #  e2 <- apply((x - PC$s)^2,1,sum)
  nn <- dim(x)[1]
  e2 <- array(apply((x - PC$s)^2,1,sum),dim=c(nn,1))[,1]
  if (h.method != "dpill"){
    he2_1 <- diff(range(PC$lambda))/5
  }else{
    he2 <- dpill(PC$lambda, e2,trim=.01)
    cte.h <- 2.214 # h1 and h2 are computed for Gaussian kernel, and
    # locpolreg uses Epanechnikov kernel
    he2_1 <- he2*cte.h
    # prevention against NaN produced by dpill
    if (is.na(he2_1)) he2_1 <- diff(range(PC$lambda))/5
  }
  he2_2 <- 2*max(diff(sort(PC$lambda)))
  he2 <- max(he2_1,he2_2) 
  CondOrthVar <- locpolreg_no_plot(PC$lambda, e2,h=he2)$mxgr
  CondOrthVar2 <- CondOrthVar[aux$ix]
  
  # recovering original ordering and preventing for negative estimations
  CondOrthVar <- pmax(CondOrthVar[aux$ix],0) 
  
  # Var. over the PC
  VS <- var(PC$lambda)
  
  # Computing local varinaces, covariance and correlation
  LV1 <- VS * cos(alpha)^2 + CondOrthVar*sin(alpha)^2
  LV2 <- VS * sin(alpha)^2 + CondOrthVar*cos(alpha)^2
  
  LCovGC <- (VS - CondOrthVar)*sin(alpha)*cos(alpha)
  LCorGC <- LCovGC/sqrt(LV1*LV2)
  
  # Computing Generalized Covariance and Correlation along GC
  CovGC <- sqrt(mean(LCovGC^2))
  CorGC <- sqrt(mean(LCorGC^2))
  
  # Value:
  return(list(PC=PC,CovGC=CovGC,CorGC=CorGC,LCovGC=LCovGC,LCorGC=LCorGC))
}

# locpolreg_no_plot.R Non-parametric estimation for the r-th derivative
#             of the regression function m(x) by local polinomial fitting.
#
# Input: 
#      x,y  Pairs of observations (n,1)
#      h    Smoothing parameter
#      p    degree for the polinomial locally fitted (default=1)
#      r    derivative of m(x) to be estimated (default=0)
#      xg   points where the r-th derivative of m(x) is estimated (default=x)
#
# Output:
#      mxgr Estimation of the r-th derivative of m(x) at xg
#
# Uso:
#      result <- locpolreg_no_plot(x,y,h,p,r,xg)
# 'result' is a list with two elements: 
#      'mxgr' the required estimation
#      'S'    the smoothing matriz giving this estimation
# 
locpolreg_no_plot <- function(x,y,h=(max(x)-min(x))/5,p=1,r=0,xg=x,...){
  if (sum(diff(xg)<0)>0) xg <- sort(xg)
  
  n <- length(x);
  m <- length(xg);
  mxgr <- seq(1,m)*0;
  S <- matrix(0,nrow=m,ncol=n)
  
  factr <- max(1,prod(1:r))
  
  for (i in seq(1,m)){
    Ih <- abs(x-xg[i])<h;
    n <- sum(Ih);     
    xh <- x[Ih]-xg[i];
    Dp <- matrix(1,nrow=n,ncol=p+1);
    if (p>0){for (j in 1:p) Dp[,j+1] <- xh^j}
    Wx <- epan(xh/h)/h;
    Wm <- Wx %*% matrix(1, 1, p+1)
    Dpp <- Wm*Dp;
    Si <- solve(t(Dp)%*%Dpp)%*%t(Dpp);
    beta <- Si%*%y[Ih];
    mxgr[i] <- factr*beta[r+1];
    S[i,Ih] <- Si[r+1,]
  }
  return(list(mxgr=mxgr,S=S))
}

# pcop.R
# Compute the Principal Curve of Oriented Points (Delicado 2001, Delicado & Huerta 2003)
# for a given p-dimensional data matrix x of lenght n
#
# Arguments:
#         x: a matrix of n points in dimension p
#       C_H: The smoothing parameter h is C_H times the value given by the normal reference rule.
#            Default value: 0.75. Constraints: 0.5 <= C_H <=  1.5 
#       C_D: The distance between two consecutive principal oriented points in a PCOP is about C_D times 
#            the value of the smoothing parameter h.
#            Default value: 0.3. Constraints: 0.25 <= C_D <= .5
# plot.true: if TRUE, the function produce a plot
#       ...: Additional parameters passed to function "get_lam"
#
# Require: princurve, application "PCOP.exe"
pcop <- function(x,Ch=1.5,Cd=.3,plot.true=FALSE,...){
  x <- round(x,8)
  write.table(x,file="proyec.dat",col.names=FALSE,row.names=FALSE,append=FALSE)
  #   system("del setup.dat")
  
  C_H <- paste("C_H ",Ch,sep="")
  C_D <- paste("C_D ",Cd,sep="")
  cat(file="setup.dat", "datafile proyec.dat", "outfile proyec.alpha", C_H, C_D, sep="\n", append=FALSE)
  
  system("PCOP.exe", invisible = TRUE)
  
  #   read.table("proyec.dat")
  #   system("rm proyec.dat")
  
  proyec <- read.table("proyec.alpha")
  pr.curve <- proyec[,-1]
  
  # Format 1 for the output principal curve.
  # ----------------------------------------
  # The structure of the data.frame "proyec" is as follows: 
  # There is a line for each identified principal oriented point. 
  # The columns are:
  # 1. Value of the parameter t such the the principal oriented point is PCOP(t).
  # 2. Density estimation for the random variable induced over the PCOP at t.
  # 3. Span: proportion of original data involved in the determination of the principal oriented point.
  # 4. Variance over the hyperplane orthogonal to the PCOP at the principal oriented point.
  # 5:(5+p-1). The p coordinates of the principal oriented point.
  # (5+p):(5+2p-1). The p coordinates of the principal direction for the principal oriented point.
  
  
  p <- dim(x)[2]
  
  names(pr.curve) <- c("param", "dens", "span", "orth.var", 
                       paste("pop", 1:p, sep = ""), 
                       paste("pr.dir", 1:p, sep = ""))
  # pr.curve: principal curve of oriented points 
  #           with original format (format 1)
  
  # pc: principal curve of oriented points 
  #     with format as in HS's "princurv" library (format 2)
  s<-as.matrix(pr.curve[,5:(5+p-1)])
  pc <- project_to_curve(x,s,...)
  
  if (plot.true) {
    plot(x[, 1:2], xlim = adjust.range(x[, 1], 1.4), ylim = adjust.range(x[, 
                                                                           2], 1.4))
    lines(pc$s[pc$ord, 1:2])
  }
  
  
  return(list(pcop.f1=pr.curve,pcop.f2=pc))
}
