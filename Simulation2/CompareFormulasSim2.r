rm(list=ls(all=TRUE));
conditions <- expand.grid(  cluster.size=5,
                               d=.3,
                               allocation=c(.5,.6,.7),
                               n=c(300,400,500,600),
                               icc=c(.1,.2),
                               unequalVariances=0 ); # assume equal variances;
conditions$num.clusters <- conditions$n * conditions$allocation /
           conditions$cluster.size;
conditions$num.singletons <- conditions$n * (1-conditions$allocation);

conditions$tau2u <- NA;
conditions$tau2u[which(conditions$unequalVariances==0)] <- conditions$icc[which(conditions$unequalVariances==0)]/
                            (1-conditions$icc[which(conditions$unequalVariances==0)]);
conditions$tau2u[which(conditions$unequalVariances==1)] <- 0.8075*conditions$icc[which(conditions$unequalVariances==1)]/
                            (1-conditions$icc[which(conditions$unequalVariances==1)]);
conditions$sigma2e0 <- NA;
conditions$sigma2e0[which(conditions$unequalVariances==0)] <- .5775;
conditions$sigma2e0[which(conditions$unequalVariances==1)] <- .770;
conditions$sigma2e1 <- NA;
conditions$sigma2e1[which(conditions$unequalVariances==0)] <- .5775;
conditions$sigma2e1[which(conditions$unequalVariances==1)] <- .385;

rho <- conditions$icc;
J0 <- .8*conditions$num.singletons;
J1 <- conditions$num.clusters;
n <- .8*conditions$cluster.size;
rhopp <- .65;

conditions$ancova.var.gamma <- rho/(4*(1-rho)*J1)+((1-rhopp^2)/(4*J1*n))+((1-rhopp^2)/(4*J0));
conditions$ancova.var.gamma.other.way <-  conditions$tau2u/(4*conditions$num.clusters)+
  conditions$sigma2e1/(4*.8*conditions$num.clusters*conditions$cluster.size)+
  conditions$sigma2e0/(4*.8*conditions$num.singletons);
plot(conditions$ancova.var.gamma,conditions$ancova.var.gamma.other.way);abline(a=0,b=1);
conditions$ancova.lambda <-  ((conditions$d/2)^2)/conditions$ancova.var.gamma;
conditions$ancova.predicted <- 1-pf(qf(.95,1,conditions$num.clusters-17),
                                    1,
                                    conditions$num.clusters-17,
                                    ncp=conditions$ancova.lambda);
conditions$threeway.var.gamma <- rho/(4*(1-rho)*J1) + (1-rhopp)/(2*J1*n) + (1-rhopp)/(2*J0);
conditions$threeway.lambda <-  ((conditions$d/2)^2)/conditions$threeway.var.gamma;
conditions$threeway.predicted <- 1-pf(qf(.95,1,conditions$num.clusters-17),
                                    1,
                                    conditions$num.clusters-17,
                                    ncp=conditions$threeway.lambda);



plot(conditions$ancova.predicted,conditions$threeway.predicted);abline(a=0,b=1);
summary(conditions$ancova.predicted-conditions$threeway.predicted);
