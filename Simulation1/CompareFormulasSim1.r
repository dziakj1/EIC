rm(list=ls(all=TRUE));
scenarios <- expand.grid(      cluster.size=c(5,10),
                                n=c(300,400,500,600),
                               d=c(.2,.3,.5),
                              icc=c(.1,.2) ); 
J <- scenarios$n/scenarios$cluster.size;     # number of clusters;
n <- scenarios$cluster.size * .8;   # 20 percent missingness;
rho <- scenarios$icc;
ancova.var.gamma <- (1-(.65^2))/(J*n)+rho/(1-rho)/J;
ancova.lambda <-  ((scenarios$d/2)^2)/ancova.var.gamma;
ancova.predicted <- 1-pf(qf(.95,1,J-16),
                                     1,
                                     J-16,
                                     ncp=ancova.lambda);
threelevel.var.gamma <- rho/((1-rho)*J) + 2*(1-.65)/(J*n);
threelevel.lambda <-  ((scenarios$d/2)^2)/threelevel.var.gamma;
threelevel.predicted <- 1-pf(qf(.95,1,J-16),
                         1,
                         J-16,
                         ncp=threelevel.lambda);
plot(ancova.predicted,threelevel.predicted);abline(a=0,b=1);
print(summary(ancova.predicted-threelevel.predicted));
print(c(scenarios[which.max(ancova.predicted-threelevel.predicted),],
        ancova.predicted[which.max(ancova.predicted-threelevel.predicted)],
        threelevel.predicted[which.max(ancova.predicted-threelevel.predicted)]));



