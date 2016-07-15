rm(list=ls(all=TRUE)); 
path <- "C:\\Documents and Settings\\jjd264\\My Documents\\Paper-ShaniDziakCollins\\ShaniSim1\\";
all.scenarios <- expand.grid( fractional=c(0,1),
                               d=c(0,.2,.3,.5),
                               n=c(300,400,500,600),
                               cluster.size=c(5,10),
                               icc=c(.1,.2),
                               nsims=5000);
all.scenarios$Condition <- 1:nrow(all.scenarios);
the.data <- read.table(paste(path,"allanswers.csv",sep=""),
                       header=TRUE,
                       sep=",",
                       na.strings=".",
                       fill=TRUE);
the.cps <- read.table(paste(path,"allanswers.csv",sep=""),
                       header=TRUE,
                       sep=",",
                       na.strings=".",
                       fill=TRUE);
################################################################
# EXTRANEOUS EFFECTS
power.data <- the.data[which(!is.na(the.data$rep)&
                                (length(the.data$Effect)>1)&
                                (the.data$X_STAT_=="MEAN")),];
print(summary(lapply(split(power.data,power.data$Effect),summary)));
print(summary(split(power.data,power.data$Effect)$x3-split(power.data,power.data$Effect)$x1));
################################################################
# MAIN EFFECT VS INTERACTION
power.x1.data <- the.data[which(!is.na(the.data$rep)&
                                (the.data$Effect=="x1")&
                                (the.data$X_STAT_=="MEAN")),];
power.x1.data <- merge(power.x1.data,all.scenarios,"Condition");
power.x13.data <- the.data[which(!is.na(the.data$rep)&
                                (the.data$Effect=="x1*x3")&
                                (the.data$X_STAT_=="MEAN")),];
power.x13.data <- merge(power.x13.data,all.scenarios,"Condition");
print(summary(power.x13.data$power[which(power.x13.data$d==0)]-
      power.x1.data$power[which(power.x1.data$d==0)]));
print(summary(power.x13.data$power[which(power.x13.data$d>0)]-
      power.x1.data$power[which(power.x1.data$d>0)]));
###################################################################
# TYPE ONE ERROR
power.x1.data$num.clusters <-  power.x1.data$n/power.x1.data$cluster.size;
type1error.x1.data <- power.x1.data[which(power.x1.data$d==0),];
print(summary(type1error.x1.data));
par(mfrow=c(2,1));
hist(type1error.x1.data$power);
plot(type1error.x1.data$num.clusters,type1error.x1.data$power);
###################################################################
# POWER FORMULA
true.power.x1.data <- data.frame(power.x1.data[which(power.x1.data$d>0),]);
true.power.x1.data$adj.num.clusters <- true.power.x1.data$num.clusters;
#true.power.x1.data$var.gamma <- true.power.x1.data$icc/((1-true.power.x1.data$icc)*true.power.x1.data$adj.num.clusters)+
#                                (1-.65^2)/(.8*true.power.x1.data$adj.num.clusters*true.power.x1.data$cluster.size);
tauSqdU <- true.power.x1.data$icc/(1-true.power.x1.data$icc);
J <- true.power.x1.data$num.clusters;
n <- true.power.x1.data$cluster.size;
true.power.x1.data$var.gamma <- (1-(.65^2))/(J*.8*n)+tauSqdU/J;
true.power.x1.data$lambda <-  ((true.power.x1.data$d/2)^2)/true.power.x1.data$var.gamma;
true.power.x1.data$predicted <- 1-pf(qf(.95,1,true.power.x1.data$num.clusters-16),
                                     1,
                                     true.power.x1.data$num.clusters-16,
                                     ncp=true.power.x1.data$lambda);
print(summary(true.power.x1.data$power[which(true.power.x1.data$fractional==0)]-
              true.power.x1.data$predicted[which(true.power.x1.data$fractional==0)]));
hist(true.power.x1.data$power[which(true.power.x1.data$fractional==0)]-
              true.power.x1.data$predicted[which(true.power.x1.data$fractional==0)]);
print(summary(true.power.x1.data$power[which(true.power.x1.data$fractional==1)]-
              true.power.x1.data$predicted[which(true.power.x1.data$fractional==1)]));
par(mfrow=c(2,1));
plot(true.power.x1.data$predicted,true.power.x1.data$power);
abline(a=0,b=1);
plot(true.power.x1.data$num.clusters,
     true.power.x1.data$power-true.power.x1.data$predicted);
abline(a=0,b=1);
print(round(sort(true.power.x1.data$power-true.power.x1.data$predicted),4));
print(order(true.power.x1.data$power-true.power.x1.data$predicted));
print(head(true.power.x1.data[order(true.power.x1.data$power-true.power.x1.data$predicted),]));
###################################################################
# COMPLETE VERSUS FRACTIONAL
complete <- type1error.x1.data[which(type1error.x1.data$fractional==0 &
                                       type1error.x1.data$num.clusters>30),];
complete <- complete[order(complete$d,complete$n,complete$cluster.size,complete$icc),];
fractional <- type1error.x1.data[which(type1error.x1.data$fractional==1 &
                                       type1error.x1.data$num.clusters>30),];
fractional <- fractional[order(fractional$d,fractional$n,fractional$cluster.size,fractional$icc),];
summary(complete$power-fractional$power);
plot(fractional$power,complete$power);
abline(a=0,b=1);
complete <- true.power.x1.data[which(true.power.x1.data$fractional==0 &
                                       true.power.x1.data$num.clusters>30),];
complete <- complete[order(complete$d,complete$n,complete$cluster.size,complete$icc),];
fractional <- true.power.x1.data[which(true.power.x1.data$fractional==1 &
                                       true.power.x1.data$num.clusters>30),];
fractional <- fractional[order(fractional$d,fractional$n,fractional$cluster.size,fractional$icc),];
summary(complete$power-fractional$power);
plot(fractional$power,complete$power);
abline(a=0,b=1);
#################################################
print(true.power.x1.data[order(true.power.x1.data$fractional,
                               true.power.x1.data$d,
                               true.power.x1.data$n,
                               true.power.x1.data$cluster.size),
                               c("d","fractional","n","cluster.size","power","predicted")]);
                               