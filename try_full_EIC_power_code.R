CalculatePowerFullEIC <- function(alpha, 
                                  cluster_size, 
                                  icc,
                                  nclusters, 
                                  nfactors,
                                  nregcoefs,
                                  pre_post_corr,
                                  sigma,
                                  the_coef,
                                  silent ){
  df2 <- nclusters - nregcoefs;
  if(df2 < 1){
    stop("Macro error: The number of clusters would be too small to fit the model.");
  }
  num <- the_coef*the_coef; 
  term1 <- icc / (nclusters*(1-icc));
  term2 <- (1-pre_post_corr^2)/(nclusters*cluster_size);
  den <- (sigma^2)*( term1 + term2 );
  lambda <- num/den;
  if(is.infinite(lambda)){
    stop("Macro error: Could not calculate noncentrality parameter.");
  }
  df1 <- 1;
  crit <- qf(1-alpha,df1,df2,0);
  power <- round(1-pf(crit,
                      df1,
                      df2,
                      min(100,lambda)),
                 digits=4);
  if(silent == 0){
    print(paste("The calculated power is",
                as.character(power),
                sep=" "));
#    if(assignment == "between_clusters"){
      ncells <- 2^nfactors;
      if(nclusters < ncells){
        print(paste("However, a complete factorial requires at least ",
                    as.character(ncells),
                    " clusters.",
                    sep=""));
      }
#    }
  }
  return(power);
} 


scenarios <- expand.grid(cluster_size=c(5,10),
                         N=c(300,400,500,600),
                         icc=c(.1,.2),
                         d=c(.2,.3,.5),
                         dropout=.2)

power_estimates <- rep(NA, nrow(scenarios))

for (scenario_id in 1:nrow(scenarios)) {
this_scenario <- scenarios[scenario_id,]
ans1 <- CalculatePowerFullEIC(
  alpha=.05, 
  cluster_size=this_scenario$cluster_size*(1-this_scenario$dropout), 
  icc=this_scenario$icc,
  nfactors=5,
  nclusters=this_scenario$N/this_scenario$cluster_size, 
  nregcoefs=17,
  pre_post_corr=.65,
  sigma=1,
  the_coef=this_scenario$d/2,
  silent=TRUE)
power_estimates[scenario_id] <- ans1
}

print(cbind(scenarios[,c("N","cluster_size","d","icc")],
            power_estimates))
