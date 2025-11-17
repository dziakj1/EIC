CalculatePowerPartialEIC <- function(alpha, 
                                     cluster_size, 
                                     icc,
                                     nclusters, 
                                     nfactors,
                                     nregcoefs,
                                     nsingletons,
                                     pre_post_corr,
                                     sigma,
                                     the_coef,
                                     variance_ratio_unclustered_clustered ){
  print(c(nclusters, nregcoefs))
  df2 <- nclusters - nregcoefs;
  if(df2 < 1) {
    power <- NA;
  } else {
    num <- the_coef*the_coef; 
    term1 <- icc / (4*(1-icc)*nclusters);  # cluster-level variance contribution
    term2 <- (1-pre_post_corr^2) / (4*nclusters*cluster_size);  # individual-level variance contribution from clustered individuals
    term3 <- (1-pre_post_corr^2) / (4*nsingletons);   # individual-level variance contribution from singletons
    den <- (sigma^2)*( term1 + term2 + term3);
    lambda <- num/den;
    if(is.infinite(lambda)){
      power <- NA;
    } else {
      df1 <- 1;
      crit <- qf(1-alpha,df1,df2,0);
      power <- round(1-pf(crit,
                          df1,
                          df2,
                          min(100,lambda)),
                     digits=4);
    }
  }  
  return(power);
} 


scenarios <- expand.grid(alloc_to_clusters = c(.5,.6,.7),
                         N = c(300,400,500,600),
                         cluster_size = 5,
                          icc = c(.1,.2), 
                         d = .3,
                         dropout = .2)

power_estimates <- rep(NA, nrow(scenarios))

for (scenario_id in 1:nrow(scenarios)) {
  this_scenario <- scenarios[scenario_id,]
  this_nclusters <- floor(this_scenario$N * this_scenario$alloc_to_clusters ) /
    this_scenario$cluster_size
  this_effective_cluster_size <- this_scenario$cluster_size * (1-this_scenario$dropout) 
  this_nsingletons <- floor(this_scenario$N * (1-this_scenario$dropout) * (1-this_scenario$alloc_to_clusters))
  ans1 <- CalculatePowerPartialEIC(
    alpha=.05, 
    cluster_size=this_effective_cluster_size, 
    icc=this_scenario$icc,
    nfactors=5,
    nclusters=this_nclusters, 
    nregcoefs=17,
    nsingletons = this_nsingletons,
    pre_post_corr=.65,
    sigma=1,
    the_coef=this_scenario$d/2,
    variance_ratio_unclustered_clustered=this_scenario$variance_ratio_unclustered_clustered)
  power_estimates[scenario_id] <- ans1
}

print(cbind(scenarios[,c("N",
                         "alloc_to_clusters",
                         "cluster_size",
                         "d",
                         "icc")],
            power_estimates))
