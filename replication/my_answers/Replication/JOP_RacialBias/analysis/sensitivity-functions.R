# with intercept, Charlotte as ref
estimate.poisson.rhoU2 <- function(data,confounder){
  vars <- c("White","Houston","King_County","Los_Angeles",
            "Orlando","San_Jose","Seattle",
            "Tucson","trauma10")
  vars_rhs <- vars[!vars %in% confounder]
  f <- as.formula(paste(confounder,
                        paste(vars_rhs,collapse = "+"),
                        sep="~"))
  fit <- glm(f, data = data,
             family = poisson(link = "log"))
  return(fit)
}

bias.factor <- function(RR_UY, RR_rhoU){
  b <- (RR_UY*RR_rhoU) / (RR_UY + RR_rhoU - 1)
  return(b)
}

cluster.pois2 <- function(data, indices){
  d <- data[indices,]
  fit <- fepois(fatal ~ White + trauma10 + city_factor,
                data = d,
                se = "cluster",
                cluster = cluster_variable)
  coef <- fit$coefficients
  return(coef)
}
