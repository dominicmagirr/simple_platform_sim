platform_binary <- function(n_1 = 200,
                            n_2 = 100,
                            p_ctrl = c(0.3, 0.2),
                            p_A = c(0.15, 0.0933),
                            p_B = 0.2){
  
  ## time period 1
  data_1 <- data.frame(y = c(rbinom(n_1, size = 1, prob = p_ctrl[1]), 
                             rbinom(n_1, size = 1, prob = p_A[1])),
                       trt = c(rep("ctrl", n_1),
                               rep("A", n_1)),
                       time = c(rep("1", n_1),
                                rep("1", n_1)))
  ## time period 2
  data_2 <- data.frame(y = c(rbinom(n_2, size = 1, prob = p_ctrl[2]), 
                             rbinom(n_2, size = 1, prob = p_A[2]),
                             rbinom(n_2, size = 1, prob = p_B)), 
                       trt = c(rep("ctrl", n_2),
                               rep("A", n_2),
                               rep("B", n_2)),
                       time = c(rep("2", n_2),
                                rep("2", n_2),
                                rep("2", n_2)))
  
  ## Relevel factors for convenience
  data_both <- rbind(data_1, data_2)
  data_both$trt <- relevel(data_both$trt, "ctrl")
  data_both$time <- relevel(data_both$time, "2")
  
  ## ignore time effect
  ignore_time <- glm(y ~ trt, data = data_both, family = "binomial")
  
  ## fit time period
  fit_time <- glm(y ~ trt + time, data = data_both, family = "binomial")
  
  data.frame(ci_B_ignore_time = coef(ignore_time)["trtB"] + qnorm(.975) * coef(summary(ignore_time))["trtB","Std. Error"],
             ci_B_fit_time = coef(fit_time)["trtB"] + qnorm(.975) * coef(summary(fit_time))["trtB","Std. Error"])
  
}

#########################################
# constant trt effect (A vs ctrl)
res_const <- purrr::map_df(rep(200, 1e4), 
                           platform_binary, 
                           p_A = c(0.15, 0.0933))

mean(res_const[,"ci_B_ignore_time"] < 0) ## one-sided type 1 error
mean(res_const[,"ci_B_fit_time"] < 0) ## one-sided type 1 error
 

# non-constant trt effect (A vs ctrl)
res_non_const <- purrr::map_df(rep(200, 1e4), 
                               platform_binary, 
                               p_A = c(0.15, 0.12))

mean(res_non_const[,"ci_B_ignore_time"] < 0) ## one-sided type 1 error
mean(res_non_const[,"ci_B_fit_time"] < 0) ## one-sided type 1 error


