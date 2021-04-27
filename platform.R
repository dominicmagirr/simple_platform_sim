platform_trial <- function(n_1 = 200,
                           n_2 = 100,
                           mean_ctrl = c(0, 0.2),
                           mean_A = c(0.2, 0.5),
                           mean_B = 0.2){
  
  ## time period 1
  data_1 <- data.frame(y = c(rnorm(n_1, mean = mean_ctrl[1], sd = 1), 
                             rnorm(n_1, mean = mean_A[1], sd = 1)),
                       trt = c(rep("ctrl", n_1),
                               rep("A", n_1)),
                       time = c(rep("1", n_1),
                                rep("1", n_1)))
  ## time period 2
  data_2 <- data.frame(y = c(rnorm(n_2, mean = mean_ctrl[2], sd = 1), 
                             rnorm(n_2, mean = mean_A[2], sd = 1),
                             rnorm(n_2, mean = mean_B, sd = 1)), 
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
  ignore_time <- lm(y ~ trt, data = data_both)
  
  ## fit time period
  fit_time <- lm(y ~ trt + time, data = data_both)
  
  data.frame(ignore_time = coef(ignore_time)["trtB"],
             fit_time = coef(fit_time)["trtB"])
  
}

# constant trt effect (A vs ctrl)
res_const <- purrr::map_df(rep(200, 1e4), 
                           platform_trial, 
                           mean_A = c(0.2, 0.4))
colMeans(res_const)  


# non-constant trt effect (A vs ctrl)
res_non_const <- purrr::map_df(rep(200, 1e4), 
                               platform_trial, 
                               mean_A = c(0.2, 0.5))
colMeans(res_non_const) 

