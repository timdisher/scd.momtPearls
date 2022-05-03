## code to prepare `DATASET` dataset goes here
library(dplyr)

set.seed(101)
dat <- purrr::map(1:10, ~ {
  n <- 10000 # Number of patients in trial
  trt <- rbinom(n, 1, 0.5) # Simple randomized treatment
  u <- rnorm(n, 1) # Underlying severity is distributed standard normal
  d_mort <- log(0.75)# Treatment effect on mortality

  lo_mort <- qlogis(0.2) + log(1.5)*u + d_mort*trt# Baseline probability of death plus effect of severity
  lo_cp <- qlogis(0.01) + log(4)*u # CP is affected by severity but not treatment


  r_mort <- rbinom(n, 1, plogis(lo_mort))
  r_cp <- rbinom(n, 1, plogis(lo_cp)) # CP event before censoring for mortality


  dat <- data.frame(trt,
                    r_mort,
                    r_cp) %>%
    dplyr::mutate(
      r_cp = ifelse(r_mort == 1, 0, r_cp) # For simplicity, no mortality happens after CP
    )

  dat %>% dplyr::group_by(trt) %>%
    dplyr::summarise_all(mean)

  m1 <- glm(r_mort ~ trt, family = binomial, data = dat)
  m2 <- glm(r_cp ~ trt, family = binomial, data = dat)

  data.frame(
    out = c("mort","cp"),
    point = c(exp(coef(m1))[["trt"]], exp(coef(m2))[["trt"]]),
    se =  sqrt(c(vcov(m1)[2,2], vcov(m2)[2,2]))
  ) %>%
    dplyr::mutate(
      lwr = exp(log(point) - 1.96 * se),
      upr = exp(log(point) + 1.96 * se)
    )


}) %>%
  dplyr::bind_rows(.id = "study")





usethis::use_data(dat, overwrite = TRUE)

