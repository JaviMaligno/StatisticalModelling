library(dplyr)
library(ggplot2)
library(quantreg)

data_pnad <- read.csv('data_Brazil.csv', 
                       head = TRUE)

data_paraiba <- data_pnad %>% 
  filter(federal_unit == 25) %>% 
  filter(economical_activity == 1) %>% 
  filter(age > 18) %>% 
  mutate(income = ifelse(income >= 9e11, NA, income)) %>% 
  filter(!is.na(income)) %>% 
  filter(income > 0) %>% 
  mutate(race = ifelse(race == 2, "White", "Non-white")) %>% 
  mutate(gender = ifelse(sex == 2, "Male", "Female"))


png("img/disp_rendimento.png")
ggplot(data_paraiba) +
  aes(y = income, x = age) + 
  geom_point() +
  ylim(0, 7500) +
  theme_bw() +
  facet_grid(race ~ gender) +
  labs(title = "Dispersion of income with age") 
dev.off()

png("img/disp_anosEstudo.png")
ggplot(data_paraiba) +
  aes(y = income, x = years_education) + 
  geom_point() +
  ylim(0, 7500) +
  theme_bw() +
  facet_grid(race ~ gender) +
  labs(title = "Dispersion of income with age") 
dev.off()



png("img/hist_genero.png")
ggplot(data_paraiba) +
  aes(x = income, y = ..density..) + 
  geom_histogram(fill = "darkviolet", colour = "grey75") +
  xlim(0, 7500) +
  theme_minimal() +
  facet_wrap(~ gender) +
  labs(title = "Income distribution", 
       y = "density") 
dev.off()

png("img/hist_etnia.png")
ggplot(data_paraiba) +
  aes(x = income, y = ..density..) + 
  geom_histogram(fill = "darkviolet", colour = "grey75") +
  xlim(0, 7500) +
  theme_minimal() +
  facet_wrap(~ race) +
  labs(title = "Income distribution", 
       y = "density") 
dev.off()



model <- rq(income ~ gender + race + age + years_education, 
            tau = 2:18/20, data = data_paraiba)
resumo_freq <- summary(model, se = 'rank')
plot(resumo_freq)

coef_lm <- 
  coef(summary(lm(income ~ gender + race + age + years_education, 
                  data = data_paraiba))) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 

coef_lm <- coef_lm %>% 
  mutate(variable = rownames(coef_lm))

rownames(coef_lm) <- NULL

coef_lm <- coef_lm %>% 
  mutate(lower_bd = estimate + qnorm(0.025) * std_error, 
         upper_bd = estimate + qnorm(0.975) * std_error)


coef_models <- 
  lapply(resumo_freq, function(a) {
    data_info <- data.frame(a$coefficients)
    data_info$variable <- rownames(a$coefficients)
    rownames(data_info) <- NULL
    data_info$est_lm <- coef_lm$estimate
    data_info$lower_bd_lm <- coef_lm$lower_bd
    data_info$upper_bd_lm <- coef_lm$upper_bd
    data_info
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  mutate(tau = rep(2:18/20, each = 5)) %>% 
  janitor::clean_names() %>% 
  mutate(variable = factor(variable), 
         variable = forcats::fct_recode(variable, 
                                        "years education" = "years_education", 
                                        "race = White" = "raceWhite", 
                                        "gender = Male" = "genderMale"))

g_rq <- ggplot(filter(coef_models, variable != "(Intercept)")) +
  theme_minimal() +
  aes(x = tau) +
  geom_line(aes(y = coefficients)) + 
  geom_line(aes(y = lower_bd), linetype = 2) + 
  geom_line(aes(y = upper_bd), linetype = 2) + 
  geom_line(aes(y = est_lm), color = "red") + 
  geom_line(aes(y = lower_bd_lm), linetype = 2, color = "red") + 
  geom_line(aes(y = upper_bd_lm), linetype = 2, color = "red") + 
  facet_wrap(~ variable, scales = "free")

png("img/coef_reg_quantilica.png", width = 900)
g_rq
dev.off()

make_plot_coefficient <- function(nome_var){
  ggplot(filter(coef_models, variable == nome_var)) +
    theme_minimal() +
    aes(x = tau) +
    geom_line(aes(y = coefficients)) + 
    geom_line(aes(y = lower_bd), linetype = 2) + 
    geom_line(aes(y = upper_bd), linetype = 2) + 
    geom_line(aes(y = est_lm), color = "red") + 
    geom_line(aes(y = lower_bd_lm), linetype = 2, color = "red") + 
    geom_line(aes(y = upper_bd_lm), linetype = 2, color = "red") + 
    facet_wrap(~ variable, scales = "free")
  
}

g_rq2 <- make_plot_coefficient(levels(coef_models$variable)[2]) 
g_rq3 <- make_plot_coefficient(levels(coef_models$variable)[3]) 
g_rq4 <- make_plot_coefficient(levels(coef_models$variable)[4]) 
g_rq5 <- make_plot_coefficient(levels(coef_models$variable)[5]) 

png("img/coef_reg_quantilica2.png", width = 900)
g_rq2
dev.off()

png("img/coef_reg_quantilica3.png", width = 900)
g_rq3
dev.off()

png("img/coef_reg_quantilica4.png", width = 900)
g_rq4
dev.off()

png("img/coef_reg_quantilica5.png", width = 900)
g_rq5
dev.off()




model_bayes <- 
  bayesQR::bayesQR(income ~ gender + race + age + years_education, 
                   quantile = 2:18/20, 
                   data = data_paraiba, 
                   ndraw = 110000, 
                   keep = 100)

model_bayes_2 <- lapply(2:18/20, function(a){
  R2BayesX::bayesx(income ~ gender + race + age + years_education, 
                   data = data_paraiba,
                   iter = 110000, burnin = 10000,
                   step = 100, method = "MCMC",
                   family = "quantreg", quantile = a, 
                   control =
                     R2BayesX::bayesx.control(outfile = "/Users/macadmin/work_related/short_course_EPBEST/Slides/resultsBX/"))
})

resumo <- summary(model_bayes, burnin = 100, credint=c(.05,.95))
coef_models <- 
  lapply(resumo, function(a) {
    data_info <- data.frame(a$betadraw[, 1:3])
    data_info$variable <- rownames(a$betadraw)
    rownames(data_info) <- NULL
    data_info
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  mutate(tau = rep(2:18/20, each = 5)) %>% 
  janitor::clean_names() 

ggplot(filter(coef_models, variable != "(Intercept)")) +
  theme_minimal() +
  geom_line(aes(x = tau, y = bayes_estimate)) + 
  geom_line(aes(x = tau, y = lower), linetype = 2) + 
  geom_line(aes(x = tau, y = upper), linetype = 2) + 
  facet_wrap(~ variable, scales = "free")






##### bamlss

library(bamlss)

f <- income ~ gender + race + age + years_education 

b1 <- bamlss(f, family = "gamma", data = data_paraiba)

resumo_bamlss <- summary(b1)

res <- lapply(resumo_bamlss$model.matrix, function(a){
  dim_x <- dim(a)[1]
  a[-dim_x, c(1,2,4)]
}) 

res[[1]] %>% knitr::kable('html', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("scale_down")) %>% 
  kableExtra::save_kable("img/tabela_coeficientes.png")


png("plot_bamlss.png")
plot(b1, which = "samples")
dev.off()

DIC(b1)


## Polinomial
f2 <- income ~ gender + race + poly(age, 3) + poly(years_education, 3)

b2 <- bamlss(f2, family = "gamma", data = data_paraiba)

resumo_bamlss <- summary(b2)

##

nd <- data.frame(years_education = seq(1, 16, len = 100), 
                 age = seq(18, 80, len = 100))

nd$pIdade <- predict(b2, newdata = nd, 
                     model = "mu", term = "age",
                     FUN = c95, intercept = FALSE)

nd$pAnosEstudo <- predict(b2, newdata = nd, 
                          model = "mu", term = "years_education",
                          FUN = c95, intercept = FALSE)


par(mfrow = c(1, 2))
ylim <- range(c(nd$pIdade, nd$pAnosEstudo))
plot2d(pIdade ~ age, data = nd, ylim = ylim)
plot2d(pAnosEstudo ~ years_education, data = nd, ylim = ylim)


## Trying different models

b2_1 <- bamlss(f2, family = "gamma", data = data_paraiba)

b2_2 <- bamlss(f2, family = "weibull", data = data_paraiba)

b2_3 <- bamlss(f2, family = "lognormal", data = data_paraiba)

b2_4 <- bamlss(f2, family = "gpareto", data = data_paraiba)

DIC(b2_1, b2_2, b2_3, b2_4)

par(mfrow = c(2,2))

plot(b2_1, which = c("hist-resid", "qq-resid"))

plot(b2_2, which = c("hist-resid", "qq-resid"))

plot(b2_3, which = c("hist-resid", "qq-resid"))

plot(b2_4, which = c("hist-resid", "qq-resid"))


f3 <- list(income ~ gender + race + poly(age, 3) + poly(years_education, 3), 
           sigma ~ gender + race + poly(age, 3) + poly(years_education, 3))

b3 <- bamlss(f3, family = "gamma", data = data_paraiba)


DIC(b2_1, b3)

nd$pIdade <- predict(b3, newdata = nd, 
                     model = "mu", term = "age",
                     FUN = c95, intercept = FALSE)

nd$pAnosEstudo <- predict(b3, newdata = nd, 
                          model = "mu", term = "years_education",
                          FUN = c95, intercept = FALSE)


nd$pIdade_sigma <- predict(b3, newdata = nd, 
                           model = "sigma", term = "age",
                           FUN = c95, intercept = FALSE)

nd$pAnosEstudo_sigma <- predict(b3, newdata = nd, 
                                model = "sigma", term = "years_education",
                                FUN = c95, intercept = FALSE)


ylim <- range(c(nd$pIdade, nd$pAnosEstudo))
plot2d(pIdade ~ age, data = nd, ylim = ylim)
plot2d(pAnosEstudo ~ years_education, data = nd, ylim = ylim)


par(mfrow = c(1, 2))
ylim2 <- range(c(nd$pIdade_sigma, nd$pAnosEstudo_sigma))
plot2d(pIdade_sigma ~ age, data = nd, ylim = ylim2)
plot2d(pAnosEstudo_sigma ~ years_education, data = nd, ylim = ylim2)


resumo_bamlss3 <- summary(b3)

res3 <- lapply(resumo_bamlss3$model.matrix, function(a){
  dim_x <- dim(a)[1]
  a[-dim_x, c(1,2,4)]
}) 

res3[[2]] %>% knitr::kable('html', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c("scale_down")) %>% 
  kableExtra::save_kable("img/tabela_coeficientes_3_sigma.png")


### Model with nonlinear effect
f4 <- list(income ~ gender + race + age + s(years_education), 
           sigma ~ gender + race + s(age) + s(years_education))

b4 <- bamlss(f4, family = "gamma", data = data_paraiba, 
             n.iter = 25000, burnin = 5000, thin = 20)

nd <- data.frame(years_education = seq(1, 16, len = 100), 
                 age = seq(18, 80, len = 100))

nd$pAnosEstudo <- predict(b4, newdata = nd, 
                          model = "mu", term = "years_education",
                          FUN = c95, intercept = FALSE)

nd$pIdade_sigma <- predict(b4, newdata = nd, 
                           model = "sigma", term = "age",
                           FUN = c95, intercept = FALSE)

nd$pAnosEstudo_sigma <- predict(b4, newdata = nd, 
                                model = "sigma", term = "AnosEstudo",
                                FUN = c95, intercept = FALSE)



par(mfrow = c(1, 3))
ylim <- range(c(nd$pAnosEstudo, nd$pIdade_sigma, nd$pAnosEstudo_sigma))
plot2d(pAnosEstudo ~ AnosEstudo, data = nd, ylim = ylim)
plot2d(pIdade_sigma ~ Idade, data = nd, ylim = ylim)
plot2d(pAnosEstudo_sigma ~ AnosEstudo, data = nd, ylim = ylim)



plot2d(pAnosEstudo_sigma ~ AnosEstudo, data = nd)


### Creating gif with images

## list file names and read in
library(magick)
imgs <- list.files("img/figuras_3d/animacao_3D/", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 4)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "img/gif_resultado.gif")


## Plot with data
imgs <- list.files("img/figuras_3d/dados_3D/", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 4)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "img/gif_dados.gif")



