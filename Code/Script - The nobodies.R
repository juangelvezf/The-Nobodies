### -----------------------------------
## 
## Script name: "“Los nadies y las nadies”: The effect of peace-building on political behavior in Colombia"
## Author: Juan Gelvez-Ferreira
## Date created: Oct, 5th, 2023
## Website: https://juangelvezf.github.io/
## Email: j.gelvez@umd.edu
##
## ------------------------------------

# clear all the space objects
rm(list=ls()) 

# Load up the packages 

packages = c("tidyverse","readxl","ggplot2","car","psych","writexl","multcomp",
             "BiocManager","haven","plyr","readxl", "dplyr",
             "writexl","stargazer","outreg","lctools","here","rgdal","margins","dotwhisker","jtools",
             "interplot","reshape","gridExtra","sjPlot","MatchIt","dplyr","viridis","hrbrthemes")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
    
  }})

# setwd("-"
setwd("C:/Users/usuario/Dropbox/....")

# open data
# database_name <- rio::import("/.../name_database.csv")
db <- rio::import(here::here("carpet_name","file_name.csv"))

#summary dataset
skim(db)


## OLS models

### Reg analisis and plots ####
#Just main regressions
r1 <-lm(Petro_1ra_porcentaje ~ acc_subversivas_log_1+PDET+interaction + acc_subversivas2  
        + leaders+percent_petro_2018_1ra
        + coalicion_presidente_2019+
          +DI_desemp_int+h_coca+nbi
        +slave_ratio_norm + 
          discapital+altura+log(pobl_tot)
        + fallecidos_covid_rate+turn_out1ra
        + depto, data = db)


r2<- lm(Petro_2da_porcentaje ~ acc_subversivas_log_1+PDET+interaction + acc_subversivas2 
        + leaders+percent_petro_2018_1ra
        + coalicion_presidente_2019+
          +DI_desemp_int+h_coca+nbi
        +slave_ratio_norm + 
          discapital+altura+log(pobl_tot)
        + fallecidos_covid_rate+turn_out2da
        + depto, data = db)

plot_summs(r1, r2,
           coefs = c("Subversive actions" = "acc_subversivas_log_1", 
                     "PDET" = "PDET", "Subversive actions and PDET "= "interaction"),
           model.names = c("First Round", "Second round"))

export_summs(r1, r2, scale = TRUE)


#VIPAA data
vipaa_1ra<- lm(Petro_1ra_porcentaje ~ log(violencia_76_02_no_na+1)+violencia_76_02_no_na2
               +log(violencia_03_11_no_na+1)+violencia_03_11_no_na2
               +log(violence_12_16_no_na+1)+violence_12_16_no_na2
               +log(violence_17_19_no_na+1)+violence_17_19_no_na2
               +PDET
               + percent_petro_2018_1ra
               + coalicion_presidente_2019+
                 +DI_desemp_int+h_coca+nbi
               +slave_ratio_norm + 
                 discapital+altura+log(pobl_tot)
               + fallecidos_covid_rate+leaders+turn_out1ra+ depto, data = db)
summary(vipaa_1ra)#first round

vipaa__1ra_paramilitaries<- lm(Petro_1ra_porcentaje ~ paramilitaries_vipaa*PDET+paramilitaries_vipaa2
                               + percent_petro_2018_1ra+coalicion_presidente_2019+DI_desemp_int+log(h_coca+1)
                               + nbi + slave_ratio_norm + fallecidos_covid_rate+leaders+turn_out1ra
                               +discapital+altura+log(pobl_tot)+ depto, data = db)
summary(vipaa__1ra_paramilitaries)#first round


### Propensity score estimation
#logit model where the outcome variable is a binary variable indicating PDET
#difference in means
pdet_cov <- c('acc_subversivas', 'DI_desemp_int', 'h_coca', 'nbi')


lapply(pdet_cov, function(v) {
  t.test(db[, v] ~ db[, 'PDET'])
})

psm <- glm(PDET ~ acc_subversivas + DI_desemp_int + log(h_coca+1) + nbi,
           family = binomial(), data = db)
summary(psm)

#Using this model, we can now calculate the propensity score for each municipality

psm_df <- data.frame(pr_score = predict(psm, type = "response"),
                     PDET = psm$model$PDET)
head(psm_df)

#Histrogram the region of common support
labs <- paste("Actual type of municipality:", c("PDET", "non PDET"))
psm_df %>%
  mutate(PDET = ifelse(PDET == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~PDET) +
  xlab("Probability of beign PDET") +
  theme_bw()


## restrict the sample to observations within the region of common support
#and then to divide the sample within the region of common support into 5 quintiles, 
#based on the estimated propensity score.
m.out <- matchit(PDET ~ acc_subversivas + DI_desemp_int + log(h_coca+1) + nbi,
                 data = db, method = "nearest", ratio=5)
summary(m.out) 
dta_m <- match.data(m.out)

### Effect of PDET using matching
lm1 <- lm(Petro_1ra_porcentaje ~ PDET+log(acc_subversivas+1)+
            percent_petro_2018_1ra
          + coalicion_presidente_2019+
            +DI_desemp_int+h_coca+nbi
          +slave_ratio_norm + 
            discapital+altura+log(pobl_tot)
          + fallecidos_covid_rate+rainfall
          + depto, data = dta_m) 
summary(lm1)#first round

lm2 <- lm(Petro_2da_porcentaje ~ PDET+log(acc_subversivas+1)+
            percent_petro_2018_1ra
          + coalicion_presidente_2019+
            +DI_desemp_int+h_coca+nbi
          +slave_ratio_norm + 
            discapital+altura+log(pobl_tot)
          + fallecidos_covid_rate+rainfall
          + depto, data = dta_m)
summary(lm2)#second round

#Paralelal trends and diff-in-diff
diff <- ggplot(turnout_pdet2, aes(year, turnout_percentage100, color = programa_PDET)) +
  stat_summary(geom = 'line', size = 1.1) +
  geom_vline(xintercept = 2018) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
diff + labs(y = "Turnout percentage", colour = "Peace program")

reg_turnout_PDET <- lm(turnout_percentage ~ after*PDET, data = turnout_pdet2)
summary(reg_turnout_PDET)