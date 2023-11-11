#Set working directoty
setwd("C:/Users/monic/OneDrive/Desktop/PHP2050/Project 2/")

#Libraries
library(readr)
library(naniar)
library(gtsummary)
library(tableone)
library(dplyr)
library(ggplot2)
library(gtable)
library(kableExtra)
library(mice)
library(corrplot)
library(ggpubr)
library(lme4)
library(MASS)
library(pROC)

#Import Data
df <- read.csv("project2.csv")


#Exclude duplicates ID
df <- df %>% filter(record_id != 2000824)



#Change classes
df <- df %>% 
  mutate_if(is.character,as.factor)
df <- df %>% 
  mutate(center = factor(center),
         mat_race = factor(mat_race),
         mat_ethn = factor(mat_ethn),
         del_method = factor(del_method),
         ventilation_support_level.36 = factor(ventilation_support_level.36),
         ventilation_support_level_modified.44 = factor(ventilation_support_level_modified.44),
         Trach = factor(Trach)
  )
df <- df %>%
  mutate_if(is.integer, as.numeric)
df <- df %>% 
  mutate(Trach = case_when(Trach == "0" ~ "No",
                           Trach == "1" ~ "Yes")) %>%
  mutate(Trach = as.factor(Trach))

#which(df$center==21)
df$center[806] <- 20

df <- df %>% dplyr::select(!mat_race) #droppin race because of coding error
df <- df %>% 
  mutate(com_prenat_ster = case_when(prenat_ster == "No" ~ "No", TRUE ~ as.character(com_prenat_ster))) 
df <- df %>% mutate(com_prenat_ster = as.factor(com_prenat_ster))
df <- df %>% droplevels()


################################
################################Composite Outcome
df <- df %>% 
  mutate(new_trach = case_when( Trach == "Yes" ~ "Yes",
                                (Trach == "No" & Death == "Yes") ~ "Yes",
                                (Trach == "No" & Death == "No") ~ "No")
  ) 
df$new_trach <- as.factor(df$new_trach)

################################
################################Creating three data frames for the three models
df_birthmodel <- df[,c(1:14,30)]
df_36wkmodel <- df[,c(1:20,27,30)]
df_44wkmodel <- df %>% dplyr::select(!Trach & !Death)

#Impute
#Birthmodel
df_birthmodel_out <- mice(df_birthmodel[,-1], 5, pri=F, seed = 1)
df_imp_birthmodel <- vector("list",5)    
for (i in 1:5){df_imp_birthmodel[[i]] <- mice::complete(df_birthmodel_out,i)}


#36-wk model
df_36wk_out <- mice(df_36wkmodel[,-1], 5, pri=F, seed = 1)
df_imp_36wk <- vector("list",5)    
for (i in 1:5){df_imp_36wk[[i]] <- mice::complete(df_36wk_out,i)}

#select correct population
for (i in 1:5) {
  df_imp_36wk[[i]] <- df_imp_36wk[[i]] %>% filter(hosp_dc_ga > 36) %>% dplyr::select(!hosp_dc_ga)
}

#44-wk model
df_44wk_out <- mice(df_44wkmodel[,-1], 5, pri=F, seed = 1)
df_imp_44wk <- vector("list",5)    
for (i in 1:5){df_imp_44wk[[i]] <- mice::complete(df_44wk_out,i)}

#select correct population
for (i in 1:5) {
  df_imp_44wk[[i]] <- df_imp_44wk[[i]] %>% filter(hosp_dc_ga > 44) %>% dplyr::select(!hosp_dc_ga)
}

#########################################
##########################################Divide each imputed dataset into testing and training
#BIRTHMODEL
training_sets_birthmodel <- list()
testing_sets_birthmodel <- list()

set.seed(1)

for (i in 1:5) {
  n <- nrow(df_imp_birthmodel[[i]])
  num_test <- ceiling(n * .25)
  
  test_indices <- sample(1:n, num_test)
  
  training <- df_imp_birthmodel[[i]][-test_indices, ]
  testing <- df_imp_birthmodel[[i]][test_indices, ]
  
  training_sets_birthmodel[[i]] <- training
  testing_sets_birthmodel[[i]] <- testing
}

#36-weekmodel
training_sets_36wk <- list()
testing_sets_36wk <- list()

set.seed(1)

for (i in 1:5) {
  n <- nrow(df_imp_36wk[[i]])
  num_test <- ceiling(n * .25)
  
  test_indices <- sample(1:n, num_test)
  
  training <- df_imp_36wk[[i]][-test_indices, ]
  testing <- df_imp_36wk[[i]][test_indices, ]
  
  training_sets_36wk[[i]] <- training
  testing_sets_36wk[[i]] <- testing
}

#44-weekmodel
training_sets_44wk <- list()
testing_sets_44wk <- list()

set.seed(1)

for (i in 1:5) {
  n <- nrow(df_imp_44wk[[i]])
  num_test <- ceiling(n * .25)
  
  test_indices <- sample(1:n, num_test)
  
  training <- df_imp_44wk[[i]][-test_indices, ]
  testing <- df_imp_44wk[[i]][test_indices, ]
  
  training_sets_44wk[[i]] <- training
  testing_sets_44wk[[i]] <- testing
}




#####################################################
#####################################################
########################################TRY GLMMLASSO
# Cross-Validation
k <- 10
n <- nrow(df_imp_birthmodel[[1]])
lambda <- seq(0.001,50, length = 50)

set.seed(1)
folds <- sample(1:k, n, replace=TRUE)
cv.errors <- matrix(NA,k,length(lambda))


coef_birthmodel_glmm_all <- matrix(NA,nrow = dim(df_imp_birthmodel[[1]])[2] -1, ncol = 5)
coef_center__birthmodel_glmm_all <-  matrix(NA,nrow = 9, ncol = 5)
rownames(coef_center__birthmodel_glmm_all) <- c("center1","center2","center3","center4","center5","center7","center12","center16","center20")



for (k in 1:5){
  for (j in 1:length(lambda)) {
    for (i in 1:10) {
      mod <- glmmLasso(fix = as.numeric(new_trach)-1 ~ mat_ethn + bw + ga + blength + birth_hc + 
                         del_method + prenat_ster + com_prenat_ster + mat_chorio + gender +
                         sga + any_surf, 
                       rnd = list(center=~1), 
                       data = df_imp_birthmodel[[k]][folds != i,], 
                       family = binomial(),
                       lambda = lambda[j], 
                       control = list(print.iter=TRUE),
                       final.re=TRUE
      )
      coef_glmm_birthmodel <- coef(mod)
      coef_center_glmm_birthmodel <- mod$ranef
      
      x_new <- df_imp_birthmodel[[k]][folds == i,]
      x_new <- model.matrix(new_trach ~ mat_ethn + bw + ga + blength + birth_hc + del_method + prenat_ster +
                              com_prenat_ster + mat_chorio +  gender + sga + any_surf +center,
                            x_new,
                            contrasts.arg = list(center = contrasts(df_imp_birthmodel[[k]][folds == i,]$center, 
                                                                    contrasts = FALSE)))
      pred <- x_new %*% c(coef_glmm_birthmodel,coef_center_glmm_birthmodel)
      pred <- exp(pred)/(exp(pred)+1)
      
      y <- df_imp_birthmodel[[k]][folds == i,]$new_trach
      levels(y) <- c(0,1)
      roc1 <- roc(predictor = pred, response = y, levels = c(0,1), direction = "<")
      auc(roc1)
      cv.errors[i,j] <- auc(roc1)
    }
  }
  
  lambda[which.min(colMeans(cv.errors))] #train model with this lambda on whole dataset
  
  mod1 <- glmmLasso(fix = as.numeric(new_trach)-1 ~ mat_ethn + bw + ga + blength + birth_hc + 
                      del_method + prenat_ster + com_prenat_ster + mat_chorio + gender +
                      sga + any_surf, 
                    rnd = list(center=~1), 
                    data = df_imp_birthmodel[[k]][folds != i,], 
                    family = binomial(),
                    lambda = lambda[which.min(colMeans(cv.errors))], 
                    control = list(print.iter=TRUE),
                    final.re=TRUE
  )
  
  coef_birthmodel_glmm_all[,k] <- mod1$coefficients
  coef_center__birthmodel_glmm_all[,k] <- mod1$ranef
}
coef_birthmodel_glmm_all <- rowMeans(coef_birthmodel_glmm_all)
coef_center__birthmodel_glmm_all <- rowMeans(coef_center__birthmodel_glmm_all)



##Predict
auc_birthmodel_glmm <- rep(NA,5)
brier_birthmodel_glmm <- rep(NA,5)
sensitivity_birthmodel_glmm <- rep(NA,5)
specificity_birthmodel_glmm <- rep(NA,5)
threshold_birthmodel_glmm <- rep(NA,5)

for (i in 1:5) {
  x_new <- testing_sets_birthmodel[[i]]
  #x_new <- x_new %>% dplyr::select(any_surf, blength,ga,mat_ethn,prenat_ster,sga, new_trach,center)
  #x_new <- cbind(1,x_new)
  x_new <- model.matrix(new_trach ~ mat_ethn + bw + ga + blength + birth_hc + del_method + prenat_ster +
                          com_prenat_ster + mat_chorio +  gender + sga + any_surf +center,
                        x_new,
                        contrasts.arg = list(center = contrasts(df_imp_birthmodel[[i]]$center, 
                                                                contrasts = FALSE)))
  
  pred <- x_new %*% c(coef_birthmodel_glmm_all,coef_center__birthmodel_glmm_all)
  pred <- exp(pred)/(exp(pred)+1)
 
  y <- testing_sets_birthmodel[[i]]$new_trach
  levels(y) = c(0,1)
  
  roc1 <- roc(predictor = pred, response = y, levels = c(0,1), direction = "<")
  #plot(roc1, print.thres=TRUE)
  k <- coords(roc=roc1, x = "best")
  
  #store metrics
  auc_birthmodel_glmm[i] <- auc(roc1)
  brier_birthmodel_glmm[i] <- mean((pred - (as.numeric(y)-1))^2)
  sensitivity_birthmodel_glmm[i] <- k$sensitivity
  specificity_birthmodel_glmm[i] <- k$specificity
  threshold_birthmodel_glmm[i] <- k$threshold
}

auc_birthmodel_glmm_pooled <- mean(auc_birthmodel_glmm)
brier_birthmodel_glmm_pooled <- mean(brier_birthmodel_glmm)
sensitivity_birthmodel_glmm_pooled <- mean(sensitivity_birthmodel_glmm)
specificity_birthmodel_glmm_pooled <- mean(specificity_birthmodel_glmm)
threshold_birthmodel_glmm_pooled <- mean(threshold_birthmodel_glmm)  














