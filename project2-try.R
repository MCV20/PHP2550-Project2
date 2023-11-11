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

#Import Data
df <- read.csv("project2.csv")
View(df)

df %>%
  group_by(record_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

df <- df %>% filter(record_id != 2000824)


#Missing Data
#Creating Missing Values Table
missing_table <- df %>%
  summarize(across(everything(), ~ sum(is.na(.x)))) %>%
  t() %>%
  as.data.frame() %>%
  mutate(n=V1) %>%
  select(n) %>%
  arrange(desc(n)) %>% 
  mutate("%" = round(n/dim(df)[1],4)*100)

#Visualization for Missing Data
mis_vars <- missing_table %>% filter(n > 0) %>% rownames()

vis_miss(df %>% select(all_of(mis_vars)))+
  theme(axis.text.x=element_text(size=rel(.9), angle = 90))

vis_miss(df)+theme(axis.text.x=element_text(size=rel(.9), angle = 90))

missing_table_death_no <- df %>% filter(Death != "No") %>%
  summarize(across(everything(), ~ sum(is.na(.x)))) %>%
  t() %>%
  as.data.frame() %>%
  mutate(n=V1) %>%
  select(n) %>%
  arrange(desc(n)) %>% 
  mutate("%" = round(n/dim(df)[1],4)*100)
missing_table_death_yes <- df %>% filter(Death != "Yes") %>%
  summarize(across(everything(), ~ sum(is.na(.x)))) %>%
  t() %>%
  as.data.frame() %>%
  mutate(n=V1) %>%
  select(n) %>%
  arrange(desc(n)) %>% 
  mutate("%" = round(n/dim(df)[1],4)*100)


#plot missing values by death?
p1 <- vis_miss(df %>% filter(Death != "No"))+theme(axis.text.x=element_text(size=rel(.9), angle = 90))
p2 <- vis_miss(df %>% filter(Death != "Yes"))+theme(axis.text.x=element_text(size=rel(.9), angle = 90))
p3 <- vis_miss(df)+theme(axis.text.x=element_text(size=rel(.9), angle = 90))
#library(ggpubr)
ggarrange(p1,p2,p3, ncol = 3)
#lot of data is maybe given by death.. how to deal with that? 
#is there a different way to imputa data for death?

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
                           Trach == "1" ~ "Yes"))



#Summary
#should I include summary of variables before imputation? YES
CreateTableOne(data = df %>% select(!record_id), includeNA = T)
df %>% select(!record_id) %>% tbl_summary()


ggplot(df, aes(x = bw, fill = Trach))+ geom_density()+theme_minimal()

ggplot(df, aes( x = bw, y = ga, color = Trach))+geom_point(alpha = .1)+theme_minimal()

df1 <- df %>% select(bw,center) %>% na.omit()
ggplot(df1, aes(y = bw, x = center))+ geom_boxplot()+theme_minimal()


ggplot(df, aes(x = del_method, fill = Trach))+
  geom_bar(position = position_dodge())+theme_minimal()
ggplot(df, aes(x = prenat_ster, fill = Trach))+
  geom_bar(position = position_dodge())+theme_minimal()
ggplot(df, aes(x = com_prenat_ster, fill = Trach))+
  geom_bar(position = position_dodge())+theme_minimal()
ggplot(df, aes(x = mat_chorio, fill = Trach))+
  geom_bar(position = position_dodge())+theme_minimal()
ggplot(df, aes(x = sga, fill = Trach))+
  geom_bar(position = position_dodge())+theme_minimal()
ggplot(df, aes(x = any_surf, fill = Trach))+
  geom_bar(position = position_dodge())+theme_minimal()

corrplot(cor(df %>% select_if(is.numeric) %>% na.omit()),
         tl.cex = .5,
         tl.col = "black",
         cl.ratio = .2, 
         bg = "white",
         col = colorRampPalette(c("white","grey","black"))(200))
corrplot(cor(df %>% select_if(is.numeric) %>% na.omit()),
         tl.cex = .5,
         tl.col = "black",
         cl.ratio = .2)

tab_death <- df %>% select(!record_id) %>%
  tbl_summary(digits = list(everything() ~ c(2)),
                   statistic = list(all_continuous() ~ "{mean} ({sd})"),
                   by = Death) %>%
  add_overall() %>% 
  add_n() %>%
  add_p() %>%
  modify_header(label ~ "**Death**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") %>%
  bold_labels()
tab_trach <- df %>% select(!record_id) %>%
  tbl_summary(digits = list(everything() ~ c(2)),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              by = Trach) %>%
  add_overall() %>% 
  add_n() %>%
  add_p() %>%
  modify_header(label ~ "**Death**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") %>%
  bold_labels()

tbl_merge(
  tbls = list(tab_death, tab_trach),
  tab_spanner = c("**Death**", "**Tracheostromy**")
)



df %>% select(Trach,Death) %>% table()
#do composite.. predict trach = 1, death yes and so on?

#df <- df %>% 
#  mutate(composite = case_when((Trach == 1 & Death == "Yes") ~ "Trach&Death", 
#                               (Trach == 0 & Death == "No") ~ "NoTrach&NoDeath",
#                               (Trach == 1 & Death == "No") ~ "Trach&NoDeath",
#                               (Trach == 0 & Death == "Yes") ~ "NoTrach&Death"))


df <- df %>% mutate(
  new_trach = case_when( Trach == "Yes" ~ "Yes",
                         (Trach == "No" & Death == "Yes") ~ "Yes",
                         (Trach == "No" & Death == "No") ~ "No")
)
df$new_trach <- as.factor(df$new_trach)

#Creating three data frames for the three models
df_birthmodel <- df[,c(1:15,31)]
df_36wkmodel <- df[,c(1:21,31)]
df_44wkmodel <- df %>% select(!Trach & !Death & !hosp_dc_ga)


#Now we need to only have in our data the "correct" population. That is, select individuals that were alive 
vis_miss(df_36wkmodel %>% 
           select(weight_today.36,ventilation_support_level.36,
                  inspired_oxygen.36,p_delta.36,peep_cm_h2o_modified.36,
                  med_ph.36,hosp_dc_ga,new_trach))

vis_miss(df_44wkmodel %>% 
           select(weight_today.36,ventilation_support_level.36,
                  inspired_oxygen.36,p_delta.36,peep_cm_h2o_modified.36,
                  med_ph.36,weight_today.44, ventilation_support_level_modified.44, 
                  inspired_oxygen.44, p_delta.44, peep_cm_h2o_modified.44, 
                  med_ph.44, hosp_dc_ga))



#Now we need to only have in our data the "correct" population. That is, select individuals that were alive 
#Selecting correct population for 36week
pop_exclude_36 <- df_36wkmodel %>% 
  select(weight_today.36,ventilation_support_level.36,inspired_oxygen.36,
         p_delta.36, peep_cm_h2o_modified.36, med_ph.36) %>%
  is.na() %>%
  rowSums()
pop_exclude_36 <- which(pop_exclude_36 == 6)
df_36wkmodel <- df_36wkmodel[-pop_exclude_36,] 

#Selecting correct population for 44week
pop_exclude_44 <- df_44wkmodel %>% 
  select(weight_today.44,ventilation_support_level_modified.44,inspired_oxygen.44,
         p_delta.44, peep_cm_h2o_modified.44, med_ph.44) %>%
  is.na() %>%
  rowSums()
pop_exclude_44 <- which(pop_exclude_44 == 6)
df_44wkmodel <- df_44wkmodel[-pop_exclude_44,] 








ggpairs(df_birthmodel %>% select_if(is.numeric))+theme_minimal()
ggpairs(df_birthmodel[,-c(1,2)])

mod1 <- glmer(new_trach ~ . + (1 | center),
           data = df_birthmodel[,-1],
           family = binomial)
mod2 <- glmer(new_trach ~ mat_race + mat_ethn + log(bw) + del_method + prenat_ster +
                com_prenat_ster + mat_chorio + gender + sga + any_surf + (1 | center),
              data = df_birthmodel[,-1],
              family = binomial,
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))

summary(mod2)
se <- sqrt(diag(vcov(mod2)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(mod2), LL = fixef(mod2) - 1.96 * se, UL = fixef(mod2) + 1.96 *
                se))

summary(glm(new_trach ~ mat_race + mat_ethn + log(bw) + del_method + prenat_ster +
              com_prenat_ster + mat_chorio + gender + sga + any_surf  ,data = df_birthmodel[,-1], family = "binomial"))


ggplot(df_birthmodel, aes(x = sqrt(bw)))+geom_density()+theme_minimal()





summary(glmer(new_trach ~ log(bw) + del_method+(1 | center),
              data = df_birthmodel[,-1],
              family = binomial,
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5))))








xx <- df_imp_birthmodel[[1]]
xx1 <- xx[,-1] #eliminating center as this will be treated as a random effect
k <- 10 #number of folds
n <- dim(df_train)[1]

set.seed(1)
folds <- sample(1:k, n, replace=TRUE)

df_train <- xx1[folds != 1,]
df_test <- xx1[folds == 1,]

# Fit the model
model <- glm(new_trach ~., data = df_train, family = binomial) %>%
  stepAIC(trace = FALSE)
# Summarize the final selected model
summary(model)
# Make predictions
probabilities <- model %>% predict(df_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
# Model accuracy
mean(predicted.classes==df_test$new_trach)






#any_surf + birth_hc + blength + bw + com_prenat_ster + del_method + ga + 
#  gender + inspired_oxygen.36 + inspired_oxygen.44 + mat_chorio + med_ph.36 + med_ph.44 +
#  p_delta.36 + p_delta.44 + peep_cm_h2o_modified.44 + prenat_ster + sga + ventilation_support_level.36 +
#  ventilation_support_level_modified.44+weight_today.36+weight_today.44+ 





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
  #levels(y) = c(0,1)
  
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












mat_ethn + bw + ga + blength + birth_hc + 
  del_method + prenat_ster + com_prenat_ster + mat_chorio + gender +
  sga + any_surf + weight_today.36  + inspired_oxygen.36 + p_delta.36 + peep_cm_h2o_modified.36 + med_ph.36,



