# PHP2550-Project2
Project 2: Predicting Tracheostomy in Infants with SDP

The optimal criteria and timing for tracheostomy placement in neonates with severe bronchopulmonary dysplasia (sBPD) remain uncertain.This study aims to construct predictive models at three crucial stages in an infant's life: Birth, 36 weeks, and 44 weeks, while comparing the performance of a mixed model with StepAIC and a LASSO model. It employs two statistical techniques for each dataset, initially fitting a generalized mixed model to account for the nested data structure. Variable selection is carried out using the stepwise Akaike Information Criterion (stepAIC) algorithm. This process is repeated independently for each training imputed dataset, ensuring a comprehensive approach. Subsequently, a Lasso logistic known for reliable predictions and feature selection, is applied. Cross-validation determines the optimal penalization parameter, and the final model is derived by pooling coefficients from each training imputed dataset. This methodology is systematically applied across six distinct models for a comprehensive comparative analysis.


The file project2-try contains some exploratory data analysis not presented in the written text but was used as a first glimpse at the data. The file project2.qmd contains the analysis performed for the study. 

Additionally, we attempted to implement cross-validation for a mixed model with a LASSO penalty. However, due to time constraints, this endeavor was not completed. Consequently, we proceeded with LASSO considering the center as a fixed effect. The files for this attempts are:glmmlasso-project2-BIRTHMODEL.R and  glmmlasso-project2-36WK.R.
