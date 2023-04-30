library(survival)
library(survminer)
library(stats)
library(dplyr)
library(MASS)
library(ggplot2)


dataset <- read.csv("ibm_attrition.csv")
dataset$Attrition <- ifelse(dataset$Attrition == "Yes", 1, 0)
survobj <- with(dataset, Surv(YearsAtCompany, Attrition))
dataset <- dplyr::select(dataset, -EmployeeCount, -EmployeeNumber, -StandardHours, -Over18, -Attrition, -YearsAtCompany)


var_list <- names(dataset)

categorical <- c("BusinessTravel", "Department", "Education", "EducationField", 
                 "EnvironmentSatisfaction", "Gender", "JobInvolvement", "JobRole"
                 , "JobSatisfaction", "MaritalStatus", "PerformanceRating", "RelationshipSatisfaction", 
                 "StockOptionLevel", "WorkLifeBalance")


dataset <- dataset %>% 
  mutate_at(vars(one_of(categorical)), factor)

formula_str <- paste("survobj ~", paste(var_list, collapse = "+"))


cox_model <- coxph(survobj ~ Age + BusinessTravel, data = dataset)
print("test")
#print(summary(cox_model))

full_cox_model <- coxph(formula = as.formula(formula_str), data = dataset)
print("full")
#print(summary(full_cox_model))


backward_selected_model <- stepAIC(full_cox_model, direction = "backward", trace = 0)

print("backwards")
#print(summary(backward_selected_model))

null_coxph_model <- coxph(survobj ~ 1, data = dataset)
forward_selected_model <- stepAIC(null_coxph_model, scope = list(lower = ~1, upper = as.formula(formula_str)),
                                  direction = "forward", trace = 0)
print("forwards")
#print(summary(forward_selected_model))


#forwards and backwards produce same model

#proportional hazards assumption violated
ph_backwards = cox.zph(backward_selected_model)
print(ph_backwards)
plot(ph_backwards)


#martingale linearity check
int_var_final <- c("Age", "DistanceFromHome", "NumCompaniesWorked", "TotalWorkingYears", "TrainingTimesLastYear", "YearsInCurrentRole", 
                   "YearsSinceLastPromotion", "YearsWithCurrManager")
martingale_resids <- residuals(backward_selected_model, type = "martingale")


martingale_resid_df <- data.frame(MartingaleResiduals = martingale_resids, dataset[, int_var_final])


create_martingale_plot <- function(predictor_name, data) {
  plot <- ggplot(data, aes_string(x = predictor_name, y = "MartingaleResiduals")) +
    geom_point() +
    geom_smooth(method = "loess", span = 0.75, se = FALSE, color = "red") +
    theme_bw() +
    labs(title = paste("Martingale Residuals vs.", predictor_name),
         x = predictor_name,
         y = "Martingale Residuals")
  return(plot)
}


for (predictor in int_var_final) {
  plot <- create_martingale_plot(predictor, martingale_resid_df)
  print(plot)
}



