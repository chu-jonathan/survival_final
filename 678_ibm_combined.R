library(survival)
library(survminer)
library(stats)
library(dplyr)
library(MASS)
library(ggplot2)


dataset <- read.csv("ibm_attrition.csv")
dataset$Attrition <- ifelse(dataset$Attrition == "Yes", 1, 0)
survobj <- with(dataset, Surv(YearsAtCompany, Attrition))
dataset$YearlyIncome <- dataset$MonthlyIncome * 12
dataset <- dplyr::select(dataset, -EmployeeCount, -EmployeeNumber, -StandardHours, -Over18, -Attrition, -YearsAtCompany, -DailyRate, -HourlyRate
                         , -MonthlyRate, -MonthlyIncome, -JobLevel , -EducationField, -RelationshipSatisfaction, -JobInvolvement)

var_list <- names(dataset)

#level reduction
dataset$EnvironmentSatisfaction <- ifelse(dataset$EnvironmentSatisfaction %in% c(1, 2), 0, 1)
dataset$JobSatisfaction <- ifelse(dataset$JobSatisfaction %in% c(1, 2), 0, 1)
dataset$WorkLifeBalance <- ifelse(dataset$WorkLifeBalance %in% c(1, 2), 0, 1)
dataset$StockOptionLevel <- ifelse(dataset$StockOptionLevel %in% c(0, 1), 0, 1)
dataset$JobRole <- ifelse(dataset$JobRole %in% c("Laboratory Technician", "Sales Representative", "Sales Executive"),
                          dataset$JobRole,
                          "Other")
dataset$MaritalStatus <- ifelse(dataset$MaritalStatus %in% c("Married", "Divorced"), 0, 1)




#checks


categorical <- c("BusinessTravel", "Department", "Education",
                 "EnvironmentSatisfaction", "Gender", "JobInvolvement", "JobRole"
                 , "JobSatisfaction", "MaritalStatus", "PerformanceRating", "RelationshipSatisfaction", 
                 "StockOptionLevel", "WorkLifeBalance")


dataset <- dataset %>% 
  mutate_at(vars(one_of(categorical)), factor)

dataset$JobRole <- factor(dataset$JobRole, levels = c("Other", "Laboratory Technician", "Sales Representative", "Sales Executive"))


formula_str <- paste("survobj ~", paste(var_list, collapse = "+"))


full_cox_model <- coxph(formula = as.formula(formula_str), data = dataset)
print("full")
#print(summary(full_cox_model))


bw <- stepAIC(full_cox_model, direction = "backward", trace = 0)

print("backwards")
print(summary(bw))

#null_coxph_model <- coxph(survobj ~ 1, data = dataset)
#forward_selected_model <- stepAIC(null_coxph_model, scope = list(lower = ~1, upper = as.formula(formula_str)),
#                                  direction = "forward", trace = 0)
#print("forwards")
#print(summary(forward_selected_model))


#forwards and backwards produce same model


#proportional hazards assumption violated for business travel, numcompaniesworked, totalworkingyears, yearswithcurrmanager
dataset$x_cat<- ifelse(dataset$YearlyIncome >= median(dataset$YearlyIncome), "above_median", "below_median")
bw = coxph(formula = survobj ~ Age + BusinessTravel + DistanceFromHome + 
             EnvironmentSatisfaction + Gender + JobRole + 
             JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + 
             TotalWorkingYears + TrainingTimesLastYear + 
             WorkLifeBalance + YearsInCurrentRole + YearsSinceLastPromotion + 
             YearsWithCurrManager + YearlyIncome, data = dataset)
print(summary(bw))


print('log-log')
bw_fit = survfit(bw)

plot(bw_fit, col=c("black", "red"), fun="cloglog", xlab="Time in Years", ylab="Log-Log Survival Business Travel")


#martingale linearity check
int_var_final <- c("Age", "DistanceFromHome", "NumCompaniesWorked", "TotalWorkingYears", "TrainingTimesLastYear", "YearsInCurrentRole", 
                   "YearsSinceLastPromotion", "YearsWithCurrManager" , "YearlyIncome")
martingale_resids <- residuals(bw, type = "martingale")


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





