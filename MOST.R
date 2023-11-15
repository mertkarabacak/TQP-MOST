library(readr)
library(AutoScore)
library(pROC)
library(knitr)
library(kableExtra)
library(predtools)
library(magrittr)
library(dplyr)
library(DescTools)
library(PRROC)
library(ggplot2)


set.seed(42)


#PRIMARY ANALYSIS


#Prepare data.
training_data <- read_csv("Data/final_data_training.csv")
class(training_data) <- "data.frame"
validation_data <- read_csv("Data/final_data_validation.csv")
class(validation_data) <- "data.frame"
test_data <- read_csv("Data/final_data_test.csv")
class(test_data) <- "data.frame"

training_data$"GCS - Motor"  <- factor(training_data$"GCS - Motor", levels = c('None', 'Extension', 'Abnormal flexion', 'Normal flexion', 'Localizing', 'Obeys commands'))
training_data$"GCS - Verbal"  <- factor(training_data$"GCS - Verbal", levels = c('Intubated','None', 'Sounds', 'Words', 'Confused', 'Oriented'))
training_data$"GCS - Eye"  <- factor(training_data$"GCS - Eye", levels = c('None', 'To pressure', 'To sound', 'Spontaneous'))
training_data$"Pupillary Response"  <- factor(training_data$"Pupillary Response", levels = c('Neither reactive', 'One reactive', 'Both reactive'))

validation_data$"GCS - Motor"  <- factor(validation_data$"GCS - Motor", levels = c('None', 'Extension', 'Abnormal flexion', 'Normal flexion', 'Localizing', 'Obeys commands'))
validation_data$"GCS - Verbal"  <- factor(validation_data$"GCS - Verbal", levels = c('Intubated','None', 'Sounds', 'Words', 'Confused', 'Oriented'))
validation_data$"GCS - Eye"  <- factor(validation_data$"GCS - Eye", levels = c('None', 'To pressure', 'To sound', 'Spontaneous'))
validation_data$"Pupillary Response"  <- factor(validation_data$"Pupillary Response")

test_data$"GCS - Motor"  <- factor(test_data$"GCS - Motor", levels = c('None', 'Extension', 'Abnormal flexion', 'Normal flexion', 'Localizing', 'Obeys commands'))
test_data$"GCS - Verbal"  <- factor(test_data$"GCS - Verbal", levels = c('Intubated', 'None', 'Sounds', 'Words', 'Confused', 'Oriented'))
test_data$"GCS - Eye"  <- factor(test_data$"GCS - Eye", levels = c('None', 'To pressure', 'To sound', 'Spontaneous'))
test_data$"Pupillary Response"  <- factor(test_data$"Pupillary Response", levels = c('Neither reactive', 'One reactive', 'Both reactive'))

train_set_oM <- training_data[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label_oM")]
colnames(train_set_oM) <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score","label")
check_data(train_set_oM)
validation_set_oM <- validation_data[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label_oM")]
colnames(validation_set_oM) <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label")
check_data(validation_set_oM)
test_set_oM <- test_data[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label_oM")]
colnames(test_set_oM) <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label")
check_data(test_set_oM)

test_set_threedM <- test_data[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label_threedM")]
colnames(test_set_threedM) <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label")
check_data(test_set_threedM)

test_set_sevendM <- test_data[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label_sevendM")]
colnames(test_set_sevendM) <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label")
check_data(test_set_sevendM)

test_set_fourteendM <- test_data[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label_fourteendM")]
colnames(test_set_fourteendM) <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label")
check_data(test_set_fourteendM)

test_set_thirtydM <- test_data[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label_thirtydM")]
colnames(test_set_thirtydM) <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "TBI Severity", "CRASH Score", "IMPACT Score", "label")
check_data(test_set_thirtydM)


#Define final variables.
final_variables <- c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response")


#Define performance metrics.
perfmet <- c("threshold", "specificity", "sensitivity", "accuracy")


#Initial weighting.
cut_vec <- AutoScore_weighting( 
  train_set = train_set_oM, validation_set = validation_set_oM,
  final_variables = final_variables, max_score = 100,
  categorize = "quantile", quantiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
)

#Rounding up age variables to a nice number.
cut_vec$'Age' <- c(35, 55, 70, 80)


#Fine-tuning.
scoring_table <- AutoScore_fine_tuning(
  train_set = train_set_oM, validation_set = validation_set_oM, 
  final_variables = final_variables, cut_vec = cut_vec, max_score = 100
)


#Testing for oM.
pred_score_oM <- AutoScore_testing(
  test_set = test_set_oM, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE, metrics_ci = TRUE
)

pred_score_oM$Label <- as.factor(pred_score_oM$Label)

test_set_oM$'AutoScore Score'<- pred_score_oM$pred_score

conversion_table_oM <- conversion_table(
  pred_score = pred_score_oM, by = "score", values = c(10, 20, 30, 40, 50, 60, 70, 80, 90)
)

conversion_table_oM <- kable_styling(kable_input = conversion_table_oM, bootstrap_options = c("striped", "hover"))
save_kable(conversion_table_oM, "Results/Conversion Table (Overall Mortality).txt")

#Testing for threedM.
pred_score_threedM <- AutoScore_testing(
  test_set = test_set_threedM, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE, metrics_ci = TRUE
)

pred_score_threedM$Label <- as.factor(pred_score_threedM$Label)

test_set_threedM$'AutoScore Score'<- pred_score_threedM$pred_score


conversion_table_threedM <- conversion_table(
  pred_score = pred_score_threedM, by = "score", values = c(10, 20, 30, 40, 50, 60, 70, 80, 90)
)

conversion_table_threedM <- kable_styling(kable_input = conversion_table_threedM, bootstrap_options = c("striped", "hover"))
save_kable(conversion_table_threedM, "Results/Conversion Table (3-Day Mortality).txt")


#Testing for sevendM.
pred_score_sevendM <- AutoScore_testing(
  test_set = test_set_sevendM, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE, metrics_ci = TRUE
)

pred_score_sevendM$Label <- as.factor(pred_score_sevendM$Label)

conversion_table_sevendM <- conversion_table(
  pred_score = pred_score_sevendM, by = "score", values = c(10, 20, 30, 40, 50, 60, 70, 80, 90)
)

conversion_table_sevendM <- kable_styling(kable_input = conversion_table_sevendM, bootstrap_options = c("striped", "hover"))
save_kable(conversion_table_sevendM, "Results/Conversion Table (7-Day Mortality).txt")


#Testing for fourteendM.
pred_score_fourteendM <- AutoScore_testing(
  test_set = test_set_fourteendM, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE, metrics_ci = TRUE
)

pred_score_fourteendM$Label <- as.factor(pred_score_fourteendM$Label)

conversion_table_fourteendM <- conversion_table(
  pred_score = pred_score_fourteendM, by = "score", values = c(10, 20, 30, 40, 50, 60, 70, 80, 90)
)

conversion_table_fourteendM <- kable_styling(kable_input = conversion_table_fourteendM, bootstrap_options = c("striped", "hover"))
save_kable(conversion_table_fourteendM, "Results/Conversion Table (14-Day Mortality).txt")


#Testing for thirtydM.
pred_score_thirtydM <- AutoScore_testing(
  test_set = test_set_thirtydM, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE, metrics_ci = TRUE
)

pred_score_thirtydM$Label <- as.factor(pred_score_thirtydM$Label)

conversion_table_thirtydM <- conversion_table(
  pred_score = pred_score_thirtydM, by = "score", values = c(10, 20, 30, 40, 50, 60, 70, 80, 90)
)

conversion_table_thirtydM <- kable_styling(kable_input = conversion_table_thirtydM, bootstrap_options = c("striped", "hover"))
save_kable(conversion_table_thirtydM, "Results/Conversion Table (30-Day Mortality).txt")


#Calculate ROC curves (oM).
ROC_AutoScore_oM <- roc(test_set_oM$label, test_set_oM$"AutoScore Score")
ROC_CRASH_oM <- roc(test_set_oM$label, test_set_oM$"CRASH Score")
ROC_IMPACT_oM <- roc(test_set_oM$label, test_set_oM$"IMPACT Score")


#Calculate performance metrics (oM).
auc_AutoScore_oM <- unname(ci.auc(ROC_AutoScore_oM, conf.level=0.95)[c(2, 1, 3)])
auc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", auc_AutoScore_oM[1], auc_AutoScore_oM[2], auc_AutoScore_oM[3])
metrics_AutoScore_oM <- ci.coords(ROC_AutoScore_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_AutoScore_oM <- metrics_AutoScore_oM$accuracy[c(2, 1, 3)]
acc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", acc_AutoScore_oM[1], acc_AutoScore_oM[2], acc_AutoScore_oM[3])
sen_AutoScore_oM <- metrics_AutoScore_oM$sensitivity[c(2, 1, 3)]
sen_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", sen_AutoScore_oM[1], sen_AutoScore_oM[2], sen_AutoScore_oM[3])
spe_AutoScore_oM <- metrics_AutoScore_oM$specificity[c(2, 1, 3)]
spe_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", spe_AutoScore_oM[1], spe_AutoScore_oM[2], spe_AutoScore_oM[3])

auc_CRASH_oM <- unname(ci.auc(ROC_CRASH_oM, conf.level=0.95)[c(2, 1, 3)])
auc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", auc_CRASH_oM[1], auc_CRASH_oM[2], auc_CRASH_oM[3])
metrics_CRASH_oM <- ci.coords(ROC_CRASH_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_CRASH_oM <- metrics_CRASH_oM$accuracy[c(2, 1, 3)]
acc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", acc_CRASH_oM[1], acc_CRASH_oM[2], acc_CRASH_oM[3])
sen_CRASH_oM <- metrics_CRASH_oM$sensitivity[c(2, 1, 3)]
sen_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", sen_CRASH_oM[1], sen_CRASH_oM[2], sen_CRASH_oM[3])
spe_CRASH_oM <- metrics_CRASH_oM$specificity[c(2, 1, 3)]
spe_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", spe_CRASH_oM[1], spe_CRASH_oM[2], spe_CRASH_oM[3])

auc_IMPACT_oM <- unname(ci.auc(ROC_IMPACT_oM, conf.level=0.95)[c(2, 1, 3)])
auc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", auc_IMPACT_oM[1], auc_IMPACT_oM[2], auc_IMPACT_oM[3])
metrics_IMPACT_oM <- ci.coords(ROC_IMPACT_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_IMPACT_oM <- metrics_IMPACT_oM$accuracy[c(2, 1, 3)]
acc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", acc_IMPACT_oM[1], acc_IMPACT_oM[2], acc_IMPACT_oM[3])
sen_IMPACT_oM <- metrics_IMPACT_oM$sensitivity[c(2, 1, 3)]
sen_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", sen_IMPACT_oM[1], sen_IMPACT_oM[2], sen_IMPACT_oM[3])
spe_IMPACT_oM <- metrics_IMPACT_oM$specificity[c(2, 1, 3)]
spe_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", spe_IMPACT_oM[1], spe_IMPACT_oM[2], spe_IMPACT_oM[3])


#Plot ROC Curves (oM).
pdf("Figures/ROCs (Overall Mortality).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(ROC_AutoScore_oM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(ROC_CRASH_oM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(ROC_IMPACT_oM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", ROC_AutoScore_oM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", ROC_CRASH_oM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", ROC_IMPACT_oM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Calculate ROC curves (threedM).
ROC_AutoScore_threedM <- roc(pred_score_threedM$Label, pred_score_threedM$pred_score)
ROC_CRASH_threedM <- roc(test_set_threedM$label, test_set_threedM$"CRASH Score")
ROC_IMPACT_threedM <- roc(test_set_threedM$label, test_set_threedM$"IMPACT Score")


#Calculate performance metrics (threedM).
auc_AutoScore_threedM <- unname(ci.auc(ROC_AutoScore_threedM, conf.level=0.95)[c(2, 1, 3)])
auc_AutoScore_threedM <- sprintf("%.3f (%.3f - %.3f)", auc_AutoScore_threedM[1], auc_AutoScore_threedM[2], auc_AutoScore_threedM[3])
metrics_AutoScore_threedM <- ci.coords(ROC_AutoScore_threedM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_AutoScore_threedM <- metrics_AutoScore_threedM$accuracy[c(2, 1, 3)]
acc_AutoScore_threedM <- sprintf("%.3f (%.3f - %.3f)", acc_AutoScore_threedM[1], acc_AutoScore_threedM[2], acc_AutoScore_threedM[3])
sen_AutoScore_threedM <- metrics_AutoScore_threedM$sensitivity[c(2, 1, 3)]
sen_AutoScore_threedM <- sprintf("%.3f (%.3f - %.3f)", sen_AutoScore_threedM[1], sen_AutoScore_threedM[2], sen_AutoScore_threedM[3])
spe_AutoScore_threedM <- metrics_AutoScore_threedM$specificity[c(2, 1, 3)]
spe_AutoScore_threedM <- sprintf("%.3f (%.3f - %.3f)", spe_AutoScore_threedM[1], spe_AutoScore_threedM[2], spe_AutoScore_threedM[3])

auc_CRASH_threedM <- unname(ci.auc(ROC_CRASH_threedM, conf.level=0.95)[c(2, 1, 3)])
auc_CRASH_threedM <- sprintf("%.3f (%.3f - %.3f)", auc_CRASH_threedM[1], auc_CRASH_threedM[2], auc_CRASH_threedM[3])
metrics_CRASH_threedM <- ci.coords(ROC_CRASH_threedM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_CRASH_threedM <- metrics_CRASH_threedM$accuracy[c(2, 1, 3)]
acc_CRASH_threedM <- sprintf("%.3f (%.3f - %.3f)", acc_CRASH_threedM[1], acc_CRASH_threedM[2], acc_CRASH_threedM[3])
sen_CRASH_threedM <- metrics_CRASH_threedM$sensitivity[c(2, 1, 3)]
sen_CRASH_threedM <- sprintf("%.3f (%.3f - %.3f)", sen_CRASH_threedM[1], sen_CRASH_threedM[2], sen_CRASH_threedM[3])
spe_CRASH_threedM <- metrics_CRASH_threedM$specificity[c(2, 1, 3)]
spe_CRASH_threedM <- sprintf("%.3f (%.3f - %.3f)", spe_CRASH_threedM[1], spe_CRASH_threedM[2], spe_CRASH_threedM[3])

auc_IMPACT_threedM <- unname(ci.auc(ROC_IMPACT_threedM, conf.level=0.95)[c(2, 1, 3)])
auc_IMPACT_threedM <- sprintf("%.3f (%.3f - %.3f)", auc_IMPACT_threedM[1], auc_IMPACT_threedM[2], auc_IMPACT_threedM[3])
metrics_IMPACT_threedM <- ci.coords(ROC_IMPACT_threedM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_IMPACT_threedM <- metrics_IMPACT_threedM$accuracy[c(2, 1, 3)]
acc_IMPACT_threedM <- sprintf("%.3f (%.3f - %.3f)", acc_IMPACT_threedM[1], acc_IMPACT_threedM[2], acc_IMPACT_threedM[3])
sen_IMPACT_threedM <- metrics_IMPACT_threedM$sensitivity[c(2, 1, 3)]
sen_IMPACT_threedM <- sprintf("%.3f (%.3f - %.3f)", sen_IMPACT_threedM[1], sen_IMPACT_threedM[2], sen_IMPACT_threedM[3])
spe_IMPACT_threedM <- metrics_IMPACT_threedM$specificity[c(2, 1, 3)]
spe_IMPACT_threedM <- sprintf("%.3f (%.3f - %.3f)", spe_IMPACT_threedM[1], spe_IMPACT_threedM[2], spe_IMPACT_threedM[3])


#Plot ROC Curves (threedM).
pdf("Figures/ROCs (3-Day Mortality).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(ROC_AutoScore_threedM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(ROC_CRASH_threedM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(ROC_IMPACT_threedM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", ROC_AutoScore_threedM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", ROC_CRASH_threedM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", ROC_IMPACT_threedM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Calculate ROC curves (sevendM).
ROC_AutoScore_sevendM <- roc(pred_score_sevendM$Label, pred_score_sevendM$pred_score)
ROC_CRASH_sevendM <- roc(test_set_sevendM$label, test_set_sevendM$"CRASH Score")
ROC_IMPACT_sevendM <- roc(test_set_sevendM$label, test_set_sevendM$"IMPACT Score")


#Calculate performance metrics (sevendM).
auc_AutoScore_sevendM <- unname(ci.auc(ROC_AutoScore_sevendM, conf.level=0.95)[c(2, 1, 3)])
auc_AutoScore_sevendM <- sprintf("%.3f (%.3f - %.3f)", auc_AutoScore_sevendM[1], auc_AutoScore_sevendM[2], auc_AutoScore_sevendM[3])
metrics_AutoScore_sevendM <- ci.coords(ROC_AutoScore_sevendM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_AutoScore_sevendM <- metrics_AutoScore_sevendM$accuracy[c(2, 1, 3)]
acc_AutoScore_sevendM <- sprintf("%.3f (%.3f - %.3f)", acc_AutoScore_sevendM[1], acc_AutoScore_sevendM[2], acc_AutoScore_sevendM[3])
sen_AutoScore_sevendM <- metrics_AutoScore_sevendM$sensitivity[c(2, 1, 3)]
sen_AutoScore_sevendM <- sprintf("%.3f (%.3f - %.3f)", sen_AutoScore_sevendM[1], sen_AutoScore_sevendM[2], sen_AutoScore_sevendM[3])
spe_AutoScore_sevendM <- metrics_AutoScore_sevendM$specificity[c(2, 1, 3)]
spe_AutoScore_sevendM <- sprintf("%.3f (%.3f - %.3f)", spe_AutoScore_sevendM[1], spe_AutoScore_sevendM[2], spe_AutoScore_sevendM[3])

auc_CRASH_sevendM <- unname(ci.auc(ROC_CRASH_sevendM, conf.level=0.95)[c(2, 1, 3)])
auc_CRASH_sevendM <- sprintf("%.3f (%.3f - %.3f)", auc_CRASH_sevendM[1], auc_CRASH_sevendM[2], auc_CRASH_sevendM[3])
metrics_CRASH_sevendM <- ci.coords(ROC_CRASH_sevendM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_CRASH_sevendM <- metrics_CRASH_sevendM$accuracy[c(2, 1, 3)]
acc_CRASH_sevendM <- sprintf("%.3f (%.3f - %.3f)", acc_CRASH_sevendM[1], acc_CRASH_sevendM[2], acc_CRASH_sevendM[3])
sen_CRASH_sevendM <- metrics_CRASH_sevendM$sensitivity[c(2, 1, 3)]
sen_CRASH_sevendM <- sprintf("%.3f (%.3f - %.3f)", sen_CRASH_sevendM[1], sen_CRASH_sevendM[2], sen_CRASH_sevendM[3])
spe_CRASH_sevendM <- metrics_CRASH_sevendM$specificity[c(2, 1, 3)]
spe_CRASH_sevendM <- sprintf("%.3f (%.3f - %.3f)", spe_CRASH_sevendM[1], spe_CRASH_sevendM[2], spe_CRASH_sevendM[3])

auc_IMPACT_sevendM <- unname(ci.auc(ROC_IMPACT_sevendM, conf.level=0.95)[c(2, 1, 3)])
auc_IMPACT_sevendM <- sprintf("%.3f (%.3f - %.3f)", auc_IMPACT_sevendM[1], auc_IMPACT_sevendM[2], auc_IMPACT_sevendM[3])
metrics_IMPACT_sevendM <- ci.coords(ROC_IMPACT_sevendM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_IMPACT_sevendM <- metrics_IMPACT_sevendM$accuracy[c(2, 1, 3)]
acc_IMPACT_sevendM <- sprintf("%.3f (%.3f - %.3f)", acc_IMPACT_sevendM[1], acc_IMPACT_sevendM[2], acc_IMPACT_sevendM[3])
sen_IMPACT_sevendM <- metrics_IMPACT_sevendM$sensitivity[c(2, 1, 3)]
sen_IMPACT_sevendM <- sprintf("%.3f (%.3f - %.3f)", sen_IMPACT_sevendM[1], sen_IMPACT_sevendM[2], sen_IMPACT_sevendM[3])
spe_IMPACT_sevendM <- metrics_IMPACT_sevendM$specificity[c(2, 1, 3)]
spe_IMPACT_sevendM <- sprintf("%.3f (%.3f - %.3f)", spe_IMPACT_sevendM[1], spe_IMPACT_sevendM[2], spe_IMPACT_sevendM[3])


#Plot ROC Curves (sevendM).
pdf("Figures/ROCs (7-Day Mortality).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(ROC_AutoScore_sevendM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(ROC_CRASH_sevendM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(ROC_IMPACT_sevendM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", ROC_AutoScore_sevendM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", ROC_CRASH_sevendM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", ROC_IMPACT_sevendM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Calculate ROC curves (fourteendM).
ROC_AutoScore_fourteendM <- roc(pred_score_fourteendM$Label, pred_score_fourteendM$pred_score)
ROC_CRASH_fourteendM <- roc(test_set_fourteendM$label, test_set_fourteendM$"CRASH Score")
ROC_IMPACT_fourteendM <- roc(test_set_fourteendM$label, test_set_fourteendM$"IMPACT Score")


#Calculate performance metrics (fourteendM).
auc_AutoScore_fourteendM <- unname(ci.auc(ROC_AutoScore_fourteendM, conf.level=0.95)[c(2, 1, 3)])
auc_AutoScore_fourteendM <- sprintf("%.3f (%.3f - %.3f)", auc_AutoScore_fourteendM[1], auc_AutoScore_fourteendM[2], auc_AutoScore_fourteendM[3])
metrics_AutoScore_fourteendM <- ci.coords(ROC_AutoScore_fourteendM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_AutoScore_fourteendM <- metrics_AutoScore_fourteendM$accuracy[c(2, 1, 3)]
acc_AutoScore_fourteendM <- sprintf("%.3f (%.3f - %.3f)", acc_AutoScore_fourteendM[1], acc_AutoScore_fourteendM[2], acc_AutoScore_fourteendM[3])
sen_AutoScore_fourteendM <- metrics_AutoScore_fourteendM$sensitivity[c(2, 1, 3)]
sen_AutoScore_fourteendM <- sprintf("%.3f (%.3f - %.3f)", sen_AutoScore_fourteendM[1], sen_AutoScore_fourteendM[2], sen_AutoScore_fourteendM[3])
spe_AutoScore_fourteendM <- metrics_AutoScore_fourteendM$specificity[c(2, 1, 3)]
spe_AutoScore_fourteendM <- sprintf("%.3f (%.3f - %.3f)", spe_AutoScore_fourteendM[1], spe_AutoScore_fourteendM[2], spe_AutoScore_fourteendM[3])

auc_CRASH_fourteendM <- unname(ci.auc(ROC_CRASH_fourteendM, conf.level=0.95)[c(2, 1, 3)])
auc_CRASH_fourteendM <- sprintf("%.3f (%.3f - %.3f)", auc_CRASH_fourteendM[1], auc_CRASH_fourteendM[2], auc_CRASH_fourteendM[3])
metrics_CRASH_fourteendM <- ci.coords(ROC_CRASH_fourteendM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_CRASH_fourteendM <- metrics_CRASH_fourteendM$accuracy[c(2, 1, 3)]
acc_CRASH_fourteendM <- sprintf("%.3f (%.3f - %.3f)", acc_CRASH_fourteendM[1], acc_CRASH_fourteendM[2], acc_CRASH_fourteendM[3])
sen_CRASH_fourteendM <- metrics_CRASH_fourteendM$sensitivity[c(2, 1, 3)]
sen_CRASH_fourteendM <- sprintf("%.3f (%.3f - %.3f)", sen_CRASH_fourteendM[1], sen_CRASH_fourteendM[2], sen_CRASH_fourteendM[3])
spe_CRASH_fourteendM <- metrics_CRASH_fourteendM$specificity[c(2, 1, 3)]
spe_CRASH_fourteendM <- sprintf("%.3f (%.3f - %.3f)", spe_CRASH_fourteendM[1], spe_CRASH_fourteendM[2], spe_CRASH_fourteendM[3])

auc_IMPACT_fourteendM <- unname(ci.auc(ROC_IMPACT_fourteendM, conf.level=0.95)[c(2, 1, 3)])
auc_IMPACT_fourteendM <- sprintf("%.3f (%.3f - %.3f)", auc_IMPACT_fourteendM[1], auc_IMPACT_fourteendM[2], auc_IMPACT_fourteendM[3])
metrics_IMPACT_fourteendM <- ci.coords(ROC_IMPACT_fourteendM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_IMPACT_fourteendM <- metrics_IMPACT_fourteendM$accuracy[c(2, 1, 3)]
acc_IMPACT_fourteendM <- sprintf("%.3f (%.3f - %.3f)", acc_IMPACT_fourteendM[1], acc_IMPACT_fourteendM[2], acc_IMPACT_fourteendM[3])
sen_IMPACT_fourteendM <- metrics_IMPACT_fourteendM$sensitivity[c(2, 1, 3)]
sen_IMPACT_fourteendM <- sprintf("%.3f (%.3f - %.3f)", sen_IMPACT_fourteendM[1], sen_IMPACT_fourteendM[2], sen_IMPACT_fourteendM[3])
spe_IMPACT_fourteendM <- metrics_IMPACT_fourteendM$specificity[c(2, 1, 3)]
spe_IMPACT_fourteendM <- sprintf("%.3f (%.3f - %.3f)", spe_IMPACT_fourteendM[1], spe_IMPACT_fourteendM[2], spe_IMPACT_fourteendM[3])


#Plot ROC Curves (fourteendM).
pdf("Figures/ROCs (14-Day Mortality).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(ROC_AutoScore_fourteendM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(ROC_CRASH_fourteendM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(ROC_IMPACT_fourteendM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", ROC_AutoScore_fourteendM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", ROC_CRASH_fourteendM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", ROC_IMPACT_fourteendM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Calculate ROC curves (thirtydM).
ROC_AutoScore_thirtydM <- roc(pred_score_thirtydM$Label, pred_score_thirtydM$pred_score)
ROC_CRASH_thirtydM <- roc(test_set_thirtydM$label, test_set_thirtydM$"CRASH Score")
ROC_IMPACT_thirtydM <- roc(test_set_thirtydM$label, test_set_thirtydM$"IMPACT Score")


#Calculate performance metrics (thirtydM).
auc_AutoScore_thirtydM <- unname(ci.auc(ROC_AutoScore_thirtydM, conf.level=0.95)[c(2, 1, 3)])
auc_AutoScore_thirtydM <- sprintf("%.3f (%.3f - %.3f)", auc_AutoScore_thirtydM[1], auc_AutoScore_thirtydM[2], auc_AutoScore_thirtydM[3])
metrics_AutoScore_thirtydM <- ci.coords(ROC_AutoScore_thirtydM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_AutoScore_thirtydM <- metrics_AutoScore_thirtydM$accuracy[c(2, 1, 3)]
acc_AutoScore_thirtydM <- sprintf("%.3f (%.3f - %.3f)", acc_AutoScore_thirtydM[1], acc_AutoScore_thirtydM[2], acc_AutoScore_thirtydM[3])
sen_AutoScore_thirtydM <- metrics_AutoScore_thirtydM$sensitivity[c(2, 1, 3)]
sen_AutoScore_thirtydM <- sprintf("%.3f (%.3f - %.3f)", sen_AutoScore_thirtydM[1], sen_AutoScore_thirtydM[2], sen_AutoScore_thirtydM[3])
spe_AutoScore_thirtydM <- metrics_AutoScore_thirtydM$specificity[c(2, 1, 3)]
spe_AutoScore_thirtydM <- sprintf("%.3f (%.3f - %.3f)", spe_AutoScore_thirtydM[1], spe_AutoScore_thirtydM[2], spe_AutoScore_thirtydM[3])

auc_CRASH_thirtydM <- unname(ci.auc(ROC_CRASH_thirtydM, conf.level=0.95)[c(2, 1, 3)])
auc_CRASH_thirtydM <- sprintf("%.3f (%.3f - %.3f)", auc_CRASH_thirtydM[1], auc_CRASH_thirtydM[2], auc_CRASH_thirtydM[3])
metrics_CRASH_thirtydM <- ci.coords(ROC_CRASH_thirtydM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_CRASH_thirtydM <- metrics_CRASH_thirtydM$accuracy[c(2, 1, 3)]
acc_CRASH_thirtydM <- sprintf("%.3f (%.3f - %.3f)", acc_CRASH_thirtydM[1], acc_CRASH_thirtydM[2], acc_CRASH_thirtydM[3])
sen_CRASH_thirtydM <- metrics_CRASH_thirtydM$sensitivity[c(2, 1, 3)]
sen_CRASH_thirtydM <- sprintf("%.3f (%.3f - %.3f)", sen_CRASH_thirtydM[1], sen_CRASH_thirtydM[2], sen_CRASH_thirtydM[3])
spe_CRASH_thirtydM <- metrics_CRASH_thirtydM$specificity[c(2, 1, 3)]
spe_CRASH_thirtydM <- sprintf("%.3f (%.3f - %.3f)", spe_CRASH_thirtydM[1], spe_CRASH_thirtydM[2], spe_CRASH_thirtydM[3])

auc_IMPACT_thirtydM <- unname(ci.auc(ROC_IMPACT_thirtydM, conf.level=0.95)[c(2, 1, 3)])
auc_IMPACT_thirtydM <- sprintf("%.3f (%.3f - %.3f)", auc_IMPACT_thirtydM[1], auc_IMPACT_thirtydM[2], auc_IMPACT_thirtydM[3])
metrics_IMPACT_thirtydM <- ci.coords(ROC_IMPACT_thirtydM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_IMPACT_thirtydM <- metrics_IMPACT_thirtydM$accuracy[c(2, 1, 3)]
acc_IMPACT_thirtydM <- sprintf("%.3f (%.3f - %.3f)", acc_IMPACT_thirtydM[1], acc_IMPACT_thirtydM[2], acc_IMPACT_thirtydM[3])
sen_IMPACT_thirtydM <- metrics_IMPACT_thirtydM$sensitivity[c(2, 1, 3)]
sen_IMPACT_thirtydM <- sprintf("%.3f (%.3f - %.3f)", sen_IMPACT_thirtydM[1], sen_IMPACT_thirtydM[2], sen_IMPACT_thirtydM[3])
spe_IMPACT_thirtydM <- metrics_IMPACT_thirtydM$specificity[c(2, 1, 3)]
spe_IMPACT_thirtydM <- sprintf("%.3f (%.3f - %.3f)", spe_IMPACT_thirtydM[1], spe_IMPACT_thirtydM[2], spe_IMPACT_thirtydM[3])


#Plot ROC Curves (thirtydM).
pdf("Figures/ROCs (30-Day Mortality).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(ROC_AutoScore_thirtydM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(ROC_CRASH_thirtydM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(ROC_IMPACT_thirtydM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", ROC_AutoScore_thirtydM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", ROC_CRASH_thirtydM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", ROC_IMPACT_thirtydM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Compare for models for overall mortality.
oM_AS_vs_CRASH <- roc.test(ROC_AutoScore_oM, ROC_CRASH_oM, method="bootstrap")$p.value
oM_AS_vs_IMPACT <- roc.test(ROC_AutoScore_oM, ROC_IMPACT_oM, method="bootstrap")$p.value
oM_CRASH_vs_IMPACT <- roc.test(ROC_CRASH_oM, ROC_IMPACT_oM, method="bootstrap")$p.value
oM_comparison <- rbind(oM_AS_vs_CRASH, oM_AS_vs_IMPACT, oM_CRASH_vs_IMPACT)
row.names(oM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(oM_comparison) <- c("Overall Mortality")
oM_comparison <- as.data.frame(oM_comparison)


#Compare for models for 3-day mortality.
threedM_AS_vs_CRASH <- roc.test(ROC_AutoScore_threedM, ROC_CRASH_threedM, method="bootstrap")$p.value
threedM_AS_vs_IMPACT <- roc.test(ROC_AutoScore_threedM, ROC_IMPACT_threedM, method="bootstrap")$p.value
threedM_CRASH_vs_IMPACT <- roc.test(ROC_CRASH_threedM, ROC_IMPACT_threedM, method="bootstrap")$p.value
threedM_comparison <- rbind(threedM_AS_vs_CRASH, threedM_AS_vs_IMPACT, threedM_CRASH_vs_IMPACT)
row.names(threedM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(threedM_comparison) <- c("3-Day Mortality")
threedM_comparison <- as.data.frame(threedM_comparison)


#Compare for models for 7-day mortality.
sevendM_AS_vs_CRASH <- roc.test(ROC_AutoScore_sevendM, ROC_CRASH_sevendM, method="bootstrap")$p.value
sevendM_AS_vs_IMPACT <- roc.test(ROC_AutoScore_sevendM, ROC_IMPACT_sevendM, method="bootstrap")$p.value
sevendM_CRASH_vs_IMPACT <- roc.test(ROC_CRASH_sevendM, ROC_IMPACT_sevendM, method="bootstrap")$p.value
sevendM_comparison <- rbind(sevendM_AS_vs_CRASH, sevendM_AS_vs_IMPACT, sevendM_CRASH_vs_IMPACT)
row.names(sevendM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(sevendM_comparison) <- c("7-Day Mortality")
sevendM_comparison <- as.data.frame(sevendM_comparison)


#Compare for models for 14-day mortality.
fourteendM_AS_vs_CRASH <- roc.test(ROC_AutoScore_fourteendM, ROC_CRASH_fourteendM, method="bootstrap")$p.value
fourteendM_AS_vs_IMPACT <- roc.test(ROC_AutoScore_fourteendM, ROC_IMPACT_fourteendM, method="bootstrap")$p.value
fourteendM_CRASH_vs_IMPACT <- roc.test(ROC_CRASH_fourteendM, ROC_IMPACT_fourteendM, method="bootstrap")$p.value
fourteendM_comparison <- rbind(fourteendM_AS_vs_CRASH, fourteendM_AS_vs_IMPACT, fourteendM_CRASH_vs_IMPACT)
row.names(fourteendM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(fourteendM_comparison) <- c("14-Day Mortality")
fourteendM_comparison <- as.data.frame(fourteendM_comparison)


#Compare for models for 30-day mortality.
thirtydM_AS_vs_CRASH <- roc.test(ROC_AutoScore_thirtydM, ROC_CRASH_thirtydM, method="bootstrap")$p.value
thirtydM_AS_vs_IMPACT <- roc.test(ROC_AutoScore_thirtydM, ROC_IMPACT_thirtydM, method="bootstrap")$p.value
thirtydM_CRASH_vs_IMPACT <- roc.test(ROC_CRASH_thirtydM, ROC_IMPACT_thirtydM, method="bootstrap")$p.value
thirtydM_comparison <- rbind(thirtydM_AS_vs_CRASH, thirtydM_AS_vs_IMPACT, thirtydM_CRASH_vs_IMPACT)
row.names(thirtydM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(thirtydM_comparison) <- c("30-Day Mortality")
thirtydM_comparison <- as.data.frame(thirtydM_comparison)


#Merge comparisons.
among_comparison <- cbind(oM_comparison, threedM_comparison, sevendM_comparison, fourteendM_comparison, thirtydM_comparison)
write.csv(among_comparison, "Results/Model Comparison.csv")


#Calibration analysis for overall mortality.
test_set_oM$'AutoScore Score'<- pred_score_oM$pred_score
log_AutoScore_oM <- glm(test_set_oM$"label" ~ test_set_oM$"AutoScore Score", test_set_oM, family = binomial(link = "logit"))
test_set_oM$'AutoScore_probs' <- predict(log_AutoScore_oM, test_set_oM, type = "response")
cc_AutoScore_oM <- calibration_plot(data = test_set_oM, obs = "label", pred = "AutoScore_probs", nTiles = 20)
ggsave(filename = "Figures/Calibration Plot (Overall Mortality - AutoScore).pdf", plot = cc_AutoScore_oM$calibration_plot, width = 5, height = 5, dpi = 300)

log_CRASH_oM <- glm(test_set_oM$"label" ~ test_set_oM$"CRASH Score", test_set_oM, family = binomial(link = "logit"))
test_set_oM$'CRASH_probs' <- predict(log_CRASH_oM, test_set_oM, type = "response")
cc_CRASH_oM <- calibration_plot(data = test_set_oM, obs = "label", pred = "CRASH_probs")
ggsave(filename = "Figures/Calibration Plot (Overall Mortality - CRASH).pdf", plot = cc_CRASH_oM$calibration_plot, width = 5, height = 5, dpi = 300)

log_IMPACT_oM <- glm(test_set_oM$"label" ~ test_set_oM$"IMPACT Score", test_set_oM, family = binomial(link = "logit"))
test_set_oM$'IMPACT_probs' <- predict(log_IMPACT_oM, test_set_oM, type = "response")
cc_IMPACT_oM <- calibration_plot(data = test_set_oM, obs = "label", pred = "IMPACT_probs")
ggsave(filename = "Figures/Calibration Plot (Overall Mortality - IMPACT).pdf", plot = cc_IMPACT_oM$calibration_plot, width = 5, height = 5, dpi = 300)

brier_AutoScore_oM <- BrierScore(test_set_oM$label, test_set_oM$AutoScore_probs)
brier_AutoScore_oM <- sprintf("%.5f", brier_AutoScore_oM)
brier_CRASH_oM <- BrierScore(test_set_oM$label, test_set_oM$CRASH_probs)
brier_CRASH_oM <- sprintf("%.5f", brier_CRASH_oM)
brier_IMPACT_oM <- BrierScore(test_set_oM$label, test_set_oM$IMPACT_probs)
brier_IMPACT_oM <- sprintf("%.5f", brier_IMPACT_oM)


#Calibration analysis for 3-day mortality.
test_set_threedM$'AutoScore Score'<- pred_score_threedM$pred_score
log_AutoScore_threedM <- glm(test_set_threedM$"label" ~ test_set_threedM$"AutoScore Score", test_set_threedM, family = binomial(link = "logit"))
test_set_threedM$'AutoScore_probs' <- predict(log_AutoScore_threedM, test_set_threedM, type = "response")
cc_AutoScore_threedM <- calibration_plot(data = test_set_threedM, obs = "label", pred = "AutoScore_probs", nTiles = 20)
ggsave(filename = "Figures/Calibration Plot (3-Day Mortality - AutoScore).pdf", plot = cc_AutoScore_threedM$calibration_plot, width = 5, height = 5, dpi = 300)

log_CRASH_threedM <- glm(test_set_threedM$"label" ~ test_set_threedM$"CRASH Score", test_set_threedM, family = binomial(link = "logit"))
test_set_threedM$'CRASH_probs' <- predict(log_CRASH_threedM, test_set_threedM, type = "response")
cc_CRASH_threedM <- calibration_plot(data = test_set_threedM, obs = "label", pred = "CRASH_probs")
ggsave(filename = "Figures/Calibration Plot (3-Day Mortality - CRASH).pdf", plot = cc_CRASH_threedM$calibration_plot, width = 5, height = 5, dpi = 300)

log_IMPACT_threedM <- glm(test_set_threedM$"label" ~ test_set_threedM$"IMPACT Score", test_set_threedM, family = binomial(link = "logit"))
test_set_threedM$'IMPACT_probs' <- predict(log_IMPACT_threedM, test_set_threedM, type = "response")
cc_IMPACT_threedM <- calibration_plot(data = test_set_threedM, obs = "label", pred = "IMPACT_probs")
ggsave(filename = "Figures/Calibration Plot (3-Day Mortality - IMPACT).pdf", plot = cc_IMPACT_threedM$calibration_plot, width = 5, height = 5, dpi = 300)

brier_AutoScore_threedM <- BrierScore(test_set_threedM$label, test_set_threedM$AutoScore_probs)
brier_AutoScore_threedM <- sprintf("%.5f", brier_AutoScore_threedM)
brier_CRASH_threedM <- BrierScore(test_set_threedM$label, test_set_threedM$CRASH_probs)
brier_CRASH_threedM <- sprintf("%.5f", brier_CRASH_threedM)
brier_IMPACT_threedM <- BrierScore(test_set_threedM$label, test_set_threedM$IMPACT_probs)
brier_IMPACT_threedM <- sprintf("%.5f", brier_IMPACT_threedM)

#Calibration analysis for 7-day mortality.
test_set_sevendM$'AutoScore Score'<- pred_score_sevendM$pred_score
log_AutoScore_sevendM <- glm(test_set_sevendM$"label" ~ test_set_sevendM$"AutoScore Score", test_set_sevendM, family = binomial(link = "logit"))
test_set_sevendM$'AutoScore_probs' <- predict(log_AutoScore_sevendM, test_set_sevendM, type = "response")
cc_AutoScore_sevendM <- calibration_plot(data = test_set_sevendM, obs = "label", pred = "AutoScore_probs", nTiles = 20)
ggsave(filename = "Figures/Calibration Plot (7-Day Mortality - AutoScore).pdf", plot = cc_AutoScore_sevendM$calibration_plot, width = 5, height = 5, dpi = 300)

log_CRASH_sevendM <- glm(test_set_sevendM$"label" ~ test_set_sevendM$"CRASH Score", test_set_sevendM, family = binomial(link = "logit"))
test_set_sevendM$'CRASH_probs' <- predict(log_CRASH_sevendM, test_set_sevendM, type = "response")
cc_CRASH_sevendM <- calibration_plot(data = test_set_sevendM, obs = "label", pred = "CRASH_probs")
ggsave(filename = "Figures/Calibration Plot (7-Day Mortality - CRASH).pdf", plot = cc_CRASH_sevendM$calibration_plot, width = 5, height = 5, dpi = 300)

log_IMPACT_sevendM <- glm(test_set_sevendM$"label" ~ test_set_sevendM$"IMPACT Score", test_set_sevendM, family = binomial(link = "logit"))
test_set_sevendM$'IMPACT_probs' <- predict(log_IMPACT_sevendM, test_set_sevendM, type = "response")
cc_IMPACT_sevendM <- calibration_plot(data = test_set_sevendM, obs = "label", pred = "IMPACT_probs")
ggsave(filename = "Figures/Calibration Plot (7-Day Mortality - IMPACT).pdf", plot = cc_IMPACT_sevendM$calibration_plot, width = 5, height = 5, dpi = 300)

brier_AutoScore_sevendM <- BrierScore(test_set_sevendM$label, test_set_sevendM$AutoScore_probs)
brier_AutoScore_sevendM <- sprintf("%.5f", brier_AutoScore_sevendM)
brier_CRASH_sevendM <- BrierScore(test_set_sevendM$label, test_set_sevendM$CRASH_probs)
brier_CRASH_sevendM <- sprintf("%.5f", brier_CRASH_sevendM)
brier_IMPACT_sevendM <- BrierScore(test_set_sevendM$label, test_set_sevendM$IMPACT_probs)
brier_IMPACT_sevendM <- sprintf("%.5f", brier_IMPACT_sevendM)


#Calibration analysis for 14-day mortality.
test_set_fourteendM$'AutoScore Score'<- pred_score_fourteendM$pred_score
log_AutoScore_fourteendM <- glm(test_set_fourteendM$"label" ~ test_set_fourteendM$"AutoScore Score", test_set_fourteendM, family = binomial(link = "logit"))
test_set_fourteendM$'AutoScore_probs' <- predict(log_AutoScore_fourteendM, test_set_fourteendM, type = "response")
cc_AutoScore_fourteendM <- calibration_plot(data = test_set_fourteendM, obs = "label", pred = "AutoScore_probs", nTiles = 20)
ggsave(filename = "Figures/Calibration Plot (14-Day Mortality - AutoScore).pdf", plot = cc_AutoScore_fourteendM$calibration_plot, width = 5, height = 5, dpi = 300)

log_CRASH_fourteendM <- glm(test_set_fourteendM$"label" ~ test_set_fourteendM$"CRASH Score", test_set_fourteendM, family = binomial(link = "logit"))
test_set_fourteendM$'CRASH_probs' <- predict(log_CRASH_fourteendM, test_set_fourteendM, type = "response")
cc_CRASH_fourteendM <- calibration_plot(data = test_set_fourteendM, obs = "label", pred = "CRASH_probs")
ggsave(filename = "Figures/Calibration Plot (14-Day Mortality - CRASH).pdf", plot = cc_CRASH_fourteendM$calibration_plot, width = 5, height = 5, dpi = 300)

log_IMPACT_fourteendM <- glm(test_set_fourteendM$"label" ~ test_set_fourteendM$"IMPACT Score", test_set_fourteendM, family = binomial(link = "logit"))
test_set_fourteendM$'IMPACT_probs' <- predict(log_IMPACT_fourteendM, test_set_fourteendM, type = "response")
cc_IMPACT_fourteendM <- calibration_plot(data = test_set_fourteendM, obs = "label", pred = "IMPACT_probs")
ggsave(filename = "Figures/Calibration Plot (14-Day Mortality - IMPACT).pdf", plot = cc_IMPACT_fourteendM$calibration_plot, width = 5, height = 5, dpi = 300)

brier_AutoScore_fourteendM <- BrierScore(test_set_fourteendM$label, test_set_fourteendM$AutoScore_probs)
brier_AutoScore_fourteendM <- sprintf("%.5f", brier_AutoScore_fourteendM)
brier_CRASH_fourteendM <- BrierScore(test_set_fourteendM$label, test_set_fourteendM$CRASH_probs)
brier_CRASH_fourteendM <- sprintf("%.5f", brier_CRASH_fourteendM)
brier_IMPACT_fourteendM <- BrierScore(test_set_fourteendM$label, test_set_fourteendM$IMPACT_probs)
brier_IMPACT_fourteendM <- sprintf("%.5f", brier_IMPACT_fourteendM)


#Calibration analysis for 30-day mortality.
test_set_thirtydM$'AutoScore Score'<- pred_score_thirtydM$pred_score
log_AutoScore_thirtydM <- glm(test_set_thirtydM$"label" ~ test_set_thirtydM$"AutoScore Score", test_set_thirtydM, family = binomial(link = "logit"))
test_set_thirtydM$'AutoScore_probs' <- predict(log_AutoScore_thirtydM, test_set_thirtydM, type = "response")
cc_AutoScore_thirtydM <- calibration_plot(data = test_set_thirtydM, obs = "label", pred = "AutoScore_probs", nTiles = 20)
ggsave(filename = "Figures/Calibration Plot (30-Day Mortality - AutoScore).pdf", plot = cc_AutoScore_thirtydM$calibration_plot, width = 5, height = 5, dpi = 300)

log_CRASH_thirtydM <- glm(test_set_thirtydM$"label" ~ test_set_thirtydM$"CRASH Score", test_set_thirtydM, family = binomial(link = "logit"))
test_set_thirtydM$'CRASH_probs' <- predict(log_CRASH_thirtydM, test_set_thirtydM, type = "response")
cc_CRASH_thirtydM <- calibration_plot(data = test_set_thirtydM, obs = "label", pred = "CRASH_probs")
ggsave(filename = "Figures/Calibration Plot (30-Day Mortality - CRASH).pdf", plot = cc_CRASH_thirtydM$calibration_plot, width = 5, height = 5, dpi = 300)

log_IMPACT_thirtydM <- glm(test_set_thirtydM$"label" ~ test_set_thirtydM$"IMPACT Score", test_set_thirtydM, family = binomial(link = "logit"))
test_set_thirtydM$'IMPACT_probs' <- predict(log_IMPACT_thirtydM, test_set_thirtydM, type = "response")
cc_IMPACT_thirtydM <- calibration_plot(data = test_set_thirtydM, obs = "label", pred = "IMPACT_probs")
ggsave(filename = "Figures/Calibration Plot (30-Day Mortality - IMPACT).pdf", plot = cc_IMPACT_thirtydM $calibration_plot, width = 5, height = 5, dpi = 300)

brier_AutoScore_thirtydM <- BrierScore(test_set_thirtydM$label, test_set_thirtydM$AutoScore_probs)
brier_AutoScore_thirtydM <- sprintf("%.5f", brier_AutoScore_thirtydM)
brier_CRASH_thirtydM <- BrierScore(test_set_thirtydM$label, test_set_thirtydM$CRASH_probs)
brier_CRASH_thirtydM <- sprintf("%.5f", brier_CRASH_thirtydM)
brier_IMPACT_thirtydM <- BrierScore(test_set_thirtydM$label, test_set_thirtydM$IMPACT_probs)
brier_IMPACT_thirtydM <- sprintf("%.5f", brier_IMPACT_thirtydM)


#Merge performance metrics.
perf_AutoScore_oM <- data.frame(Outcome = 'Overall Mortality', Model = 'AutoScore', AUROC = auc_AutoScore_oM, Accuracy = acc_AutoScore_oM, Sensitivity = sen_AutoScore_oM, Specificity = spe_AutoScore_oM, 'Brier Score' = brier_AutoScore_oM)
perf_CRASH_oM <- data.frame(Outcome = 'Overall Mortality', Model = 'CRASH', AUROC = auc_CRASH_oM, Accuracy = acc_CRASH_oM, Sensitivity = sen_CRASH_oM, Specificity = spe_CRASH_oM, 'Brier Score' = brier_IMPACT_oM)
perf_IMPACT_oM <- data.frame(Outcome = 'Overall Mortality', Model = 'IMPACT', AUROC = auc_IMPACT_oM, Accuracy = acc_IMPACT_oM, Sensitivity = sen_IMPACT_oM, Specificity = spe_IMPACT_oM, 'Brier Score' = brier_IMPACT_oM)

perf_AutoScore_threedM <- data.frame(Outcome = '3-Day Mortality', Model = 'AutoScore', AUROC = auc_AutoScore_threedM, Accuracy = acc_AutoScore_threedM, Sensitivity = sen_AutoScore_threedM, Specificity = spe_AutoScore_threedM, 'Brier Score' = brier_AutoScore_threedM)
perf_CRASH_threedM <- data.frame(Outcome = '3-Day Mortality', Model = 'CRASH', AUROC = auc_CRASH_threedM, Accuracy = acc_CRASH_threedM, Sensitivity = sen_CRASH_threedM, Specificity = spe_CRASH_threedM, 'Brier Score' = brier_IMPACT_threedM)
perf_IMPACT_threedM <- data.frame(Outcome = '3-Day Mortality', Model = 'IMPACT', AUROC = auc_IMPACT_threedM, Accuracy = acc_IMPACT_threedM, Sensitivity = sen_IMPACT_threedM, Specificity = spe_IMPACT_threedM, 'Brier Score' = brier_IMPACT_threedM)

perf_AutoScore_sevendM <- data.frame(Outcome = '7-Day Mortality', Model = 'AutoScore', AUROC = auc_AutoScore_sevendM, Accuracy = acc_AutoScore_sevendM, Sensitivity = sen_AutoScore_sevendM, Specificity = spe_AutoScore_sevendM, 'Brier Score' = brier_AutoScore_sevendM)
perf_CRASH_sevendM <- data.frame(Outcome = '7-Day Mortality', Model = 'CRASH', AUROC = auc_CRASH_sevendM, Accuracy = acc_CRASH_sevendM, Sensitivity = sen_CRASH_sevendM, Specificity = spe_CRASH_sevendM, 'Brier Score' = brier_IMPACT_sevendM)
perf_IMPACT_sevendM <- data.frame(Outcome = '7-Day Mortality', Model = 'IMPACT', AUROC = auc_IMPACT_sevendM, Accuracy = acc_IMPACT_sevendM, Sensitivity = sen_IMPACT_sevendM, Specificity = spe_IMPACT_sevendM, 'Brier Score' = brier_IMPACT_sevendM)

perf_AutoScore_fourteendM <- data.frame(Outcome = '14-Day Mortality', Model = 'AutoScore', AUROC = auc_AutoScore_fourteendM, Accuracy = acc_AutoScore_fourteendM, Sensitivity = sen_AutoScore_fourteendM, Specificity = spe_AutoScore_fourteendM, 'Brier Score' = brier_AutoScore_fourteendM)
perf_CRASH_fourteendM <- data.frame(Outcome = '14-Day Mortality', Model = 'CRASH', AUROC = auc_CRASH_fourteendM, Accuracy = acc_CRASH_fourteendM, Sensitivity = sen_CRASH_fourteendM, Specificity = spe_CRASH_fourteendM, 'Brier Score' = brier_IMPACT_fourteendM)
perf_IMPACT_fourteendM <- data.frame(Outcome = '14-Day Mortality', Model = 'IMPACT', AUROC = auc_IMPACT_fourteendM, Accuracy = acc_IMPACT_fourteendM, Sensitivity = sen_IMPACT_fourteendM, Specificity = spe_IMPACT_fourteendM, 'Brier Score' = brier_IMPACT_fourteendM)

perf_AutoScore_thirtydM <- data.frame(Outcome = '30-Day Mortality', Model = 'AutoScore', AUROC = auc_AutoScore_thirtydM, Accuracy = acc_AutoScore_thirtydM, Sensitivity = sen_AutoScore_thirtydM, Specificity = spe_AutoScore_thirtydM, 'Brier Score' = brier_AutoScore_thirtydM)
perf_CRASH_thirtydM <- data.frame(Outcome = '30-Day Mortality', Model = 'CRASH', AUROC = auc_CRASH_thirtydM, Accuracy = acc_CRASH_thirtydM, Sensitivity = sen_CRASH_thirtydM, Specificity = spe_CRASH_thirtydM, 'Brier Score' = brier_IMPACT_thirtydM)
perf_IMPACT_thirtydM <- data.frame(Outcome = '30-Day Mortality', Model = 'IMPACT', AUROC = auc_IMPACT_thirtydM, Accuracy = acc_IMPACT_thirtydM, Sensitivity = sen_IMPACT_thirtydM, Specificity = spe_IMPACT_thirtydM, 'Brier Score' = brier_IMPACT_thirtydM)

performance_metrics <- rbind(perf_AutoScore_oM, perf_CRASH_oM, perf_IMPACT_oM, perf_AutoScore_threedM, perf_CRASH_threedM, perf_IMPACT_threedM, perf_AutoScore_sevendM, perf_CRASH_sevendM, perf_IMPACT_sevendM, perf_AutoScore_fourteendM, perf_CRASH_fourteendM, perf_IMPACT_fourteendM, perf_AutoScore_thirtydM, perf_CRASH_thirtydM, perf_IMPACT_thirtydM)
write.csv(performance_metrics, "Results/Model Performances.csv")


#SENSITIVITY ANALYSIS (MILD TBI)


#Prepare data.
mildtbi_test_set_oM <- test_set_oM[test_set_oM$'TBI Severity'=='Mild',] 
mildtbi_test_set_threedM <- test_set_threedM[test_set_threedM$'TBI Severity'=='Mild',] 
mildtbi_test_set_sevendM <- test_set_oM[test_set_sevendM$'TBI Severity'=='Mild',] 
mildtbi_test_set_fourteendM <- test_set_oM[test_set_fourteendM$'TBI Severity'=='Mild',] 
mildtbi_test_set_thirtydM <- test_set_thirtydM[test_set_thirtydM$'TBI Severity'=='Mild',] 


#Calculate ROC curves (oM).
mildtbi_ROC_AutoScore_oM <- roc(mildtbi_test_set_oM$label, mildtbi_test_set_oM$"AutoScore Score")
mildtbi_ROC_CRASH_oM <- roc(mildtbi_test_set_oM$label, mildtbi_test_set_oM$"CRASH Score")
mildtbi_ROC_IMPACT_oM <- roc(mildtbi_test_set_oM$label, mildtbi_test_set_oM$"IMPACT Score")


#Calculate performance metrics (oM).
mildtbi_auc_AutoScore_oM <- unname(ci.auc(mildtbi_ROC_AutoScore_oM, conf.level=0.95)[c(2, 1, 3)])
mildtbi_auc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_auc_AutoScore_oM[1], mildtbi_auc_AutoScore_oM[2], mildtbi_auc_AutoScore_oM[3])
mildtbi_metrics_AutoScore_oM <- ci.coords(mildtbi_ROC_AutoScore_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
mildtbi_acc_AutoScore_oM <- mildtbi_metrics_AutoScore_oM$accuracy[c(2, 1, 3)]
mildtbi_acc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_acc_AutoScore_oM[1], mildtbi_acc_AutoScore_oM[2], mildtbi_acc_AutoScore_oM[3])
mildtbi_sen_AutoScore_oM <- mildtbi_metrics_AutoScore_oM$sensitivity[c(2, 1, 3)]
mildtbi_sen_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_sen_AutoScore_oM[1], mildtbi_sen_AutoScore_oM[2], mildtbi_sen_AutoScore_oM[3])
mildtbi_spe_AutoScore_oM <- mildtbi_metrics_AutoScore_oM$specificity[c(2, 1, 3)]
mildtbi_spe_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_spe_AutoScore_oM[1], mildtbi_spe_AutoScore_oM[2], mildtbi_spe_AutoScore_oM[3])

mildtbi_auc_CRASH_oM <- unname(ci.auc(mildtbi_ROC_CRASH_oM, conf.level=0.95)[c(2, 1, 3)])
mildtbi_auc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_auc_CRASH_oM[1], mildtbi_auc_CRASH_oM[2], mildtbi_auc_CRASH_oM[3])
mildtbi_metrics_CRASH_oM <- ci.coords(mildtbi_ROC_CRASH_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
mildtbi_acc_CRASH_oM <- mildtbi_metrics_CRASH_oM$accuracy[c(2, 1, 3)]
mildtbi_acc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_acc_CRASH_oM[1], mildtbi_acc_CRASH_oM[2], mildtbi_acc_CRASH_oM[3])
mildtbi_sen_CRASH_oM <- mildtbi_metrics_CRASH_oM$sensitivity[c(2, 1, 3)]
mildtbi_sen_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_sen_CRASH_oM[1], mildtbi_sen_CRASH_oM[2], mildtbi_sen_CRASH_oM[3])
mildtbi_spe_CRASH_oM <- mildtbi_metrics_CRASH_oM$specificity[c(2, 1, 3)]
mildtbi_spe_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_spe_CRASH_oM[1], mildtbi_spe_CRASH_oM[2], mildtbi_spe_CRASH_oM[3])

mildtbi_auc_IMPACT_oM <- unname(ci.auc(mildtbi_ROC_IMPACT_oM, conf.level=0.95)[c(2, 1, 3)])
mildtbi_auc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_auc_IMPACT_oM[1], mildtbi_auc_IMPACT_oM[2], mildtbi_auc_IMPACT_oM[3])
mildtbi_metrics_IMPACT_oM <- ci.coords(mildtbi_ROC_IMPACT_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
mildtbi_acc_IMPACT_oM <- mildtbi_metrics_IMPACT_oM$accuracy[c(2, 1, 3)]
mildtbi_acc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_acc_IMPACT_oM[1], mildtbi_acc_IMPACT_oM[2], mildtbi_acc_IMPACT_oM[3])
mildtbi_sen_IMPACT_oM <- mildtbi_metrics_IMPACT_oM$sensitivity[c(2, 1, 3)]
mildtbi_sen_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_sen_IMPACT_oM[1], mildtbi_sen_IMPACT_oM[2], mildtbi_sen_IMPACT_oM[3])
mildtbi_spe_IMPACT_oM <- mildtbi_metrics_IMPACT_oM$specificity[c(2, 1, 3)]
mildtbi_spe_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", mildtbi_spe_IMPACT_oM[1], mildtbi_spe_IMPACT_oM[2], mildtbi_spe_IMPACT_oM[3])


#Plot ROC Curves (oM).
pdf("Figures/ROCs (Sensitivity Analysis - Mild TBI).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(mildtbi_ROC_AutoScore_oM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(mildtbi_ROC_CRASH_oM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(mildtbi_ROC_IMPACT_oM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", mildtbi_ROC_AutoScore_oM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", mildtbi_ROC_CRASH_oM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", mildtbi_ROC_IMPACT_oM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()

#Merge performance metrics.
mildtbi_perf_AutoScore_oM <- data.frame(TBI_Severity = 'Mild', Model = 'AutoScore', AUROC = mildtbi_auc_AutoScore_oM, Accuracy = mildtbi_acc_AutoScore_oM, Sensitivity = mildtbi_sen_AutoScore_oM, Specificity = mildtbi_spe_AutoScore_oM)
mildtbi_perf_CRASH_oM <- data.frame(TBI_Severity = 'Mild', Model = 'CRASH', AUROC = mildtbi_auc_CRASH_oM, Accuracy = mildtbi_acc_CRASH_oM, Sensitivity = mildtbi_sen_CRASH_oM, Specificity = mildtbi_spe_CRASH_oM)
mildtbi_perf_IMPACT_oM <- data.frame(TBI_Severity = 'Mild', Model = 'IMPACT', AUROC = mildtbi_auc_IMPACT_oM, Accuracy = mildtbi_acc_IMPACT_oM, Sensitivity = mildtbi_sen_IMPACT_oM, Specificity = mildtbi_spe_IMPACT_oM)


#Compare for models for mild TBI.
mildtbi_oM_AS_vs_CRASH <- roc.test(mildtbi_ROC_AutoScore_oM, mildtbi_ROC_CRASH_oM, method="bootstrap")$p.value
mildtbi_oM_AS_vs_IMPACT <- roc.test(mildtbi_ROC_AutoScore_oM, mildtbi_ROC_IMPACT_oM, method="bootstrap")$p.value
mildtbi_oM_CRASH_vs_IMPACT <- roc.test(mildtbi_ROC_CRASH_oM, mildtbi_ROC_IMPACT_oM, method="bootstrap")$p.value
mildtbi_oM_comparison <- rbind(mildtbi_oM_AS_vs_CRASH, mildtbi_oM_AS_vs_IMPACT, mildtbi_oM_CRASH_vs_IMPACT)
row.names(mildtbi_oM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(mildtbi_oM_comparison) <- c("Mild TBI")
mildtbi_oM_comparison <- as.data.frame(mildtbi_oM_comparison)


#SENSITIVITY ANALYSIS (MODERATE TBI)


#Prepare data.
moderatetbi_test_set_oM <- test_set_oM[test_set_oM$'TBI Severity'=='Moderate',] 


#Calculate ROC curves (oM).
moderatetbi_ROC_AutoScore_oM <- roc(moderatetbi_test_set_oM$label, moderatetbi_test_set_oM$"AutoScore Score")
moderatetbi_ROC_CRASH_oM <- roc(moderatetbi_test_set_oM$label, moderatetbi_test_set_oM$"CRASH Score")
moderatetbi_ROC_IMPACT_oM <- roc(moderatetbi_test_set_oM$label, moderatetbi_test_set_oM$"IMPACT Score")


#Calculate performance metrics (oM).
moderatetbi_auc_AutoScore_oM <- unname(ci.auc(moderatetbi_ROC_AutoScore_oM, conf.level=0.95)[c(2, 1, 3)])
moderatetbi_auc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_auc_AutoScore_oM[1], moderatetbi_auc_AutoScore_oM[2], moderatetbi_auc_AutoScore_oM[3])
moderatetbi_metrics_AutoScore_oM <- ci.coords(moderatetbi_ROC_AutoScore_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
moderatetbi_acc_AutoScore_oM <- moderatetbi_metrics_AutoScore_oM$accuracy[c(2, 1, 3)]
moderatetbi_acc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_acc_AutoScore_oM[1], moderatetbi_acc_AutoScore_oM[2], moderatetbi_acc_AutoScore_oM[3])
moderatetbi_sen_AutoScore_oM <- moderatetbi_metrics_AutoScore_oM$sensitivity[c(2, 1, 3)]
moderatetbi_sen_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_sen_AutoScore_oM[1], moderatetbi_sen_AutoScore_oM[2], moderatetbi_sen_AutoScore_oM[3])
moderatetbi_spe_AutoScore_oM <- moderatetbi_metrics_AutoScore_oM$specificity[c(2, 1, 3)]
moderatetbi_spe_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_spe_AutoScore_oM[1], moderatetbi_spe_AutoScore_oM[2], moderatetbi_spe_AutoScore_oM[3])

moderatetbi_auc_CRASH_oM <- unname(ci.auc(moderatetbi_ROC_CRASH_oM, conf.level=0.95)[c(2, 1, 3)])
moderatetbi_auc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_auc_CRASH_oM[1], moderatetbi_auc_CRASH_oM[2], moderatetbi_auc_CRASH_oM[3])
moderatetbi_metrics_CRASH_oM <- ci.coords(moderatetbi_ROC_CRASH_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
moderatetbi_acc_CRASH_oM <- moderatetbi_metrics_CRASH_oM$accuracy[c(2, 1, 3)]
moderatetbi_acc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_acc_CRASH_oM[1], moderatetbi_acc_CRASH_oM[2], moderatetbi_acc_CRASH_oM[3])
moderatetbi_sen_CRASH_oM <- moderatetbi_metrics_CRASH_oM$sensitivity[c(2, 1, 3)]
moderatetbi_sen_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_sen_CRASH_oM[1], moderatetbi_sen_CRASH_oM[2], moderatetbi_sen_CRASH_oM[3])
moderatetbi_spe_CRASH_oM <- moderatetbi_metrics_CRASH_oM$specificity[c(2, 1, 3)]
moderatetbi_spe_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_spe_CRASH_oM[1], moderatetbi_spe_CRASH_oM[2], moderatetbi_spe_CRASH_oM[3])

moderatetbi_auc_IMPACT_oM <- unname(ci.auc(moderatetbi_ROC_IMPACT_oM, conf.level=0.95)[c(2, 1, 3)])
moderatetbi_auc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_auc_IMPACT_oM[1], moderatetbi_auc_IMPACT_oM[2], moderatetbi_auc_IMPACT_oM[3])
moderatetbi_metrics_IMPACT_oM <- ci.coords(moderatetbi_ROC_IMPACT_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
moderatetbi_acc_IMPACT_oM <- moderatetbi_metrics_IMPACT_oM$accuracy[c(2, 1, 3)]
moderatetbi_acc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_acc_IMPACT_oM[1], moderatetbi_acc_IMPACT_oM[2], moderatetbi_acc_IMPACT_oM[3])
moderatetbi_sen_IMPACT_oM <- moderatetbi_metrics_IMPACT_oM$sensitivity[c(2, 1, 3)]
moderatetbi_sen_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_sen_IMPACT_oM[1], moderatetbi_sen_IMPACT_oM[2], moderatetbi_sen_IMPACT_oM[3])
moderatetbi_spe_IMPACT_oM <- moderatetbi_metrics_IMPACT_oM$specificity[c(2, 1, 3)]
moderatetbi_spe_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", moderatetbi_spe_IMPACT_oM[1], moderatetbi_spe_IMPACT_oM[2], moderatetbi_spe_IMPACT_oM[3])


#Plot ROC Curves (oM).
pdf("Figures/ROCs (Sensitivity Analysis - Moderate TBI).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(moderatetbi_ROC_AutoScore_oM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(moderatetbi_ROC_CRASH_oM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(moderatetbi_ROC_IMPACT_oM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", moderatetbi_ROC_AutoScore_oM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", moderatetbi_ROC_CRASH_oM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", moderatetbi_ROC_IMPACT_oM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Merge performance metrics.
moderatetbi_perf_AutoScore_oM <- data.frame(TBI_Severity = 'Moderate', Model = 'AutoScore', AUROC = moderatetbi_auc_AutoScore_oM, Accuracy = moderatetbi_acc_AutoScore_oM, Sensitivity = moderatetbi_sen_AutoScore_oM, Specificity = moderatetbi_spe_AutoScore_oM)
moderatetbi_perf_CRASH_oM <- data.frame(TBI_Severity = 'Moderate', Model = 'CRASH', AUROC = moderatetbi_auc_CRASH_oM, Accuracy = moderatetbi_acc_CRASH_oM, Sensitivity = moderatetbi_sen_CRASH_oM, Specificity = moderatetbi_spe_CRASH_oM)
moderatetbi_perf_IMPACT_oM <- data.frame(TBI_Severity = 'Moderate', Model = 'IMPACT', AUROC = moderatetbi_auc_IMPACT_oM, Accuracy = moderatetbi_acc_IMPACT_oM, Sensitivity = moderatetbi_sen_IMPACT_oM, Specificity = moderatetbi_spe_IMPACT_oM)


#Compare for models for moderate TBI.
moderatetbi_oM_AS_vs_CRASH <- roc.test(moderatetbi_ROC_AutoScore_oM, moderatetbi_ROC_CRASH_oM, method="bootstrap")$p.value
moderatetbi_oM_AS_vs_IMPACT <- roc.test(moderatetbi_ROC_AutoScore_oM, moderatetbi_ROC_IMPACT_oM, method="bootstrap")$p.value
moderatetbi_oM_CRASH_vs_IMPACT <- roc.test(moderatetbi_ROC_CRASH_oM, moderatetbi_ROC_IMPACT_oM, method="bootstrap")$p.value
moderatetbi_oM_comparison <- rbind(moderatetbi_oM_AS_vs_CRASH, moderatetbi_oM_AS_vs_IMPACT, moderatetbi_oM_CRASH_vs_IMPACT)
row.names(moderatetbi_oM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(moderatetbi_oM_comparison) <- c("Moderate TBI")
moderatetbi_oM_comparison <- as.data.frame(moderatetbi_oM_comparison)


#SENSITIVITY ANALYSIS (SEVERE TBI)


#Prepare data.
severetbi_test_set_oM <- test_set_oM[test_set_oM$'TBI Severity'=='Severe',] 


#Calculate ROC curves (oM).
severetbi_ROC_AutoScore_oM <- roc(severetbi_test_set_oM$label, severetbi_test_set_oM$"AutoScore Score")
severetbi_ROC_CRASH_oM <- roc(severetbi_test_set_oM$label, severetbi_test_set_oM$"CRASH Score")
severetbi_ROC_IMPACT_oM <- roc(severetbi_test_set_oM$label, severetbi_test_set_oM$"IMPACT Score")


#Calculate performance metrics (oM).
severetbi_auc_AutoScore_oM <- unname(ci.auc(severetbi_ROC_AutoScore_oM, conf.level=0.95)[c(2, 1, 3)])
severetbi_auc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_auc_AutoScore_oM[1], severetbi_auc_AutoScore_oM[2], severetbi_auc_AutoScore_oM[3])
severetbi_metrics_AutoScore_oM <- ci.coords(severetbi_ROC_AutoScore_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
severetbi_acc_AutoScore_oM <- severetbi_metrics_AutoScore_oM$accuracy[c(2, 1, 3)]
severetbi_acc_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_acc_AutoScore_oM[1], severetbi_acc_AutoScore_oM[2], severetbi_acc_AutoScore_oM[3])
severetbi_sen_AutoScore_oM <- severetbi_metrics_AutoScore_oM$sensitivity[c(2, 1, 3)]
severetbi_sen_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_sen_AutoScore_oM[1], severetbi_sen_AutoScore_oM[2], severetbi_sen_AutoScore_oM[3])
severetbi_spe_AutoScore_oM <- severetbi_metrics_AutoScore_oM$specificity[c(2, 1, 3)]
severetbi_spe_AutoScore_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_spe_AutoScore_oM[1], severetbi_spe_AutoScore_oM[2], severetbi_spe_AutoScore_oM[3])

severetbi_auc_CRASH_oM <- unname(ci.auc(severetbi_ROC_CRASH_oM, conf.level=0.95)[c(2, 1, 3)])
severetbi_auc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_auc_CRASH_oM[1], severetbi_auc_CRASH_oM[2], severetbi_auc_CRASH_oM[3])
severetbi_metrics_CRASH_oM <- ci.coords(severetbi_ROC_CRASH_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
severetbi_acc_CRASH_oM <- severetbi_metrics_CRASH_oM$accuracy[c(2, 1, 3)]
severetbi_acc_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_acc_CRASH_oM[1], severetbi_acc_CRASH_oM[2], severetbi_acc_CRASH_oM[3])
severetbi_sen_CRASH_oM <- severetbi_metrics_CRASH_oM$sensitivity[c(2, 1, 3)]
severetbi_sen_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_sen_CRASH_oM[1], severetbi_sen_CRASH_oM[2], severetbi_sen_CRASH_oM[3])
severetbi_spe_CRASH_oM <- severetbi_metrics_CRASH_oM$specificity[c(2, 1, 3)]
severetbi_spe_CRASH_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_spe_CRASH_oM[1], severetbi_spe_CRASH_oM[2], severetbi_spe_CRASH_oM[3])

severetbi_auc_IMPACT_oM <- unname(ci.auc(severetbi_ROC_IMPACT_oM, conf.level=0.95)[c(2, 1, 3)])
severetbi_auc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_auc_IMPACT_oM[1], severetbi_auc_IMPACT_oM[2], severetbi_auc_IMPACT_oM[3])
severetbi_metrics_IMPACT_oM <- ci.coords(severetbi_ROC_IMPACT_oM, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
severetbi_acc_IMPACT_oM <- severetbi_metrics_IMPACT_oM$accuracy[c(2, 1, 3)]
severetbi_acc_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_acc_IMPACT_oM[1], severetbi_acc_IMPACT_oM[2], severetbi_acc_IMPACT_oM[3])
severetbi_sen_IMPACT_oM <- severetbi_metrics_IMPACT_oM$sensitivity[c(2, 1, 3)]
severetbi_sen_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_sen_IMPACT_oM[1], severetbi_sen_IMPACT_oM[2], severetbi_sen_IMPACT_oM[3])
severetbi_spe_IMPACT_oM <- severetbi_metrics_IMPACT_oM$specificity[c(2, 1, 3)]
severetbi_spe_IMPACT_oM <- sprintf("%.3f (%.3f - %.3f)", severetbi_spe_IMPACT_oM[1], severetbi_spe_IMPACT_oM[2], severetbi_spe_IMPACT_oM[3])


#Plot ROC Curves (oM).
pdf("Figures/ROCs (Sensitivity Analysis - Severe TBI).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(severetbi_ROC_AutoScore_oM, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(severetbi_ROC_CRASH_oM, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
lines(severetbi_ROC_IMPACT_oM, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", severetbi_ROC_AutoScore_oM$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.17, paste("CRASH AUROC =", sprintf("%.3f", severetbi_ROC_CRASH_oM$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", severetbi_ROC_IMPACT_oM$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Merge performance metrics.
severetbi_perf_AutoScore_oM <- data.frame(TBI_Severity = 'Severe', Model = 'AutoScore', AUROC = severetbi_auc_AutoScore_oM, Accuracy = severetbi_acc_AutoScore_oM, Sensitivity = severetbi_sen_AutoScore_oM, Specificity = severetbi_spe_AutoScore_oM)
severetbi_perf_CRASH_oM <- data.frame(TBI_Severity = 'Severe', Model = 'CRASH', AUROC = severetbi_auc_CRASH_oM, Accuracy = severetbi_acc_CRASH_oM, Sensitivity = severetbi_sen_CRASH_oM, Specificity = severetbi_spe_CRASH_oM)
severetbi_perf_IMPACT_oM <- data.frame(TBI_Severity = 'Severe', Model = 'IMPACT', AUROC = severetbi_auc_IMPACT_oM, Accuracy = severetbi_acc_IMPACT_oM, Sensitivity = severetbi_sen_IMPACT_oM, Specificity = severetbi_spe_IMPACT_oM)


#Compare for models for severe TBI.
severetbi_oM_AS_vs_CRASH <- roc.test(severetbi_ROC_AutoScore_oM, severetbi_ROC_CRASH_oM, method="bootstrap")$p.value
severetbi_oM_AS_vs_IMPACT <- roc.test(severetbi_ROC_AutoScore_oM, severetbi_ROC_IMPACT_oM, method="bootstrap")$p.value
severetbi_oM_CRASH_vs_IMPACT <- roc.test(severetbi_ROC_CRASH_oM, severetbi_ROC_IMPACT_oM, method="bootstrap")$p.value
severetbi_oM_comparison <- rbind(severetbi_oM_AS_vs_CRASH, severetbi_oM_AS_vs_IMPACT, severetbi_oM_CRASH_vs_IMPACT)
row.names(severetbi_oM_comparison) <- c("AutoScore vs. CRASH", "AutoScore vs. IMPACT", "CRASH vs. IMPACT")
colnames(severetbi_oM_comparison) <- c("Severe TBI")
severetbi_oM_comparison <- as.data.frame(severetbi_oM_comparison)


#Merge performance metrics from TBI severity sensitivity analyses.
performance_metrics <- rbind(mildtbi_perf_AutoScore_oM, mildtbi_perf_CRASH_oM, mildtbi_perf_IMPACT_oM, moderatetbi_perf_AutoScore_oM, moderatetbi_perf_CRASH_oM, moderatetbi_perf_IMPACT_oM, severetbi_perf_AutoScore_oM, severetbi_perf_CRASH_oM, severetbi_perf_IMPACT_oM)
write.csv(performance_metrics, "Results/Model Performances (Sensitivity Analysis - TBI Severity).csv")


#Merge comparisons from TBI severity sensitivity analyses.
tbiseverity_among_comparison <- cbind(mildtbi_oM_comparison, moderatetbi_oM_comparison, severetbi_oM_comparison)
write.csv(tbiseverity_among_comparison, "Results/Model Comparison (Sensitivity Analysis - TBI Severity.csv")


#SENSITIVITY ANALYSIS (CRASH)

#Prepare data.
data_sa_CRASH <- read_csv("Data/final_data_crash.csv")
class(data_sa_CRASH) <- "data.frame"

data_sa_CRASH$"GCS - Motor"  <- as.factor(data_sa_CRASH$"GCS - Motor")
data_sa_CRASH$"GCS - Verbal"  <- as.factor(data_sa_CRASH$"GCS - Verbal")
data_sa_CRASH$"GCS - Eye"  <- as.factor(data_sa_CRASH$"GCS - Eye")
data_sa_CRASH$"Pupillary Response"  <- as.factor(data_sa_CRASH$"Pupillary Response")

test_set_sa_CRASH <- data_sa_CRASH[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "CRASH Score", "label")]
check_data(test_set_sa_CRASH)


#Testing for sensitivity analysis (CRASH).
pred_score_sa_CRASH <- AutoScore_testing(
  test_set = test_set_sa_CRASH, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE, metrics_ci = TRUE
)

pred_score_sa_CRASH$Label <- as.factor(pred_score_sa_CRASH$Label)

test_set_sa_CRASH$'AutoScore Score'<- pred_score_sa_CRASH$pred_score


#Calculate ROC curves for sensitivity analysis (CRASH).
ROC_AutoScore_sa_CRASH <- roc(test_set_sa_CRASH$label, test_set_sa_CRASH$"AutoScore Score")
ROC_CRASH_sa_CRASH <- roc(test_set_sa_CRASH$label, test_set_sa_CRASH$"CRASH Score")


#Calculate performance metrics for sensitivity analysis (CRASH).
auc_AutoScore_sa_CRASH <- unname(ci.auc(ROC_AutoScore_sa_CRASH, conf.level=0.95)[c(2, 1, 3)])
auc_AutoScore_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", auc_AutoScore_sa_CRASH[1], auc_AutoScore_sa_CRASH[2], auc_AutoScore_sa_CRASH[3])
metrics_AutoScore_sa_CRASH <- ci.coords(ROC_AutoScore_sa_CRASH, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_AutoScore_sa_CRASH <- metrics_AutoScore_sa_CRASH$accuracy[c(2, 1, 3)]
acc_AutoScore_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", acc_AutoScore_sa_CRASH[1], acc_AutoScore_sa_CRASH[2], acc_AutoScore_sa_CRASH[3])
sen_AutoScore_sa_CRASH <- metrics_AutoScore_sa_CRASH$sensitivity[c(2, 1, 3)]
sen_AutoScore_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", sen_AutoScore_sa_CRASH[1], sen_AutoScore_sa_CRASH[2], sen_AutoScore_sa_CRASH[3])
spe_AutoScore_sa_CRASH <- metrics_AutoScore_sa_CRASH$specificity[c(2, 1, 3)]
spe_AutoScore_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", spe_AutoScore_sa_CRASH[1], spe_AutoScore_sa_CRASH[2], spe_AutoScore_sa_CRASH[3])

auc_CRASH_sa_CRASH <- unname(ci.auc(ROC_CRASH_sa_CRASH, conf.level=0.95)[c(2, 1, 3)])
auc_CRASH_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", auc_CRASH_sa_CRASH[1], auc_CRASH_sa_CRASH[2], auc_CRASH_sa_CRASH[3])
metrics_CRASH_sa_CRASH <- ci.coords(ROC_CRASH_sa_CRASH, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_CRASH_sa_CRASH <- metrics_CRASH_sa_CRASH$accuracy[c(2, 1, 3)]
acc_CRASH_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", acc_CRASH_sa_CRASH[1], acc_CRASH_sa_CRASH[2], acc_CRASH_sa_CRASH[3])
sen_CRASH_sa_CRASH <- metrics_CRASH_sa_CRASH$sensitivity[c(2, 1, 3)]
sen_CRASH_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", sen_CRASH_sa_CRASH[1], sen_CRASH_sa_CRASH[2], sen_CRASH_sa_CRASH[3])
spe_CRASH_sa_CRASH <- metrics_CRASH_sa_CRASH$specificity[c(2, 1, 3)]
spe_CRASH_sa_CRASH <- sprintf("%.3f (%.3f - %.3f)", spe_CRASH_sa_CRASH[1], spe_CRASH_sa_CRASH[2], spe_CRASH_sa_CRASH[3])


#Plot ROC Curves for sensitivity analysis (CRASH).
pdf("Figures/ROCs (Sensitivity Analysis - CRASH).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(ROC_AutoScore_sa_CRASH, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(ROC_CRASH_sa_CRASH, col = rgb(red=1, green=0, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", ROC_AutoScore_sa_CRASH$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("CRASH AUROC =", sprintf("%.3f", ROC_CRASH_sa_CRASH$auc)), col = "red", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Compare for models for sensitivity analysis (CRASH).
AS_vs_CRASH_sa_CRASH <- roc.test(ROC_AutoScore_sa_CRASH, ROC_CRASH_sa_CRASH, method="bootstrap")$p.value
AS_vs_CRASH_sa_CRASH <- as.data.frame(AS_vs_CRASH_sa_CRASH)


#SENSITIVITY ANALYSIS (IMPACT)

#Prepare data.
data_sa_IMPACT <- read_csv("Data/final_data_impact.csv")
class(data_sa_IMPACT) <- "data.frame"

data_sa_IMPACT$"GCS - Motor"  <- as.factor(data_sa_IMPACT$"GCS - Motor")
data_sa_IMPACT$"GCS - Verbal"  <- as.factor(data_sa_IMPACT$"GCS - Verbal")
data_sa_IMPACT$"GCS - Eye"  <- as.factor(data_sa_IMPACT$"GCS - Eye")
data_sa_IMPACT$"Pupillary Response"  <- as.factor(data_sa_IMPACT$"Pupillary Response")

test_set_sa_IMPACT <- data_sa_IMPACT[c("Age", "GCS - Motor", "GCS - Verbal", "GCS - Eye", "Pupillary Response", "IMPACT Score", "label")]
check_data(test_set_sa_IMPACT)


#Testing for sensitivity analysis (IMPACT).
pred_score_sa_IMPACT <- AutoScore_testing(
  test_set = test_set_sa_IMPACT, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE, metrics_ci = TRUE
)

pred_score_sa_IMPACT$Label <- as.factor(pred_score_sa_IMPACT$Label)

test_set_sa_IMPACT$'AutoScore Score'<- pred_score_sa_IMPACT$pred_score


#Calculate ROC curves for sensitivity analysis (IMPACT).
ROC_AutoScore_sa_IMPACT <- roc(test_set_sa_IMPACT$label, test_set_sa_IMPACT$"AutoScore Score")
ROC_IMPACT_sa_IMPACT <- roc(test_set_sa_IMPACT$label, test_set_sa_IMPACT$"IMPACT Score")


#Calculate performance metrics for sensitivity analysis (IMPACT).
auc_AutoScore_sa_IMPACT <- unname(ci.auc(ROC_AutoScore_sa_IMPACT, conf.level=0.95)[c(2, 1, 3)])
auc_AutoScore_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", auc_AutoScore_sa_IMPACT[1], auc_AutoScore_sa_IMPACT[2], auc_AutoScore_sa_IMPACT[3])
metrics_AutoScore_sa_IMPACT <- ci.coords(ROC_AutoScore_sa_IMPACT, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_AutoScore_sa_IMPACT <- metrics_AutoScore_sa_IMPACT$accuracy[c(2, 1, 3)]
acc_AutoScore_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", acc_AutoScore_sa_IMPACT[1], acc_AutoScore_sa_IMPACT[2], acc_AutoScore_sa_IMPACT[3])
sen_AutoScore_sa_IMPACT <- metrics_AutoScore_sa_IMPACT$sensitivity[c(2, 1, 3)]
sen_AutoScore_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", sen_AutoScore_sa_IMPACT[1], sen_AutoScore_sa_IMPACT[2], sen_AutoScore_sa_IMPACT[3])
spe_AutoScore_sa_IMPACT <- metrics_AutoScore_sa_IMPACT$specificity[c(2, 1, 3)]
spe_AutoScore_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", spe_AutoScore_sa_IMPACT[1], spe_AutoScore_sa_IMPACT[2], spe_AutoScore_sa_IMPACT[3])

auc_IMPACT_sa_IMPACT <- unname(ci.auc(ROC_IMPACT_sa_IMPACT, conf.level=0.95)[c(2, 1, 3)])
auc_IMPACT_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", auc_IMPACT_sa_IMPACT[1], auc_IMPACT_sa_IMPACT[2], auc_IMPACT_sa_IMPACT[3])
metrics_IMPACT_sa_IMPACT <- ci.coords(ROC_IMPACT_sa_IMPACT, x = "best", transpose = FALSE, input = "threshold", ret = perfmet)
acc_IMPACT_sa_IMPACT <- metrics_IMPACT_sa_IMPACT$accuracy[c(2, 1, 3)]
acc_IMPACT_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", acc_IMPACT_sa_IMPACT[1], acc_IMPACT_sa_IMPACT[2], acc_IMPACT_sa_IMPACT[3])
sen_IMPACT_sa_IMPACT <- metrics_IMPACT_sa_IMPACT$sensitivity[c(2, 1, 3)]
sen_IMPACT_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", sen_IMPACT_sa_IMPACT[1], sen_IMPACT_sa_IMPACT[2], sen_IMPACT_sa_IMPACT[3])
spe_IMPACT_sa_IMPACT <- metrics_IMPACT_sa_IMPACT$specificity[c(2, 1, 3)]
spe_IMPACT_sa_IMPACT <- sprintf("%.3f (%.3f - %.3f)", spe_IMPACT_sa_IMPACT[1], spe_IMPACT_sa_IMPACT[2], spe_IMPACT_sa_IMPACT[3])


#Plot ROC Curves for sensitivity analysis (IMPACT).
pdf("Figures/ROCs (Sensitivity Analysis - IMPACT).pdf", width = 9, height = 9)
par(font.lab=2, xaxs = "i", yaxs = "i", cex.axis=1.5, cex.lab=1.5)
plot(1, type = "n", xlab = "Specificity", ylab = "Sensitivity", xlim = c(0,1), ylim = c(0,1))
lines(ROC_AutoScore_sa_IMPACT, col = rgb(red=0, green=0, blue=1, alpha=0.8), lwd=4.0)
lines(ROC_IMPACT_sa_IMPACT, col = rgb(red=0, green=1, blue=0, alpha=0.8), lwd=4.0)
legend(x = 0.01, y = 0.08, paste("The MOST AUROC =", sprintf("%.3f", ROC_AutoScore_sa_IMPACT$auc)), col = "blue", lwd=4.0, bty = "n", cex=1.5, text.font=1)
legend(x = 0.01, y = 0.125, paste("IMPACT AUROC =", sprintf("%.3f", ROC_IMPACT_sa_IMPACT$auc)), col = "green", lwd=4.0, bty = "n", cex=1.5, text.font=1)
dev.off()


#Compare for models for sensitivity analysis (IMPACT).
AS_vs_IMPACT_sa_IMPACT <- roc.test(ROC_AutoScore_sa_IMPACT, ROC_IMPACT_sa_IMPACT, method="bootstrap")$p.value
AS_vs_IMPACT_sa_IMPACT <- as.data.frame(AS_vs_IMPACT_sa_IMPACT)


#Merge performance metrics for sensitivity analyses.
perf_AutoScore_sa_CRASH <- data.frame(Outcome = '14-Day Mortality', Model = 'AutoScore', AUROC = auc_AutoScore_sa_CRASH, Accuracy = acc_AutoScore_sa_CRASH, Sensitivity = sen_AutoScore_sa_CRASH, Specificity = spe_AutoScore_sa_CRASH)
perf_CRASH_sa_CRASH <- data.frame(Outcome = '14-Day Mortality', Model = 'CRASH', AUROC = auc_CRASH_sa_CRASH, Accuracy = acc_CRASH_sa_CRASH, Sensitivity = sen_CRASH_sa_CRASH, Specificity = spe_CRASH_sa_CRASH)

perf_AutoScore_sa_IMPACT <- data.frame(Outcome = 'Overall Mortality', Model = 'AutoScore', AUROC = auc_AutoScore_sa_IMPACT, Accuracy = acc_AutoScore_sa_IMPACT, Sensitivity = sen_AutoScore_sa_IMPACT, Specificity = spe_AutoScore_sa_IMPACT)
perf_IMPACT_sa_IMPACT <- data.frame(Outcome = 'Overall Mortality', Model = 'IMPACT', AUROC = auc_IMPACT_sa_IMPACT, Accuracy = acc_IMPACT_sa_IMPACT, Sensitivity = sen_IMPACT_sa_IMPACT, Specificity = spe_IMPACT_sa_IMPACT)

performance_metrics <- rbind(perf_AutoScore_sa_CRASH, perf_CRASH_sa_CRASH, perf_AutoScore_sa_IMPACT, perf_IMPACT_sa_IMPACT)
write.csv(performance_metrics, "Results/Model Performances (Sensitivity Analysis - CRASH-IMPACT).csv")


#Merge comparisons from TBI severity sensitivity analyses.
sa_among_comparison <- cbind(AS_vs_CRASH_sa_CRASH, AS_vs_IMPACT_sa_IMPACT)
write.csv(sa_among_comparison, "Results/Model Comparison (Sensitivity Analysis - CRASH-IMPACT).csv")