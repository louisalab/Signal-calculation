#Load training dataset and prepare data for logistic regression
data <- readRDS("training_data.rds")
dada1<-data[,c(colnames(data)[1:10],Uni_log_sig_drugs)]

dada1$AGE[as.numeric(dada1$AGE)>0&as.numeric(as.numeric(dada1$AGE))<=20]="20"
dada1$AGE[as.numeric(dada1$AGE)>20&as.numeric(dada1$AGE)<=30]="20_30"
dada1$AGE[as.numeric(dada1$AGE)>30&as.numeric(dada1$AGE)<=40]="30_40"
dada1$AGE[as.numeric(dada1$AGE)>40&as.numeric(dada1$AGE)<=50]="40_50"
dada1$AGE[as.numeric(dada1$AGE)>50]="50"

dada1$WT[as.numeric(dada1$WT)>0&as.numeric(dada1$WT)<=50]="50"
dada1$WT[as.numeric(dada1$WT)>50&as.numeric(dada1$WT)<=75]="50_75"
dada1$WT[as.numeric(dada1$WT)>75&as.numeric(dada1$WT)<=100]="75_100"
dada1$WT[as.numeric(dada1$WT)>100&as.numeric(dada1$WT)<=125]="100_125"
dada1$WT[as.numeric(dada1$WT)>125&as.numeric(dada1$WT)<=150]="125_150"
dada1$WT[as.numeric(dada1$WT)>150]="150"

# Build formula
predictor_vars<-c("AGE","WT",Uni_log_sig_drugs)
full_formula <- as.formula(paste("Y", "~", paste(predictor_vars, collapse = " + ")))

# Fit multiple logisticregression model
fitMul <- glm(full_formula, data = dada1, family = binomial()) 

# Summarize results
fitSum <- summary(fitMul)
muti_log <- cbind(fitSum$coef, exp(fitSum$coef[, 'Estimate']), exp(confint(fitMul)))
muti_log <- data.frame(muti_log)
muti_log <- data.frame(Variables = rownames(muti_log), muti_log)
colnames(muti_log)<-c("Variables","Estimate","Std.Error","z.value","Pr(>|z|)","OR","2.5%","97.5%")

# Apply Bonferroni correction for multiple testing
muti_log$p_adjust<-(muti_log$`Pr(>|z|)`)*(nrow(muti_log)-1)

# Save results to CSV file
write.csv(muti_log,file = "muti_log.csv")

# Evaluate model performance using ROC curve(training set)
# Load the package
library(pROC)
library(rmda)
library(ggsci)

# Predict probabilities on training data
predvalue<-predict(fitMul,type = "response",newdata = dada1)
ROC <- roc(dada1$Y,predvalue)

# Plot ROC curve and save asPDF
pdf("training_ROC.pdf",width=5,height=5)
plot(ROC,
     print.auc=TRUE,
     print.auc.x=0.5,
     print.auc.y=0.5,
     auc.polygon=TRUE,
     auc.polygon.col="#FF9900FF",
     grid=FALSE,
     legacy.axes=TRUE)
dev.off()

# Validation phase: apply model to validation dataset
data2 <- readRDS("validation_data.rds")

data2$AGE1[as.numeric(data2$AGE1)>0&as.numeric(as.numeric(data2$AGE1))<=20]="20"
data2$AGE1[as.numeric(data2$AGE1)>20&as.numeric(data2$AGE1)<=30]="20_30"
data2$AGE1[as.numeric(data2$AGE1)>30&as.numeric(data2$AGE1)<=40]="30_40"
data2$AGE1[as.numeric(data2$AGE1)>40&as.numeric(data2$AGE1)<=50]="40_50"
data2$AGE1[as.numeric(data2$AGE1)>50]="50"

data2$WT1[as.numeric(data2$WT1)>0&as.numeric(data2$WT1)<=50]="50"
data2$WT1[as.numeric(data2$WT1)>50&as.numeric(data2$WT1)<=75]="50_75"
data2$WT1[as.numeric(data2$WT1)>75&as.numeric(data2$WT1)<=100]="75_100"
data2$WT1[as.numeric(data2$WT1)>100&as.numeric(data2$WT1)<=125]="100_125"
data2$WT1[as.numeric(data2$WT1)>125&as.numeric(data2$WT1)<=150]="125_150"
data2$WT1[as.numeric(data2$WT1)>150]="150"

# Predict probabilitieson validation set
predvalue<-predict(fitMul,type = "response",newdata = data2)

# Compute ROC and AUC for validation data
ROC <- roc(data2$Y,predvalue)

# Plot ROC curve fortest (validation) set
pdf("test_ROC.pdf",width=5,height=5)
plot(ROC,
     print.auc=TRUE,
     print.auc.x=0.5,
     print.auc.y=0.5,
     auc.polygon=TRUE,
     auc.polygon.col="#79CC3DFF",
     grid=FALSE,
     legacy.axes=TRUE)
dev.off()


