# Load dataset from RDS file
data <- readRDS("unicox_data.rds")

# Initialize an empty result container
Result<-c()

# Univariate logistic regression analysis of drugs with positive signals
for (i in 1:length(positive_signal_drugs)){
  fit<-glm(substitute(Y~x,list(x=as.name(positive_signal_drugs[i]))),data=data,family=binomial())
  fitSum<-summary(fit)
  result1<-c()
  result1<-rbind(result1,fitSum$coef)
  OR<-exp(fitSum$coef[,'Estimate'])
  result1<-data.frame(cbind(result1,cbind(OR,exp(confint(fit)))))
  result1$Characteristics<-drug_names[i]   
  Result<-rbind(Result,result1[-1,])
  print(i)
}

# Extract relevant columns: Estimate, P value, OR, and Confidence Intervals
Uni_log<-data.frame(Result[,c(1,4:8)]) 
colnames(Uni_log)[2:5]<-c("P","OR","CIlower","CIupper")
Uni_log_sig<-Uni_log[Uni_log$P<0.05,]

# Apply simple Bonferronicorrection for multiple testing
Uni_log_sig$p_adjust<-(Uni_log_sig$P)*nrow(Uni_log_sig)

# Save the significant results to a CSV file
write.csv(Uni_log_sig,file = "Uni_log_sig.csv")