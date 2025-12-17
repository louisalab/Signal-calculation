# Load the package
library(data.table)

#Read the RDS fileinto
df <- readRDS("data.rds")

#Calculate ROR, PRR, EBGM, and BCPNN
df[,ROR:=((a*d)/(b*c))]
df[,RORL:=exp(log(ROR)-1.96*sqrt(1/a+1/b+1/c+1/d))]
df[,RORU:=exp(log(ROR)+1.96*sqrt(1/a+1/b+1/c+1/d))]
df[,PRR:=(a/(a+b))/(c/(c+d))]
df[,XX:=((a*d-b*c)*(a*d-b*c)*(a+b+c+d))/((a+b)*(c+d)*(a+c)*(b+d))]
df[,EBGM:=(a*(a+b+c+d))/((a+b)*(a+c))]
df[,EBGM05:=exp(log(EBGM)-1.64*(sqrt(1/a+1/b+1/c+1/d)))]
df[,IC2:=log2((a*(a+b+c+d))/((a+c)*(a+b)))]
df[,GMAE:=((a+b+c+d+2)*(a+b+c+d+2))/((a+b+1)*(a+c+1))]
df[,EIC:=log2(((a+1)*(a+b+c+d+2)*(a+b+c+d+2))/((a+b+c+d+GMAE)*(a+b+1)*(a+c+1)))]
df[,VIC:=(1/(log(2)))*(1/(log(2)))*((a+b+c+d-3+GMAE)/(3*(1+a+b+c+d+GMAE))+(a+b+c+d-a-b+1)/((a+b+1)*(1+a+b+c+d+2))+(a+b+c+d-a-c+1)/((a+c+1)*(a+b+c+d+3)))]
df[,SD:=sqrt(VIC)]
df[,BCPNN250:=EIC-2*SD]
df[,C025:=IC2-2*SD]

# Ensure df is a data.table
setDT(df)

# Apply chi-squared testto each row
apply(df[,.(a,b,c,d)],1,function(x)
{
  matrix(x,nrow=2)->inter
  chisq.test(inter)$p.value
})->pvalues

df[,pvalue:=pvalues]

# Apply Bonferroni correction for multiple testing
df[, adjusted_pvalue := p.adjust(pvalue, method = "bonferroni")]

# Save the final table to a CSV file
write.csv(df,file = "df.csv")