

library(readxl)
library(Benchmarking)
# read the original file
data <- data.frame(read_excel(path='data2.xls',sheet=2))
df_i <- data
company_name <- df_i[,2]
df_i <- df_i[,-c(1,2,3)]
# rename the desired metrics
names(df_i) <- c("total_assets","net_income",
                 "current_assets","current_liabilities",
                 "long_term_debt","working_capital","cash_flow")
# define a matrix for storing the Calculated metrics
df <- data.frame(matrix(rep(0,nrow(df_i)*4),nrow = nrow(df_i)))
# name the desired metrics
names(df) <- c("WCTA","CATA","TDTA","CLTA")
# calculate the needed metrics
for(i in 1:nrow(df_i)){
  
  df[i,1] <- df_i[i,6]/df_i[i,1]
  df[i,2] <- df_i[i,3]/df_i[i,1]
  df[i,3] <- (df_i[i,4]+df_i[i,5])/df_i[i,1]
  df[i,4] <- df_i[i,4]/df_i[i,1]
  
}
# Define the number of input variables 
m <- 2
# Define the number of output variables
s <- ncol(df)-m
# calculate the number of rows of the original file
N <- nrow(df)

# solve the DEA model via the dea.add function 
results <- dea.add(df[,1:m],df[,(m+1):(m+s)],RTS=0)
# store the results
EES <- data.frame(results$sum,results$lambda,results$sx,results$sy,results$slack)
# name the variable in the result
colnames(EES) <- c('sum',rep('lambda',N),rep('s-',m),rep('s+',s),'bankrupt')
# combine the results with their company names
row.names(EES) <- company_name
# output the results
write.csv(EES,'additive-CCR.csv')


bcc_data <- data.frame(read_excel(path='data2.xls',sheet=2))
df_i <- bcc_data
df_i <- df_i[,-c(1,2,3)]
names(df_i) <- c("total_assets","net_income",
                 "current_assets","current_liabilities",
                 "long_term_debt","working_capital","cash_flow")

df <- data.frame(matrix(rep(0,nrow(df_i)*4),nrow = nrow(df_i)))


names(df) <- c("WCTA","CATA","TDTA","CLTA")
for(i in 1:nrow(df_i)){
  
  
  df[i,1] <- df_i[i,6]/df_i[i,1]
  df[i,2] <- df_i[i,3]/df_i[i,1]
  df[i,3] <- (df_i[i,4]+df_i[i,5])/df_i[i,1]
  df[i,4] <- df_i[i,4]/df_i[i,1]
}



m <- 2
s <- ncol(df)-m
N <- nrow(df)
results <- dea.add(df[,1:m],df[,(m+1):(m+s)],RTS = 1)
EES <- data.frame(results$sum,results$lambda,results$sx,results$sy,results$slack)
colnames(EES) <- c('sum',rep('lambda',N),rep('s-',m),rep('s+',s),'bankrupt')
row.names(EES) <- company_name
write.csv(EES,'additive-BCC.csv')


