###################################
#######READ-IN report dates########
###################################

reportdates = read.csv("EPS_Report_Dates_bereinigt.csv",sep=";")

#Transform dates to dateformat

years = 2019:2023

for (y in years) {
	column = paste("YR_",y,sep="")
	reportdates[,column] = as.Date(reportdates[,column],format="%d.%m.%Y")
}


#Reformat data: ISIN - Name - Group - Year - Reportdate
reportdates_new = data.frame()
for (y in years) {
	dummy = cbind(reportdates[,c("ISIN.CODE","Name","INDUSTRY.GROUP")], data.frame(Year=rep(y,length(reportdates[,1]))), reportdates[,paste("YR_",y,sep="")])
	
	reportdates_new = rbind(reportdates_new, dummy)
}
names(reportdates_new) = c("ISIN","Name","Group","Year","Reportdate")

reportdates = reportdates_new

###################################################################
#######(Step 1) READ-IN predicted earning / surprise scores########
###################################################################

surprisescores = read.csv("Estimates.csv",sep=";")

#Transform dates to dateformat and reduce columns to relevant
surprisescores[,"Reporting.date"] = as.Date(surprisescores[,"Reporting.date"],format="%d.%m.%Y")

names(surprisescores) = c("ISIN","Year","Reportdate","Actual","Estimate","Mean","Surprise","SurprisePercent","SurpriseCategory","SurpriseSD")

#reorganize columns
surprisescores = surprisescores[,c("ISIN","Reportdate","Actual","Estimate","Mean","Surprise","SurpriseSD","SurprisePercent","SurpriseCategory")]

surprisescores$Actual = as.numeric(surprisescores$Actual)
surprisescores$Estimate = as.numeric(surprisescores$Estimate)
surprisescores$Mean = as.numeric(surprisescores$Mean)
surprisescores$Surprise = as.numeric(surprisescores$Surprise)
surprisescores$SurpriseSD = as.numeric(surprisescores$SurpriseSD)
surprisescores$SurprisePercent = as.numeric(surprisescores$SurprisePercent)
surprisescores$SurpriseCategory = factor(surprisescores$SurpriseCategory,levels=c("Negativ","Neutral","Positive"))


#Merge surprise scores and reporting dates along ISIN and reporting date

reports = merge(reportdates, surprisescores, by=c("ISIN","Reportdate"))

#check if merge was successful:
print(sum(is.na(reports)))
#returns 0 - no missing values, all could be merges successfully.


##########################################
#######(Step 2) READ-IN closing prices####
##########################################

#Read-in individual closing prices for the companies

closingprices = read.csv("Closing_Prices_bereinigt.csv",sep=";")

#transpose dataset

isincodes = closingprices[,"ISIN.CODE"]
data = as.data.frame(t(closingprices[,2:length(closingprices[1,])]))

data = cbind(rownames(data),data)

names(data) = c("Date", isincodes)

#Transform date column to dates
data$Date = as.Date(substring(data$Date,2), format="%d.%m.%Y")

closingprices = data


#Read-in Market Index closing prices (SPI)

spi_closingprices = read.csv("SPI_Benchmark.csv",sep=";")
names(spi_closingprices) = c("Date","SPI")

#Transform date column to dates
spi_closingprices$Date = as.Date(spi_closingprices$Date, format="%d.%m.%Y")


#Merge Individual prices and SPI. Remove missing data

closingprices = merge(closingprices, spi_closingprices, by="Date")


###################################
########compute returns############
###################################
closingreturns = closingprices
n = length(closingreturns[1,])
closingreturns[,2:n] = rbind(0, apply(closingreturns[,2:n], 2,
				function(x) {return( (x[2:length(x)] - x[1:(length(x)-1)]) / x[1:(length(x)-1)]) }
			))
			



#########################################################
########(Step 3) Estimation of expected returns##########
#########################################################

#install.packages("lmtest") #for Breusch-Pagan-Test of heteroscedasticity
library(lmtest)

###CAPM-market model: R_i = R_F + beta_i * (R_M - R_F) + error
#where R_i is the individual return, R_F is the risk-free return and R_M the market return
#Here: R_M ist modeled as SPI, and R_F is not used. Instead:

#Simplified CAPM-market model: R_i = a_i + beta_i * R_m + error
#(Risk free return is summarized in a_i = R_F(1 - beta_i) but is therefore allowed to be 0 etc.)

#Estimation takes place for every company (i) and
#every year separately in the horizon t (trading days) in [-250,-30] towards the report date (t = 0)


capm_df = data.frame()

for (eventcount in 1:length(reports[,1])) {

	line = reports[eventcount,]
		
	#get relevant data (trading days in [-250,-30])
	
	#With dates (NOT USED):
	#data = closingreturns[closingreturns$Date >= line$Reportdate-365 & closingreturns$Date <= line$Reportdate-30, c("Date",line$ISIN,"SPI")]
	
	#With trading days:
	reportdate_index = max(which(closingreturns$Date <= line$Reportdate))
	data = closingreturns[(reportdate_index - 250):(reportdate_index - 30),c("Date",line$ISIN,"SPI")]
	
	#For checking:
	#print(line)
	#print(data[1:5,])
	
	#Fitting CAPM: INDIVIDUAL ~ SPI
	result = lm(data[,2] ~ data[,3])
	result_detail = summary(result)
	
	#Breusch-Pagan-Test for heteroscedasticity
	bpresult = bptest(result)
	
	newline = data.frame(CAPM_a = coef(result_detail)[1,1],
						CAPM_a_sd = coef(result_detail)[1,2],
						CAPM_a_p = coef(result_detail)[1,4],
						CAPM_beta = coef(result_detail)[2,1],
						CAPM_beta_sd = coef(result_detail)[2,2],
						CAPM_beta_p = coef(result_detail)[2,4],
						CAPM_BPtest_p = bpresult$p.value
						)
	
	capm_df = rbind(capm_df, newline)
	
}

#Merge report information and CAPM fitting results
reports = cbind(reports, capm_df)

#Analysis of heteroscedasticity:
hist(reports$CAPM_BPtest_p,breaks=100,xlab="Breusch-Pagan-Test p value",main="p-values of Breusch Pagan Test")
#Analyze the data with strongest evidence for heteroscedasticity (p < 0.01):
hsc_events = which(reports$CAPM_BPtest_p < 0.005)

par(mfrow=c(5,5),mar = c(3, 3, 2, 1))
for (eventcount in hsc_events) {
	line = reports[eventcount,]
	reportdate_index = max(which(closingreturns$Date <= line$Reportdate))
	data = closingreturns[(reportdate_index - 250):(reportdate_index - 30),c("Date",line$ISIN,"SPI")]
	plot(data[,3],data[,2],main=paste("CAPM model ",line$Name,"\nYear ",line$Year,sep=""),xlab="Return SPI",ylab="Return Comp",cex=0.3)
}
par(mfrow=c(1,1),mar = c(5,4,4,2)+0.1) #back to standard plotting


#An inspection of several plots showed that heteroscedasticity detection
#is typically produced by OUTLIERS and not by a specific heteroscedasticity
#in the errors --> instead of a GLM, use robust linear model estimation

#install.packages("MASS") #for robust estimation
library(MASS)



capm_robust_df = data.frame()

for (eventcount in 1:length(reports[,1])) {

	line = reports[eventcount,]

	reportdate_index = max(which(closingreturns$Date <= line$Reportdate))
	data = closingreturns[(reportdate_index - 250):(reportdate_index - 30),c("Date",line$ISIN,"SPI")]
	
	#Fitting ROBUST CAPM: INDIVIDUAL ~ SPI
	result = rlm(data[,2] ~ data[,3])
	result_detail = summary(result)
	
	
	newline = data.frame(CAPM_robust_a = coef(result_detail)[1,1],
						CAPM_robust_a_sd = coef(result_detail)[1,2],
						CAPM_robust_beta = coef(result_detail)[2,1],
						CAPM_robust_beta_sd = coef(result_detail)[2,2]
						)
	
	capm_robust_df = rbind(capm_robust_df, newline)
	
}

#Merge report information and robust CAPM fitting results
reports = cbind(reports, capm_robust_df)

#Print CAPM results to a file
write.csv2(reports,file="Result_CAPMfit.csv")


######!!!!!!!!
######!!!!!!!!
######In the following, the robust CAPM results are used!
######!!!!!!!!
######!!!!!!!!




###############################################
#######(Step 4) compute abnormal returns#######
#######company-wise computation################
###############################################

abnormalreturn_df = data.frame()

for (eventcount in 1:length(reports[,1])) {

	line = reports[eventcount,]
		
	#get relevant data (trading days in [-1,+30] which
	#summarizes both short-term [-1,+3] and middle-term [+1,+30])
	
	reportdate_index = max(which(closingreturns$Date <= line$Reportdate))
	data = closingreturns[(reportdate_index - 1):(reportdate_index + 30),c("Date",line$ISIN,"SPI")]
	
	
	#Predict returns with CAPM (robust estimates)
	predictedreturns = line$CAPM_robust_a + line$CAPM_robust_beta * data$SPI
	
	actualreturns = data[,line$ISIN]

	abnormal_return = actualreturns - predictedreturns
	
	
	#Compute CAR (Cumulated abnormal return)
	CAR_shortterm = sum(abnormal_return[1:5]) #refers to [-1,+3] = Index 1,2,3,4,5
	CAR_middleterm = sum(abnormal_return[3:32]) #refers to [+1,+30] = Index 3,...,32
	
	#Compute BHAR (Buy-and-hold abnormal return)
	BHAR_shortterm = prod(1 + actualreturns[1:5]) - prod(1 + predictedreturns[1:5])
	BHAR_middleterm = prod(1 + actualreturns[3:32]) - prod(1 + predictedreturns[3:32])
	
	#Add to dataframe
	newline = data.frame(CAR_shortterm = CAR_shortterm,
						CAR_middleterm = CAR_middleterm,
						BHAR_shortterm = BHAR_shortterm,
						BHAR_middleterm = BHAR_middleterm)
	
	abnormalreturn_df = rbind(abnormalreturn_df, newline)

}


reports = cbind(reports, abnormalreturn_df)

write.csv2(reports,file="Result_CAPMfit_abnormalreturns.csv")



###############################################
#######(Step 5) Testing for significancy#######
#######Cross-Sectional, over all companies#####
###############################################


#Testing (for each year) if abnormal returns are significantly different from zero (0)


par(mfrow=c(2,2))

##Visualization

years = 2019:2023
for (y in years) {
	
	data = reports[reports$Year == y,]
	
	hist(data$CAR_shortterm, xlab="CAR shortterm [-1,+3] days relative to event",ylab="Frequency",main=paste("CAR shortterm distribution, year ",y,"\nover all companies",sep=""))
	
	hist(data$CAR_middleterm, xlab="CAR middleterm [+1,+30] days relative to event",ylab="Frequency",main=paste("CAR middleterm distribution, year ",y,"\nover all companies",sep=""))
	
	hist(data$BHAR_shortterm, xlab="BHAR shortterm [-1,+3] days relative to event",ylab="Frequency",main=paste("BHAR shortterm distribution, year ",y,"\nover all companies",sep=""))
	hist(data$BHAR_middleterm, xlab="BHAR middleterm [+1,+30] days relative to event",ylab="Frequency",main=paste("BHAR middleterm distribution, year ",y,"\nover all companies",sep=""))
		
}

#All years accumulated
par(mfrow=c(2,2))

hist(reports$CAR_shortterm, xlab="CAR shortterm [-1,+3] days relative to event",ylab="Frequency",main=paste("CAR shortterm distribution,\nover all years and companies",sep=""))
	
	hist(reports$CAR_middleterm, xlab="CAR middleterm [+1,+30] days relative to event",ylab="Frequency",main=paste("CAR middleterm distribution,\nover all years and companies",sep=""))
	
	hist(reports$BHAR_shortterm, xlab="BHAR shortterm [-1,+3] days relative to event",ylab="Frequency",main=paste("BHAR shortterm distribution,\nover all years and companies",sep=""))
	hist(reports$BHAR_middleterm, xlab="BHAR middleterm [+1,+30] days relative to event",ylab="Frequency",main=paste("BHAR middleterm distribution,\nover all years and companies",sep=""))




##Testing

results_testing_df = data.frame()

variables = c("CAR_shortterm","CAR_middleterm","BHAR_shortterm","BHAR_middleterm")

for (y in years) {
	
	for (v in variables) {
		
		data = reports[reports$Year == y,v]
				
		#t-Test
		t_result = t.test(data, mu = 0, alternative = "two.sided")
		
		#Generalized sign test (binomial test for signs)
		sign_result = binom.test(sum(data > 0), n=length(data), p = 0.5)
		
		#Bootstrap t-test
		B <- 10000
		data_centered = data - mean(data)
		
		t_obs = mean(data)/(sd(data)/sqrt(length(data)))
		
		t_boot = replicate(B, {
		  data_star = sample(data_centered, size = length(data), replace = TRUE)
		  mean(data_star) / (sd(data_star) / sqrt(length(data)))
		})
		p_value = mean(abs(t_boot) >= abs(t_obs))
		
		newline = data.frame(Year=y, Variable=v, N = length(data), Mean=mean(data), SD = sd(data), Ttest_stat = t_result$statistic, Ttest_p = t_result$p.value, Signtest_prob = sign_result$estimate, Signtest_p = sign_result$p.value, Bootstrap_Ttest_p = p_value)
		
		results_testing_df = rbind(results_testing_df, newline)
		
	}
}


#Over all years

for (v in variables) {
		
	data = reports[,v]
			
	#t-Test
	t_result = t.test(data, mu = 0, alternative = "two.sided")
	
	#Generalized sign test (binomial test for signs)
	sign_result = binom.test(sum(data > 0), n=length(data), p = 0.5)
	
	#Bootstrap t-test
	B <- 10000
	data_centered = data - mean(data)
	
	t_obs = mean(data)/(sd(data)/sqrt(length(data)))
	
	t_boot = replicate(B, {
	  data_star = sample(data_centered, size = length(data), replace = TRUE)
	  mean(data_star) / (sd(data_star) / sqrt(length(data)))
	})
	p_value = mean(abs(t_boot) >= abs(t_obs))
	
	newline = data.frame(Year="2019-2023", Variable=v, N = length(data), Mean=mean(data), SD = sd(data), Ttest_stat = t_result$statistic, Ttest_p = t_result$p.value, Signtest_prob = sign_result$estimate, Signtest_p = sign_result$p.value, Bootstrap_Ttest_p = p_value)
	
	results_testing_df = rbind(results_testing_df, newline)
	
}




#Save results to file
write.csv2(results_testing_df,file="Results_Testing_Abnormalreturns_equal_0.csv")

#One can see that shortterm abnormal returns are much more often significantly
#different from 0 compared to the middleterm abnormal returns.





########################################################
#######(Step 5 - part 2) Testing for significant########
#######correlation with surprisal scores################
#######Cross-Sectional, over all companies##############
########################################################


#TODO