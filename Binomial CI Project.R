#Hyper Parameters for this small simulation
Max_N <- 250 #The number of runs for the Binomial, or the Maximum number of Successes.
Actual_Prob <- 0.5 #The True Probability of Success for the Binomial.
Repeats <- 1 #The number of Samples Drawn for Each Binomial Condition.
Random_Intializations <- 10000 #Number of Times This Random Process is Simulated, from Random Intializations.

#Variable Specification for the loops to Fill with Results
Running_Prob <- matrix(0,nrow=Random_Intializations,ncol=Max_N)
Lower_bound <- matrix(0,nrow=Random_Intializations,ncol=Max_N)
Upper_bound <- matrix(0,nrow=Random_Intializations,ncol=Max_N)
Correct_Intervals <- matrix(NA,nrow=Random_Intializations,ncol=Max_N)
Proportion_of_Correct_Intervals <- matrix(0,nrow=Random_Intializations,ncol=Max_N)
Index <- 0

#Loop that Runs through Each Random Intialization
for (k in 1:Random_Intializations) {

#Loop that Generates the Binomial Data and 95% Confidence Intervals for them  
String <- sample(1:Max_N,Max_N,replace=FALSE)
for (i in String) {
  Index = Index + 1
  
  Run <- mean(rbinom(Repeats,i,Actual_Prob)) #Mean number of Successes
  Run_Proportion <- Run / i #Proportion of Successes
  Running_Prob[k,i] <- Run_Proportion  
  
  Running_Average <- sum(Running_Prob)/Index #Returns the Proportion of Successes across all runs
  
  Binom_CI <- qbinom(c(0.025,0.975),i*Repeats,Actual_Prob)/Repeats #Creates a 95% CI in terms of Number of Success
  Lower_bound[k,i] <- L <- (Binom_CI[1]/i) #Converts to Proportion, and imputs to data matrix
  Upper_bound[k,i] <- U <- (Binom_CI[2]/i) #Converts to Proportion, and imputs to data matrix
  
  counter = 0
  if (Run_Proportion > L & Run_Proportion < U) {counter = counter + 1} #Determines if Sample falls within CI
  Correct_Intervals[k,i] = counter #Records this Information

}
if (k %% 10 == 0) {print(paste("Initialization",k,"out of",Random_Intializations,"has ended.",sep=" "))}
}

#Outcomes of Interest Collapsed Across Random Intializations
Running_Prob_Collapsed <- apply(Running_Prob, MARGIN = 2, FUN = mean)
Upper_bound_collapsed <- apply(Upper_bound, MARGIN = 2, FUN = mean)
Lower_bound_collapsed <- apply(Lower_bound, MARGIN = 2, FUN = mean)
Correct_Intervals_collapsed <- apply(Correct_Intervals, MARGIN = 2, FUN = mean)

#Specifies and Creates the Model Fit to the second Graph illustrating the Percentage of correct CIs
Degree <- 4 # Specifies what degree polynomial fit will be used to construct the trend line and 95% CI lines.
Fit_Data <- matrix(0,ncol=Degree,nrow=Max_N)
for (i in 1:Degree) {Fit_Data[,i] = (1:Max_N)^i}
Fit <- summary(lm(Correct_Intervals_collapsed ~ Fit_Data))
Pred <- Correct_Intervals_collapsed - Fit$residuals

#Plot of Proportion Estimates for Given N/# of Trials and Sample size/Repeats (specified as a hyper parameter), with 95% CI lines.
plot(1:Max_N,Running_Prob_Collapsed,type='l',ylim=c(Lower_bound_collapsed[1],Upper_bound_collapsed[1]),main="Proportion Point Estimates with 95% CIs", xlab="N",yaxt="none", ylab="Probability")
lines(1:Max_N,Running_Prob_Collapsed); lines(1:Max_N,Upper_bound_collapsed,col="red"); lines(1:Max_N,Lower_bound_collapsed,col="red"); axis(2,seq(0,1,by=0.05))

#Plot of the accuracy of the 95% confidence Intervals above, more random initializations will give better results.
plot(1:Max_N,Correct_Intervals_collapsed, main="Proportion of Correct 95% CIs, with 95% Confidence lines",xlab="N",ylab="Proportion Correct",ylim=c(0,1.1),yaxt="none")
lines(1:Max_N,Pred); lines(1:Max_N,Pred+1.96*Fit$sigma,col="Red"); lines(1:Max_N,Pred-1.96*Fit$sigma,col="Red"); axis(2,seq(0,1.1,by=0.05))

