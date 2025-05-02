library(stats)                                                            #Library that is used to some of the following functions

### Means new section ###
### XML
# Means comments or description to the specific line in the code

### Variables ###
## n1 - the group which receive treatment 1 / sample size 1
## n2 - the group which receive treatment 2 / sample size 2
## sd1 - The standard deviation for x^bar_1
## sd2 - The standard deviation for x^bar_2 
## alpha - The significant level
## m - mean of prior (same as delta')
## v - variance of prior
## n - the total sample size (used in power calculation)
## es - the true effect size used in power calculation (same as delta')
## sd - the standard deviation used in power calculation
## za_os/za_beta_os - The upper 100alpha% significant point of the standard normal distribution for one-sided test.
## za_ts/za_beta_ts - The upper 100alpha% significant point of the standard normal distribution for two-sided test.


### Variables functions ###

### <FunctionName> Tau function </FunctionName>
### <Summary> The function calculate the variable tau, which is the standard deviation for the difference in the estimated sample means.
### This is formula 4 in the methodology section </Summary>
### <Variables>
# "sd1" - The standard deviation for treatment 1
# "sd2" - The standard deviation for treatment 2
# "n1" - the sample size for treatment 1
# "n1" - The sample size for treatment 2
### </Variables>

### <Function>
tau <- function(sd1, sd2, n1, n2) {    
  sqrt(sd1^2/n1 + sd2^2/n2)
}
### </Function>
### <Output> the variance tau, as a numeric output </Output>


### Assurances functions ###


### <FunctionName> Assurance function for a one-sided test </FunctionName>
### <Summary> Calculates assurance for a one-sided test, based on the on formula (10) from the methodology section </Summary>
### <Variables> 
# "alpha" - the significant level, 
# "sd1" - the standard deviation for treatment 1, 
# "sd2" - the standard deviation for treatment 2, 
# "n1" the sample size who receive treatment 1, 
# "n2" - the sample size who receive treatment 2,
# "m" - the mean of the prior,
# "v" - the variance of the prior
### </Variable>

### <Function>
gamma_os <- function(alpha, sd1, sd2, n1, n2, m, v) {
  tau_calc_os <- tau(sd1, sd2, n1, n2)
  za_os <- qnorm(1-alpha)
  
  pnorm((-(tau_calc_os)*za_os+m)/(sqrt(tau_calc_os^2+v)))
}
### </Function>



### <FunctionName> Assurance function for a two-sided test </FunctionName>
### <Summary> calculates the three types of assurance for a two_sided test, based on formula (11), (12) and (13) from the methodology section </Summary>
## <Variables> 
# "alpha" - the significant level, 
# "sd1" - the standard deviation for treatment 1, 
# "sd2" - the standard deviation for treatment 2, 
# "n1" the sample size who receive treatment 1, 
# "n2" - the sample size who receive treatment 2,
# "m" - the mean of the prior,
# "v" - the variance of the prior
### </Variable>

### <Function>
gamma_ts <- function(alpha, sd1, sd2, n1, n2, m, v) {
  tau_calc_ts <- tau(sd1, sd2, n1, n2)
  za_ts <- qnorm(1-alpha/2)
  gamma_2 = pnorm((-(tau_calc_ts)*za_ts+m)/(sqrt(tau_calc_ts^2+v)))                      # Assurance with data favouring for treatment 2
  gamma_1 = pnorm((-(tau_calc_ts)*za_ts-m)/(sqrt(tau_calc_ts^2+v)))                      # Assurance with data favoring for treatment 1
  gamma_2 + gamma_1                                                                      # The overall assurance of rejecting the null hypothesis
  list(gamma = gamma_2+gamma_1, gamma_1 = gamma_1, gamma_2 = gamma_2)                    # Creates a list with the three deffrient kind op assurance for a two_sided test.
}
### </Function>

### <FunctionName> Assurance function for both a one-sided and two-sided test </FunctionName>
### <Summary> combine the two functions for assurance for a one-sided test (gamma_os) and for a two-sided test (gamma_ts) by an if/else statement.
#If you don't pick "one-sided" or "two-sided" as a test_type, then the function tells you to do that </Summary>
### <Variables> 
# "alpha" - the significant level, 
# "sd1" - the standard deviation for treatment 1, 
# "sd2" - the standard deviation for treatment 2,   
# "n1" the sample size who receive treatment 1, 
# "n2" - the sample size who receive treatment 2,
# "m" - the mean of the prior,
# "v" - the variance of the prior,
# "test_type" - the type of the test, you can pick "one_sided" or "two_sided"
### </Variable>

### <Function>
gamma_OneAndTwo <- function(alpha, sd1, sd2, n1, n2, m, v, test_type) {
  if (test_type == "one_sided") {
    gamma_os(alpha, sd1, sd2, n1, n2, m, v)
  }
  else if (test_type == "two_sided") {
    gamma_ts(alpha, sd1, sd2, n1, n2, m, v)
  }
  else print ("pick one_sided or two_sided in test_type")
}
### </Function>


### Power functions ###

### <FunctionName> power.t.test function </FunctionName>
### <Summary> Calculates power by the built-in R-function "power.t.test". This is the only built-in R-function in the script. 
#This function is based on a t-test, and you can therefor see a different in the result of this function and the "Power function" in the plot in the app 
### </Summary>
### <Variables> 
# "n" - the sample size
# "es" - The true effect size (same as delta' in the methodology section)
# "alpha" - the significant level
# "sd" - the standard deviation
# "test_type2" - the type of the test (same as test_type), can be "one_sided" or "two_sided"
### </Variables>

### <Function>
powfunN <- function(n,es,alpha,sd,test_type2){
  power.t.test(delta=es,sig.level=alpha,n=n,sd=sd, alternative =test_type2)$power   
}

### </Function>

### <FunctionName> Beta function for a one-sided test </FunctionName>
### <Summary> Calculates type II error (beta) for a one-sided test, based on function (2) in the methodology section </Summary>
### <Variables> 
# "alpha" - the significant level, 
# "sd1" - the standard deviation for treatment 1, 
# "sd2" - the standard deviation for treatment 2,   
# "n1" the sample size who receive treatment 1, 
# "n2" - the sample size who receive treatment 2,
# "m" - the mean of the prior
### </Variables>

### <Function>
betafunc_os <- function(alpha, n1, n2, sd1, sd2, m) {
  tau_calc_beta_os <- tau(sd1, sd2, n1, n2)
  za_beta_os <- qnorm(1-alpha)
  
  pnorm((za_beta_os * tau_calc_beta_os - abs(m))/(tau_calc_beta_os))
}
### </Function>


### <FunctionName> Beta function for a two-sided test </FunctionName>
### <Summary> Calculates type II error (beta) for a two-sided test, based on function (3) in the methodology section </Summary>
## <Variables> 
# "alpha" - the significant level, 
# "sd1" - the standard deviation for treatment 1, 
# "sd2" - the standard deviation for treatment 2,   
# "n1" the sample size who receive treatment 1, 
# "n2" - the sample size who receive treatment 2,
# "m" - the mean of the prior
### </Variables>

### <Function>
betafunc_ts <- function(alpha, n1, n2, sd1, sd2, m) {
  tau_calc_beta_ts <- tau(sd1, sd2, n1, n2)
  za_beta_ts <- qnorm(1-alpha/2)
  pnorm((za_beta_ts * tau_calc_beta_ts - abs(m))/(tau_calc_beta_ts))
}
### </Function>

### <FunctionName> Power function for a one-sided test </FunctionName>
### <Summary> Calculates power for a one-sided test, based on formula (2) in the methodology section </Summary>
### <Variables> 
# "alpha" - the significant level, 
# "sd1" - the standard deviation for treatment 1, 
# "sd2" - the standard deviation for treatment 2,   
# "n1" the sample size who receive treatment 1, 
# "n2" - the sample size who receive treatment 2,
# "m" - the mean of the prior
### </Variables>

### <Function>
powerfunc_os <- function(alpha, n1, n2, sd1, sd2, m){
  1 - betafunc_os(alpha, n1, n2, sd1, sd2, m)
} 
### </Function>

### <FunctionName> Power function for a two-sided test </FunctionName>
### <Summary> Calculates power for a two-sided test, based on formula (3) in the methodology section </Summary>
### <Variables> 
# "alpha" - the significant level, 
# "sd1" - the standard deviation for treatment 1, 
# "sd2" - the standard deviation for treatment 2,   
# "n1" the sample size who receive treatment 1, 
# "n2" - the sample size who receive treatment 2,
# "m" - the mean of the prior
### </Variables>

### <Function>
powerfunc_ts <- function(alpha, n1, n2, sd1, sd2, m){
  1 - betafunc_ts(alpha, n1, n2, sd1, sd2, m)
}
### </Function>


### Assurance plot ###

### <FunctionName> Assurance plot, both for a one-sided and a two-sided test </FunctionName>
### <Summary> A plot with "n1" at the x-axes and "Assurance" at the y-axes. 
# By an if/else statement, the function can plot both assurance for a one-sided or a two-sided test.
#Depending on rather you have picked the test_type as "one-sided", or "two-sided",
#the plot shows the calculated assurance depending on the sample size made by the function "gamma_OneAndTwo" 
#(the red line ind the one-sided plot and the red, green, and blue line in the two-sided plot).
#The plot also shows both the "power function" (the grey line) and the "power.t.test function" (the black line) made by the power function above in this script.
#There is a horisontical line (the pink line), which is the picked power-target, and a vertical line (the purple line) which is the picked n1 value.
#In the right side of the plot, there is a box, explaining what all of the lines represents, and chaneges when the test-type is changing. 
### </Summary>
### <Variables>
# "alpha" - the significant level
# "sd1" - the standard deviation for treatment 1
# "sd2" - the standard deviation for treatment 2
# "m" - the mean of the prior/the true effect size (same as delta' in the methedology section)
# "v" - variance of the prior
# "test_type" - the type of the test, can be "one-sided" or "two-sided". If it's not one of these, then it will tell you to pick on of these
# "maxsize" - the maximum size of the plot (x-axes)
# "ratio_ss" - the ratio between the n1 and n2
# "es" - the true effect size/the mean of the prior (same as delta' in the methodology section)
# "pwr" - the picked power target or desired power
# "sd" - the standard deviation for treatment 1 (same as sd1)
# "test_type2" - the type of the test (same as test_type)
# "q" - picked n1 value
### </Variables>



### <Function>
assuranceplot <- function(alpha, sd1, sd2, m, v, test_type, maxsize, ratio_ss, es, pwr, q){      #Ratio_ss st?r for ration in sample size (n1 and n2)
  n1 <- seq(0, maxsize)
  dim(n1) <- c(1,length(n1))
  assurance <- apply(n1, 1, gamma_OneAndTwo, n2=n1*ratio_ss, sd1=sd1, sd2=sd2, alpha=alpha, m=m, v=v, test_type=test_type)
  
  ifelse(test = test_type == "one_sided", yes = alternative <- "one.sided", no = alternative <- "two.sided")
  power_target <- apply(n1, 1, powfunN, es=es, alpha=alpha, sd=sd1, test_type2 = alternative)
  power_calc_os <- apply(n1, 1, powerfunc_os, alpha=alpha, sd1=sd1, sd2=sd2, n2=n1*ratio_ss, m=m)
  power_calc_ts <- apply(n1, 1, powerfunc_ts, alpha=alpha, sd1=sd1, sd2=sd2, n2=n1*ratio_ss, m=m)
  
  if (test_type == "one_sided") {                                                      # The plot for a "one-sided" test
    plot(n1, assurance, type = "l",main = "Assuranceplot for one_sided testing", 
         xlab = paste("n1 (n2 = n1 *", round(ratio_ss, digits = 2),")"),
         ylim = c(0,1), col="red", ylab = "Probability of success")
    #lines(n1, power_target, type = "l", col="black")                                   # Adds the power-t-test
    legend("right", legend = c("Assurance", "Picked sample size", "Calculated power", "Picked power target"),
           col = c("red", "purple", "grey", "pink"), lty = c(1,1,1,1,1))
    lines(n1, power_calc_os, type = "l", col="grey")                                   # Adds the manually calculated power
    abline(h=pwr, col="pink")                                                          # Adds picked power target value
    abline(v=q, col="purple")                                                          # Adds picked n1-value
  }
  else if (test_type == "two_sided") {                                                 # The plot for a "two-sided" test
    plot(n1, assurance[[1]]$gamma, type="l", col="red", main = "Assuranceplot for two_sided testing" ,
         xlab = paste( "n1 (n2 = n1 *", round(ratio_ss, digits = 2),")"), 
         ylab = "Probability of success", ylim = c(0,1)) 
    legend("right", 
           legend = c("The overall assurance", "Assurance < 1", "Assurance < 2", "Calculated power", "Picked sample size", "Picked power target"),
           col=c("red", "green", "blue", "gray", "purple", "pink"), lty=c(1,1,1,1,1,1))
    lines(n1, assurance[[1]]$gamma_1, col="green")                                     # Adds assurance with data favoring for treatment 1
    lines(n1, assurance[[1]]$gamma_2, col= "blue")                                     # Adds assurance with data favoring for treatment 2
    #lines(n1, power_target, type = "l", col="black")                                   # Adds the power-t-test
    lines(n1, power_calc_ts, type = "l", col="grey")                                   # Adds the manually calculated power
    abline(h=pwr, col="pink")                                                          # Adds the picked power target value
    abline(v=q, col="purple")                                                          # Adds the picked n1-value
    
    
  }
  else print("Pick one_sided or two_sided in test_type")                               # If you haven't picked rather "one-sided" or "two-sided" as the test_type
  
}
### </Function>
assuranceplot(alpha = 0.05, sd1 = 1, sd2 = 1, m = 1, v = 1, test_type = "one_sided", maxsize = 100, ratio_ss = 1.5, es = 1, pwr = 0.8, q = 25)

### Prior ###

### <FunctionName> Prior plot </FunctionName>
### <Summary> Plot the prior as a normal distribution with the critical values.
#By an if/else statement, the function plots the prior distribution for both a "one-sided" or a "two-sided" test, depending on the picked test_type.
#The green area(s) in the plot represent the probability of success, and the red area represent the probability of rejecting the null hypothesis.
### </Summary>
### <Variables>
# "m" - mean of prior/the true effect size (same as delta' in the methodology section)
# "v" - variance of the prior
# "alpha" - the significant level
# "sd1" - the standard deviation for treatment 1
# "sd2" - the standard deviation for treatment 2
# "n1" - the sample size for treatment 1
# "n2" - the sample size for treatment 2
# "test_type" - the type of the test. Can be "one_sided" or "two_sided"
### </Variables>
critical_value <- function(alpha, sd1, sd2, n1, n2, test_type, es) {
  if (test_type=="one_sided"){
    if (es < 0){
      qnorm(alpha)*sqrt(sd1^2/n1+sd2^2/n2)
    }                # The function for calculating the critical value for a one-sided test
    else {qnorm(1-alpha)*sqrt(sd1^2/n1+sd2^2/n2)}}
  else if (test_type=="two_sided"){                                                   # The function for calculating the critical value for a two-sided test
    qnorm(1-alpha/2)*sqrt(sd1^2/n1+sd2^2/n2)}
  else
    print ("Pick one-sided or two-sided in test_type")
}

### <Function>
normplot <- function(m, v, alpha, sd1, sd2, n1, n2, test_type, es){
  x <- seq(m-6*v, m+6*v, by=.1)
  y <- dnorm(x, mean= m, sd= sqrt(v))
  if (test_type=="one_sided"){                                                             # Creates the plot for a "one-sided" test
    plot(x,y, type = "l", 
         ylab = "Probability",                                                             # Names the y-axes
         xlab = "Expected effect",                                                         # Names the x-axes
         main = "Plot of prior distribution for the effect")
    c <- critical_value(alpha, sd1, sd2, n1, n2, test_type, es)# headline of the plot
    #c <- qnorm(1-alpha)*sqrt(sd1^2/n1+sd2^2/n2)
    if (es < 0){
      segments(x0 = c, y0 = -0.1, y1 = dnorm(c, mean = m, sd = sqrt(v)))
      polygon(x = c(c, x[x>=c], m+6*v, c, c),                                                  # Color the area for probability of rejecting H_0
              y = c(dnorm(c, mean=m, sd=sqrt(v)), dnorm(x[x>=c], mean=m, sd=sqrt(v)),0, 0, dnorm(c, mean=m, sd=sqrt(v))),
              col = "red")
      polygon(x = c(m-6*v, x[x<=c], c, c, m-6*v),                                                   # Color area for probability of success
              y = c(0, dnorm(x[x<=c], mean=m, sd=sqrt(v)), dnorm(c, mean=m, sd=sqrt(v)), 0, 0),
              col = "green")
    } 
    else {                                             # Makes the critical value
      segments(x0 = c, y0 = -0.1, y1 = dnorm(c, mean = m, sd = sqrt(v)))                      # Adds the line for the critical value, between -0.1 and the curve
      polygon(x = c(x[x<=c], c, c, -4*v-1),                                                  # Color the area for probability of rejecting H_0
              y = c(dnorm(x[x<=c], mean=m, sd=sqrt(v)), dnorm(c, mean=m, sd=sqrt(v)), 0,0),
              col = "red")
      polygon(x = c(c, x[x>=c], 4*v+1, c),                                                    # Color area for probability of success
              y = c(dnorm(c, mean=m, sd=sqrt(v)), dnorm(x[x>=c], mean=m, sd=sqrt(v)), 0, 0),
              col = "green")}}
  
  else if (test_type=="two_sided"){                                                        # Creates the plot for a "two-sided" test
    plot(x,y, type="l", 
         xlab = "Expected effect",                                                         # Names the x-axes
         ylab = "Probability",                                                             # Names the y-axes
         main = "Plot of prior distribution for the effect") # headline of the plot
    c1 <- qnorm(1-alpha/2)*sqrt(sd1^2/n1+sd2^2/n2)                                         # Makes the critical value
    c2 <- -qnorm(1-alpha/2)*sqrt(sd1^2/n1+sd2^2/n2)                                        # Makes the critical value
    segments(x0 = c1, y0 = -0.1, y1 = dnorm(c1, mean = m, sd = sqrt(v)) )                  # Adds the a line for the critical value, between -0.1 and the curve
    segments(x0 = c2, y0 = -0.1, y1 = dnorm(c2, mean = m, sd = sqrt(v)) )                  # Adds the a line for the critical value, between -0.1 and the curve 
    polygon(x = c(x), y = c(dnorm(x, mean=m, sd=sqrt(v))), col = "red")                    # Color the area for probability of rejecting H_0
    polygon(x = c(x[x<=c2], c2, c2, m-6*v),                                                # Color the areas of probability of success
            y = c(dnorm(x[x<=c2], mean=m, sd=sqrt(v)), dnorm(c2, mean = m, sd= sqrt(v)),0, 0), 
            col = "green")
    polygon(x = c(c1, x[x>=c1], m+6*v, c1),                                                # Color the area of probability of success
            y = c(dnorm(c1, mean=m, sd=sqrt(v)), dnorm(x[x>=c1], mean=m, sd=sqrt(v)),0, 0),
            col = "green")
  }
  
  else print("no") }                                                                       # In case of wrongly picked test_type
### </Function>


### <FunctionName> the critical value </FunctionName>
### <Summary> calculates the critical value, both for a "one-sided" or a "two-sided" test, 
# based on the formula "Z_alpha*tau" which is contained in formula (10), (11) and (12) in the methodology section 
### </Summary>
### <Variables>
# "alpha" - the significant level
# "sd1" - the standard deviation for treatment 1
# "sd2" - the standard deviation for treatment 2
# "n1" - the sample size for treatment 1
# "n2" - the sample size for treatment 2
# "test_type" - the type of the test. Can be "one-sided" or "two-sided".
### </Variables>

### <Function>
critical_value <- function(alpha, sd1, sd2, n1, n2, test_type, es) {
  if (test_type=="one_sided"){
    if (es < 0){
      qnorm(alpha)*sqrt(sd1^2/n1+sd2^2/n2)
    }                # The function for calculating the critical value for a one-sided test
    else {qnorm(1-alpha)*sqrt(sd1^2/n1+sd2^2/n2)}}
  else if (test_type=="two_sided"){                                                   # The function for calculating the critical value for a two-sided test
    qnorm(1-alpha/2)*sqrt(sd1^2/n1+sd2^2/n2)}
  else
    print ("Pick one-sided or two-sided in test_type")                                # In case of wrongly picked test_type
  
}
### </Function>






