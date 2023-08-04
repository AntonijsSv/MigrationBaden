#Data Analysis from sister and her husband

#Spatial Autoregressive Model -> find out correlation between municipalities for certain factors -> could simplify the analysis if its too difficult + you can add time lag
#Urban development model to research/understand how these factors might be influenced
#! By people moving, factors will change automatically -> chain reaction 
#-> Do we want to include this or neglect it??
#Finding Causality will be difficult as many factors will change at the same time, still need to ask how to find that

cor(x,y,method=c("pearson","kendall","spearman")) #3 different methods to calculate correlation, gives a correlation coefficient between-1 and 1
cor.test(...)#same thing as cor() but also gives p-value (significance level) we'll assume anything under 0.05/5% suggests a significant correlation
#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

grangertest(X, Y, order = 1) #find whether y is caused by x, order is the time delay between the cause x and effect y
#https://www.r-bloggers.com/2021/11/granger-causality-test-in-r-with-example/
#DO it in reverse aswell, to see if x is dependant on y

#We could also maybe take inspiration from epidemic models (more complex):
#Susceptible, Infected, recovered -> SIR models
#or Birth Death Process