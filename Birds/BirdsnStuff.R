data=read.csv(choose.files())

#Three population models will be used to model a realtively new bird colony. The models
#will then be compared to the data, and each other, to decide which model fits the
#data best. Sexual organisms tend to not follow logistic growth exactly, and this
#avian population is known to group together in order to protect itself from active
#raptor predators. This could imply that there could be some minimum population needed
#in order for the species to survive. Therefore, model 3, the model containing
#the (P/L-1) term to account for this, where L is this minimum, could be the best 
#fit. The second model, containing the (-m) term, is to account for raptor predators,
#who are assumed to be taking from the population at a constant rate. The first 
#model is simply logistic growth.

#Defines the variables from the data set. t is time and P is population.
t=data$n
P=data$P

#Defines dP, the change in population per time step, and Pn, the new population
#after one time step.
dP=c(0.4, 0.4, 0.6, 1, 1.2, 1.6, 1.9, 2.3, 1.3, 0.2, 0)
Pn=P+dP

#The data set is plotted; time vs. population.
plot(t,P,xlim=c(0,11),ylim=c(0,20), col="black", xlab = ("t(years)"), ylab = ("P(population in thousands)"))
legend("topleft",c("Data", "Model1","Model2","Model3"),pch=19,col=c("black","red","salmon","purple"))

#A non-linear regression is then performed on the first model to estimate r and k;
#where r is the intrinsic growth rate and k is carrying capacity. 
M1=nls(Pn~P+r1*P*(1-P/k1), start = list(r1=.1,k1=16.5))
x1=seq(0,11, length=11)
y1=predict(M1, list(t=x1))

#The carrying capacity and intrinsic growth rate is then estmated.
C1=coef(M1)
r1=C1[1]
k1=C1[2]

#We can then plot the first model against the data. It seems that the model grows 
#slower than the data; it starts and finishes at a slightly higher population. 
points(x1, y1, col="red")
#Residual sum of squares: 3.671.

#Another non-linear regression is used to estimate r,k, and m, the amount of birds
#killed by raptor predators or other natural processes. The results are then 
#plotted with the data.
M2=nls(Pn~P+r2*P*(1-P/k2)-m, start = list(r2=.1,k2=16.5,m=1))
x2=seq(0,11, length=11)
y2=predict(M2, list(t=x2))

#The carrying capacity, intrinsic growth rate, and m is then determined.
C2=coef(M2)
r2=C2[1]
k2=C2[2]
m=C2[3]

#We can then plot the second model against the first and the data. It seems that 
#this model shows the population growing slightly faster than the data, though
#the initial condition, P(0), and the carrying capacity seem to have approximately 
#the same values as the data.
points(x2, y2, col="salmon")
#Residual sum of squares: 0.477.

#Yet another non-linear regression is performed to estimate r,k and L. L is the 
#that the population can be or else it will die out. The results are then plotted 
#with the data.
M3=nls(Pn~P+r3*P*(1-P/k3)*(P/L-1), start = list(r3=.1,k3=16.5,L=1))
x3=seq(0,11, length=11)
y3=predict(M3, list(t=x3))

#The carrying capacity, intrinsic growth rate, and L is then determined.
C3=coef(M3)
r3=C3[1]
k3=C3[2]
L=C3[3]

#We can then plot the third model against the others and the data.This model's
#initial condition, P(0), is slightly higher than the data, though it seems to 
#grow at about the same rate and fit the carrying capacity for the data.
points(x3, y3, col="purple")
#Residual sum of squares: 0.09141.

#To determine which model fits the data best, we can look at the residual sum of 
#squares of each model. The third model, with a residual sum of squares value
#of 0.09141, seems to fit the data best.

