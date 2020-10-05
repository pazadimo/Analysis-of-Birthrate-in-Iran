library(rmatio)
data <- read.mat("C:/Users/novin/Desktop/Q2/Fard90.mat")
Data=data$Fard90
f=0;
c=0;
c2=0
pesar_fard=list();
dokhtar_fard=list();
mother=matrix(list(), nrow = 9, ncol = 4)
Data[is.na(Data)]<-0
age1=list()
age2=list()
age3=list()
age4=list()
age5=list()
age6=list()
age7=list()
age8=list()
mothers_children=list()
edu=list()
shahri=list()
work_statues=list()
indexes=list()
for (i in 1:9) {
  for (j in 1:4) {
    mother[i,j]=0
  }
}
for (i in 1:1481586) {
  y=Data[i,]
  print(i)
  if(is.na(y[3])) {
    c=c+1
  }
  else if(as.numeric(y[3]) != 2){
    c2=c2+1
  }
  else{
    if(is.na(y[18])){
      f=1
    }
    else if(is.na(y[19])){
      f=2
    }
    else if((as.numeric(y[18]) + as.numeric(y[19]))<8){
      #mothers_children=c(mothers_children,as.numeric(y[18])+as.numeric(y[19]))
      #edu=c(edu,y[7])
      indexes=c(indexes,i)
      index=(as.numeric(y[18]) + as.numeric(y[19]));
      mother[index,1]=as.numeric(mother[index,1])+1
      mother[index,3]=as.numeric(mother[index,3])+as.numeric(y[18])
      mother[index,4]=as.numeric(mother[index,4])+as.numeric(y[19])
      if(y[7]==1){
        mother[index,2]=as.numeric(mother[index,2])+1
      }
      
      #     if(index==1){
      #      age1=c(age1,y[4])
      #   }
      #    if(index==2){
      #     age2=c(age2,y[4])
      #  }
      #      if(index==3){
      #       age3=c(age3,y[4])
      #    }
      #   if(index==4){
      #    age4=c(age4,y[4])
      # }
      #      if(index==5){
      #       age5=c(age5,y[4])
      #    }
      #   if(index==6){
      #    age6=c(age6,y[4])
      # }
      #      if(index==7){
      #       age7=c(age7,y[4])
      #    }
      #   if(index==8){
      #    age8=c(age8,y[4])
      # }
    }
    else {
      index=8
      #mothers_children=c(mothers_children,as.numeric(y[18])+as.numeric(y[19]))
      #edu=c(edu,y[7])
      indexes=c(indexes,i)
      mother[index,1]=as.numeric(mother[index,1])+1
      mother[index,3]=as.numeric(mother[index,3])+as.numeric(y[18])
      mother[index,4]=as.numeric(mother[index,4])+as.numeric(y[19])
      age8=c(age8,y[4])
      if(y[7]==1){
        mother[index,2]=as.numeric(mother[index,2])+1
      }
    }
  }
}
for (i in 1:8) {
  mother[9,1]=as.numeric(mother[9,1])+as.numeric(mother[i,1])
  mother[9,2]=as.numeric(mother[9,2])+as.numeric(mother[i,2])
  mother[9,3]=as.numeric(mother[9,3])+as.numeric(mother[i,3])
  mother[9,4]=as.numeric(mother[9,4])+as.numeric(mother[i,4])
}
for (i in 1:9) {
  print(as.numeric(mother[i,2])/as.numeric(mother[i,1]))
  
}

for (i in 1:9) {
  print(as.numeric(mother[i,3])/as.numeric(mother[i,4]))
  
}

edu=list()
shahri=list()
work_statues=list()
mothers_children=list()
age=list()
internet=list()
Is_Job_Ready=list()
for (i in 1:185000) {
    edu[length(edu)+1]<-Data[as.numeric(indexes[i]), 7]
    mothers_children[length(mothers_children)+1]<-(Data[as.numeric(indexes[i]), 18]+Data[as.numeric(indexes[i]), 19])
    shahri[length(shahri)+1]<-Data[as.numeric(indexes[i]), 1]
    work_statues[length(work_statues)+1]<-Data[as.numeric(indexes[i]), 13]
    age[length(age)+1]<-Data[as.numeric(indexes[i]), 4]
    internet[length(internet)+1]<-Data[as.numeric(indexes[i]), 6]
    Is_Job_Ready[length(Is_Job_Ready)+1]<-Data[as.numeric(indexes[i]), 11]
}

y =as.numeric( mothers_children)
x = as.numeric(edu)
x2= as.numeric(shahri)
x3=as.numeric(work_statues)
x4=as.numeric(age)
x5=as.numeric(internet)

#plot(x,y, main="scatter plot", xlab = "Number of Child", ylab = "Percent of Knowledge")
#plot.new()
#plot(x,y, main="scatter plot", xlab = "Number of Child", ylab = "Percent of Knowledge")
#abline(lm(y~x+x2+x3))
fit <- lm( y ~ x+x2+x3)
summary(fit)
print(fit$coefficients)
res = fit$residuals
standard_error = sd(res)
print(standard_error)



fit <- glm( y ~ x+x2+x3, family = poisson())
summary(fit)
print(fit$coefficients)
res = fit$residuals
standard_error = sd(res)
print(standard_error)





#-------------part e
age_of_mother=x4
internet_statues=x5
Working_Statues=x3
num_of_child=y
education=x
df=data.frame(num_of_child ,age_of_mother, education ,Working_Statues)
cor(df,use="pairwise")

mod1 <- lm(num_of_child~age_of_mother,data=df)
summary(mod1)

mod2 <- lm(num_of_child~Working_Statues,data=df)
summary(mod2) 

fit1 <- glm(num_of_child~age_of_mother,data=df)
summary(fit1)

fit2 <- glm(num_of_child~Working_Statues,data=df)
summary(fit2)





#-----------------part f----------------
library("ggpubr")
age_of_mother=x4
internet_statues=x5
Working_Statues=x3
IsJobReady=as.numeric(Is_Job_Ready)
num_of_child=y

df_here=data.frame("X"=IsJobReady,"Y"=num_of_child)
ggboxplot(df_here, x="X", y="Y")


interaction.plot(x.factor = IsJobReady, trace.factor = internet_statues, 
                 response = num_of_child, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Age", ylab="Number of Children",
                 pch=c(1,19), col = c("red", "blue"))



residuals_state1<-lm(num_of_child ~ internet_statues + IsJobReady)
residuals_state2<-lm(num_of_child ~ internet_statues + IsJobReady + internet_statues*IsJobReady)
summary(residuals_state1)
summary(residuals_state2)
anova(residuals_state2)
anova(residuals_state1,residuals_state2)





residuals_state3_GLM<-glm(num_of_child ~ internet_statues + IsJobReady, family = poisson())
residuals_state4_GLM<-glm(num_of_child ~ internet_statues + IsJobReady + internet_statues*IsJobReady, family = poisson())
summary(residuals_state3_GLM)
summary(residuals_state4_GLM)
anova(residuals_state4_GLM)
anova(residuals_state3_GLM,residuals_state4_GLM)




