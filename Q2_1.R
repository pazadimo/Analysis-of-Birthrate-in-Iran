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
for (i in 1:9) {
  for (j in 1:4) {
    mother[i,j]=0
  }
}
for (i in 1:1481586) {
  y=Data[i,]
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
      index=(as.numeric(y[18]) + as.numeric(y[19]));
      mother[index,1]=as.numeric(mother[index,1])+1
      mother[index,3]=as.numeric(mother[index,3])+as.numeric(y[18])
      mother[index,4]=as.numeric(mother[index,4])+as.numeric(y[19])
      if(y[7]==1){
        mother[index,2]=as.numeric(mother[index,2])+1
      }
      
 #     if(index==1){
#        age1=c(age1,y[4])
 #     }
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

y = c(1,2,3,4,5,6,7,8)
x = c(.9391,.8983,.7828,.6021,.4297,.3066,.2180,.1382)

plot(x,y, main="scatter plot", xlab = "Percent of Knowledge", ylab = "Number of Child")
plot.new()
plot(x,y, main="scatter plot", xlab = "Percent of Knowledge", ylab = "Number of Child")
abline(lm(y~x))
fit <- lm( y ~ x)
print(fit$coefficients)
res = fit$residuals
standard_error = sd(res)
print(standard_error)


