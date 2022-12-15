#The Goal is to practice modelling and analyzing plant growth in an effort to have something ready for KeeFarms
#Should only need tweaking eventually
#Do a Markdown too later

#To Do
#add some visualization to the growth rates and curve fitting


#Attach data
dataset<-Orange
#dataset <- read_excel(NULL)

#Assess the data to see if it is missing anything and reveal the formatting
Clean.and.Analyze<-function(dataset){
  data<-dataset
  #glimpse(data)
  full.record<-complete.cases(data)
  data<-data[full.record,] # Removes NA. will have to figure out how to treat with that
  #print(summary(data))
  return(data)
}


Basic.Visualizing<-function(dataset){
  data<-dataset
  print(summary(data))
  glimpse(data)
  p<- data %>% ggplot(mapping=aes(x = age, y = circumference, colour = Tree, text = paste("ID: Tree #", Tree)))+
    geom_point(alpha = 0.6, size = 1.8)+
    geom_line(alpha = 0.6)+
    geom_abline(intercept = 17.39, slope = 0.1068)+#taken from a linear model
    labs(title = "Circumferences of Orange Trees at different ages", x="Age(days)", y="Circumference(cm)")
 p.interactive<-ggplotly(p, tooltips = c("x","y", "text"))
 print(p.interactive)
 p2<-pairs(data[-1,])
 print(paste(" Pairwise comparisons of the variables are as follows:", p2))
 p3<-cor(data$circumference, data$age)
 print(paste("The correlation bewteen age and circumference is:", p3))
}

#Need to see whats happening here
testing<-function(dataset){
  print(pairs(dataset))
  print(cor(dataset$age,dataset$circumference))
  linear.model<-lm(dataset$circumference~dataset$age)
  print(linear.model)
  print(paste("Summary of Linerar Model:", summary(linear.model)))
  #Basic.Visualizing(dataset)
  View(linear.model)
  
  #get mean and standard dev
  #sd(dataset$circumference)
  #mean(dataset$circumference)
  
  #Attempt to Get mean of each before analyzing
  t<-dataset
  t<-t %>% group_by(age) %>% summarise(circumference = mean(circumference))
  graph.test<-ggplot(t, mapping = aes(x = age, y= circumference))+
    geom_point(alpha = 0.8, size =2)+
    geom_line(colour = "blue", size = 1)+
    geom_abline(intercept = 17.39, slope = 0.1068, colour = "green")+
    labs(x="age", y="mean circumference", title = "Linear model and mean circumference", subtitle = "circumference = 17.39 + 0.1068 *age(days)")+
    theme(plot.title = element_text(hjust = 0.5))
  print(graph.test)
  
  graph.test<-ggplot(t, mapping = aes(x = age, y= circumference))+
    geom_point(alpha = 0.8, size =2)+
    geom_line(colour = "blue", size = 1)+
    geom_smooth(linetype = "dashed", colour = "red", se = FALSE)+
    labs(x="age", y="mean circumference", title = "Smooth Line (loess) and mean circumference")+
    theme(plot.title = element_text(hjust = 0.5))
  print(graph.test)
}   

#Attempt the Multiple Polynomial fitting thing, now it works still needs to be visualized
#https://www.statology.org/curve-fitting-in-r/
Curve.Fitting.Attempt<-function(dataset){
  d<-dataset[,c(2,3)]
  age2<-d$age^2
  age3<-d$age^3
  age4<-d$age^4
  model1<-lm(circumference ~ age, data = d)
  model2<-lm(circumference ~ age + age2,data = d)
  model3<-lm(circumference ~ age + age2 + age3,data = d)
  model4<-lm(circumference ~ age + age2 + age3 + age4,data = d)
  model1.summary<-aov(model1)
  model2.summary<-aov(model2)
  model3.summary<-aov(model3)
  # look at sgnificance added by each term to see which model is sufficient
  print("Summary of the linear model")
  print(summary(model1.summary))
  print("Summary of the quadratic model")
  print(summary(model2.summary))
  print("Summary of the cubic model")
  print(summary(model3.summary))
  print("Summary of the best model")
  print(summary(model1))# model 1 in this case, but whichever model is best
}


#may need to redo the math, but the structure is ok generally, very much in progress
# VERY MUCH A WORK IN PROGRESS
Calc.Doubling.Rate<-function(dataset){
  data<- dataset %>% group_by(Tree) %>% mutate(prev.age = lag(age, 1, default = NA))
  data<- data %>% mutate(prev.circumference = lag(circumference, 1, default = NA))
  data<- data %>% mutate(k = (circumference - prev.circumference)/(age-prev.age))
  data<- data %>% mutate(percent.k = (k/prev.circumference)*100)
    
    # work with growth in percentage terms
    perc.growth.rates<-na.omit(data$percent.k)
    perc.growth.rates<-perc.growth.rates[is.finite(perc.growth.rates)]
    avg.percent.growth<-round(mean(perc.growth.rates),4)
    sd.percent.growth<-round(sd(perc.growth.rates),4)
    
    
    #work with growth in absolute terms
    growth.rates<-na.omit(data$k)
    growth.rates<-growth.rates[is.finite(growth.rates)]
    avg.growth.rate<-round(mean(growth.rates),4)
    sd.growth.rate<-round(sd(growth.rates),4)
    
    
    doubling.time<-(log(2)/log((1+(avg.percent.growth/100))))# this eqn might need to be modified.
    doubling.time<-round(doubling.time)
    #where % k is used in calculating doubling time, it must be expressed as a decimal again, divide by 100
    
    print(paste("The mean growth rate is to be", avg.growth.rate, " cm per day + or -", sd.growth.rate))
    print(paste("The mean percentage growth rate is to be", avg.percent.growth, " % per day + or -", sd.percent.growth))
    print(paste("The estimated mean time to double in size is ", doubling.time, " days(to the nearest day)"))
    
    #Time for a whole mess of visualizations
    
    graph.1<-ggplot(data, mapping = aes(x =age, y = k, colour = Tree))+
      geom_point(alpha = 0.8, size = 2)+
      geom_line(alpha = 0.8)+
      geom_hline(yintercept = avg.growth.rate, linetype = "dashed", size = 1)+
      #geom_ribbon(alpha = 0.1, aes(ymin=(avg.growth.rate-sd.growth.rate), ymax=(avg.growth.rate+sd.growth.rate)))+ #shows std deviation
      labs(x="age(days)", y = "Growth Rate(k), cm/day", title="Growth Rate showing mean growth rate")+
      theme(plot.title = element_text(hjust = 0.5))
    print(graph.1)
   
    
    graph.3<-ggplot(data, mapping = aes(x =age, y = percent.k, colour = Tree))+
      geom_point(alpha = 0.8, size = 2)+
      geom_line(alpha = 0.8)+
      labs(x="age(days)", y = "percentage Growth Rate(k)", title="Percentage Growth Rate showing mean")+
      geom_hline(yintercept = avg.percent.growth, linetype = "dashed", size = 1)+
      #geom_ribbon(alpha =0.1, aes(ymax=avg.percent.growth+sd.percent.growth, ymin = avg.percent.growth-sd.percent.growth))+#shows std deviation
      theme(plot.title = element_text(hjust = 0.5))
    print(graph.3)
    
    graph.4<-data %>% ggplot(mapping = aes(y = k, colour = Tree))+
      geom_boxplot()+
      labs(title = "Box plot of growth rate")+
      theme(plot.title = element_text(hjust = 0.5))
    print(graph.4)  
    
    graph.5<-data %>% ggplot(mapping = aes(y = percent.k, colour = Tree))+
      geom_boxplot()+
      labs(title = "Box plot of percentage growth rate")+
      theme(plot.title = element_text(hjust = 0.5))
    print(graph.5)
    
    graph6<-data |> ggplot(mapping = aes(x = k, colour = Tree))+
      geom_density()+
      labs(title = "Density plot of k")+
      theme(plot.title = element_text(hjust = 0.5))
    print(graph6)
    
    return(data)
}

# all those scattered visualizations are hogging the rest, throw em here
#work on this
Other.Visualizations<-function(dataset){
  data<- dataset
  
  graph.1<-ggplot(data, mapping = aes(x =age, y = k, colour = Tree))+
    geom_point(alpha = 0.8, size = 2)+
    geom_line(alpha = 0.8)+
    labs(x="age(days)", y = "Growth Rate(k), cm/day", title="Growth Rate")+
    theme(plot.title = element_text(hjust = 0.5))
  print(graph.1)
  print("graph 1 done")
  
  graph.2<-ggplot(data, mapping = aes(x =age, y = k, colour = Tree))+
    geom_point(alpha = 0.8, size = 2)+
    geom_line(alpha = 0.8)+
    geom_hline(yintercept = avg.growth.rate, linetype = "dashed", size = 1.2)+
    labs(x="age(days)", y = "Growth Rate(k), cm/day", title="Growth Rate")+
    theme(plot.title = element_text(hjust = 0.5))
  print(graph.2)
  print("graph 2 done")
  
  graph.3<-ggplot(data, mapping = aes(x =age, y = percent.k, colour = Tree))+
    geom_point(alpha = 0.8, size = 2)+
    geom_line(alpha = 0.8)+
    labs(x="age(days)", y = "percentage Growth Rate(k)", title="Percentage Growth Rate")+
    theme(plot.title = element_text(hjust = 0.5))
  print(graph.3)
  
  graph.4<-data %>% ggplot(mapping = aes(y = k, colour = Tree))+
    geom_boxplot()+
    labs(title = "Box plot of growth rate")
  print(graph.4)  
  
  graph.5<-data %>% ggplot(mapping = aes(y = percent.k, colour = Tree))+
    geom_boxplot()+
    labs(title = "Box plot of percentage growth rate")
  print(graph.5)
  
  graph6<-data |> ggplot(mapping = aes(x = k, colour = Tree))+
    geom_density()+
    labs("Density plot of k")
  print(graph6)
}


#Load packages
library(tidyverse)
library(ggpubr)
library(lubridate)
library(plotly)
library(readxl)

#This is the actual thing
#dataset<-Orange
#dataset <- read_excel(NULL) This will be where i bring in the seaweed data eventually
dataset<-Clean.and.Analyze(dataset)
Basic.Visualizing(dataset)
#testing(dataset)
Curve.Fitting.Attempt(dataset)
dataset<-Calc.Doubling.Rate(dataset)
#Other.Visualizations(dataset)



