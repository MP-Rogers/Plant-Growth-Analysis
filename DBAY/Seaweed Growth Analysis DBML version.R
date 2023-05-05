###
#This version of the analysis has names changed to accommodate the Variables in the "Seaweed Data" sheet
#Began Editting for Seaweed Data Nov 9, 2022
#Add an aesthetic for different species
#First round of changes done on "Basic.Visualizing", "Clean.and.Analyze", "Curve.Fitting", and "Calc.Doubling.Rate"
###

#Assess the data to see if it is missing anything and reveal the formatting
Clean.and.Analyze<-function(dataset){
  names<-colnames(dataset)
  names[4]<-"Mass"
  names[5]<-"Day"
  #names[5]<-"Length"
  colnames(dataset)<-names
  dataset$Identifier<-as.factor(dataset$Identifier)
  dataset$Method<-as.factor(dataset$Method)
  dataset$Day<-as.Date(dataset$Day, format= "%d/%m/%Y")
  data<-dataset
  glimpse(data)
  #full.record<-complete.cases(data)
  #data<-data[full.record,] # Removes NA. will have to figure out how to treat with that
  #print(summary(data))
  return(data)
}

Basic.Visualizing<-function(dataset){
  data<-dataset
  print(summary(data))
  #glimpse(data)
  p<- data %>% ggplot(mapping=aes(x = Day, y = Mass, colour = Identifier))+
    geom_point(alpha = 0.6, size = 1.8, shape = data$Method)+
    geom_line(alpha = 0.6)+
    #geom_abline(intercept = 17.39, slope = 0.1068)+#taken from a linear model
    #Maybe add an aesthetic for species
    labs(title = "Mass of Seaweed Samples at Different Days", x="Day", y="Mass(grams)")
 p.interactive<-ggplotly(p, tooltips = c("x","y", "text"))
 print(p.interactive)
 #numeric.data<-data |> select_if(is.numeric)
 #p2<-pairs(numeric.data)
 #print(paste(" Pairwise comparisons of the variables are as follows:", p2))
 #p3<-cor(numeric.data)
 #cor.plot<-plot_ly(p3, type = "heatmap")
 #print(cor.plot)
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
  age2<-d$Day^2
  age3<-d$Day^3
  age4<-d$Day^4
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


#the structure is ok generally, very much in progress
Calc.Doubling.Rate<-function(dataset){
  
  data<- dataset %>% group_by(Identifier) %>% mutate(prev.Day = lag(Day, 1, default = NA))
  
  data<- data %>% mutate(prev.Mass = lag(Mass, 1, default = NA))
  data<- data %>% mutate(k = (Mass - prev.Mass)/as.double.difftime(Day - prev.Day, unit = "days"))
  data<- data %>% mutate(percent.k = (k/prev.Mass)*100)
    
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
    
    print(paste("The mean growth rate is to be", avg.growth.rate, " grams per day + or -", sd.growth.rate))
    print(paste("The mean percentage growth rate is to be", avg.percent.growth, " % per day + or -", sd.percent.growth))
    print(paste("The estimated mean time to double in size is ", doubling.time, " days(to the nearest day)"))
    
    
    #Time for a whole mess of visualizations
    
    graph.1<-ggplot(data, mapping = aes(x = Day, y = k, colour = Identifier))+
      geom_point(alpha = 0.8, size = 2)+
      geom_line(alpha = 0.8)+
      geom_hline(yintercept = avg.growth.rate, linetype = "dashed", size = 1)+
      #geom_ribbon(alpha = 0.1, aes(ymin=(avg.growth.rate-sd.growth.rate), ymax=(avg.growth.rate+sd.growth.rate)))+ #shows std deviation
      labs(x="Days", y = "Growth Rate(k), g/day", title="Growth Rate showing mean growth rate")+
      theme(plot.title = element_text(hjust = 0.5))
    graph.1<-ggplotly(graph.1)
    print(graph.1)
    
    graph.3<-ggplot(data, mapping = aes(x = Day, y = percent.k, colour = Identifier))+
      geom_point(alpha = 0.8, size = 2)+
      geom_line(alpha = 0.8)+
      labs(x="Days", y = "percentage Growth Rate(k)", title="Percentage Growth Rate showing mean")+
      geom_hline(yintercept = avg.percent.growth, linetype = "dashed", size = 1)+
      #geom_ribbon(alpha =0.1, aes(ymax=avg.percent.growth+sd.percent.growth, ymin = avg.percent.growth-sd.percent.growth))+#shows std deviation
      theme(plot.title = element_text(hjust = 0.5))
    graph.3<-ggplotly(graph.3)
    print(graph.3)
    
    
    
    return(data)
}

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
#dataset <- read_excel("Seaweed Data.xlsx",  sheet = "Growth Assesment 2.1") #This will be where i bring in the seaweed data eventually
dataset<-read_excel("DBML Seaweed Data.xlsx")
dataset<-Clean.and.Analyze(dataset)
Basic.Visualizing(dataset)
#testing(dataset)
#Curve.Fitting.Attempt(dataset)
dataset<-Calc.Doubling.Rate(dataset)
#Other.Visualizations(dataset)


#Notes
#add mean growth rates for every week
#have a total mass for each day and track that
#have avg mass per day and plot that
