library(parallel)
library(doParallel)
library(GA)

#Simple genetic algorithm
dataset<-read.csv("Rotogrinders MLB Projections 6 15 2016.csv", stringsAsFactors = FALSE)
dataset$position <- as.factor(dataset$position)
str(dataset)

table(dataset$position)
salarylimit <- 50000


#Set initial population
#Ensure that the intitial population meets all the criteria in the evaluation function
initialPopul <- rep(0,nrow(dataset))
initialPopul[8]<-1
initialPopul[24]<-1
initialPopul[7]<-1
initialPopul[23]<-1
initialPopul[4]<-1
initialPopul[5]<-1
initialPopul[10]<-1
initialPopul[3]<-1
initialPopul[18]<-1
initialPopul[125]<-1

#View initial population and initial population values
sum(dataset$salary[initialPopul==1])
sum(dataset$fpts[initialPopul==1])
dataset$player[initialPopul==1]
dataset$position[initialPopul==1]

#Define fitness function to ensure that solutions meet criteria
evalFunc <- function(x) {
  current_solution_salary <- x %*% dataset$salary
  current_solution_fpts <- x %*% dataset$fpts
  
  if(sum(dataset$position[x==1] == "1B")  ==  0 &&
     sum(dataset$position[x==1] == "1B/2B") ==0 &&
     sum(dataset$position[x==1] == "1B/3B") ==0 &&
     sum(dataset$position[x==1] == "1B/OF") ==0 &&
     sum(dataset$position[x==1] == "1B/SS") ==0 &&
     sum(dataset$position[x==1] == "1B/C")  ==0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "2B")  ==  0 &&
     sum(dataset$position[x==1] == "1B/2B") ==0 &&
     sum(dataset$position[x==1] == "2B/3B") ==0 &&
     sum(dataset$position[x==1] == "2B/OF") ==0 &&
     sum(dataset$position[x==1] == "2B/SS") ==0 &&
     sum(dataset$position[x==1] == "2B/C")  ==0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "3B")  ==  0 &&
     sum(dataset$position[x==1] == "1B/3B") ==0 &&
     sum(dataset$position[x==1] == "2B/3B") ==0 &&
     sum(dataset$position[x==1] == "3B/OF") ==0 &&
     sum(dataset$position[x==1] == "3B/SS") ==0 &&
     sum(dataset$position[x==1] == "3B/C")  ==0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "SS")  ==  0 &&
     sum(dataset$position[x==1] == "1B/SS") ==0 &&
     sum(dataset$position[x==1] == "2B/SS") ==0 &&
     sum(dataset$position[x==1] == "OF/SS") ==0 &&
     sum(dataset$position[x==1] == "3B/SS") ==0 &&
     sum(dataset$position[x==1] == "C/SS")  ==0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "C")  ==  0 &&
     sum(dataset$position[x==1] == "1B/C") ==0 &&
     sum(dataset$position[x==1] == "2B/C") ==0 &&
     sum(dataset$position[x==1] == "3B/C") ==0 &&
     sum(dataset$position[x==1] == "C/OF") ==0 &&
     sum(dataset$position[x==1] == "C/SS")  ==0    )
    return(0)
  
  if(  sum(x)>10                             ||
       current_solution_salary > salarylimit ||
       sum(dataset$position[x==1] =="1B")>1  ||
       
       sum(dataset$position[x==1] =="2B")>1  ||
       sum(dataset$position[x==1] =="3B")>1  ||
       sum(dataset$position[x==1] =="SS")>1  || 
       sum(dataset$position[x==1] =="OF")>3  || 
       sum(dataset$position[x==1] =="C") >1  ||
       sum(dataset$position[x==1] =="SP")>2)
    return(0) else return(current_solution_fpts)
  
}

#Run gnetic algorithm
#Modify the parameters to find more optimal solutions
GAmodel <-ga(type="binary",nBits = nrow(dataset), fitness=evalFunc,
             suggestions = initialPopul, popSize=100, monitor = TRUE,
             pmutation = .6, pcrossover = .9,
             maxiter=1000, parallel= TRUE, names = dataset$player)


#View the solution
sol <-summary(GAmodel)$solution
dataset[sol==1]

