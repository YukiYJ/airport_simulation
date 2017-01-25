library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(mixtools)
library(msm)
library(parallel)
#######################################################
testHours <- 0.4
testTime <- ceiling(testHours * 60 * 60)
lambdaPas_all <- 0.83
nPas_all <- lambdaPas_all*testTime
pcr <- 0 #pre-check rate

nDoc_normal <- 45
nBody_normal <- 45
nBag_normal <- 45
meanDoc_normal <- 10
meanBag_normal <- 75
meanBagDrop_normal <- 60 #best 61.5
lambda_normal <- c(0.4,0.4,0.2)
meanBody_normal <- c(15,10,40)
sigma_normal <- c(0.5,0.5,2)
limitLineBag_normal <- 8
limitLineBody_normal <- 1

existingPeople_normal <- 80
existingPeople_pre <- 10
nDoc_pre <- 15
nBag_pre <- 15
nBody_pre <- 15
meanDoc_pre <- 10
meanBag_pre <- 17
meanBagDrop_pre <- 2
lambda_pre <- c(0.4,0.4,0.2)
meanBody_pre <- c(15,10,40)
sigma_pre <- c(0.5,0.5,2)
limitLineBag_pre <- 8
limitLineBody_pre <- 1
#######################################################
#result <- mclapply(1:6, function(indexMc){
#nDoc_normal <- c(15,15,15,14,14,14)[indexMc]
#nDoc_pre <- c(5,6,6,6,7,7)[indexMc]
#nBag_normal <- c(45,44,43,42,41,40)[indexMc]
#nBag_pre <- c(15,16,17,18,19,20)[indexMc]
###################################################
tempResult <- list(list(),list())
nPas_normal <- round(nPas_all*(1 - pcr))
nPas_pre <- round(nPas_all - nPas_normal)
if(pcr <= 0){
  numLoop <- 1
} else{
  numLoop <- 2
}
for(preOrNot in 1:numLoop){
  if(preOrNot == 1){ #calculate normal lines
    nPas <- nPas_normal
    lambdaPas <- lambdaPas_all * (1 - pcr)
    nDoc <- nDoc_normal
    nBag <- nBag_normal
    nBody <- nBody_normal
    meanDoc <- meanDoc_normal
    meanBag <- meanBag_normal
    meanBagDrop <- meanBagDrop_normal
    lambda <- lambda_normal
    meanBody <- meanBody_normal
    sigma <- sigma_normal
    limitLineBag <- limitLineBag_normal
    limitLineBody <- limitLineBody_normal
    existingPeople <- existingPeople_normal
  } else{ #calculate precheck lines
    nPas <- nPas_pre
    lambdaPas <- lambdaPas_all * pcr
    nDoc <- nDoc_pre
    nBag <- nBag_pre
    nBody <- nBody_pre
    meanDoc <- meanDoc_pre
    meanBag <- meanBag_pre
    meanBagDrop <- meanBagDrop_pre
    lambda <- lambda_pre
    meanBody <- meanBody_pre
    sigma <- sigma_pre
    limitLineBag <- limitLineBag_pre
    limitLineBody <- limitLineBody_pre
    existingPeople <- existingPeople_pre
  }
  ##############################################
  #Status of Document Check Counter
  #n  -> Occupied, n stands for current passanger
  #-1 -> Available
  #-2 -> Closed     --------------------------> develop in the future
  
  statusPas <- rep(0,nPas)
  timeLeftPas <- rep(0,nPas)
  doc <- rep(-1,nDoc)
  lineDoc <- data.frame()
  #generate random arrival time by exponential distribution
  timeArrivalPas <- rexp(n=nPas, rate=lambdaPas)
  timeArrivalPas[1:existingPeople] <- 0
  #timeArrivalPas <- rep(0.55,nPas)
  for (i in 2:nPas) {
    timeArrivalPas[i] <- timeArrivalPas[i] + timeArrivalPas[i-1]
  }
  
  timeArrivalPas <- round(timeArrivalPas)
  timeEndPas <- rep(0,nPas)
  timeFinishBagDrop <- rep(0,nPas)
  timeFinishBody <- rep(0,nPas)
  #timeBagDrop <- rep(25,nPas)
  timeBagDrop <- round(rtnorm(nPas, mean = meanBagDrop, sd = 0.1, lower=1, upper=testTime))
  #timeBagDrop <- rep(25,nPas)
  timeDoc <- round(rtnorm(nPas, mean = meanDoc, sd = 1, lower=1, upper=testTime))
  #timeDoc <- rep(10,nPas)
  #timeBag <- rep(15,nPas)
  timeBag <- round(rtnorm(nPas, mean = meanBag, sd = 1, lower=1, upper=testTime))
  #timeDoc[timeDoc <= 0] <- meanDoc
  #timeBag[timeBag <= 0] <- meanBag
  timeBody<-round(rmvnormmix(nPas,lambda, meanBody, sigma))
  timeBody[timeBody <= 0] <- meanBody[1]
  #timeBody <- rep(15,nPas)
  pas <- data.frame(timeArrivalPas, timeDoc, timeBagDrop, timeBody, timeBag, statusPas, timeLeftPas, timeEndPas)
  bag <- rep(-1,nBag)
  body <- rep(-1,nBody)
  lineBag <- data.frame()
  lineBody <- rep(list(data.frame()),nBody)
  lengthLineBody <- data.frame()
  lengthLineDoc <- data.frame()
  lengthLineBag <- data.frame()
  throughput <- data.frame(time = 1:testTime, throughput = rep(0,testTime))
  ######################################################
  #0-have not come
  #1-wait before document check
  #2-document check
  #3-wait before bag scan
  #4-drop bag
  #5-wait before body scan
  #6-body scan
  #7-end(add wait bag time)
  controlDoc <- function(lengthLine){
    if (lengthLine < limitLineBag) {
      return(TRUE)#should not control and stop zone A's running
    }
    else{
      return(FALSE)#shoud stop the running of zone A
    }
  }
  controlBag <- function(lengthLine){
    if (lengthLine < limitLineBody) {
      return(TRUE)#should not control and stop zone A's running
    }
    else{
      return(FALSE)#shoud stop the running of zone A
    }
  }
  bagToBody <- function(num){
    return(ceiling(num/bp))
    #return(num)
  }
  time <- 0
  while (time < testTime) {
    #add people to the line when time is 0
    for (j in 1:nPas){
      #while (as.numeric(pas[j,"timeArrivalPas"]) <= as.numeric(time)) {   
      if(pas[j,"timeArrivalPas"] == time) {
        lineDoc[nrow(lineDoc)+1,1] <- j #move this passenger to the line
        pas[j,"statusPas"] <- 1 #change the status for the passenger
      }
    }
    
    ###PROCESS OF BODY SCAN###
    #2-ITERATE ALL SECURITY CHECK LINES
    for (j in 1:length(body)){ #iterate all security points
      #print(j)
      #cat("timeLeft:",pas[sec[j],"timeLeftPas"])
      lineBodyThis <- lineBody[[j]]
      cat("*****",time,"-",j,"\n")
      print(lineBodyThis)
      if(body[j] == -1){   #2.1-if the counter is available
        if(nrow(lineBodyThis) > 0){#2.1.1-if there are somebody in the line
          body[j] <- lineBodyThis[1,1]  #update status for the security point
          if(nrow(lineBodyThis) <= 1){
            lineBodyThis <- data.frame()
          }
          else{
            lineBodyThis <- data.frame(lineBodyThis[-1,]) #remove the passanger from the line
          }
          pas[body[j],"statusPas"] <- 6
          pas[body[j],"timeLeftPas"] <- pas[body[j],"timeBody"] #countdown the item time
          if(pas[body[j],"timeFinishBagDrop"] == time - 1){
            pas[body[j],"timeLeftPas"] <- pas[body[j],"timeLeftPas"] - 1
          }
        }
      } else if(body[j] > -1){#2.2-if the counter is occupied
        if(pas[body[j],"timeLeftPas"] <= 1){#2.2.1-if the man is ready to go
          #put the passenger into next line
          #lineSec[nrow(lineSec)+1,1] <- doc[j] add this passenger to next line
          #update the passenger status and add picking up bags time
          pas[body[j],"statusPas"] <- 7
          if(time <= testTime){
            throughput[time,2] <- throughput[time,2] + 1#count throughput
          }
          pas[body[j],"timeFinishBody"] <- time
          if(pas[body[j],"timeBag"] > pas[body[j],"timeBody"]) {#if the passenger need to wait for bags
            pas[body[j],"timeEndPas"] <- time + pas[body[j],"timeBag"] - pas[body[j],"timeBody"]
          }
          else{#if the passenger do not need to wait for bags
            pas[body[j],"timeEndPas"] <- time    #stop here
          }
          body[j] <- -1 #remove the passanger from the counter
          if(nrow(lineBodyThis) > 0){
            body[j] <- lineBodyThis[1,1] #get next passenger
            if(nrow(lineBodyThis) <= 1){
              lineBodyThis <- data.frame()
            }
            else{
              lineBodyThis <- data.frame(lineBodyThis[-1,]) #remove the passanger from the line
            }
            pas[body[j],"statusPas"] <- 6 #update passenger status 
            pas[body[j],"timeLeftPas"] <- pas[body[j],"timeBody"] #countdown starts
          }
        }
        else{ #2.2.2-if the man is not ready to go
          pas[body[j],"timeLeftPas"] <- pas[body[j],"timeLeftPas"] - 1#change countdown
        }
      }
      lineBody[[j]] <- lineBodyThis
      print(body[j])
      print(lineBodyThis)
    }
    
    
    ###PROCESS OF DROPPING BAGS###
    #ITERATE ALL DROPPING POINTS
    for (j in 1:length(bag)){
      #Dynamic Control Flow of Dropping Zone
      if(bag[j] == -1){   #2.1-if the counter is available
        if(nrow(lineBag) > 0){#2.1.1-if there are somebody in the line
          bag[j] <- lineBag[1,1]  #update status for the counter
          if(nrow(lineBag) == 1){
            lineBag <- data.frame()
          }
          else{
            lineBag <- data.frame(lineBag[-1,]) #remove the passanger from the line
          }
          pas[bag[j],"statusPas"] <- 4
          pas[bag[j],"timeLeftPas"] <- pas[bag[j],"timeBagDrop"] #countdown the item time
          if(pas[bag[j],"timeFinishA"] == time - 1){
            pas[bag[j],"timeLeftPas"] <- pas[bag[j],"timeLeftPas"] - 1
          }
        }
      } else if(bag[j] > -1){#2.2-if the counter is occupied
        if(pas[bag[j],"timeLeftPas"] == 1 && controlBag(nrow(lineBody[[bagToBody(j)]]))){#2.2.1-if the man is ready to go and lineSec is open
          #put the passenger into next line
          pas[bag[j],"timeFinishBagDrop"] <- time
          #print(bag[j])
          temp <- lineBody[[bagToBody(j)]]
          temp[nrow(temp)+1,1] <- bag[j]
          lineBody[[bagToBody(j)]] <- temp
          #print(temp)
          #print(lineBody[[j]])
          pas[bag[j],"statusPas"] <- 5 #update passenger status
          bag[j] <- -1 #remove the passanger from the counter
          if(nrow(lineBag) > 0){
            bag[j] <- lineBag[1,1] #get next passenger
            if(nrow(lineBag) == 1){
              lineBag <- data.frame()
            }
            else{
              lineBag <- data.frame(lineBag[-1,]) #remove the passanger from the line
            }
            pas[bag[j],"statusPas"] <- 4 #update passenger status 
            pas[bag[j],"timeLeftPas"] <- pas[bag[j],"timeBagDrop"] #countdown starts
          }
        } else if(pas[bag[j],"timeLeftPas"] > 1){#2.2.2-if the man is not ready to go
          pas[bag[j],"timeLeftPas"] <- pas[bag[j],"timeLeftPas"] - 1#change countdown
        }
      }
      #print(lineBody)
    }
    
    
    
    
    ###PROCESS OF DOCUMENT CHECK AND FIRST LINE###
    #ITERATE ALL COUNTERS
    for (j in 1:length(doc)){ #iterate all counters
      #Dynamic Control Flow of Zone A
      if(doc[j] == -1){   #2.1-if the counter is available
        if(nrow(lineDoc) > 0){#2.1.1-if there are somebody in the line
          doc[j] <- lineDoc[1,1]  #update status for the counter
          if(nrow(lineDoc) == 1){
            lineDoc <- data.frame()
          }
          else{
            lineDoc <- data.frame(lineDoc[-1,]) #remove the passanger from the line
          }
          pas[doc[j],"statusPas"] <- 2
          pas[doc[j],"timeLeftPas"] <- pas[doc[j],"timeDoc"] #countdown the item time
        }
      } else if(doc[j] > -1){#2.2-if the counter is occupied
        if(pas[doc[j],"timeLeftPas"] == 1 && controlDoc(nrow(lineBag))){#2.2.1-if the man is ready to go and lineSec is open
          #put the passenger into next line
          pas[doc[j],"timeFinishA"] <- time
          lineBag[nrow(lineBag)+1,1] <- doc[j]
          pas[doc[j],"statusPas"] <- 3 #update passenger status
          doc[j] <- -1 #remove the passanger from the counter
          if(nrow(lineDoc) > 0){
            doc[j] <- lineDoc[1,1] #get next passenger
            if(nrow(lineDoc) == 1){
              lineDoc <- data.frame()
            }
            else{
              lineDoc <- data.frame(lineDoc[-1,]) #remove the passanger from the line
            }
            pas[doc[j],"statusPas"] <- 2 #update passenger status 
            pas[doc[j],"timeLeftPas"] <- pas[doc[j],"timeDoc"] #countdown starts
          }
        } else if(pas[doc[j],"timeLeftPas"] > 1){#2.2.2-if the man is not ready to go
          pas[doc[j],"timeLeftPas"] <- pas[doc[j],"timeLeftPas"] - 1#change countdown
        }
      }
    }
    lengthLineDoc[nrow(lengthLineDoc)+1,1] <- nrow(lineDoc)
    lengthLineBag[nrow(lengthLineBag)+1,1] <- nrow(lineBag)
    #print(lineSec)
    #cat("sec[j]b:",sec[j])
    time <- time + 1
  }
  #print(lengthLineDoc)
  pas <- pas %>%
    mutate(waitTime = timeEndPas - timeArrivalPas) %>%
    filter(waitTime >= 0)
  lengthLineDoc <- lengthLineDoc %>%
    slice(seq(1,testTime))
  lengthLineBag <- lengthLineBag %>%
    slice(seq(1,testTime))
  tempResult[[preOrNot]] <- c(pas = pas, lengthLineDoc = lengthLineDoc, lengthLineBag = lengthLineBag)
  ###############################################
}
#tempResult
#Plot of Wait Time
temp1 <- data.frame(waitTime = tempResult[[1]]$pas.waitTime, tag = "normal")
if(pcr <= 0){
  waitTime_sum <- temp1
  meanWaitTime_normal <- mean(temp1$waitTime)
  varWaitTime_normal <- var(temp1$waitTime)
} else{
  temp2 <- data.frame(waitTime = tempResult[[2]]$pas.waitTime, tag = "pre")
  meanWaitTime_pre <- mean(temp2$waitTime)
  varWaitTime_pre <- var(temp2$waitTime)
  waitTime_sum <- rbind(temp1, temp2)
}

#Plot of lengthLineDoc
temp1 <- data.frame(time = c(1:length(tempResult[[1]]$lengthLineDoc.V1)), num = tempResult[[1]]$lengthLineDoc.V1, type = "regular")
if(pcr <= 0){
  lengthLineDoc_sum <- temp1
} else{
  temp2 <- data.frame(time = c(1:length(tempResult[[2]]$lengthLineDoc.V1)), num = tempResult[[2]]$lengthLineDoc.V1, type = "pre")
  lengthLineDoc_sum <- rbind(temp1, temp2) 
}

#Plot of lengthLineBag
temp1 <- data.frame(time = c(1:length(tempResult[[1]]$lengthLineBag.V1)), num = tempResult[[1]]$lengthLineBag.V1, type = "regular")
if(pcr <= 0){
  lengthLineBag_sum <- temp1
} else{
  temp2 <- data.frame(time = c(1:length(tempResult[[2]]$lengthLineBag.V1)), num = tempResult[[2]]$lengthLineBag.V1, type = "pre")
  lengthLineBag_sum <- rbind(temp1, temp2) 
}

plot_waitTime <- waitTime_sum %>%
  ggplot(aes(x=factor(tag), y = waitTime+500)) +
  geom_violin() +
  labs(x="Precheckness", y ="Waiting Time")

plot_lengthLineDoc <- lengthLineDoc_sum %>%
  ggplot(aes(x = time, y = num ,color=type)) + 
  geom_point() +
  labs(x="time/s", y ="Number of People Waiting for Document Checking")

plot_lengthLineBag <- lengthLineBag_sum %>%
  ggplot(aes(x = time, y = num ,color=type)) + 
  geom_point() +
  labs(x="time/s", y ="Number of People Waiting for Taking off Clothes")

plot_throughput <- throughput %>%
  ggplot(aes(x = time, y = throughput)) +
  geom_point() +
  labs(x="time/s", y = "Throughput at That Second")
#plot(pas$waitTime)
#plot(lengthLineDoc[,1])
#plot(lengthLineSec[,1])
tp <- (nrow(pas) - ceiling(nrow(pas)/2)) / (time - pas[ceiling(nrow(pas)/2),"timeEndPas"])
#c(tp,varWaitTime_normal)
#}, mc.cores=20)

##########TEST2
#tp_all <- c()
#var_all <- c()
#for (i in 1:length(result)){
#  tp_all <- c(tp_all,result[[i]][1])
#  var_all <- c(var_all,result[[i]][2])
#}
#final_result <- data.frame(numOfA = c(15:25), throughput = tp_all, varience = var_all)

#pdf("throughput.pdf")
#final_result %>%
#  ggplot(aes(x=numOfA, y=throughput)) + geom_point()
#dev.off()
#pdf("verience.pdf")
#final_result %>%
#  ggplot(aes(x=numOfA, y=varience)) + geom_point()
#dev.off()



#for (i in 2:nrow(throughput)) {
#  throughput[i,2] <- throughput[i,2] + throughput[i-1,2]
#}
#tp_new <- (length(tempResult[[1]]$pas.waitTime) + length(tempResult[[2]]$pas.waitTime))/testTime
mean(tempResult[[1]]$pas.waitTime)
var(tempResult[[1]]$pas.waitTime)
length(tempResult[[1]]$pas.waitTime)/testTime
