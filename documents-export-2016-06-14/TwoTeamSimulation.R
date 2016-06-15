## Team Names : 
# ARI = Arizona Cardinals       ATL = Atlanta Falcons         BAL = Baltimore Ravens
# BUF = Buffalo Bills           CAR = Carolina Panthers       CHI = Chicago Bears
# CIN = Cincinnati Bengals      CLE = Cleveland Browns        DAL = Dallas Cowboys
# DEN = Denver Broncos          DET = Detroit Lions           GB = Green Bay Packers
# HOU = Houston Texans          IND = Indinanapolis Cults     JAC = Jacksonville Jaguars
# KC = Kansas City Chiefs       MIA = Miami Dolphins          MIN = Minnesota Vikings
# NE = New England Patriots     NO = New Orleans Saints       NYG = New York Giants
# NYJ = New York Jets           OAK = Oakland Raiders         PHI = Philidelphia Eagles
# PIT = Pittsburgh Steelers     SD = San Diego Chargers       SEA = Seattle Seahawks 
# SF = San Francisco 49ers      STL = Saint Louis Rams        TB = Tampa Bay Buccaneers
# TEN = Tennessee Titans        WAS = Washington Redskins 

###load Mosaic package in order to use "do" function
library(mosaic) 
library(dplyr)

##Get data file
pbp<-read.csv(file.choose(),header=TRUE)

#### Identifies team to look at ####
team.idA="BUF"
team.idB="BUF"

#### Creates data frame with just the team ####
TEAMA=subset(pbp,OffenseTeam == team.idA)

TEAMB=subset(pbp,OffenseTeam == team.idB)

#### get kickoff distances ####
KickOffsA=subset(TEAMA,TEAMA$PlayType=="KICK OFF")

KickOffsB=subset(TEAMB,TEAMB$PlayType=="KICK OFF")


#### Computes percentages of Rush, Pass, Sacks, Incompletes for a team when they have one or two downs ####
##Reduces data set where downs is 1 or 2
TEAM.1.2.downA=subset(TEAMA,TEAMA$Down<3 & TEAMA$Down!=0)

#### calculates number of plays in order to get percentages ####
num.playsA=as.numeric(sum(TEAM.1.2.downA$IsPass)+sum(TEAM.1.2.downA$IsRush)+sum(TEAM.1.2.downA$IsSack)+
                        sum(TEAM.1.2.downA$IsInterception)+sum(TEAM.1.2.downA$IsFumble))

TEAM.1.2.downB=subset(TEAMB,TEAMB$Down<3 & TEAMB$Down!=0)

#### calculates number of plays in order to get percentages ####
num.playsB=as.numeric(sum(TEAM.1.2.downB$IsPass)+sum(TEAM.1.2.downB$IsRush)+sum(TEAM.1.2.downB$IsSack)+
                        sum(TEAM.1.2.downB$IsInterception)+sum(TEAM.1.2.downB$IsFumble))


####Calculates percent passes, rushes, etc ####
perc.passA = sum(TEAM.1.2.downA$IsPass)/num.playsA
#perc.pass *100

perc.rushA=sum(TEAM.1.2.downA$IsRush)/num.playsA
#perc.rush*100

perc.sackA=sum(TEAM.1.2.downA$IsSack)/num.playsA
perc.interceptedA=sum(TEAMA$IsInterception)/num.playsA
perc.fumbleA=sum(TEAMA$IsFumble)/num.playsA
perc.toA=perc.interceptedA+perc.fumbleA

perc.penaltyA=sum(TEAM.1.2.downA$IsPenaltyAccepted)/num.playsA

perc.passB = sum(TEAM.1.2.downB$IsPass)/num.playsB
#perc.pass *100

perc.rushB=sum(TEAM.1.2.downB$IsRush)/num.playsB
#perc.rush*100

perc.sackB=sum(TEAM.1.2.downB$IsSack)/num.playsB
perc.interceptedB=sum(TEAMB$IsInterception)/num.playsB
perc.fumbleB=sum(TEAMB$IsFumble)/num.playsB
perc.toB=perc.interceptedB+perc.fumbleB

perc.penaltyB=sum(TEAM.1.2.downB$IsPenaltyAccepted)/num.playsB


####Rushing yards for a team####
rushA=subset(TEAMA,IsRush==1)$Yards

rushB=subset(TEAMB,IsRush==1)$Yards
#summary(rush)

####Sacking yards####
sackA=subset(TEAMA,IsSack==1)$Yards
sackB=subset(TEAMB,IsSack==1)$Yards
#summary(sack)

####Passing Yards for a team####
passA=subset(TEAMA,IsPass==1 & IsIncomplete==0)$Yards
passB=subset(TEAMB,IsPass==1 & IsIncomplete==0)$Yards
#summary(pass)

#### third down strategy ####
##Repeats above but only for third downs (not enough 4th downs to include)

third.down.longA=subset(TEAMA,TEAMA$Down==3 &TEAMA$ToGo>3 &(IsPass==1|IsRush==1|IsSack==1|IsFumble==1|IsInterception==1))
third.down.shortA=subset(TEAMA,TEAMA$Down==3 & TEAMA$ToGo<4&(IsPass==1|IsRush==1|IsSack==1|IsFumble==1|IsInterception==1))

num.3downs.longA=as.numeric(sum(TEAMA$Down==3 &TEAMA$ToGo>3&(TEAMA$IsPass==1|TEAMA$IsRush==1|TEAMA$IsSack==1|TEAMA$IsFumble==1|TEAMA$IsInterception==1)))
num.3downs.shortA=as.numeric(sum(TEAMA$Down==3 & TEAMA$ToGo<4&(TEAMA$IsPass==1|TEAMA$IsRush==1|TEAMA$IsSack==1|TEAMA$IsFumble==1|TEAMA$IsInterception==1)))

third.down.longB=subset(TEAMB,TEAMB$Down==3 &TEAMB$ToGo>3 &(IsPass==1|IsRush==1|IsSack==1|IsFumble==1|IsInterception==1))
third.down.shortB=subset(TEAMB,TEAMB$Down==3 & TEAMB$ToGo<4&(IsPass==1|IsRush==1|IsSack==1|IsFumble==1|IsInterception==1))

num.3downs.longB=as.numeric(sum(TEAMB$Down==3 &TEAMB$ToGo>3&(TEAMB$IsPass==1|TEAMB$IsRush==1|TEAMB$IsSack==1|TEAMB$IsFumble==1|TEAMB$IsInterception==1)))
num.3downs.shortB=as.numeric(sum(TEAMB$Down==3 & TEAMB$ToGo<4&(TEAMB$IsPass==1|TEAMB$IsRush==1|TEAMB$IsSack==1|TEAMB$IsFumble==1|TEAMB$IsInterception==1)))

#### calculate percent third down plays ####

perc.pass.longA=sum(as.numeric(third.down.longA$IsPass==1&third.down.longA$IsFumble==0&third.down.longA$IsInterception==0))/num.3downs.longA
perc.rush.longA=sum(as.numeric(third.down.longA$IsRush==1&third.down.longA$IsFumble==0&third.down.longA$IsInterception==0))/num.3downs.longA

perc.pass.shortA=sum(as.numeric(third.down.shortA$IsPass==1&third.down.shortA$IsFumble==0&third.down.shortA$IsInterception==0))/num.3downs.shortA
perc.rush.shortA=sum(as.numeric(third.down.shortA$IsRush==1&third.down.shortA$IsFumble==0&third.down.shortA$IsInterception==0))/num.3downs.shortA

perc.sack.longA=sum(as.numeric(third.down.longA$IsSack==1&third.down.longA$IsFumble==0&third.down.longA$IsInterception==0))/num.3downs.longA
perc.sack.shortA=sum(as.numeric(third.down.shortA$IsSack==1&third.down.shortA$IsFumble==0&third.down.shortA$IsInterception==0))/num.3downs.shortA


perc.pass.longB=sum(as.numeric(third.down.longB$IsPass==1&third.down.longB$IsFumble==0&third.down.longB$IsInterception==0))/num.3downs.longB
perc.rush.longB=sum(as.numeric(third.down.longB$IsRush==1&third.down.longB$IsFumble==0&third.down.longB$IsInterception==0))/num.3downs.longB

perc.pass.shortB=sum(as.numeric(third.down.shortB$IsPass==1&third.down.shortB$IsFumble==0&third.down.shortB$IsInterception==0))/num.3downs.shortB
perc.rush.shortB=sum(as.numeric(third.down.shortB$IsRush==1&third.down.shortB$IsFumble==0&third.down.shortB$IsInterception==0))/num.3downs.shortB

perc.sack.longB=sum(as.numeric(third.down.longB$IsSack==1&third.down.longB$IsFumble==0&third.down.longB$IsInterception==0))/num.3downs.longB
perc.sack.shortB=sum(as.numeric(third.down.shortB$IsSack==1&third.down.shortB$IsFumble==0&third.down.shortB$IsInterception==0))/num.3downs.shortB


##turnover percents
perc.to.longA=sum(third.down.longA$IsFumble+third.down.longA$IsInterception)/num.3downs.longA
perc.to.shortA=sum(third.down.shortA$IsFumble+third.down.shortA$IsInterception)/num.3downs.shortA

perc.penalty.longA = sum(third.down.longA$IsPenaltyAccepted)/num.3downs.longA
perc.penalty.shortA = sum(third.down.shortA$IsPenaltyAccepted)/num.3downs.shortA

num.3downA = as.numeric(num.3downs.shortA+num.3downs.longA)
perc.penalty.thirdA = sum(third.down.shortA$IsPenaltyAccepted + third.down.longA$IsPenaltyAccepted)/num.3downA



perc.to.longB=sum(third.down.longB$IsFumble+third.down.longB$IsInterception)/num.3downs.longB
perc.to.shortB=sum(third.down.shortB$IsFumble+third.down.shortB$IsInterception)/num.3downs.shortB

perc.penalty.longB = sum(third.down.longB$IsPenaltyAccepted)/num.3downs.longB
perc.penalty.shortB = sum(third.down.shortB$IsPenaltyAccepted)/num.3downs.shortB

num.3downB = as.numeric(num.3downs.shortB+num.3downs.longB)
perc.penalty.thirdB = sum(third.down.shortB$IsPenaltyAccepted + third.down.longB$IsPenaltyAccepted)/num.3downB

#### calculate the yards earned on third down plays ####

pass.longA=subset(third.down.longA,IsPass==1&IsFumble==0&IsInterception==0)$Yards
rush.longA=subset(third.down.longA,IsRush==1&IsFumble==0&IsInterception==0)$Yards

pass.shortA=subset(third.down.shortA,IsPass==1&IsFumble==0&IsInterception==0)$Yards
rush.shortA=subset(third.down.shortA,IsRush==1&IsFumble==0&IsInterception==0)$Yards

sack.shortA=subset(third.down.shortA,IsSack==1&IsFumble==0&IsInterception==0)$Yards
sack.longA=subset(third.down.longA,IsSack==1&IsFumble==0&IsInterception==0)$Yards




pass.longB=subset(third.down.longB,IsPass==1&IsFumble==0&IsInterception==0)$Yards
rush.longB=subset(third.down.longB,IsRush==1&IsFumble==0&IsInterception==0)$Yards

pass.shortB=subset(third.down.shortB,IsPass==1&IsFumble==0&IsInterception==0)$Yards
rush.shortB=subset(third.down.shortB,IsRush==1&IsFumble==0&IsInterception==0)$Yards

sack.shortB=subset(third.down.shortB,IsSack==1&IsFumble==0&IsInterception==0)$Yards
sack.longB=subset(third.down.longB,IsSack==1&IsFumble==0&IsInterception==0)$Yards


#incomplete.pass.long=subset(third.down.long,IsPass==1&IsIncomplete==1)$Yards
#incomplete.pass.short=subset(third.down.short,IsPass==1&IsIncomplete==1)$Yards

##Incompletes

#### Field Goal Function ####

FG.fun=function(yard){
  FG.yards.done = subset(pbp, FG.distance >= yard - 3 & FG.distance <= yard + 3 & (pbp$Is.FG==1 | pbp$Is.FG==0))
  if(length(FG.yards.done$Is.FG)>0){
  is.shot = sample(FG.yards.done, 1)
  if(is.shot$Is.FG == 1){points = 3}else{points = 0}
  }else{points=0}
  points
}

#### runs a single play ####
play.fun=function(team.name){
  
  parameter_name<-paste("perc.pass",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.pass", parameter_value))
  
  parameter_name<-paste("perc.rush",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.rush", parameter_value))
  
  parameter_name<-paste("perc.sack",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.sack", parameter_value))
  
  parameter_name<-paste("pass",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("pass", parameter_value))
  
  parameter_name<-paste("rush",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("rush", parameter_value))
  
  parameter_name<-paste("sack",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("sack", parameter_value))
  
  
  play = runif(1,0,1) 
  if(play < perc.pass) {yard=sample(pass,1)} 
  else if (play< perc.pass + perc.rush) {yard=sample(rush,1)} 
  else if(play< perc.pass + perc.rush+perc.sack) {yard = sample(sack,1)} 
  else{yard = -9999} 
  play.yard=penalty.fun(yard, team.name)
  play.yard}

####runs a play when third down and long####
play.3down.long.fun=function(team.name)
{  
  
  parameter_name<-paste("perc.pass.long",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.pass.long", parameter_value))
  
  parameter_name<-paste("perc.to.long",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.to.long", parameter_value))
  
  parameter_name<-paste("perc.sack.long",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.sack.long", parameter_value))
  
  
  parameter_name<-paste("pass.long",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("pass.long", parameter_value))
  
  parameter_name<-paste("rush.long",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("rush.long", parameter_value))
  
  parameter_name<-paste("sack.long",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("sack.long", parameter_value))
  
  
  play = runif(1,0,1) 
  if(play < perc.to.long) {yard=-9999} 
  else if(play<perc.to.long+perc.sack.long) {yard=sample(sack.long,1)}
  else if(play<perc.to.long+perc.sack.long+perc.pass.long) {yard=sample(pass.long,1)}
  else {yard=sample(rush.long,1)}
  play.yard=penalty.fun(yard, team.name)
  play.yard
}

####Runs a play when thirddown and short####
play.3down.short.fun=function(team.name){
  
  
  parameter_name<-paste("perc.to.short",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.to.short", parameter_value))
  
  parameter_name<-paste("perc.sack.short",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.sack.short", parameter_value))
  
  parameter_name<-paste("perc.pass.short",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("perc.pass.short", parameter_value))
  
  parameter_name<-paste("pass.short",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("pass.short", parameter_value))
  
  parameter_name<-paste("rush.short",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("rush.short", parameter_value))
  
  parameter_name<-paste("sack.short",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("sack.short", parameter_value))
  
  
  play = runif(1,0,1) 
  if(play < perc.to.short) {yard=-9999} 
  else if(play<perc.to.short+perc.sack.short) {yard=sample(sack.short,1)}
  else if(play<perc.to.short+perc.sack.short+perc.pass.short) {yard=sample(pass.short,1)}
  else {yard=sample(rush.short,1)}
  play.yard=penalty.fun(yard, team.name)
  play.yard}

####Repeats the play function 3 times to get the results of a series. Repeats series 10 times to get results for a drive####
downs3fun = function(team.name){
  yards=0
  first.down = play.fun(team.name)
  while(first.down[2]==1){
    yards=first.down[1]+yards
    first.down = play.fun(team.name)}
  yards=first.down[1]+yards
  
  if(yards>=10){yards=yards
  }else{second.down=play.fun(team.name)
  while(second.down[2]==1){
    yards=second.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down = play.fun(team.name)
    yards=second.down[1]+yards}}
  
  if(yards>=10){yards=yards
  }else{if(yards<4){third.down=play.3down.short.fun(team.name)
  while(third.down[2]==1){
    yards=third.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    }else{third.down=play.3down.long.fun(team.name)}}
  yards=third.down[1]+yards
  }else{third.down=play.3down.long.fun(team.name)
  while(third.down[2]==1){
    yards=third.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    }else{third.down=play.3down.long.fun(team.name)}}
  yards=third.down[1]+yards}}
  yards}

#### function that returns 10 fourth downs ####
downs4fun = function(team.name){
  yards=0
  first.down = play.fun(team.name)
  while(first.down[2]==1){
    yards=first.down[1]+yards
    first.down = play.fun(team.name)}
  yards=first.down[1]+yards
  
  if(yards>=10){yards=yards
  }else{second.down=play.fun(team.name)
  while(second.down[2]==1){
    yards=second.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down = play.fun(team.name)
    yards=second.down[1]+yards}}
  
  if(yards>=10){yards=yards
  }else{if(yards<4){third.down=play.3down.short.fun(team.name)
  while(third.down[2]==1){
    yards=third.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    }else{third.down=play.3down.long.fun(team.name)}}
  yards=third.down[1]+yards
  }else{third.down=play.3down.long.fun(team.name)
  while(third.down[2]==1){
    yards=third.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    }else{third.down=play.3down.long.fun(team.name)}}
  yards=third.down[1]+yards}}
  
  if(yards>=10){yards=yards
  }else{if(yards<4){fourth.down=play.3down.short.fun(team.name)
  while(fourth.down[2]==1){
    yards=fourth.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    yards=third.down[1]+yards
    }else{third.down=play.3down.long.fun(team.name)
    yards=third.down[1]+yards}
    if(yards<4){fourth.down=play.3down.short.fun(team.name)
    }else{fourth.down=play.3down.long.fun(team.name)}}
  yards=fourth.down[1]+yards
  }else{fourth.down=play.3down.long.fun(team.name)
  while(fourth.down[2]==1){
    yards=fourth.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    yards=third.down[1]+yards
    }else{third.down=play.3down.long.fun(team.name)
    yards=third.down[1]+yards}
    if(yards<4){fourth.down=play.3down.short.fun(team.name)
    }else{fourth.down=play.3down.long.fun(team.name)}}
  yards=fourth.down[1]+yards}}
  yards}


#### Can return 3rd and 4th downs to choose if a team is going to punt, kick, go for it ####
downs3.4fun = function(team.name){
  yards=0
  yards3=0
  yards4=0
  first.down = play.fun(team.name)
  while(first.down[2]==1){
    yards=first.down[1]+yards
    first.down = play.fun(team.name)}
  yards=first.down[1]+yards
  
  if(yards>=10){yards=yards
  }else{second.down=play.fun(team.name)
  while(second.down[2]==1){
    yards=second.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down = play.fun(team.name)
    yards=second.down[1]+yards}}
  
  if(yards>=10){yards=yards
  }else{if(yards<4){third.down=play.3down.short.fun(team.name)
  while(third.down[2]==1){
    yards=third.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    }else{third.down=play.3down.long.fun(team.name)}}
  yards=third.down[1]+yards
  yards3=yards
  }else{third.down=play.3down.long.fun(team.name)
  while(third.down[2]==1){
    yards=third.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    }else{third.down=play.3down.long.fun(team.name)}}
  yards=third.down[1]+yards}}
  yards3=yards
  
  if(yards>=10){yards=yards
  }else{if(yards<4){fourth.down=play.3down.short.fun(team.name)
  while(fourth.down[2]==1){
    yards=fourth.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    yards=third.down[1]+yards
    }else{third.down=play.3down.long.fun(team.name)
    yards=third.down[1]+yards}
    if(yards<4){fourth.down=play.3down.short.fun(team.name)
    }else{fourth.down=play.3down.long.fun(team.name)}}
  yards=fourth.down[1]+yards
  yards4=yards
  }else{fourth.down=play.3down.long.fun(team.name)
  while(fourth.down[2]==1){
    yards=fourth.down[1]+yards
    first.down = play.fun(team.name)
    yards=first.down[1]+yards
    second.down=play.fun(team.name)
    yards=second.down[1]+yards
    if(yards<4){third.down=play.3down.short.fun(team.name)
    yards=third.down[1]+yards
    }else{third.down=play.3down.long.fun(team.name)
    yards=third.down[1]+yards}
    if(yards<4){fourth.down=play.3down.short.fun(team.name)
    }else{fourth.down=play.3down.long.fun(team.name)}}
  yards=fourth.down[1]+yards}}
  yards4=yards
  
  yards3and4<-c(yards3,yards4)
  yards3and4}


####Punting Function: Performs a punt. #### 
##Uses all data, not just data from team

punt.fun=function(yard, team.name){
  Punts<-subset(pbp,pbp$PlayType=="PUNT")
  distance=sample(Punts$Punt.Distance,1)
  end.yard=yard+distance
  if (end.yard>=100){end.yard=80}
  end.yard
} 

####Penalty Function####
is.playA=subset(TEAMA, PlayType == "FIELD GOAL" | PlayType =="PASS" | PlayType == "RUSH" | PlayType == "SACK" | 
                  PlayType =="FUMBLE" | PlayType == "SCRAMBLE")

is.playB=subset(TEAMB, PlayType == "FIELD GOAL" | PlayType =="PASS" | PlayType == "RUSH" | PlayType == "SACK" | 
                  PlayType =="FUMBLE" | PlayType == "SCRAMBLE")


penalty.fun=function(yards, team.name){
  
  parameter_name<-paste("is.play",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("is.play", parameter_value))
  
  is.penalty = sample(is.play,1)
  if(is.penalty$PenaltyYards==0) {end.yard=yards}
  else if(is.penalty$PenaltyTeam==is.penalty$OffenseTeam) {end.yard=yards - is.penalty$PenaltyYards}
  else {end.yard=yards + is.penalty$PenaltyYards}
  
  if(is.penalty$PenaltyType=="FACE MASK (15 YARDS)" | is.penalty$PenaltyType=="DEFENSIVE HOLDING" | is.penalty$PenaltyType=="ILLEGAL CONTACT" | 
     is.penalty$PenaltyType=="DEFENSIVE PASS INTERFERENCE" | is.penalty$PenaltyType=="ROUGHING THE PASSER" |
     is.penalty$PenaltyType=="UNSPORTSMANLIKE CONDUCT") {is.auto.first.down=1}
  else{is.auto.first.down=0}
  
  c(end.yard,is.auto.first.down)
}

####Team A always goes for it on fourth down. ####
##This function computes the points for 1 drive and the yards for the other team (Team B)
drive.TeamA.fun=function(begin.yard, team.name){
  
  parameter_name<-paste("KickOffs",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("KickOffs", parameter_value))
  
  get10=t((do(10)*downs4fun(team.name)))
  is.first.down=as.numeric(get10>=10)
  is.turn.over=as.numeric(get10<=-1000)
  yards=cumsum(get10)+begin.yard
  yards
  if(is.first.down[1]==0) {total.yards=yards[1]}
  else if(sum(is.first.down)==10) {total.yards=yards[10]}
  else {total.yards=yards[1:order(is.first.down)[1]]}
  
  if(max(total.yards)>=100) {points=7} else{points=0}
  if(points==7) {teamb.yards=sample(KickOffs$Kickoff.yards,1)}
  else if(is.turn.over[1]==1) {teamb.yards=100-begin.yard}
  else {teamb.yards=100-max(total.yards)}
  c(points,teamb.yards)
}

####This function lets the team have any option. #####
##Can go for it on fourth down always, Can never go for it on fourth down, 
##can sometimes go for it on fourth down.
##This function can be used exclusively
drive.TeamA.all.opt.fun=function(begin.yard,punt.yard,fourth.d.yard,FG.min, FG.max, team.name){
  
  parameter_name<-paste("KickOffs",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("KickOffs", parameter_value))
  
  get10=t((do(10)*downs3.4fun(team.name)))
  punt.no=cumsum(get10[1,])+begin.yard
  punt.id=as.numeric(punt.no>punt.yard)
  third.d.stop=rep(0,10)
  fourth.d.stop=rep(0,10)
  for (i in 1:10) 
    if(get10[1,i]<fourth.d.yard |punt.id[i]==0) {third.d.stop[i]=1} else if(get10[1,i]>FG.min & get10[1,i]<FG.max) {third.d.stop[i]=1} else{third.d.stop[i]=0}
  for (i in 1:10) 
    if(third.d.stop[i]==1) {fourth.d.stop[i]=0} else{fourth.d.stop[i]=1}
  yards=rep(0,10)
  for (i in 1:10)
    if(get10[1,i]>=10) {yards[i]=get10[1,i]} else if(third.d.stop[i]==1) {yards[i]=get10[1,i]} else {yards[i]=get10[2,i]}
  is.1down=as.numeric(yards>=10)
  is.turn.over=as.numeric(yards<=-500)
  if(is.1down[1]==0) {total.yards=yards[1]+begin.yard} else if(sum(is.1down)==10) {total.yards=cumsum(yards)+begin.yard} else{total.yards=cumsum(yards[1:order(is.1down)[1]])+begin.yard}
  get10
  yards
  total.yards
  
  if(max(total.yards)>=100) {points=7} 
  else if(max(total.yards)>=FG.min &max(total.yards)<=FG.max & min(total.yards)>=-100 & third.d.stop[length(total.yards)]==1) {points=FG.fun(max(total.yards))} 
  else{points=0}
  if(points>0) {teamb.yards=sample(KickOffs$Kickoff.yards,1)} 
  else if(length(total.yards)==1&total.yards[1]<=-500) {teamb.yards=100-begin.yard} 
  else if(min(total.yards)<=-5000) {teamb.yards=100-max(total.yards)}
  else if(max(total.yards)<=FG.min &third.d.stop[length(total.yards)]==1) {teamb.yards=100-punt.fun(min(abs(max(total.yards))+1,64))}
  
  else {teamb.yards=100-max(total.yards)}
  
  c(points,min(abs(teamb.yards),99))
}



####This function is for the team that never goes for it on fourth down. Either kick fg or punt####
##Returns points and other team start yards


drive.TeamB.fun=function(start.yard, team.name){
  
  parameter_name<-paste("KickOffs",team.name, sep="")
  parameter_value<- eval(parse(text = parameter_name))
  do.call("<-",list("KickOffs", parameter_value))
  
  get10=t((do(10)*downs3fun(team.name)))
  isfirstdown=as.numeric(get10>=10)
  if(sum(isfirstdown)==10) {series.final=get10} else {series.final=get10[1:(order(isfirstdown)[1]-1)]}
  is.turn.over=as.numeric(get10[order(isfirstdown)[1]]<=-5000)
  final.yards=start.yard+sum(series.final)
  final.yards
  if(final.yards>100){points=7} 
  else if(final.yards<65) {points=0} 
  else if(final.yards>=65 &get10[length(series.final)+1]<=-500 ) {points=0}
  else {points=FG.fun(final.yards)}
  if(final.yards<=-500 & points==0) {other.team.start=100-start.yard}
  else if(is.turn.over==1 & points==0) {other.team.start=100-final.yards}  
  else if(final.yards>=66 & points==0) {other.team.start=100-final.yards}  
  else if(points==7|points==3) {other.team.start=sample(KickOffs$Kickoff.yards,1)}  
  else {other.team.start=100-punt.fun(abs(final.yards))}
  c(points,min(99,abs(other.team.start)))}

####Simulating one game####

one.game.fun=function(punt.yard,fourth.d.yard,FG.min, FG.max){
  drive1.TeamA=drive.TeamA.all.opt.fun(sample(KickOffsA$Kickoff.yards,1),punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive1.TeamB=drive.TeamA.all.opt.fun(drive1.TeamA[2],64,100,65,99, "B")
  drive2.TeamA=drive.TeamA.all.opt.fun(drive1.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive2.TeamB=drive.TeamA.all.opt.fun(drive2.TeamA[2],64,100,65,99, "B")
  drive3.TeamA=drive.TeamA.all.opt.fun(drive2.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive3.TeamB=drive.TeamA.all.opt.fun(drive3.TeamA[2],64,100,65,99, "B")
  drive4.TeamA=drive.TeamA.all.opt.fun(drive3.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive4.TeamB=drive.TeamA.all.opt.fun(drive4.TeamA[2],64,100,65,99, "B")
  drive5.TeamA=drive.TeamA.all.opt.fun(drive4.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive5.TeamB=drive.TeamA.all.opt.fun(drive5.TeamA[2],64,100,65,99, "B")
  drive6.TeamA=drive.TeamA.all.opt.fun(drive5.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive6.TeamB=drive.TeamA.all.opt.fun(drive6.TeamA[2],64,100,65,99, "B")
  drive7.TeamB=drive.TeamA.all.opt.fun(sample(KickOffsA$Kickoff.yards,1), 64,100,65,99, "B")
  drive7.TeamA=drive.TeamA.all.opt.fun(drive7.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive8.TeamB=drive.TeamA.all.opt.fun(drive7.TeamA[2],64,100,65,99, "B")
  drive8.TeamA=drive.TeamA.all.opt.fun(drive8.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive9.TeamB=drive.TeamA.all.opt.fun(drive8.TeamA[2],64,100,65,99, "B")
  drive9.TeamA=drive.TeamA.all.opt.fun(drive9.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive10.TeamB=drive.TeamA.all.opt.fun(drive9.TeamA[2],64,100,65,99, "B")
  drive10.TeamA=drive.TeamA.all.opt.fun(drive10.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive11.TeamB=drive.TeamA.all.opt.fun(drive10.TeamA[2],64,100,65,99, "B")
  drive11.TeamA=drive.TeamA.all.opt.fun(drive11.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  drive12.TeamB=drive.TeamA.all.opt.fun(drive11.TeamA[2],64,100,65,99, "B")
  drive12.TeamA=drive.TeamA.all.opt.fun(drive12.TeamB[2],punt.yard,fourth.d.yard,FG.min, FG.max, "A")
  TeamA.score=drive1.TeamA[1]+drive2.TeamA[1]+drive3.TeamA[1]+drive4.TeamA[1]+drive5.TeamA[1]+drive6.TeamA[1]+
    drive7.TeamA[1]+drive8.TeamA[1]+drive9.TeamA[1]+drive10.TeamA[1]+drive11.TeamA[1]+drive12.TeamA[1]
  TeamB.score=drive1.TeamB[1]+drive2.TeamB[1]+drive3.TeamB[1]+drive4.TeamB[1]+drive5.TeamB[1]+drive6.TeamB[1]+
    drive7.TeamB[1]+drive8.TeamB[1]+drive9.TeamB[1]+drive10.TeamB[1]+drive11.TeamB[1]+drive12.TeamB[1]
  if(TeamA.score>TeamB.score) {winner="TeamA"} else if(TeamA.score<TeamB.score) {winner="TeamB"} else{winner="Tie"}
  
  c(winner,TeamA.score,TeamB.score)
} 

#### Simulating multiple games ####

simulate.games.fun=function(num.games,punt.yard,fourth.d.yard,FG.min, FG.max){
  resultsA=rep(0,num.games)
  resultsB = rep(0, num.games)
  resultswin=rep(0,num.games)
  for(i in 1:num.games){
    results = one.game.fun(punt.yard,fourth.d.yard,FG.min, FG.max)
    resultswin[i]=results[1]
    resultsA[i] = results[2]
    resultsB[i] = results[3]
  }
  
  teamApts = round(mean(as.numeric(resultsA)))
  teamBpts = round(mean(as.numeric(resultsB)))
  prop.Awins=round(sum(as.numeric(resultswin=="TeamA"))/(num.games-sum(as.numeric(resultswin=="Tie"))), digits=4)
  if(prop.Awins > 0.5){winner = team.idA}else{winner = team.idB}
  c(winner, prop.Awins, teamApts, teamBpts)
}


#simulate.games.fun(10,-30,-8000,600,700))
simulate.games.fun(100,30,5,75,94)

#punt.yard you are ALWAYS punting after
#fourth.d.yard (i.e 1) you are punting if you have 4th and 9 or more
#if 3 you are 4th and 7 or more
#anything above the FG max is going for it