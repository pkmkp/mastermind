####Mastermind
## 2 July, 2019
#p.kerby-miller
####
#####TO PLAY: 
##1) make sure you have tidyverse installed 
#install.packages('tidyverse')
#2) run below code
#3) close your environment tab, expand your plots tab.
#4) play using the console to enter guesses such as mastermind(blue,green,green,red)


##### setup----
library(tidyverse)
setup <- function(){
globalenv(
blue <- b <- 1
green <- g <- 2
orange <- o <- 3
pink <- p <- 4
red <- r <- 5
yellow <- y <- 6  
try <- 1

mastermind <- function(A,B,C,D,attempt=try){
  guess <- c(A,B,C,D)
  guess.i <- guess
  i <- attempt
  answer <-  vector()
  if(i==1){
    code <<-  sample(1:6,4,replace=TRUE)
    code <- code
    guess.df <<- data_frame(loc_x=c(1,2,3,4),val=c(A,B,C,D),i=i)
    for(color in unique(guess)){
      answer <- as.vector(c(answer,evaluate_color_j(color,guess,code)))
    }
    print(answer)
    ans.plot <- c(NA,NA,NA,NA)
    ans.plot[seq_along(answer)] <- answer
    new.df <- data_frame(loc_x=c(4.9,5.1,4.9,5.1),val=c(ans.plot),i=c(i+.1,i+.1,i-.1,i-.1))
    guess.df <<- bind_rows(guess.df,new.df)
    if(length(answer)>0 &length(which((answer==9)==TRUE))<4){
      print(answer)
      print("nice")}
    if(length(answer)==0){
      print("try again")}
    if(length(which((answer==9))==TRUE)==4){
      try <- 0
      print("you win!!!1!1")
    }
  }
  if(i>1&i<10){
    code <- code
    new.df <- data_frame(loc_x=c(1,2,3,4),val=c(A,B,C,D),i=i)
    guess.df <<- bind_rows(guess.df,new.df)
    for(color in unique(guess)){
      answer <- as.vector(c(answer,evaluate_color_j(color,guess,code)))
    }
    ans.plot <- c(NA,NA,NA,NA)
    ans.plot[seq_along(answer)] <- answer
    new.df <- data_frame(loc_x=c(4.9,5.1,4.9,5.1),val=c(ans.plot),i=c(i+.1,i+.1,i-.1,i-.1))
    guess.df <<- bind_rows(guess.df,new.df)
    if(length(answer)>0 & length(which((answer==9)==TRUE))<4){
      print(answer)
      print("nice")}
    if(length(answer)==0){
      print("try again")}
    if(length(which((answer==9))==TRUE)==4){
      try <- 0
      print("you win!!!1!1")
    }
  }
  if(i>=10){
    code <- code
    try <- 0
    new.df <- data_frame(loc_x=c(1,2,3,4),val=c(A,B,C,D),i=i)
    guess.df <<- bind_rows(guess.df,new.df)
    for(color in unique(guess)){
      answer <- as.vector(c(answer,evaluate_color_j(color,guess,code)))
    }      
    ans.plot <- c(NA,NA,NA,NA)
    ans.plot[seq_along(answer)] <- answer
    new.df <- data_frame(loc_x=c(4.9,5.1,4.9,5.1),val=c(ans.plot),i=c(i+.1,i+.1,i-.1,i-.1))
    guess.df <<- bind_rows(guess.df,new.df)
    if(length(answer)>0 &length(which((answer==9)==TRUE))<4){
      print(code)
      print("Better Luck Next Time.")}
    if(length(answer)==0){
      print(code)
      print("yOu loSe! sUcKs tO SuCk")}
    if(length(which((answer==9))==TRUE)==4){
      print("you win!!!1!1 Barely.")
      try <<- 0
    }
  }
  try <<- try + 1
  return(plot_game())
}
evaluate_color_j <-  function(color,guess,code){#provides answer for each color, modifying code.i and guess.i
  code.j <- code
  code.j[which((code.j==color)==FALSE)] <- 777
  code.j[which((code.j==guess)==TRUE)] <- 0
  answer.i <- c(rep(9,length=length(which((code.j==0)==TRUE))))
  answer.i <- c(answer.i,rep(8,length=length(which((guess%in%color)==TRUE))))
  while(length(answer.i)>length(which((code==color)==TRUE))|length(answer.i)>length(which((guess==color)==TRUE)))
    answer.i <- answer.i[c(seq(1,length(answer.i))-1)]
  answer.i <- as.vector(answer.i)
  return(answer.i)
}  
plot_game <- function(df=guess.df){
  df <- df %>% filter(is.na(val)==FALSE)
  ggplot(data=df, aes(x=loc_x,y=i,color=as.factor(val)))+
    geom_point(show.legend = FALSE)+
    coord_fixed(ratio=1.5,xlim=c(1,5.5),ylim=c(0.5,10.5))+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.ticks = element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank())+
    labs(x=NULL,y=NULL)+
    scale_color_manual(values=game_colors)
}
game_colors <- c("1"="blue","2"="green","3"="orange","4"="pink","5"="red","6"="yellow","8"="white","9"="black")
rules <- function(){
  print("plbttttt LOSER!")
}
)#end globalenv


print("you can ignore all of that^^^^          
~~~~~~~~~~~Welcome to mastermind!~~~~~~~~~~~~
      take this moment to close your environment tab
    You'll just need the console and plot tabs accessible.
  play by entering your guesses in the console, and see the result as a plot
   pin colors are: blue, green, orange, pink, red, and yellow
    you can enter them as their first letter only for short.
    the code is 4 digits long, and you have 10 tries to solve it.
  ~~~~~~~~ex: mastermind(b,g,g,y) ~~~~~~~~~~~
if you want a reminder as to the rules, just type rules()          
  ~~~~~~~~~~~~good luck!~~~~~~~~~~~~~~")
}


