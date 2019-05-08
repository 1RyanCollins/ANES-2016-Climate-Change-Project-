#ANES TIME SERIES ANALYSIS 2016

ANES_2016 <- read.csv(file = "C:\\Users\\ryancoll\\Desktop\\anes16.csv")
attach(ANES_2016)
names(ANES_2016)

install.packages("dplyer")
install.packages("lmtest")

names(ANES_2016)

#recoding gender variable 
Gender_M_F <- c()
Gender_M_F[V161342 ==1 ] <- 0 #Male
Gender_M_F[V161342 ==2] <- 1 #Female
Gender_M_F[V161342==3] <- NA #Other
Gender_M_F[V161342== -9] <- NA #REFUSED
length(Gender_M_F)
table(Gender_M_F)
#done v161342

Pid7 <- c()
Pid7[V161158x == 1] <- 1 #strong democrat
Pid7[V161158x == 2] <- 2 #not strong democrat
Pid7[V161158x == 3] <- 3 #ind democrat
Pid7[V161158x == 4] <- 4 #ind 
Pid7[V161158x == 5] <- 5 #ind rep
Pid7[V161158x == 6] <- 6 #not strong rep
Pid7[V161158x == 7] <- 7 #strong rep

Pid7[V161158x == -8] <- NA #Don't know
Pid7[V161158x == -9] <- NA #refused 

table(Pid7)

#done


#Born Again 
Born_Again_Chr <- c()
Born_Again_Chr[V161263 == 1] <- 1 #yes
Born_Again_Chr[V161263 ==2 ] <- 0  #no
Born_Again_Chr[V161263 ==-1] <- NA
Born_Again_Chr[V161263 ==-4] <- NA
Born_Again_Chr[V161263 ==-8] <- NA 
Born_Again_Chr[V161263 ==-9] <- NA
table(Born_Again_Chr)
length(Born_Again_Chr)
#done
head(Born_Again_Chr)


Bible <- c()
Bible[V161243 ==1] <- 1 #literally
Bible[V161243 == 2] <- 1/2 #not all literal 
Bible[V161243 == 3] <- 0 #not god, by man 
Bible[V161243 == 5] <- NA
Bible[V161243 == -8] <- NA
Bible[V161243 ==-9] <- NA
table(Bible)
#done

table(Ideology)
 

Rel_Importance <- c()
Rel_Importance[V161241==1] <- 1 #important
Rel_Importance[V161241 == 2] <- 0 #not important 
Rel_Importance[V161241 == -8] <- NA
Rel_Importance[V161241==-9] <- -NA
table(Rel_Importance)

#done

mean(Rel_Importance, na.rm = TRUE)


#Religious Guidance 
Rel_Guide <- c()
Rel_Guide[V161242== 3] <- 1 #great deal 
Rel_Guide[V161242 == 2] <- 2/3 #quite a bit 
Rel_Guide[V161242 == 1] <- 1/3 #some 
Rel_Guide[V161242 ==-1 ] <- 0
Rel_Guide[V161242 ==-8 ] <- NA
Rel_Guide[V161242 ==-9] <- NA
mean(Rel_Guide, na.rm = TRUE)
table(Rel_Guide)

#done

Rel_Service <- c()

Rel_Service[V161244  == 2] <- 0 #almost ever week 
Rel_Service[V161244  == 1] <- 1 #every week 
Rel_Service[V161244  == -8] <- NA
Rel_Service[V161244  == -9] <- NA
table(Rel_Service)
mean(Rel_Service, na.rm = TRUE)

#done

INCOME
length(INCOME)
table(INCOME)

Income_bracket <- c()  #0-27
#0-25000 - 20
Income_bracket[V161361x== -9] <- NA
Income_bracket[V161361x == 1] <- 1
Income_bracket[V161361x ==2] <- 2
Income_bracket[V161361x ==3] <- 3
Income_bracket[V161361x ==4] <- 4
Income_bracket[V161361x ==5] <- 5
Income_bracket[V161361x ==6] <- 6
Income_bracket[V161361x ==7] <- 7
Income_bracket[V161361x ==8] <- 8
Income_bracket[V161361x ==9] <- 9
Income_bracket[V161361x ==10] <-10
Income_bracket[V161361x ==11] <- 11
Income_bracket[V161361x ==12] <- 12
Income_bracket[V161361x ==13] <- 13
Income_bracket[V161361x ==14] <- 14
Income_bracket[V161361x ==15] <- 15
#50000-75000 - 60
Income_bracket[V161361x ==16] <- 16
Income_bracket[V161361x ==17] <- 17
Income_bracket[V161361x ==18] <- 18
Income_bracket[V161361x ==19] <- 19
#75000-125000 - 80
Income_bracket[V161361x ==20] <- 20
Income_bracket[V161361x ==21] <- 21
Income_bracket[V161361x ==22] <- 22

Income_bracket[V161361x ==23] <- 23

Income_bracket[V161361x ==24] <- 24
#125000,200000


Income_bracket[V161361x ==25] <- 25
Income_bracket[V161361x ==26] <- 26
Income_bracket[V161361x ==27] <- 27


Income_bracket[V161361x ==28] <- 28
#greater than 250000
table(Income_bracket)
mean(Income_bracket, na.rm = TRUE)

#Ideology 

Lib_Con_7 <- c()
Lib_Con_7[V161126 == 1] <- 1 #extr lib 
Lib_Con_7[V161126 == 2] <- 2
Lib_Con_7[V161126 == 3] <- 3
Lib_Con_7[V161126 == 4] <- 4 #moderate 
Lib_Con_7[V161126 == 5] <- 5
Lib_Con_7[V161126 == 6] <- 6
Lib_Con_7[V161126 == 7] <- 7
Lib_Con_7[V161126 == 99] <- NA
Lib_Con_7[V161126 == -6] <- NA
Lib_Con_7[V161126 == -7] <- NA
Lib_Con_7[V161126 == -9] <- NA
table(Lib_Con_7)

mean(Lib_Con_7, na.rm = TRUE)

#done


#Race
#White

Whites <- c()
Whites[V161310a ==1] <- 1 #selected
Whites[V161310a ==0] <- 0 #not selected
Whites[V161310a ==-8] <- NA
Whites[V161310a ==-9] <- NA
mean(Whites, na.rm = TRUE)
#done

Blacks <- c()
Blacks[V161310b ==1] <- 1 #selected
Blacks[V161310b ==0] <- 0 #not selected
Blacks[V161310b ==-8] <- NA
Blacks[V161310b ==-9] <- NA
mean(Blacks, na.rm = TRUE)
#done

Hispanics <- c()
Hispanics[V161309 ==1] <- 1 #selected
Hispanics[V161309 ==2] <- 0 #not selected
Hispanics[V161309 ==-8] <- NA
Hispanics[V161309 ==-9] <- NA
mean(Hispanics, na.rm = TRUE)
#done

#Education 
Education <- c()
Education[V161270 ==1] <- 1
Education[V161270==2] <- 1
Education[V161270==3] <- 1
Education[V161270==4] <- 1
Education[V161270==5] <- 1
Education[V161270==6] <- 1
Education[V161270==7] <- 1
Education[V161270==8] <- 1
#less than high school ^

Education[V161270==9] <- 2
#high school diploma

Education[V161270==10] <- 3 
#some college , no degree ^

Education[V161270==11] <- 4
Education[V161270==12] <- 4
#associates


Education[V161270==13] <- 5
#bach

Education[V161270==14] <- 6
#masters


Education[V161270==15] <- 7
Education[V161270==16] <- 7
#phd/md

Education[V161270==90] <- NA
Education[V161270==95] <- NA
Education[V161270==-9] <- NA

mean(Education, na.rm = TRUE)

#done

#Age
AGE_GROUP
table(AGE_GROUP)


Age <- c()
Age[V161267x ==1] <- 1
Age[V161267x ==2] <- 2
#18-24

Age[V161267x ==3] <- 3
Age[V161267x ==4] <- 4
Age[V161267x ==5] <- 5
#25-39

Age[V161267x ==6] <- 6
Age[V161267x ==7] <- 7
#40-49


Age[V161267x ==8] <- 8
Age[V161267x ==9] <- 9
Age[V161267x ==10] <- 10

Age[V161267x ==11] <- 11
Age[V161267x ==12] <- 12
Age[V161267x ==13] <- 13

Age[V161267x ==-1] <- NA
table(Age)    
table(Age)
names(ANES_2016)
V161267x


names(ANES_2016)

ENV_7 <- c()
ENV_7[V161201 ==1] <- 1 #most regulation 
ENV_7[V161201 ==2] <- 2
ENV_7[V161201  ==3] <- 3

ENV_7[V161201  ==4] <- 4
ENV_7[V161201  ==5] <- 5


ENV_7[V161201  ==6] <- 6
ENV_7[V161201  ==7] <- 7 #no regulation 

ENV_7[V161201  ==99] <- NA
ENV_7[V161201  ==-8] <- NA
ENV_7[V161201  ==-9] <- NA
mean(ENV_7, na.rm =TRUE)
table(ENV_7)


#Federal Budget Spending - STEM 
Sci_Tech <- c()
Sci_Tech[V161207 == 1] <- 1 #inc
Sci_Tech[V161207 ==2] <- 0 #dec
Sci_Tech[V161207 ==3] <- 1/2 #same
Sci_Tech[V161207 ==-8] <- NA
Sci_Tech[V161207 ==-9] <- NA
table(Sci_Tech)
mean(Sci_Tech, na.rm = TRUE)

V161207

#Federal Budget Spending - Environment 

V161212

Env_Spend <- c()
Env_Spend[V161212 == 1] <- 1 #inc
Env_Spend[V161212 == 2] <- 0 #dec
Env_Spend[V161212 == 3] <- 1/2 #same
Env_Spend[V161212 == -8] <- NA
Env_Spend[V161212 == -9] <- NA
table(Env_Spend)
mean(ENV_7[Pid7 == 7], na.rm = TRUE)
mean(ENV_7[Pid7 == 1], na.rm = TRUE)


cor.test(Sci_Tech, Bible)
#Done recoding variables?


#test1

regression1 <- summary(lm(Env_Spend ~ Age + Gender_M_F + Income_bracket + Education + Blacks + Hispanics + Whites + Pid7+ Lib_Con_7), data = ANES_2016)
regression1

regression2 <- summary(lm(Env_Spend ~ Age + Gender_M_F + Income_bracket + Education + Blacks + Hispanics + Whites + Pid7+ Lib_Con_7 + Rel_Importance + Rel_Guide + Rel_Service), data = ANES_2016)
regression2

regression3 <- summary(lm(Env_Spend ~ Age + Gender_M_F + Income_bracket + Education + Blacks + Hispanics + Whites + Pid7+ Lib_Con_7 + Rel_Importance + Rel_Guide + Rel_Service + Born_Again_Chr + Bible), data = ANES_2016)
regression3

regression4 <- summary(lm(ENV_7 ~ Age + Gender_M_F + Income_bracket + Education + Blacks + Hispanics + Whites + Pid7+ Lib_Con_7 + Rel_Importance + Rel_Service),data = ANES_2016)
regression4

regression5 <- summary(lm(Env_Spend ~Age + Gender_M_F + Rel_Service))
regression5




#Summary Stats

#Party Id / ENV 7
#1 - most
#7 - no reg

mean(ENV_7[Pid7 == 7], na.rm = TRUE) #str rep
mean(ENV_7[Pid7 == 1], na.rm = TRUE) #str dem

sd(ENV_7[Pid7 == 7], na.rm = TRUE) #str rep
sd(ENV_7[Pid7 == 1], na.rm = TRUE) #str dem


mean(ENV_7[Lib_Con_7 == 1], na.rm = TRUE) #lib
mean(ENV_7[Lib_Con_7 == 7], na.rm = TRUE) #cons

sd(ENV_7[Lib_Con_7 == 1], na.rm = TRUE) #lib
sd(ENV_7[Lib_Con_7 == 7], na.rm = TRUE) #cons


mean(ENV_7[Rel_Importance == 0:1], na.rm = TRUE)
mean(ENV_7[Rel_Importance == 1], na.rm = TRUE)
mean(ENV_7[Rel_Importance == 0], na.rm = TRUE)

sd(ENV_7[Rel_Importance = 1], na.rm = TRUE)

Rel_Importance


