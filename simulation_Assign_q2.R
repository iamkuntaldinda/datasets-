

###for 300 homes
set.seed(123)
notathome <- 25
##sample of 75% from 1000
homes <- (homes - (notathome/100)*homes)

prob_female  <- 0.8
prob_male <- 0.2
homes <- 300
x <- rbinom(homes,1,prob_female)
x

x1<- ifelse(x == 1, "Female","Male")
as.data.frame(x1) -> df
df

##probability for donation by males
p_m  = 0.4
totalmales_donation <- p_m*prob_male

##probability for donation by females
p_f = 0.7
totalfemales_donation <- p_f*prob_female

g <- totalmales_donation + totalfemales_donation

###random numbers for Male

mean_male<- 10 
sd_male<-2
male <- rnorm(homes*totalmales_donation,mean_male,sd_male)

###random numbers for female 
mean_female  <- 20
sd_female <- 3
female <- rnorm(homes*totalfemales_donation,mean_female,sd_female)

total_amount <- sum(male) + sum(female)
total_amount
###total donation is 