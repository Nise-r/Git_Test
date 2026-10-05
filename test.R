d = 90
class(d)
s = as.integer(d)
class(s)
a = as.character(d)
class(a)
#########################################

a.bgn = 44
########################################
b <- 45
# <- & -> are arrow operators
45 -> b
# alt -
########################################
b <- c(56, 89, 22, 50, 102)
class(b)
length(b)
b[3]
b[2:4]
0.5*b
a <- c(109, 201, 222, 450, 120)
a+b

d <- c(2,5,6)
a+d
#################################
b <- c(56, 89, 22, 50, 102)
a <- c(109, 201, 222, 450, 120)
ab <- rbind(a,b)
ab
ab <- cbind(a,b)
ab
class(ab)
ab[1,2]
## appending
h <- c(a,b)
h
# binding of unequal vectors
b <- c(56, 89, 22, 50, 102)
a <- c(109, 201, 222, 450)
ab <- rbind(a,b)
ab
######################################

v <- c(45, 9, "sf", 89)
class(v)
###################################
w <- c(52, 78, 12, 20, 90, 23)
w<50
w[w<50]
####################################
f <- list(p=45, q="FT", r=T, s=9.0)
f[2]
f$r
f <- list(p=45, q=c("FT","IO","IU"),
          r=T, s=list(a=9.0,b=89))
f[2]
f$q
f$s
class(f$r)
names(f)
###################################
# factor
s <- c("m", "f", "f", "m", "m", "f")
class(s)
fs <- factor(s)
class(fs)
fs
as.integer(fs)
# re-order
fs <- factor(s, levels = c("m", "f"))
fs
as.integer(fs)
#########################################
d <- NA
is.na(d)
v <- c(89, NA, 45, 90, NA)
is.na(v)
d+8
#############
b <- 0
a <- 0
s <- a/b
s
b <- 0
a <- 34
s <- a/b
s
is.finite(s)
is.infinite(s)
#####################################
m <- matrix(c(3,6,7,8,2,4), 3,2)
m
m <- matrix(c(3,6,7,8,2,4), 3,2, byrow = T)
m
m <- matrix(c(3,6,7), 3,2, byrow = F)
m
#####################################
a <- array(dim=4)
a[1] <- 2
a[2] <- 7
a

a <- array(c(3,5,6,7),dim=4)
a
####################################
a <- c("a","b","c","d")
b <- c(34, 90, 25, 12)
df <- data.frame(a,b)
df
dim(df)
colnames(df)
names(df)
###############################
# Resident datasets in R
data()
data(airquality)
data("airquality")
dim(airquality)
str(airquality) # meta data




p = 20000
n = 3
r = 7.9
si = p*n*r/100
si

setwd("D:/Training/Academy/R Course (C-DAC)/Datasets")
funds <- read.csv("Funds.csv", 
                  stringsAsFactors = T)
str(funds)
survey <- read.csv("survey.csv", 
                   stringsAsFactors = T)
str(survey)
diamonds <- read.csv2("Diamonds.csv", 
                      stringsAsFactors = T)
str(diamonds)
###############################################
library(readxl)
sales <- read_excel("Sales.xlsx",sheet = 1)
brupt <- read_excel("bankruptcy.xlsx", sheet = 3)


data(USArrests)
write.csv(USArrests, "USarr.csv")
#############################################

salaries <- read.csv("Salaries.csv", 
                     stringsAsFactors = T)
### Slicing of data frames
salaries[3,] # gets 3rd row
salaries[50,] # gets 50th row
salaries[,3] # gets 3rd column
salaries[2,3]
salaries[c(2,6,19,35,67,90),] # gets few rows
salaries[,c(3,2,5)] # gets few columns

### subsetting the data frames
hsal <- subset(salaries, 
               salary>100000 & rank=="AssocProf")
ss <- subset(salaries, 
             select = c(yrs.service, discipline))
ss <- subset(salaries,salary>100000 & rank=="AssocProf", 
             select = c(yrs.service, discipline))




a <- 23000
if(a>25000)
  print("Fine") else
    print("Not Fine")
#or
if(a>25000){
  print("Fine")
} else {
  print("Not Fine")
}
#############################################
for(i in 1:5)
  print(i)
d <- c(3,6,7,9,1)
for( e in d ){
  print(e*e)
}
s <- 0
for( e in d ){
  s <- s + e
}
print(s)
################
s <- 0
i <- 1
while(i<=length(d)){
  s <- s + d[i]
  i <- i + 1
}
print(s)
###################################
b <- seq(2,10)
b
b <- seq(2,10,3)
b
b <- seq(10,2,-3)
b




setwd("D:/Training/Academy/R Course (C-DAC)/Datasets/")
cars2018 <- read.csv("cars2018.csv",
                     stringsAsFactors = T)
table(cars2018$Aspiration)
table(cars2018$Aspiration, cars2018$Transmission)
addmargins(table(cars2018$Aspiration, cars2018$Transmission))
#############################################################

marks <- c(23, 12, 18, 34, 29, 30, 10, 2)
ifelse(marks>=16, "Passes", "Fails")
############################################################
survey <- read.csv("survey.csv", 
                   stringsAsFactors = T)
sum(is.na(survey$Age))
sum(is.na(survey$Height))
mean(survey$Age)
mean(survey$Height, na.rm = T)
sd(survey$Height, na.rm = T)
summary(survey$Height)
summary(survey$Sex)
summary(survey)
################################################
attach(survey)
table(Smoke)
summary(Height)
################################################



fah <- 100
cel <- (fah-32)*5/9
cel

fah_to_cel <- function(fh){
  cel <- (fh-32)*5/9
  cel
}
fah_to_cel(100)
############################################

fahToCel <- function(fah) {
  cel <- (fah-32)*5/9
  cel
}
#########################################
p <- 100000
n <- 5
r <- 8.9
calc_interest <- function(p, r, n) {
  ci <- p*(1+(r/100))**n - p
  ci
}
calc_interest(100000, 8.9, 5)
#######################################
mean_sd <- function(col_name) {
  avg <- mean(col_name, na.rm = T)
  sdev <- sd(col_name, na.rm = T)
  ans <- c(am = avg, std_dev = sdev)
  ans
}
mean_sd(survey$Pulse)
#######################################













setwd("D:/Training/Academy/R Course (C-DAC)/Datasets/")

a <- c(90,45,12,10)
barplot(a)
b <- c(89,23,45,19)
ab <- rbind(a,b)
barplot(ab)
barplot(ab,beside = T)
#########################
pie(a)
########################

survey <- read.csv("survey.csv", 
                   stringsAsFactors = T)
hist(survey$Age)
hist(survey$Pulse, col = "springgreen")
boxplot(survey$Pulse)
boxplot(survey$Pulse~survey$Exer)
##############################




setwd("D:/Training/Academy/R Course (C-DAC)/Datasets/")
library(tidyverse)
survey <- read.csv("survey.csv", 
                   stringsAsFactors = T)

ggplot(data = survey,aes(x=Age, y=Pulse))+
  geom_point()
ggplot(data = survey,aes(x=Age, y=Pulse))+
  geom_point(colour = "blue")
ggplot(data = survey,
       aes(x=Age, y=Pulse,color = Sex))+
  geom_point()

ggplot(data = survey,
       aes(x=Height,y=Pulse,shape = Smoke))+
  geom_point()

ggplot(data = survey,
       aes(x=Height,y=Pulse,color=Exer,
           shape = Smoke))+
  geom_point()

# Regression Line
ggplot(data = survey,aes(x=Height, y=Pulse))+
  geom_point()+
  geom_smooth(method = "lm")
########################################
ggplot(data = survey, aes(x=Pulse))+
  geom_histogram(fill = "springgreen",
                 colour = "darkgreen",
                 bins = 10)
########################################

ggplot(data = survey, aes(y=Height))+
  geom_boxplot()
ggplot(data = survey, aes(y=Height, x=Smoke))+
  geom_boxplot()
ggplot(data = survey, aes(y=Height, fill=Smoke))+
  geom_boxplot()
ggplot(data = survey, aes(y=Height, color=Smoke))+
  geom_boxplot()
#############################################
# graph for analyzing probability distribution
# of a numerical variable
ggplot(data = survey, aes(x=Pulse))+
  geom_density(fill = "springgreen",
               colour = "darkgreen")
#############################################
ggplot(data = survey, aes(x=Exer, fill=Exer))+
  geom_bar()
ggplot(data = survey, aes(y=Exer, fill=Exer))+
  geom_bar()
ggplot(data = survey, aes(x=Exer, fill=Smoke))+
  geom_bar(position = 'dodge')
# bar plots by default plot the counts
###############################################
means_age <- survey |> 
  group_by(Exer) |> 
  summarise(avg=mean(Age,na.rm=T))
ggplot(data = means_age, 
       aes(x=Exer, y=avg, fill=Exer))+
  geom_bar(stat = 'identity')





library(tidyverse)
setwd("D:/Training/Academy/R Course (C-DAC)/Datasets/")
cars2018 <- read.csv("cars2018.csv",
                     stringsAsFactors = T)
class(cars2018)
tbl_cars <- as_tibble(cars2018)
class(tbl_cars)
#########################################
survey <- read.csv("survey.csv", 
                   stringsAsFactors = T)
arr1 <- arrange(survey, Pulse )
arr2 <- arrange(survey, Sex, Pulse )
arr2 <- arrange(survey, Sex, desc(Pulse) )
#########################################
sel_1 <- select(survey, Fold, Clap, Smoke, Age)
sel_2 <- select(survey, 2:5)
sel_3 <- select(survey, Fold:Smoke)
sel_4 <- select(survey, starts_with("W"))
sel_5 <- select(survey, ends_with("nd"))
sel_6 <- select(survey, contains("d"))
#########################################
f_1 <- filter(survey, Age>30 & Sex=="Female")
#########################################
r_1 <- rename(survey,Exercise=Exer)
#######################################
## mutate() creates new variable
r_2 <- mutate(survey, r1=Pulse/Height)
##############################################
summarise(survey, avg_age=mean(Age, na.rm=T),
          sd_age=sd(Age, na.rm = T))
summarise(survey, avg_age=mean(Age, na.rm=T),
          sd_age=sd(Age, na.rm = T),
          md_age=median(Age, na.rm = T))
##############################################
grp_df <- group_by(survey, Sex)
summarise(grp_df, avg_age=mean(Age, na.rm=T),
          sd_age=sd(Age, na.rm = T),
          md_age=median(Age, na.rm = T))
############################################
a <- read.csv("A.csv")
b <- read.csv("B.csv")
inner_join(a, b, by="IdNum")
left_join(a, b, by="IdNum")
right_join(a, b, by="IdNum")
full_join(a, b, by="IdNum")
###########################################
survey %>%
  group_by(Sex) %>%
  summarise(avg_age=mean(Age, na.rm=T),
            sd_age=sd(Age, na.rm = T),
            md_age=median(Age, na.rm = T))

survey |>
  group_by(Sex) |>
  summarise(avg_age=mean(Age, na.rm=T),
            sd_age=sd(Age, na.rm = T),
            md_age=median(Age, na.rm = T))

#################################
sw <- filter(survey, Sex=="Female") 
d <- select(sw, Exer, Pulse)
d_gy <- group_by(d, Exer)
g <- summarise(d_gy, avg_pulse=mean(Pulse, na.rm=T))
#or
h <- survey |> 
  filter(Sex=="Female") |> 
  select(Exer, Pulse) |> 
  group_by(Exer) |> 
  summarise(avg_pulse=mean(Pulse, na.rm=T))

library(tidyverse)

table4a
table4a %>% gather(`1999`, `2000`, key= "year",
                   value= "cases")
table4a %>% gather(-country, key= "year",
                   value= "cases")
#or
table4a |> pivot_longer(cols = c(`1999`, `2000`), 
                        names_to = "year", 
                        values_to = "cases")
##################################################
table2
table2 %>% spread(key = "type", value = "count")
#or
table2 %>% pivot_wider(names_from = "type",
                      values_from = "count")
#################################################
table3
table3 |> 
  separate(rate, into = c("cases", "population"))
#################################################
table5
table5 |> unite(new,century, year, sep = "" )

table5 |> unite(new,century, year)




library(tidyverse)
setwd("D:/Training/Academy/R Course (C-DAC)/Datasets/")

survey |> 
  select(Exer,Smoke, Age) |> 
  drop_na() |> 
  group_by(Exer,Smoke) |> 
  summarise(avg_age=mean(Age, na.rm = TRUE),
            .groups = "drop_last") |> 
  pivot_wider(names_from = Exer, values_from = avg_age)




setwd("D:/Training/Academy/R Course (C-DAC)/Datasets/")
library(tidyverse)
survey <- read.csv("survey.csv", 
                   stringsAsFactors = T)

ggplot(data = survey,aes(x=Age, y=Pulse))+
  geom_point()
ggplot(data = survey,aes(x=Age, y=Pulse))+
  geom_point(colour = "blue")
ggplot(data = survey,
       aes(x=Age, y=Pulse,color = Sex))+
  geom_point()

ggplot(data = survey,
       aes(x=Height,y=Pulse,shape = Smoke))+
  geom_point()

ggplot(data = survey,
       aes(x=Height,y=Pulse,color=Exer,
           shape = Smoke))+
  geom_point()

# Regression Line
ggplot(data = survey,aes(x=Height, y=Pulse))+
  geom_point()+
  geom_smooth(method = "lm")
########################################
ggplot(data = survey, aes(x=Pulse))+
  geom_histogram(fill = "springgreen",
                 colour = "darkgreen",
                 bins = 10)
########################################

ggplot(data = survey, aes(y=Height))+
  geom_boxplot()
ggplot(data = survey, aes(y=Height, x=Smoke))+
  geom_boxplot()
ggplot(data = survey, aes(y=Height, fill=Smoke))+
  geom_boxplot()
ggplot(data = survey, aes(y=Height, color=Smoke))+
  geom_boxplot()
#############################################
# graph for analyzing probability distribution
# of a numerical variable
ggplot(data = survey, aes(x=Pulse))+
  geom_density(fill = "springgreen",
               colour = "darkgreen")
#############################################
ggplot(data = survey, aes(x=Exer, fill=Exer))+
  geom_bar()
ggplot(data = survey, aes(y=Exer, fill=Exer))+
  geom_bar()
ggplot(data = survey, aes(x=Exer, fill=Smoke))+
  geom_bar(position = 'dodge')
# bar plots by default plot the counts
###############################################
means_age <- survey |> 
  group_by(Exer) |> 
  summarise(avg=mean(Age,na.rm=T))
ggplot(data = means_age, 
       aes(x=Exer, y=avg, fill=Exer))+
  geom_bar(stat = 'identity')
##############################################




setwd("D:/Training/Academy/R Course (C-DAC)/Datasets/")
library(tidyverse)
survey <- read.csv("survey.csv", 
                   stringsAsFactors = T)

ggplot(data = survey,
       aes(x=Height, y=Pulse,color = Exer))+
  geom_point()

ggplot(data = survey,
       aes(x=Height, y=Pulse,color = Exer))+
  geom_point()+
  facet_grid(Exer~.)


ggplot(data = survey,
       aes(x=Height, y=Pulse,color = Exer))+
  geom_point()+
  facet_grid(.~Exer)

ggplot(data = survey,
       aes(x=Height, y=Pulse,color = Exer))+
  geom_point()+
  facet_grid(Sex~Exer)+
  labs(title = "Pulse by Height and Exercise",
       color = "Exercise")
#####################################################
library(plotly)
p <- ggplot(data = survey,
            aes(x=Height, y=Pulse,color = Exer))+
  geom_point()
ggplotly(p)
#####################################################
p <- ggplot(data = survey, 
            aes(y=Height, x=Smoke,fill=Smoke))+
  geom_boxplot()
ggplotly(p)
####################################################
data(EuStockMarkets)
EuStockMarkets <- as.data.frame(EuStockMarkets)
EuStockMarkets$Time <- seq(1, nrow(EuStockMarkets))
p <- ggplot(data = EuStockMarkets,
            aes(x=Time, y=DAX))+
  geom_line()
ggplotly(p)
######
df <- EuStockMarkets |> 
  pivot_longer(cols = c(DAX, SMI, CAC, FTSE),
               names_to = "MarketIndex",
               values_to = "Value")
p <- ggplot(data = df,
            aes(x=Time, y=Value, color = MarketIndex))+
  geom_line()
ggplotly(p)

####################################################
data("mtcars")
mtcars$gear <- factor(mtcars$gear)
ggplot(data = mtcars, 
       aes(x=disp, y=mpg, colour=gear))+
  geom_point()
















