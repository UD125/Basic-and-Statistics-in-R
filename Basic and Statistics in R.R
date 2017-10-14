Name <- c("a","b","c","d","e","f","g","h","i","j")
Age <- c(22,43,12,17,29,5,51,56,9,44)
Sex <- c("M","F","M","M","M","F","F","M","F","F")
data1 <- data.frame(Name,Age,Sex,stringsAsFactors = FALSE)
data1
data1$Sex <- as.factor(data1$Sex)
str(data1)
which.min(data1$Age)
which.max(data1$Age)
cumsum(data1$Age)
cumprod(data1$Age)
prod(data1$Age)

x <- seq(1,100)
above10 <- function(x){
  a<- x>10
  x[a]
  
}
above10(x)

x1 <- seq(1,1000)

above50 <- function(x1){
  b <- x1>50
  x1[b]
}
above50(x1)
above50(x)
#functions
above <- function(x,n){
  Use<- x>n
  x[Use]
}
above(x,80)
above(x1,600)

y1 <- seq(1,1000)
above60 <- function(x1,y1){
  b <- x1>50
  c <- y1>60
  x1[b]
  y1[c]
  return(c(b,c))
}
above60(x1,y1)
#Simple Function

A <- function(x,y){
  x+y
}
A(2,3)
#Complex Function
A1 <- function(x,y){
  z1 <- x+y
  z2 <- 2*x+y
  z3 <- x+2*y
  z4 <- x^2+y^2
  return(c(z1,z2,z3,z4))
}
A1(2,3)
ans <- A1(2,3)
A1 <- function(x,y){
  z1 <- x+y
  z2 <- 2*x+y
  z3 <- x+2*y
  z4 <- x^2+y^2
  print(c(z1,z2,z3,z4))
}

#Control Structers in R
#For Loop & Nested for loops
#While loop
#Repeatloop
#Statistics in R

for(i in 1:100){
  print(i)
}

#1 
x <- c("a","b","c","d","e")
x

for(letters in x){
  print(letters)
}
#Print the first four letters of x
for(i in 1:4){
  print(x[i])
}

#Print the years from 2000 to 2020 through for loop
print(paste("The year is",2010))

for(i in 2000:2020){
  print(paste("The year is",i))
}
#or
Year <- seq(2000,2020)
for(i in Year){
  print(paste("The year is",i))
}

#Print the years from 2000 to 2020 through for loop , if they are only leap years
for(i in Year){
  if(i%%4){next}
  print(paste("The Leap year is",i))
}

#--------------------------------------Nested For Loops-----------------------------
z <- matrix(1:16,4,4)
seq_len(nrow(z))
#Extracting the elements of matrix through Nested for loop
for(i in seq_len(nrow(z))) {
  for(j in seq_len(ncol(z)))
    print(z[i,j])
}

for(i in 1:5){
  for(j in 1:2)
    print(i*j)
}

#---------------------------------While Loop-------------------------------
#While loop begins with testing a condition. 
#If it is true then they execute the loop body
#Once the loop body is executed ,
#the condition is tested again and so forth.

count <- 0
while(count < 10)
{print(count)
  count <- count + 1
}
## While loop can potentially results in infinite loop if not written
##properly. So we should use with care

y <- 0
i <- 100
while(i <= 200) {
  i = i+(i*.085)
  y = y+1
  print(i)
  print(y)
}

192.0604+192.0604*0.085 #After this loop condition ends

#----------------------------------------Repeat Loop-----------------------------
##Repeat loop initiates an infinite loop,these are noy commonly used in
##statistical applications but they do have their uses. The only way to exit 
## a repeat loop is to call (break)

i <- 2
repeat {
  print(i)
  i = i + 1
  if(i > 4)
    break
}

## If (break) is ommitted it will be an infinite loop



#-----------------------------Statistics in R---------------------------------
install.packages(nortest)
library(nortest)
#T - test
ClassA = c(18,22,21,17,20,17,23,20,22,21) 
ClassB = c(16,20,14,21,20,18,13,15,17,21)
length(ClassA)
length(ClassB)

mean(ClassA)
mean(ClassB)

median(ClassA)
median(ClassB)

boxplot(ClassA,ClassB)

summary(ClassA)
summary(ClassB)

#----------Two-tailed T-test-------------
#HO: There is no difference between the means
#H1: The mean of two groups are not the same

t.test(ClassA,ClassB) #We reject null H0 as pvalue = 0.03798
#When P-value is less then 0.05 we reject H0 at 95% confidence interval

#Confidence Level :90% , p-value: 0.1
#Confidence Level :95% , p-value: 0.05
#Confidence Level :99% , p-value: 0.01
#Confidence Level :99.9% , p-value: 0.001

#----------One-tailed T-test----------
##H0 : There is  no difference 
##H1 : The difference is less then 0 # Mean of ClassA is less than ClassB
t.test(ClassA,ClassB,alternative = "less",var.equal = T)

##H0 : The difference is less then 0
##H1 : The difference is greater then 0
t.test(ClassA,ClassB,alternative = "greater",var.equal = T)

#-----------------------Analysis of Variance (ANOVA)----------------
smoke <- c(38,42,14,41,41,16,36,39,18,32,36,15,28,33,17)
income <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)
age <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)

data4 <- data.frame(smoke,income,age)

##Here we want to test whether the score of smoke is different across 
##categories of income or age

#Test with age var
fit <- aov(smoke ~ age , data = data4)
summary(fit) 

#Test with income var
fit <- aov(smoke ~ income , data = data4)
summary(fit) 

##To Test
#Categorical - Continous Variables we use ANOVA
#Categorical - Categorical Variables we use  Chi-square
#Continous -Continous Variables we use Correlation

#---------------------------Chi-square Test of Independence---------------
##When both variables are categorical in nature
##Two random variables x and y are called Independent
##If the probability distribution of one variable is not affected by the presence of the another.

library(MASS)
sur <- survey
head(survey)
dim(sur)
levels(sur$Smoke)
levels(sur$Exer)
tbl = table(sur$Smoke,sur$Exer)#Contingency Table of Smoke and Exerc
tbl
#Test the hypothesis whether the student smoking habit is independent of their
#exercise level at .05 level of significance
chisq.test(tbl)
#Since , p-value is >.05 hence we fail to reject H0 : Smoking habit is independent
#of their Exercise level

#---------------------------------Normality Test------------------------------

install.packages("nortest",dependencies = TRUE)
library(nortest)

head(mtcars)
hist(mtcars$mpg)
barplot(mtcars$mpg)


##Superimpose a Normal Curve
x <- mtcars$mpg
m <- median(x)
x
m
std <- sqrt(var(x))
std
hist(x,density = 20,breaks = 20,prob = TRUE, xlab = "x-variable",
     ylim = c(0,0.15),main = "Normal curve over histogram")
curve(dnorm(x,mean = m,sd=std),col="darkblue",lwd=2,add = TRUE)

ad.test(x)

#---------------Treatment of Missing Values :Example with substituting with the mean of the series
A <- data.frame(a=1:10,b=11:20)
A[A$b<14,"b"]=NA
A
A1<-A
as.data.frame(colSums(is.na(A)))

#Imputing the values with mean value of the series
A1[is.na(A1$b),"b"] = mean(A1$b,na.rm = T)
A1

#Imputing the values with median value of the series
A1[is.na(A1$b),"b"] = mean(A1$b,na.rm = T)
A1


#------------------SQL in R using package SQLDF-----------------------
Name <- c("a","b","c","d","e","f","g","h","i","j")
Age <- c(22,43,12,17,29,5,51,56,9,44)
Sex <- c("M","M","F","M","M","M","F","M","F","M")
data1 <- data.frame(Name,Age,Sex,stringsAsFactors = FALSE)
data1
summary(data1)
data1$Sex <- as.factor(data1$Sex)
install.packages("sqldf",dependencies = TRUE)
library(sqldf)

#-----Selecting all the variables-----
S1 <- sqldf("select *
            from data1") # * - refers to all var in the dataset
S1
#-----Selecting Variables based on Names--
S2 <- sqldf("select Name, Age
            from data1")
S2
#------Creating Alias Names from the column----
S3 <- sqldf("select name as Full_name, age as Total_age
            from data1")
S3
#------------Subsetting Data with Condition----
S4 <- sqldf("select *
            from data1
            where Age > 20") #where condition
S4
#------------Subsetting Data with AND condition-----
S5 <- sqldf('select *
            from data1
            where Age > 20 AND Sex == "M" ')
S5
#------------Subsetting Data with OR condition------
S6 <- sqldf("select *
            from data1
            where Age > 20 OR Sex == 'F'")
S6
#-----------Create new column with condition------
S7 <- sqldf('select *,
            Age+10 as Age_new,
            Age-avg(Age) as Age_old
            from data1')
S7
#-----------Create a new column with condition----
S8 <- sqldf('select *,
            Age+10 as Age_new,
            Age-10 as Age_old
            from data1')
S8
#-----------Descriptive Stat in SQL------
S9 <- sqldf('select 
            min(age) as min_age,
            max(age) as max_age,
            avg(age) as avg_age,
            count(age) as count_age,
            sum(age) as sum_age
            from data1')
S9
#-----------Descriptive Stat in SQL - segregated by sex ----
S10 <- sqldf('select Sex,
             min(age) as min_age,
             max(age) as max_age,
             avg(age) as avg_age,
             count(age) as count_age,
             sum(age) as sum_age
             from data1
             group by Sex')
S10
#--------Descriptive Stat in SQL - segregated by sex - descending sorted by sex
S11 <- sqldf('select Sex,
             min(age) as min_age,
             max(age) as max_age,
             avg(age) as avg_age,
             count(age) as count_age,
             sum(age) as sum_age
             from data1
             group by Sex
             order by Sex desc')
S11
#---------Else if statement-----
S12 <- sqldf("select *,
             case 
             when Age <= 20 then 'A'
             when Age > 20 AND Age <= 40 then 'B'
             else 'C'
             end classify
             from data1
             order by classify desc")
S12

S13 <- sqldf("select *,
             case 
             when Name == 'b' then 'Correct'
             else 'Incorrect'
             end new 
             from data1")
S13
#----------Filtering Data-------
S14 <- sqldf('select *
             from data1
             where Age< 40 and Sex == "M"')
S14
#-------- Always use "having" with group by and on summary stat
#where command does'nt work on aggregated columns, we use 'Having'
#in that columns.

#Usage of Having statement
S15 <- sqldf("select Sex,Age,
             max(age) as max_age,
             min(age) as min_age
             from data1
             group by Sex 
             having max_age - min_age >5")
S15
#---------Inserting a new field--------
S16 <- sqldf("select *, square(age) sqr_age, sqrt(age) sqrt_age
             from data1")
S16
#--------Exporting data to csv-------
write.csv(S16,"SQLinR.csv")
getwd()

S17 <- sqldf("select *
             from data1
             where Age in (29,51,56)")
S17

S18 <- sqldf("select *
             from data1
             where Name in ('a','d','e','h')")
S18
