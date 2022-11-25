#Gabby

#Worksheet5

#1. The table shows the enrollment of BS in Computer Science, SY 2010-2011.
#Course Year 2019 - 2020
#  1st           80
#  2nd           75
#  3rd           70
#  4th           60

#a. Plot the data using a bar graph. Write the codes and copy the result.
x2019_2020 <- c(80,75,70,60)
numb_1a <- barplot(x2019_2020)


#b. Using the same table, label the barchart with
#Title = ” Enrollment of BS Computer Science
#horizontal axis = “Curriculum Year” and
#vertical axis = “number of students”
course <- c("1st","2nd","3rd","4th")

numb_1b <- barplot(x2019_2020,
                  main = "Enrollment of BS Computer Science",
                  xlab = "Curriculum Year", names.arg = course)
numb_1lb

#2. The monthly income of De Jesus family was spent on the following:
#60% on Food, 10% on electricity, 5% for savings, and
#25% for other miscellaneous expenses.


#a. Create a table for the above scenario.
#Write the codes and its result.
expenses <- c(60,10,5,25)

barplot(expenses,names.arg = c("Food", "Electricity", "Savings", "Miscllaneous"))


#b. Plot the data using a pie chart. Add labels, colors and legend.
#Write the codes and its result.
pie(expenses)
numb2b <- pie(expenses,
              col = rainbow(length(expenses)),
              labels = c(60,10,5,25))

ex_labels <- round(expenses/sum(expenses) * 100, 1)

ex_labels <- paste(ex_labels,"%",sep = "")
pie(expenses, main = "Expenses",col=rainbow(length(expenses)),labels = ex_labels,cex=0.8)

legend(1, c("Food", "Electricity", "Savings", "Miscllaneous"),
       cex = 0.8,fill = rainbow((length(expenses))))

#3. Open the mtcars dataset.
data("mtcars")
numb3 <- mtcars$mpg
numb3
#a. Create a simple histogram specifically for mpg (miles per gallon) variable.
#Use $ to select the mpg only. Write the codes and its result.
numb3a <-hist(numb3, xlab="Miles Per Gallon",
              main="Histogram of mpg")


#b. Colored histogram with different number of bins.
numb3b <-hist(numb3, breaks=12, col="red", xlab="Miles Per Gallon",
              main="Histogram of mpg")

#Note: breaks= controls the number of bins

#c. Add a Normal Curve

numb3c <-hist(numb3, breaks=12, col="red", xlab="Miles Per Gallon",
              main="Histogram with Normal Curve")
xfit<-seq(min(numb3),max(numb3),length=40)
yfit<-dnorm(xfit,mean=mean(numb3),sd=sd(numb3))
yfit <- yfit*diff(numb3c$mids[1:2])*length(numb3)
lines(xfit, yfit, col="blue", lwd=2)

#Copy the result.


#4. Open the iris dataset. Create a subset for each species.

#a. Write the codes and its result.
data("iris")
set <- subset(iris, Species == "setosa")
ver <- subset(iris, Species == "versicolor")
vir <- subset(iris, Species == "virginica")

#b. Get the mean for every characteristics of each species using colMeans().
#Write the codes and its result.
set <- subset(iris, Species == "setosa")
setosa <- colMeans(set[sapply(set,is.numeric)])
setosa

ver <- subset(iris, Species == "versicolor")
versicolor <- colMeans(ver[sapply(ver,is.numeric)])
versicolor

vir <- subset(iris, Species == "virginica")
virginica <- colMeans(vir[sapply(vir,is.numeric)])
virginica


#Example: setosa <- colMeans(setosa[sapply(setosaDF,is.numeric)])
#c. Combine all species by using rbind()
#The table should be look like this:
trans3 <- rbind(setosa,
                versicolor,
                virginica)
trans3

#Sepal.Length Sepal.Width Petal.Length Petal.Width
#setosa
#versicolor
#virginica


#d. From the data in 4-c: Create the barplot().
#Write the codes and its result.
#The barplot should be like this.
barplot(trans3, beside = TRUE,
        main = "Iris Mean",
        xlab = "Characteristics",
        ylab = "Mean Scores",
        col = c("red","green","blue"))
#Figure 1: Iris Data using Barplot

