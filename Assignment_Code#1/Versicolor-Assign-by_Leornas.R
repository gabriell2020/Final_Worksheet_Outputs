
#1. use data(iris)
data("iris")
iris

#Subset Iris into 3 files per Species 

#2nd File subset setosa

 subset(iris, Species == "versicolor")

#Get the total mean of each Species (3rd File: versicolor)

#Declare each column of iris to calculate the total mean of the versicolor
m1 <- iris$Sepal.Length[51:100]
m2 <- iris$Sepal.Width[51:100]
m3 <- iris$Petal.Length[51:100]
m4 <- iris$Petal.Width[51:100]

m1
m2
m3
m4

#Get the total mean
tm1 <- c(m1,m2,m3,m4)
tm1

mean(tm1) #Total mean is 3.573 in setosa Species


#GET THE MEAN OF EACH CHARACTERISTICS OF THE SPECIES
mean(m1)#Answer is 5.936
mean(m2)#Answer is 2.77
mean(m3)#Answer is 4.26
mean(m4)#Answer is 1.326
