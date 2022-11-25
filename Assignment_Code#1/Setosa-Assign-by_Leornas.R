
#1. use data(iris)
data("iris")
iris

#Subset Iris into 3 files per Species 

#2nd File subset setosa

subset(iris, Species == "setosa")

#Get the total mean of each Species (2nd File: setosa)

#Declare each column of iris to calculate the total mean of the setosa
m1 <- iris$Sepal.Length[1:50]
m2 <- iris$Sepal.Width[1:50]
m3 <- iris$Petal.Length[1:50]
m4 <- iris$Petal.Width[1:50]

m1
m2
m3
m4

#Get the total mean
tm1 <- c(m1,m2,m3,m4)
tm1

mean(tm1) #Total mean is 2.5355 in setosa Species


#GET THE MEAN OF EACH CHARACTERISTICS OF THE SPECIES
mean(m1)#Answer is 5.006
mean(m2)#Answer is 3.428
mean(m3)#Answer is 1.462
mean(m4)#Answer is 0.246
