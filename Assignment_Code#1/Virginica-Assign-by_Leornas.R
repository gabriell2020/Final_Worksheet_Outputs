
#1. use data(iris)
data("iris")


#Subset Iris into 3 files per Species 

#1st File Iris_Subset Virginica

 subset(iris, Species == "virginica")

#Get the total mean of each Species (1st Filenamed: Virginica)

#Declare each column of iris to calculate the total mean of the virginica
m1 <- iris$Sepal.Length[101:150]
m2 <- iris$Sepal.Width[101:150]
m3 <- iris$Petal.Length[101:150]
m4 <- iris$Petal.Width[101:150]

m1
m2
m3
m4

#Get the total mean
tm1 <- c(m1,m2,m3,m4)
tm1

mean(tm1) #Total mean is 4.285 in viginica Species


#GET THE MEAN OF EACH CHARACTERISTICS OF THE SPECIES
mean(m1)#Answer is 6.588
mean(m2)#Answer is 2.974
mean(m3)#Answer is 5.552
mean(m4)#Answer is 2.026


