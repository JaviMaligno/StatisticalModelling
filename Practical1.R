############################################################ 
##              Statistical Modelling 2023                ##
##                  Practical 1                           ##
##                                                        ##
##                Dr.Davide Pigoli                        ##
############################################################



# This is a script to be run in the R Statistical Software.
# Any line beginning with # is commented and will not be run.
# Line not beginning with # are code that you are meant to run
# (i.e. input in the R Console) and see what happens (more details below).

# First, follow the tutor instructions (or ask!) on how to open R and RStudio
# on your device.

# In this practical we review the essentials to start with R programming 
# and we learn to import simple datasets in R and to compute summary statistics 
# and basic graphs.

# This is a very short overview of R basics, please refer to 
# online resources such as the introduction to R that can be found 
# at \url{https://cran.r-project.org/manuals.html} and R help to learn more 
# (when needed). Your tutor is also here to help you, please
# ask about anything that is unclear.

# 1) Use of the command line

# You can use the R Console (command line) as a calculator. 

# Try typing in the following instructions, and press enter:

2+3
2*3
3*4
8/2
sqrt(2) # square root
exp(2) # natural exponential
2^3     # power

# but more often you will want to define and use variables:

a<-1
b<-2
a+b

# if you just write the variable in the console, its content will be printed at screen

a
b

# Technical comment: R variables are untyped (you don't need to
# specify in advance if the variable is going to contain a number, a character, etc..)
# but that does not mean that the object they reference is untyped:

typeof(a)

# This is important to remember because some 
# R functions behave differently depending on the type of the object 
# (e.g., numeric, character, boolean,...). 





# 2) Vector, matrices and data frame

# Vectors:

x<-c(5,2,6,1,4,1)
x

y<-c('a','b','c','d','e','f')
y

z<-1:6
z

v<-c(x,y,z)
v

# if you are using RStudio (and you should!), check out these objects in the Environment tab on the top right.

# Matrices:

A<-matrix(y,2,2)
A
A[1,2]
B<-matrix(z,2,3)
B
B[2,3]

# You can then overwrite elements:

B[1,2]<-1000

# Vectors and matrices need to contain 
# objects of the same type. 

# Data frame can include vectors of different type, 
# but they need to be of the same length, 
# because each row of the data frame is interpreted as an observation:

data<-data.frame(x,y,z)
data

head(data)

# You can then access the data frame as a matrix:

data[1,2] # element
data[1,] # row (observation)
data[,2] # column (variable)

# or using variables names:
data$x
data$y
data$z


# 3) Functions in R

# In R it is possible to build your own functions
# but many useful ones are already available, such as

mean(x)
sum(x)
max(x)
sd(x)
table(y) # frequency table
hist(x)
pie(table(y))

# so when you need to do something, look out for the functions that can help you!

# try:

help(hist)

# to see the help instructions for a function of interest.

# 4) Data import and export:

# The first step of the data analysis is necessarily to import data in R. 
# For small examples, this can be done manually by defining vectors and data.frame 
# which contain the data. However, this is not feasible for real data size. 
# It is possible to import data in R using the most common file format such as 
# .txt or .csv, using the functions read.table(), read.csv() and similar. 
# The function parameters need to be set to specify the column separator, 
# decimal symbol used in your file, etc. See the help of these functions to get an idea:

help(read.table)

# IMPORTANT: Pay attention that the files need to be in your working directory 
# to be imported. This can be found with the command 

getwd()

current<-getwd() # save the name of the current working directory. 

# and set with the command

setwd(current)

# or using the menu tabs in R/RStudio (ask the tutor is needed).

# EXERCISE: set a new working directory.
# You can for example create a folder "Practical 1" on your desktop 
# and set it as working directory.

# Now download the file protein.csv from the module KEATS
# page (in the material for Week 1) and save it in your
# working directory.

# This dataset contains the consumption
# of different foods in various countries.

# Then, import the data in R:

protein<-read.csv("protein.csv")
head(protein)

attach(protein) # this allows us to access the named variables directly
mean(RedMeat) # mean  red meat consumption

# EXERCISE: Compute the mean of the consumption
# of other types of food.

# 5) Editor and graphical interface:

# While it is possible to write your commands directly in the console, 
# it is advisable to use an editor to prepare the commands you want to run to minimise 
# errors, allow for easy changes and check what you have done. 
# The basic R installation comes with an editor and graphical interface,
# or you can use RStudio.


# EXERCISE: Try to open a new script in the editor (Left button on top of the editor 
# window in RStudio), write an arithmetic operation there and run it in the console.
# 

# Let's now draw a scatterplot of the consumption of Red Meat and Fish from
# the data above.

plot(RedMeat~Fish, col="lightblue", pch=19, cex=2,data=protein)
text(RedMeat~Fish, labels=Country,data=protein, cex=0.9, font=2)

# Try using the graphical interface to save the plot on your device.

# This will be important when preparing the figures
# for your coursework.