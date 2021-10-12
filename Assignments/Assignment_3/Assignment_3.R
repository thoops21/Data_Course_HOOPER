###########################
#                         #
#    Assignment Week 3    #
#                         # 
###########################

# Instructions ####
# Fill in this script with stuff that we do in class.
# It might be a good idea to include comments/notes as well so you remember things we talk about
# At the end of this script are some comments with blank space after them
# They are plain-text instructions for what you need to accomplish.
# Your task is to write the code that accomplished those tasks.

# Then, make sure to upload this to both Canvas and your GitHub repository




# Vector operations! #### 

# Vectors are 1-dimensional series of values in some order
1:10 # ':' only works for integers, this is a vector with 1 dimension
letters # built-in pre-made vector of a - z



vector1 <- c(1,2,3,4,5,6,7,8,9,10)
vector2 <- c(5,6,7,8,4,3,2,1,3,10)
vector3 <- letters

vector1 + 5
vector2 / 2
vector1*vector2

vector3 + 1 # can't add 1 to "a"
# cant do this because we havent assigned a number value to a

TF <- c(TRUE, FALSE, FALSE,TRUE)
TF + 1
mix <- c(TF,letters)

chars <- c(letters, vector1)[30:33]

as.numeric(chars)
as.character(1:10)



# Data Frames ####
# R has quite a few built-in data sets
data("iris") # load it like this

# For built-in data, there's often a 'help file'
?iris
iris
data("mtcars")

# "Iris" is a 'data frame.' 
# Data frames are 2-dimensional (think Excel spreadsheet)
# Rows and columns
# Each row or column is a vector


dat <- iris # can rename the object to be easier to type if you want

# ways to get a peek at our data set
names(dat)
dim(dat)
head(dat)

# You can access specific columns of a "data frame" by name using '$'
test <- dat$Species
test2 <- dat$Sepal.Length
dat$Sepal.Length

str(dat)

# You can also use square brackets to get specific 1-D or 2-D subsets of a data frame (rows and/or columns)
dat[1,1] # [Rows, Columns]
dat[1:3,5]
data.frame[rows,columns]


# Plotting ####

# Can make a quick plot....just give vectors for x and y axes
plot(x=dat$Petal.Length, y=dat$Sepal.Length)
plot(x=dat$Species, y=dat$Sepal.Length)


# Object "Classes" ####

#check the classes of these vectors
class(dat$Petal.Length)
class(dat$Species)

# plot() function behaves differently depending on classes of objects given to it!

# Check all classes (for each column in dat)
str(dat)

# "Classes" of vectors can be changed if needed (you'll need to, for sure, at some point!)

# Let's try
nums <- c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric=class

# convert to a factor
as.factor(nums) # show in console
nums_factor <- as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it

#check it out
plot(nums) 
plot(nums_factor)
# take note of how numeric vectors and factors behave differently in plot()

# Let's modify and save these plots. Why not!?
?plot()
plot(nums, main = "My Title", xlab = "My axis label", ylab = "My other axis label")


?jpeg()


dev.off()




# Making a data frame ####

# LET'S LEARN HOW TO MAKE A DATA FRAME FROM SCRATCH... WE JUST FEED IT VECTORS WITH NAMES!

# make some vectors *of equal length* (or you can pull these from existing vectors)
col1 = c("hat", "tie", "shoes", "bandana")
col2 = c(1,2,3,4)
col3 = factor(c(1,2,3,4)) # see how we can designate something as a factor             

# here's the data frame command:
data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # colname = vector, colname = vector....
df1 = data.frame(Clothes = col1, Numbers = col2, Factor_numbers = col3) # assign to df1
df1 # look at it...note column names are what we gave it.

max(vector1)
min(vector1)
median(vector1)
mean(vector1)
range(vector1)
summary(vector1)

# cumulative functions
cumsum(vector1)
cumprod(vector1)
cummin(vector1)
cummax(vector1)

dbinom


# Practice subsetting ####

# Make a data frame from the first 20 rows of iris that has only Species and Sepal.Length columns
# save it into an object called "dat3"





# WRITING OUT FILES FROM R ####
?write.csv()


# Write your new object "dat3" to a file named "LASTNAME_first_file.csv" in your PERSONAL git repository




### for-loops in R ####

#simplest example:
for(i in 1:10){
  print(i)
}

#another easy one
for(i in levels(dat$Species)){
  print(i)
}

# can calculate something for each value of i ...can use to subset to groups of interest
for(i in levels(dat$Species)){
  print(mean(dat[dat$Species == i,"Sepal.Length"]))
}

mtcars
iris

install.packages("ggplot2")
# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150
iris.file <- iris  # here I named the file to include the data in the file 
iris.file[seq(2,150,2),]  # here we got a subset of data from the iris file of all even numbered rows 

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class
iris$Sepal.Length <- as.character(iris$Sepal.Length)
iris$Sepal.Width <- as.character(iris$Sepal.Width)
iris$Petal.Length <- as.character(iris$Petal.Length)
iris$Petal.Width <- as.character(iris$Petal.Width)
iris$Species <- as.character(iris$Species)

iris_chr <- c(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, iris$Species)
# created a new file

# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width
Sepal.Area <- as.numeric(iris$Sepal.Length)*as.numeric(iris$Sepal.Width)

iris <- dat
#  I had to do this extra step because I messed up on number 4 and did  cbind with only sepal area and it  got rid of all my data, besides sepal area

# 4.  Add Sepal.Area to the iris data frame as a new column
iris.file <- cbind(Sepal.Area,iris)


# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)
subset(iris.file, Sepal.Area > 20) 
# did that to create the specific subset that included that area 
big_area_iris <- subset(iris.file, Sepal.Area > 20)



# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
# to canvas
# I should be able to run your R script and get all the right objects generated
this is my assignment3
This is Assingment #3
This is my assingment #3
