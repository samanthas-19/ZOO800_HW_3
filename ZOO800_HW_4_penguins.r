## ZOO800 HW 4
## Created by: Samantha Summerfield, Evan Peepo, Maggie Phillips
## Date: 9/25/25



#First thing that needs to be done if you don't have it is to install the package
#install.packages("palmerpenguins")

#load the library
library(palmerpenguins)


#bring in the penguins data from the package by making it into a data frame
penguins = data.frame(penguins)

##---------------------------Objective 1---------------------------##

# Function to convert continuous variable into binary variable
binary <- function(x, breakpoint, labels = c("low", "high")) {
  if (length(labels) != 2) {  #means there needs to be only two binaries for each penguin
    stop("You must provide bianary labels.") #it will give you this error message if the penguin wasn't given a defined size
  }
  
  binary_var <- ifelse(x <= breakpoint, labels[1], labels[2])
  return(binary_var) #this should turn each label for penguin into the specific binary like a yes or no if this is present
}


# Apply the function to body_mass_g
##using a user defined "breakpoint" will help to be able to change it later
penguins$body_size <- binary(
  x = penguins$body_mass_g,
  breakpoint = 4000,            
  labels = c("small", "large") 
)

#look to see if this is correct
View(penguins)


##---------------------------Objective 2---------------------------##


binary <- function(x, breakpoint, labels = NULL) {
  # Check that labels match number of intervals
  if (!is.null(labels) && length(labels) != (length(breakpoint) - 1)) {
    stop("Number of labels must equal number of intervals (length(breakpoint) - 1).")
  }
  
  categorized_var <- cut( #change the variable name to categorize because it made more sense, since this isn't a binary anymore
    x,                   #using the cut function to makre sure the data gets broken up at the breaks and by certain labels
    breaks = breakpoint,
    labels = labels,
    include.lowest = TRUE,  # include the lowest value in the first category
    right = TRUE            # intervals are (a,b], meaning upper bound inclusive
  )
  
  return(categorized_var)  # return the result, this apparently needs to be done with the function
}                          #had to use chatgpt here to debug the code, I didn't realize the return
                           #function was needed to complete the loop

#define the breaks that are wanted for each size category, I just chose randomly
breaks = c(0, 3000, 4000, 10000)

#then add the categories you want and label the, make sure they go into the data frame in a correct column
penguins$body_size <- binary(
  x = penguins$body_mass_g,
  breakpoint = breaks,            
  labels = c("small", "medium", "large") 
)

#check to make sure the sizes are sorted as small, medium, and large
View(penguins)


##---------------------------Objective 3---------------------------##

#The researchers forgot to mention there are multiple species included in this
#dataset! Obviously, what constitutes a ‘small’ or ‘large’ penguin depends on
#the species.
#a) Use the quantile function (or something similar) to determine sensible
## breakpoints for each species

#from what I understand the quantile function is used to find out 
# percentages of variables in one category. It can tell you how much of something is in the whole data set


quantile(penguins$body_mass_g, #first choose the data and what column, won't work if 
         #                        you do all the data since it isn't always numerical data
         na.rm = TRUE, # you need to tell the function that there are NAs in the data set
         probs = c(0.15, 0.25, 0.35, 0.50, 0.65, 0.75))  #now choose your percentages to look at the data, 
#                                   this will print that one number is in the --% of the data

q = c(0.25, 0.50, 0.75)

penguins %>%
  group_by(species) %>%
summarize(quant15 = quantile(body_mass_g, na.rm = TRUE, probs = 0.15), 
          quant25 = quantile(body_mass_g, na.rm = TRUE, probs = 0.25),
          quant35 = quantile(body_mass_g, na.rm = TRUE, probs = 0.35),
          quant50 = quantile(body_mass_g, na.rm = TRUE, probs = 0.50),
          quant65 = quantile(body_mass_g, na.rm = TRUE, probs = 0.65),
          quant75 = quantile(body_mass_g, na.rm = TRUE, probs = 0.75),
          quant85 = quantile(body_mass_g, na.rm = TRUE, probs = 0.85))


library(dplyr)

#using tapply, we can apply the function to multiple groups
do.call("rbind",tapply(penguins$species, penguins$body_mass_g, quantile))

#b) Modify the functions in Objective 2 to discretize body mass conditional
#on species (HINT: this will likely require a for loop or if statements inside
  #          the function)


#c) Use your function to convert body mass into a categorical variable with
#three levels with different breakpoints for each species








##---------------------------Objective 4---------------------------##










##---------------------------Objective 5---------------------------##












