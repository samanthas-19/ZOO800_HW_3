##hw 4 attempts




install.packages("palmerpenguins")

library(palmerpenguins)



penguins
penguins.df = data.frame(penguins)


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


##Objective 2


binary <- function(x, breakpoint, labels = NULL) {
  # Check that labels match number of intervals
  if (!is.null(labels) && length(labels) != (length(breakpoint) - 1)) {
    stop("Number of labels must equal number of intervals (length(breakpoint) - 1).")
  }
  
  categorized_var <- cut( #change the variable name to categorize because it made more sense, since this isn't a binary anymore
    x,
    breaks = breakpoint,
    labels = labels,
    include.lowest = TRUE,  # include the lowest value in the first category
    right = TRUE            # intervals are (a,b], meaning upper bound inclusive
  )
  
  return(categorized_var)  # <-- return the result, this apparently needs to be done with the function
}

#define the breaks that are wanted for each size category
breaks = c(0, 3000, 4000, 10000)

#then add the categories you want and label the, make sure they go into the data frame in a correct column
penguins$body_size <- binary(
  x = penguins$body_mass_g,
  breakpoint = breaks,            
  labels = c("small", "medium", "large") 
)


