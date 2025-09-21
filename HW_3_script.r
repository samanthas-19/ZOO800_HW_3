## ZOO800 HW3 
## Created by: Samantha Summerfield, Evan Peepo, Maggie Phillips
## Date: 9/18/25


##-------------Objective 1----------------##

#1. making variable as 18.5 degrees celsius
temp_C = 18.5

#2. using the calculation Celsius to Fahrenheit C*9/5 +32
temp_F = ((temp_C *9/5) +32)

#3. use print function to make the sentence
##the paste0 allows us to add the variables and characters together

print(paste0("The water temperature is ", temp_C, "°C (", temp_F, "°F)"))

##-------------Objective 2----------------##

#1. creating a vector with certain values
species_counts = c(Bluegill = 12, Bass = 7, Sunfish = 21, Carp = 3)
#type the variable next to confirm it looks correct
species_counts

#2. now find the total number of fish counted
#the function sum is able to count the numbers together
sum(species_counts)

#this will pick out the one with the most species 
#and report the number of species with it
species_counts[which.max(species_counts)]

#3. 
day1 <- c('surface' = 20, 'mid' = 13, 'bottom' = 5)
day2 <- c('surface' = 28, 'mid' = 11, 'bottom' = 8)
day3 <- c('surface' = 34, 'mid' = 16, 'bottom' = 2)

chlorophyll_array <- array(c(day1, day2, day3), dim = c(3,3)) #makes 3x3 array using 3 vectors

#4.
rowMeans(chlorophyll_array) #each row is a depth, calculates mean for each row


##-------------Objective 3----------------##

#1. 
lakes = data.frame(
            Lake = c("Mendota", "Wingra", "Monona", "Waubesa", "Kegonsa"),
            Temp_C = c(22.4, 25.1, 23.7, 24.6, 26.0),
            DO_mgL = c(8.3, 6.7, 7.5, 7.9, 6.2))


# Print the data frame to make sure it looks correct
lakes
#using the view function you can see it as a new tab 
View(lakes)

#2. calculate the average temp and DO by calling on the specific
#column in the data frame
average_temp = mean(lakes$Temp_C)
average_temp

average_DO = mean(lakes$DO_mgL)
average_DO

#3. create the formula with Temp_C to Fahrenheit

lakes$Temp_F <- lakes$Temp_C * 9/5 + 32 #adds new column Temp_F to lakes which uses values from Temp_C column and applies conversion

#4. now to do the Bonus part!
####install below
#install.packages("LakeMetabolizer")
##load library
library(LakeMetabolizer)

#------this part I am currently working on------#
O_equilibrium_concentration = data.frame(lakes_CF$Temp_C, colnames("O_equilibrium_concentration"))

colnames(O_equilibrium_concentration) = "O_equilibrium_concentration"

O_equilibrium_concentration

O_equilibrium_conc =
  o2.at.sat.base(
    O_equilibrium_concentration,
    altitude = 0,
    salinity = 0,
    model = "garcia-benson"
  )

o_equilibrium_conc = data.frame(
  O_equilibrium_concentration =
    o2.at.sat.base(
    o_equilibrium_conc,
    altitude = 0,
    salinity = 0,
   model = "garcia-benson"
))

lakes_with_o
View(lakes_with_o)

lakes = c(lakes_CF, lakes_with_o)
lakes

#Bonus (Evan's solution)
lakes$equilibrium_O2_conc <- o2.at.sat.base(lakes$Temp_C) #uses LakeMetabolizer function that takes temp in C and calculates equilibrium concentration of oxygen
lakes$percent_O2_saturation <- lakes$DO_mgL / lakes$equilibrium_O2_conc * 100 #measured dissolved oxygen concentration / equilibrium O2 concentration calculated above * 100
lakes <- lakes[order(lakes$percent_O2_saturation),] #orders dataframe by ascending percent O2 saturation. 

##-------------Objective 4----------------##

#1.
for (n in 1:10) {
  print(n^2) #squares 1-10
}
#2.
pop <- vector('numeric', 10L) #initiate empty vector with 10 elements

for (t in 1:10) {
  pop[t] <- (10 * exp(.3 * t)) #runs growth equation with each time t (1-10) and assigns to element in pop
}  

#3.
lake1 <- c(7, 14, 32, 18)
lake2 <- c(35, 21, 6, 13)
lake3 <- c(22, 11, 15, 5)
lake4 <- c(12, 33, 25, 9)
lake5 <- c(15, 21, 6, 36)

phosphorus <- list(lake1, lake2, lake3, lake4, lake5) #list of 5 vectors of lakes with 4 values each 

#4.
lake_means <- vector('numeric', 5L) #initiate empty vector with 5 elements

for (i in 1:5) {
  lake_means[i] <- mean(phosphorus[[i]]) #takes mean of each vector in the list 'phosphorus' and assigns to an element in 'lake_means'
  print(paste0('Lake', i, ' mean phosphorus = ', lake_means[i], 'µg/L')) #uses means stored in 'lake_means' 
  
}

#5.
lake_means

##----------------Objective 5------------------##

#1. chlorophyll array
apply(chlorophyll_array, 1, mean) #the 1 is for rows. order is object, row/column, function
apply(chlorophyll_array, 2, mean) #the 2 is for columns

#2. lakes data frame
apply(lakes[, c(2, 3)], 2, range) #you can subset within the apply statement to exclude the lake names column

#pop growth
lapply((1:10), function(x) x^2) #using generic function x and defining it in the lapply statement
#lapply definitely feels cleaner. For something simple like this, it might be easier, but for loops make more sense otherwise