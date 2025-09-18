## Created by: Samantha Summerfield, Evan Peepo
## Date: 9/18/25


##-------------Objective 1----------------##

##making variable as 18.5 degrees celsius
temp_C = 18.5

##using the calculation Celsius to Fahrenheit C*9/5 +32
temp_F = ((temp_C *9/5) +32)

##use print function to make the sentence
##the paste0 allows us to add the variables and characters together

print(paste0("The water temperature is ", temp_C, "°C (", temp_F, "°F)"))

##-------------Objective 2----------------##

#creating a vector with certain values
species_counts = c(Bluegill = 12, Bass = 7, Sunfish = 21, Carp = 3)
#type the variable next to confirm it looks correct
species_counts

#now find the total number of fish counted
#the function sum is able to count the numbers together
sum(species_counts)

#this will pick out the one with the most species 
#and report the number of species with it
species_counts[which.max(species_counts)]

day1 <- c('surface' = 20, 'mid' = 13, 'bottom' = 5)
day2 <- c('surface' = 28, 'mid' = 11, 'bottom' = 8)
day3 <- c('surface' = 34, 'mid' = 16, 'bottom' = 2)

chlorophyll_array <- array(c(day1, day2, day3), dim = c(3,3))
rowMeans(chlorophyll_array)


##-------------Objective 3----------------##

lakes = data.frame(
            Lake = c("Mendota", "Wingra", "Monona", "Waubesa", "Kegonsa"),
            Temp_C = c(22.4, 25.1, 23.7, 24.6, 26.0),
            DO_mgL = c(8.3, 6.7, 7.5, 7.9, 6.2))


# Print the data frame to make sure it looks correct
lakes
#using the view function you can see it as a new tab 
View(lakes)

#calculate the average temp and DO by calling on the specific
#column in the data frame
average_temp = mean(lakes$Temp_C)
average_temp

average_DO = mean(lakes$DO_mgL)
average_DO

#create the formula with Temp_C to Fahrenheit

temp_CF = lakes$Temp_C

Temp_F = (temp_CF*9/5) + 32

Temp_F

lakes_CF = data.frame(lakes, Temp_F)
lakes_CF

#now to do the Bonus part!
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

##-------------Objective 4----------------##

for (n in 1:10) {
  print(n^2)
}
pop <- vector('numeric', 10L)

for (t in 1:10) {
  pop[t] <- (10 * exp(.3 * t))
}  

lake1 <- c(7, 14, 32, 18)
lake2 <- c(35, 21, 6, 13)
lake3 <- c(22, 11, 15, 5)
lake4 <- c(12, 33, 25, 9)
lake5 <- c(15, 21, 6, 36)

lake_phos_list <- list(lake1, lake2, lake3, lake4, lake5)

lake_means <- vector('numeric', 5L)

for (i in 1:5) {
  lake_means[i] <- mean(lake_phos_list[[i]])
  print(paste0('Lake', i, ' mean phosphorus = ', lake_means[i], 'µg/L'))
  
}

lake_means