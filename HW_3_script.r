## Created by: Samantha Summerfield, 
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

#######this part is not working, not done properly
depths = c("surface", "mid", "bottom")
days = c("day_1", "day_2", "day_3")
chlorophyll = c(12, 5, 34)
measurements = c(days, depths, chlorophyll)
chlorophyll_array = array(measurements)
chlorophyll_array


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

C_F_temp = (temp_CF*9/5) + 32

C_F_temp

lakes_CF = data.frame(lakes, C_F_temp)
lakes_CF

#now to do the Bonus part!
install.packages("LakeMetabolizer")
library(LakeMetabolizer)



