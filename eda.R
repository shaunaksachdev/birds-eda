library(dplyr)

data <- read.csv("/Users/shaunaksachdev/Desktop/Lab5/ebird30.csv")
species_data <- read.csv("/Users/shaunaksachdev/Desktop/Lab5/names.csv")

# 1. Check the first few rows of the datasets to understand their structure
head(data)
head(species_data)

# 2. Calculate Total Sightings per Bird (sum of sightings across all regions)
region_data <- data[, 2:ncol(data)]  # Excluding the 'Code' column
total_sightings <- rowSums(region_data)

# Create a data frame with species code and their total sightings
# Create a data frame with species code and their total sightings
species_total_sightings <- data.frame(Species = data$Code, Total_Sightings = total_sightings)

# 3. Sort species based on the total sightings (most common first)
species_sorted <- species_total_sightings[order(-species_total_sightings$Total_Sightings), ]

# 4. Merge with species names data to get common and scientific names
# Ensure the 'Code' column in species_data corresponds to the 'Species' column in species_sorted
most_common_birds <- merge(species_sorted, species_data, by.x = "Species", by.y = "Code")

# 5. Display the top 10 most common birds (based on total sightings)
print("Top 10 most commonly spotted birds:")
top_10_birds <- head(most_common_birds, 10)
print(top_10_birds)

# 6. Birds Spotted in Only One Region
birds_spotted_once <- vector(mode = "character")
for(i in 1:nrow(data)){
  region_count <- sum(region_data[i, ] != 0)  # Count non-zero sightings
  if(region_count == 1){
    birds_spotted_once <- c(birds_spotted_once, data$Code[i])
  }
}

# Print the number of birds spotted in only one region
print(paste0("Number of birds spotted in only one region: ", length(birds_spotted_once)))

# 7. Calculate the number of regions each bird was spotted in
regions_spotted <- rowSums(region_data != 0)
birds_spotted_in_multiple_regions <- species_total_sightings[regions_spotted > 1, ]

# 8. Birds Spotted in All Regions
birds_spotted_in_all_regions <- species_total_sightings[regions_spotted == ncol(region_data), ]

# 9. Birds Spotted in Specific Region (Example for 'CA-AB' - Alberta)
birds_spotted_in_alberta <- species_total_sightings[data$CA.AB != 0, ]

# 10. Birds Spotted Only in 'CA-AB' and Not in Other Regions
birds_spotted_only_in_alberta <- species_total_sightings[data$CA.AB != 0 & rowSums(data[, 2:ncol(data)] != 0) == 1, ]

# Displaying the results
print("Birds spotted in all regions:")
print(birds_spotted_in_all_regions)

print("Birds spotted in multiple regions:")
print(birds_spotted_in_multiple_regions)

print("Birds spotted in Alberta (CA-AB):")
print(birds_spotted_in_alberta)

print("Birds spotted only in Alberta:")
print(birds_spotted_only_in_alberta)

