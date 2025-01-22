################################################################################

###IDENTIFICATION OF DUPLICATES

# This script provides examples of how to identify exact or near duplicates 
#in a dataset

#Created on: 21 January 2025 by Gersan Vasquez
#Last updated: 21 January 2025 by Gersan Vasquez
#CORE's Data and MEL unit

################################################################################


###### OPTION 1: NEAR DUPLICATE COMPARISON #####

#This option is sugggested to analyze duplicate records with slightly different 
#names. It's recommended when comparing participants from different registration
#lists. The results will provide a similarity score ranking from 0 to 1.The 
#closer the score is to 100, the more likely it is that the participants are the 
#same person.

# Step 1: Load packages
library(dplyr)
library(stringdist)

# Step 2: Create a sample data frame
# Database needs to be the same number of rows. Add NAs to make it the same. 

name_data <- data.frame(
  name_source1 = c("John Smith", "Michael Brown", "Lisa Lee", "Sarah Wilson"),
  name_source2 = c("John H Smith", "M Brown", "Ashley Green", NA),
  stringsAsFactors = FALSE
)

# Step 3: Create  pairwise combinations between the two name sources
pairwise_combinations <- expand.grid(
  name_source1 = name_data$name_source1,
  name_source2 = name_data$name_source2,
  stringsAsFactors = FALSE
)

# Step 4: Remove rows with NA in the combinations (optional)
pairwise_combinations <- pairwise_combinations %>%
  filter(!is.na(name_source2))

# Step 5: Calculate similarities
comparisons <- pairwise_combinations %>%
  mutate(
    jaccard = stringdist(name_source1, name_source2, method = "jaccard"),
    sim = 1 - jaccard
  )

# Step 6: Sort the results from highest to lowest similarity
sorted_comparisons <- comparisons %>%
  arrange(desc(sim), name_source1, name_source2)

# View the sorted results
print(sorted_comparisons)


###### OPTION 2:  DUPLICATE BASED ON DEMOGRAPHIC CHARACTERISTICS #####

#This option is used to identify participants based on similar demographic 
#characteristics and not by names. It's useful for identifying individuals
#from the same household when assistance is restricted by household

# Step 1: Load packages
library(dplyr)

# Step 2: Create a sample data frame
name_data_2 <- data.frame(
  full_name = c("John Smith", "Michael Brown", "Lisa Lee", "James Smith"),
  hh_size = c(2, 4, 1, 2),
  location = c("City A", "City B", "City B","City A"),
  address = c("Avenue 123", "Central Street 321", "No name street 567", "Avenue 123"),
  stringsAsFactors = FALSE
)

#Step 3: Identify similarities
duplicates_hh <- name_data_2[duplicated(name_data_2[c("hh_size", "location", "address")]) | 
                          duplicated(name_data_2[c("hh_size", "location", "address")], fromLast = TRUE), ]
#Step 4: Show results
print(duplicates_hh)

#The results show that John Smith and James Smith have similar HHs demographic
#characteristics based on their hh size, location and addrress. Thus, they're
#likely to be household members.


###### OPTION 3: EXACT DUPLICATE BASED ON NAME #####

# This option is recommended for identifying exact matches in names. Any 
#difference in spelling will  result in no match. For instance: John Smith and
#Jon Smith will not be a matched


# Step 1: Load packages
library(dplyr)

# Step 2: Create a sample data frame
name_data_3 <- data.frame(
  full_name = c("John Smith", "Michael Brown", "Lisa Lee", "John Smith"),
  stringsAsFactors = FALSE
)

#Step 3: Find duplicates
duplicated(name_data_3$full_name)

#The results shows a "True" result which is a duplicate


###End of script
