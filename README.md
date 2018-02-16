# Comparing Players in Fifa 18
## Ariel Aguilar Gonzalez

This purpose of this project is to compare players by attributes in the Fifa 18 video game. The full interactive visualization can be seen at THIS LINK. The code below demonstrates how I prepared and cleaned the Fifa 18 data.

The Fifa 18 players database is available through [Kaggle](https://www.kaggle.com/kevinmh/fifa-18-more-complete-player-dataset), and includes almost 18,000 players with 185 fields for each player including about 36 player attributes, such as finishing and crossing. 

```r
#-------------------------------#
#    Import Data & Libraries    #
#-------------------------------#

library("dplyr")
library("readr")
library("rvest")
library("purrr")
library("stringr")
library("tidyr")
library("highcharter")
library("ggplot2")
library("htmltools")
library("ggdendro")

player_raw <- read_csv(".../Kaggle Dataset.csv")
# 17,994 players
# 185 attributes

#-------------------------------#
#       Data Validation         #
#-------------------------------#

# Check that non-English letters in player names are read in correctly
player_raw$name[1:50]
# First 50 names looks good, different character accents are read in correctly

num_cols <- sapply(player_raw, is.numeric)
player_num <- player_raw[,num_cols]
summary(player_num)
# All of the attribute variables are on a scale of 0-100
# height_cm, weight_kg are clean
# there's also ratings for each outfield position (seperate for gks)
# stick to player preferences to describe position

#-------------------------------#
#        Data Cleaning          #
#-------------------------------#

# Any NA values?
anyNA(player_raw)
# True

# How many rows contain NA values?
nrow(player_raw[rowSums(is.na(player_raw)) > 0,])
# 17,994, all of them

# Which columns contain NA values?
colnames(player_raw)[colSums(is.na(player_raw)) > 0]
# Club/League (Free Agents)
# Release Clause (Possible)
# And position variables
# Drop these position variables will use preferences for position
position_cols <- colnames(player_raw)[colSums(is.na(player_raw)) > 0][-c(1,2,3,4)]
player_df <- player_raw[, !(colnames(player_raw) %in% position_cols)]

# Now how many rows contain NA values?
nrow(player_df[rowSums(is.na(player_df)) > 0,])
# 1,494, much less

# Select which columns to keep
colnames(player_df)
# Investigate columns which contain "prefers", "speciality", and "trait"
df_traits <- player_df[1:25, grepl("prefers|speciality|trait", colnames(player_df))]
# Binary variables, drop "speciality" and "trait" columns
player_slim <- player_df[, !grepl("speciality|trait", colnames(player_df))]
# Now at 94 columns

# Convert player_pref to clean position columns
player_pref <- player_slim[,grepl("prefers", colnames(player_slim))]

# Binary encode player preferences
for (i in 1:ncol(player_pref)){
  player_pref[,i] <- ifelse(player_pref[,i] == "False",0,1)
}

# Find out the largest number of positions a single player is associated with
max(rowSums(player_pref))
# 4

# Create player positions dataframe
player_positions <- data.frame(matrix(data = NA, nrow = nrow(player_pref), ncol = 4))

# Extract each player's position preference, up to a max of four positions
for (i in 1:nrow(player_pref)){
  positions <- gsub("prefers_", "", colnames(player_pref)[(player_pref[i,]) == 1])
  for (j in 1:length(positions)){
    player_positions[i,j] <- positions[j]
  }
}

# Rename columns
new_positions <- c("position_1", "position_2", "position_3", "position_4")
colnames(player_positions) <- new_positions

# Replace position preference cols with new position cols
player_slim2 <- cbind(player_slim[, !grepl("prefers", colnames(player_slim))], player_positions)

# Limit to identifing varibles, attributes and position variables
player_final <- player_slim2[, c(1,2,4,10,11,16,20,34:71)]
# 45 columns, 36 different attributes for each player

# Clean up the photo column
urlimage_path <- "https://cdn.sofifa.org/18/players/"
player_final$photo <- gsub(urlimage_path, "", player_final$photo)

# Clean up the environment
rm(player_df, player_raw, player_slim, player_slim2, player_positions, player_num,
   df_traits, player_pref)
   
```
It's possible to compare players across all of these attributes, however I'd like to create a visualization showing each player and their relative distance to each other in terms of similarity.

Using principal component analysis (PCA), I compressed the 36 player attributes (plus height and weight) to two components.

```r
#-------------------------------#
#  Dimensionality Reduction     #
#-------------------------------#

# Use pca to reduce attributes down to two dimensions
attribute_cols <- colnames(player_final)[c(4:5,8:41)]

# Any NA in attributes?
anyNA(player_final[,attribute_cols])
# False

# Scale variables before applying tsne
# Focus on the top 750 players in the world, according to Fifa 18
data_scaled = apply(player_final[1:750,attribute_cols], 2, function(r) {
  if (sd(r) != 0) 
    res = (r - mean(r))/sd(r) else res = 0 * r
    res
})

pc <- prcomp(data_scaled)
plot(pc, type='l',main = "PCA Scree Plot")
summary(pc)
# About 75% of variance captured in first two components
pc_df <- cbind.data.frame(pc$x[,1], pc$x[,2])

ggplot(pc_df, aes(pc$x[,1], pc$x[,2])) +
  geom_point() +
  ggtitle("Player Attributes Condensed by Principal Components") +
  ylab("Principal Component #2") +
  xlab("Principal Component #1") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```
The scree plot below shows that the two components account for 75% of the variance in the 36 player attributes. Ideally, at least 85% of the variance would be retained, but according to the scree plot that would require about four components. For the purposes of visualization, I'll limit the number of components to two.

![PCA Scree Plot](https://aaguilargonzalez.github.io/Comparing-Players-in-Fifa-18/Images/PCA-Scree-Plot.png)
