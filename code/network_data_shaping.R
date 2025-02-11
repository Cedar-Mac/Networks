# This script will create two correlation coefficient matrices
# from the difference matrices of treatment reach minus control reach
# abundances. 

x <- c("tidyverse", "vegan", "lubridate")
lapply(x, library, character.only = TRUE)

bugs <- readxl::read_xlsx("../data/2017-18_bugs.xlsx", )

bugs$CollDate <- as.factor(year(bugs$CollDate))

bugs.agg <- aggregate(Count ~ CollDate + Stream + Treatment + Family, sum, data = bugs)

bugs.agg <- spread(bugs.agg, key = "Family", value = "Count")

# Drop singleton taxa
drops = c("wtf", 
          "Coleoptera", 
          "Collembola", 
          "Empididae", 
          "Isopod", 
          "Tabanidae", 
          "Uenoidae", 
          "Taeniopterygidae", 
          "Crayfish", 
          "Clam")

bugs.agg <- bugs.agg[ , !(names(bugs.agg) %in% drops)]

# Fill NA
bugs.agg[is.na(bugs.agg)] <- 0

# Will have to drop grouping columns
grouping <- c("CollDate", "Treatment", "Stream")

# Subtract reaches in the same year to get "gap effect"
N_17_df = filter(bugs.agg, CollDate == "2017", Treatment == "N") 
N_17_df <- N_17_df[, !(names(N_17_df) %in% grouping)]

Y_17_df = filter(bugs.agg, CollDate == "2017", Treatment == "Y")
Y_17_df <- Y_17_df[, !(names(Y_17_df) %in% grouping)]
diff_17 = Y_17_df - N_17_df

# Turn into correlation matrix
diff_17_corr = cor(diff_17[,5:39], method = "pearson")
diff_17_corr[is.na(diff_17_corr)] <- 0
diff_17_corr = as.data.frame(diff_17_corr)

# Do the same for 2018
N_18_df = filter(bugs.agg, CollDate == "2018", Treatment == "N")
N_18_df <- N_18_df[, !(names(N_18_df) %in% grouping)]

Y_18_df = filter(bugs.agg, CollDate == "2018", Treatment == "Y")
Y_18_df <- Y_18_df[, !(names(Y_18_df) %in% grouping)]
diff_18 = Y_18_df - N_18_df

diff_18_corr = cor(diff_18[,5:39], method = "pearson")
diff_18_corr[is.na(diff_18_corr)] <- 0
diff_18_corr = as.data.frame(diff_18_corr)

write_csv(diff_17_corr, file = "../data/2017_diff_corr.csv")
write_csv(diff_18_corr, file = "../data/2018_diff_corr.csv")
