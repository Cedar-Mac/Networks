# This script will create two correlation coefficient matrices
# from the difference matrices of treatment reach minus control reach
# abundances. 

x <- c("tidyverse", "vegan", "lubridate", "rstatix")
lapply(x, library, character.only = TRUE)

bugs <- readxl::read_xlsx("../data/2017-18_bugs.xlsx", )

bugs$CollDate <- as.factor(year(bugs$CollDate))

bugs.agg <- aggregate(Count ~ CollDate + Stream + Treatment + Family, sum, data = bugs)

bugs.agg <- spread(bugs.agg, key = "Family", value = "Count")

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

# Filter out singleton taxa (get count of non-zero occurrences. If count less than 1 drop column)
diff_17 <- diff_17 %>% select(where(~ any(length(which(. != 0)) > 1)))

# Turn into correlation matrix
diff_17_corr = cor_mat(diff_17, method = "pearson", alternative = "two.sided")
diff_17_corr[is.na(diff_17_corr)] <- 0
p_values_17 <- cor_get_pval(diff_17_corr)

# Plot 2017 correlation matrix
png(height=1200, width=1800, file="../2017_corr_coeff_table.png")

diff_17_corr %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label=FALSE) 

dev.off()

# Do the same for 2018
N_18_df = filter(bugs.agg, CollDate == "2018", Treatment == "N")
N_18_df <- N_18_df[, !(names(N_18_df) %in% grouping)]

Y_18_df = filter(bugs.agg, CollDate == "2018", Treatment == "Y")
Y_18_df <- Y_18_df[, !(names(Y_18_df) %in% grouping)]
diff_18 = Y_18_df - N_18_df

# Filter out singleton taxa (get count of non-zero occurrences. If count less than 1 drop column)
diff_18 <- diff_18 %>% select(where(~ any(length(which(. != 0)) > 1)))

diff_18_corr = cor_mat(diff_18, method = "pearson")
diff_18_corr[is.na(diff_18_corr)] <- 0
p_values_18 <- cor_get_pval(diff_18_corr)

# Plot 2018 correlation matrix
png(height=1200, width=1800, file="../2018_corr_coeff_table.png")

diff_18_corr %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label=FALSE) 

dev.off()

# Drop rownames
diff_17_corr <- diff_17_corr[, 2:length(diff_17_corr)]
diff_18_corr <- diff_18_corr[, 2:length(diff_18_corr)]

# Write out
write_csv(diff_17_corr, file = "../data/2017_diff_corr.csv")
write_csv(diff_18_corr, file = "../data/2018_diff_corr.csv")
