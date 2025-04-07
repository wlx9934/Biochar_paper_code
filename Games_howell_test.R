# Import the required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)
library(openxlsx)
library(rstatix)

# Create the calc_ght function, Running the games howell test and letter marking
# for multiple comparisons.
calc_ght<-function(df,write_path){
  
  ms_fm<-formula(paste(colnames(df)[5],"~",colnames(df)[4]))
  # Calculate the average
  means = aggregate(ms_fm, data = df, mean)
  colnames(means)[2]<-"mean"
  # Calculate standard deviation
  sds <- aggregate(ms_fm, data = df, sd)
  colnames(sds)[2]<-"sd"
  means<-left_join(means,sds,by="NO.")
  # Reorder based on the average value
  means <- arrange(means, desc(mean))
  # Add a new group (average order)
  means$new_group <- 1:nrow(means)
  # Return to the original order (No. order)
  means <- means[order(means[, 1]), ]
  # Add new group variable to the original data in order of average values
  df$new_group <- rep(means$new_group, each = 3)
  
  ght_fm<-formula(paste(colnames(df)[5],"~","new_group"))
  # Games Howell test
  ght <- df %>% 
    games_howell_test(ght_fm, detailed = TRUE)
  
  # Forming a vector containing groups
  groups <- unique(append(ght$group1, ght$group2))
  # The format of inserting letters is determined based on the number of groups,
  # the first 26 letters are lowercase, and the last 26 letters are uppercase. 
  # If it exceeds 52, it is recommended to reduce the number of groups before analyzing.
  if (length(groups) <= 26) {
    Letters <- letters[seq(1, 26)]
  } else if (26 < length(groups) && length(groups) <= 52) {
    Letters <- append(letters[seq(1, 26)], LETTERS[seq(1, 26)])
  } else {
    print("too long!")
  }
  
  # Alphabetical order
  m <- 1
  n <- 1
  for (i in 1:length(groups)) {
    if (m < length(groups)) {
      # The group with the highest average value is directly labeled as a
      if (i <= 1) {
        # Find all comparisons with group i (i=1)
        ght_search <-
          ght[append(which(ght[, 'group1'] == i), which(ght[, 'group2'] == i)),]
        # label a
        means[which(means[, 'new_group'] == i), 'letters'] <-
          gsub('NA', '', paste(means[which(means[, 'new_group'] == i), 'letters'], Letters[i], sep = ''))
        # Search down until you find a group that is significantly different 
        # from group i.
        down_num <- length(groups) - i
        no_num <- 0
        for (j in 1:down_num) {
          k <- ght_search[j, 'group2']
          k <- as.numeric(k)
          # Mark the same letter if the difference is not significant, mark the
          # next letter if the difference is significant.
          if (ght_search[j, 'p.adj'] <= 0.05) {
            means[which(means[, 'new_group'] == k), 'letters'] <-
              gsub('NA', '', paste(means[which(means[, 'new_group'] == k), 'letters'], 
                                   Letters[i + 1], sep = ''))
            m <- k
            n <- n + 1
            break
          } else if (ght_search[j, 'p.adj'] > 0.05) {
            means[which(means[, 'new_group'] == k), 'letters'] <-
              gsub('NA', '', paste(means[which(means[, 'new_group'] == k), 'letters'], Letters[i], sep = ''))
            no_num <- no_num + 1
            if ((no_num + m) == length(groups)) {
              m <- length(groups)
              break
            }
          }
        }
        # No need to search upwards
      } else{
        # Find all comparisons with group m
        ght_search <-
          ght[append(which(ght[, 'group1'] == m), which(ght[, 'group2'] == m)),]
        # Search upwards first
        forward_num <- m - 1
        for (j in nrow(ght_search):(nrow(ght_search) - forward_num + 1)) {
          # Mark the same letter if the difference is not significant  and stop 
          # searching upwards if the difference is significant
          if (ght_search[j, 'p.adj'] > 0.05) {
            k <- ght_search[j, 'group1']
            k <- as.numeric(k)
            means[which(means[, 'new_group'] == k), 'letters'] <-
              gsub('NA', '', paste(means[which(means[, 'new_group'] == k), 'letters'], Letters[n], sep = ''))
          } else if (ght_search[j, 'p.adj'] <= 0.05) {
            m <- k
            break
          }
        }
        # Find all comparisons with group m
        ght_search <-
          ght[append(which(ght[, 'group1'] == m), which(ght[, 'group2'] == m)),]
        # Search down until you find a group that is significantly different 
        # from group m.
        down_num <- length(groups) - m
        no_num <- 0
        for (j in 1:down_num) {
          k <- ght_search[j, 'group2']
          k <- as.numeric(k)
          # Mark the same letter if the difference is not significant, mark the
          # next letter if the difference is significant.
          if (ght_search[j, 'p.adj'] <= 0.05) {
            means[which(means[, 'new_group'] == k), 'letters'] <-
              gsub('NA', '', paste(means[which(means[, 'new_group'] == k), 'letters'], 
                                   Letters[n + 1], sep = ''))
            m <- k
            n <- n + 1
            break
          } else if (ght_search[j, 'p.adj'] > 0.05) {
            means[which(means[, 'new_group'] == k), 'letters'] <-
              gsub('NA', '', paste(means[which(means[, 'new_group'] == k), 'letters'], Letters[n], sep = ''))
            no_num <- no_num + 1
            if ((no_num + m) == length(groups)) {
              m <- length(groups)
              break
            }
          }
        }
      }
    }
    else if (m >= length(groups)) {
      break
    }
  }
  
  # Delete duplicate marked letters
  for (x in 1:nrow(means)) {
    means[x, 'letters'] <-
      paste(unique(strsplit(means[x, 'letters'], split = '')[[1]]), collapse = '')
  }
  # Generate a new column, average ± standard deviation - letters
  means$label<-paste0(round(means$mean, 3), "±", round(means$sd, 3),means$letters)
  
  # Save result
  write.xlsx(means,paste0(write_path,"/",colnames(df)[5],".xlsx"))
  
}

# read in data
df <- read.xlsx("./data/....xlsx", sheet = 1)

# The first four columns of the data are tree species, temperature, time, and NO.,
# NO. is labelled according to the combination of different species, different 
# temperatures and different time, representing group information.
for(i in seq(5,ncol(df))){
  # Read the data from the first four columns and the i-th column 
  # and remove missing values
  df_new<-df[,c(1:4,i)] %>% na.omit()
  # Run the calc_ght function
  calc_ght(df_new,"./result/.../")
}



