library(RGCCA)
library(PreProcess)
library(caret)

regression_file <- read.csv('file_for_regression.csv')

#subset features to use for analysis
chemical_fish_data <- regression_file[, c("SPCSNO","SPECIES_ENCODED","CAS_Number","result_conc1_mean",
                                          "Area",'Month_Year','Concentration')] 

#normalize Data
process <- preProcess(as.data.frame(chemical_fish_data), method=c("range"))
norm_scale <- predict(process, as.data.frame(chemical_fish_data))
print(norm_scale)

# Extract columns for each block of variables and convert them into matrices
SPCSNO_matrix <- as.matrix(norm_scale$SPCSNO)
CAS_Number_matrix <- as.matrix(norm_scale$CAS_Number)
result_conc1_mean_matrix <- as.matrix(norm_scale$result_conc1_mean)
Area_matrix <- as.matrix(norm_scale$Area)
Month_Year_matrix <- as.matrix(norm_scale$Month_Year)
Concentration_matrix <- as.matrix(norm_scale$Concentration)
SPECIES_ENCODED_matrix <- as.matrix(norm_scale$SPECIES_ENCODED)

# Run RGCCA on your data matrix
rgcca_result <- rgcca(list(SPCSNO_matrix,CAS_Number_matrix,result_conc1_mean_matrix,Area_matrix,Month_Year_matrix,
                      Concentration_matrix,SPECIES_ENCODED_matrix),ncomp = rep(1, 7) )
print(rgcca_result)
# Print canonical correlation coefficients
print(rgcca_result$cor)
