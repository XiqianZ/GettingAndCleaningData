The run_analysis.R will process the raw data from the dataset "UCI HAR Dataset" and generate a analysised dataset with only the measurement of "mean()" and "std()" from the orignal raw dataset. 

The analysised dataset will be saved into the working directory as a .txt file. The file is called "dat.processed.txt"


1. Variables lists:

1.1 Experiemental acitivities and subject ID
[1] "ExperimentalActivities"    Named the experimental activites. All the data are collected from the subjects preforming the 6 categories of acitivites: 
[2] "Subject.ID"    Display the ID assigned to each of the experimental subjects
 
1.2 The mean value of the standard diviations of the sensor data collected through the experiment of each category. The unit is unknow (somehow XD)
 [3] "tBodyAcc.std...X"           
 [4] "tBodyAcc.std...Y"           
 [5] "tBodyAcc.std...Z"           
 [6] "tGravityAcc.std...X"        
 [7] "tGravityAcc.std...Y"        
 [8] "tGravityAcc.std...Z"        
 [9] "tBodyAccJerk.std...X"       
[10] "tBodyAccJerk.std...Y"       
[11] "tBodyAccJerk.std...Z"       
[12] "tBodyGyro.std...X"          
[13] "tBodyGyro.std...Y"          
[14] "tBodyGyro.std...Z"          
[15] "tBodyGyroJerk.std...X"      
[16] "tBodyGyroJerk.std...Y"      
[17] "tBodyGyroJerk.std...Z"      
[18] "tBodyAccMag.std.."          
[19] "tGravityAccMag.std.."       
[20] "tBodyAccJerkMag.std.."      
[21] "tBodyGyroMag.std.."         
[22] "tBodyGyroJerkMag.std.."     
[23] "fBodyAcc.std...X"           
[24] "fBodyAcc.std...Y"           
[25] "fBodyAcc.std...Z"           
[26] "fBodyAccJerk.std...X"       
[27] "fBodyAccJerk.std...Y"       
[28] "fBodyAccJerk.std...Z"       
[29] "fBodyGyro.std...X"          
[30] "fBodyGyro.std...Y"          
[31] "fBodyGyro.std...Z"          
[32] "fBodyAccMag.std.."          
[33] "fBodyBodyAccJerkMag.std.."  
[34] "fBodyBodyGyroMag.std.."     
[35] "fBodyBodyGyroJerkMag.std.." 

1.3 The mean value of the calcualted mean of the sensor data collected through the experiment of each category. The unit is unknow (somehow XD)
[36] "tBodyAcc.mean...X"          
[37] "tBodyAcc.mean...Y"          
[38] "tBodyAcc.mean...Z"          
[39] "tGravityAcc.mean...X"       
[40] "tGravityAcc.mean...Y"       
[41] "tGravityAcc.mean...Z"       
[42] "tBodyAccJerk.mean...X"      
[43] "tBodyAccJerk.mean...Y"      
[44] "tBodyAccJerk.mean...Z"      
[45] "tBodyGyro.mean...X"         
[46] "tBodyGyro.mean...Y"         
[47] "tBodyGyro.mean...Z"         
[48] "tBodyGyroJerk.mean...X"     
[49] "tBodyGyroJerk.mean...Y"     
[50] "tBodyGyroJerk.mean...Z"     
[51] "tBodyAccMag.mean.."         
[52] "tGravityAccMag.mean.."      
[53] "tBodyAccJerkMag.mean.."     
[54] "tBodyGyroMag.mean.."        
[55] "tBodyGyroJerkMag.mean.."    
[56] "fBodyAcc.mean...X"          
[57] "fBodyAcc.mean...Y"          
[58] "fBodyAcc.mean...Z"          
[59] "fBodyAccJerk.mean...X"      
[60] "fBodyAccJerk.mean...Y"      
[61] "fBodyAccJerk.mean...Z"      
[62] "fBodyGyro.mean...X"         
[63] "fBodyGyro.mean...Y"         
[64] "fBodyGyro.mean...Z"         
[65] "fBodyAccMag.mean.."         
[66] "fBodyBodyAccJerkMag.mean.." 
[67] "fBodyBodyGyroMag.mean.."    
[68] "fBodyBodyGyroJerkMag.mean.."

2. Data choice
2.1 First the trainning and test dataset are merged together into one dataset for processing.
2.2 Then the raw data are extracted with only the measurements on the mean and standard deviation for each measurement.
2.3 After that, the data's columns and rows are renamed with more descriptive labels. 
2.4 The data is also reorded for better view.
2.5 Then from 2.4,  the mean value of each measurement are calculated and summaried into the new data frame in "dat.processed.txt"

3. Information about the experimental study design you used
Well, actually I don't really know about it XD
