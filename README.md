Getting-and-cleaning-data-project
=================================

This is a repo for the course project in "Getting and cleaning data" coursera course


The script "run_analysis.R" takes the data from the folder "UCI HAR Dataset", that has to be located in R working directory,
and manipulates data to create 2 tidy data sets. 

Part 1 creates manipulates the raw data to create a tidy data set for the questions 1-4 in the assignement.
Part 2 creates a tidy data set for the question 5.

Part1:
Step 1. Load all the required data into R. I loaded all the files in the UCI HAR directory and the first level
of subdirectories, ignoring the "Inertial Signals" "rawest" data.

Step 2. Replace the indexes marking the activity performed during the measurement into the activity names themselves;
So, for example, 1 is replaced with "Walking" and so on.

Step 3. Set the appropriate names for the columns; the names for feature vector are taken from the file "features.txt".

Step 4. Shorten the feature vector, by exctracting only the values corresponding to either mean or standart deviation of something. 
I took the values, marked as mean(), str(), meanFreq() and the values obtained by averaging the signals in a window sample: 
gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean
That was done by creating a 86-long index vector, with each index marking a correct column in the feature vector.

Step 5. Clip together the 86-long shortened feature vector with the subject number and activity label for each measurement. This is done separately for the "train" and "test" data sets. 
Step 6. Add the column "test/train", marking if the measurement belongs to the test group, or to the training group. 
Step 7. Clip together "train" and "test" data by rows
Step 8. Write the obtained data in tidy_data.txt

Part2. Creating a second tidy data set for the question #5.
At the beginning, we already have all the files loaded into R.
We repeat steps 5-7 from the first part, skipping step 4. So, the output is a data set similar to the tidy_data, obtained in the first part, but the feature vector is now not shortened.
After that, for each activity and each subject we extract data, corresponding to only that subject and activity, and calculate column means. So, for example, for subject #18, for activity "STANDING" there are 73 measurements.
We clip together the "columnMeans" by rows, getting a matrix 180by561, with 180 corresponding to 6 times 30 possible
pairs of subject and activity, and 561 being the length of feature vector.
Then we write down the subject number and activity name for each row.
The obtained data frame is a final tidy data, and it is written into tidy_data2.txt
