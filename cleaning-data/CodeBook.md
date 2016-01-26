# Getting and Cleaning Data Project

The "run_analysis.R" script has several stages

## Set files path
First it attributes all meaningfull files' path to variables to be read later. For test, it uses these three variables: "fileTestX", "fileTestY" and "fileTestSubject". For train: "fileTrainX", "fileTrainY" and "fileTrainSubject". It also sets activities labels at "fileActivityLabels". Finally it sets the file with all column names at "fileColumnNames"

## Read files
After set all files paths, the script reads all files and sets the datas to similar variables, just replacing file to data. Activities are set to "activities" and column names to "dataColumnNames"

# Merge data
Both Test and Train data are merged with the first column as activities, second as subject and then the remaining data columns. After the column merge, Test and Train are put in the same data.frame and called simple "data"

# Set column names
The script gets column names stored in dataColumnNames and remove invallid characters and lower cases. Thoses names are preceded of "activities" and "subject" and are attributed to "data"

# Replace activities id
Activities ids are replaced with more human meaninfull labels stored at activities vector

# Get meaninful columns
All columns that contains "std" or "mean" in variables name are stored in meanstdColumns. It is then used combined with "activities" and "subject" names to subset the data

# Tidy data
The data is melted using "activities" and "subject" as ids. Then it is reshaped in a new data.frame called mean data while calculating the mean of each "activities" + "subject"

## Write tidy data to file
Both "data" and "meanData" are saved respectively in "data.txt" and "mean_data.txt"

