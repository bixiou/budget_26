# Development Plan - Budget Survey

## 🎯 Global goal
Cleanse data from a new survey (from one country) and render descriptive graphs. This will make use of packages and functions defined in .Rprofile as well as functions and structure defined in a former project (a multi-country survey) in files code_budget/former_*.R

## 🛠 Tasks
- [x] Create 1_rename.R, 2_prepare.R, 3_render.R
- [x] Load the new data from Qualtrics by adapting the call to `fetch_survey` in former_2_prepare: the name of the new survey is "Budget". You need to actually load the data (run this part in R) to know the column names, used in the next step.
- [x] Run the first lines (now commented) in former_1_rename.R to create a function rename_survey.R in 1_rename.R (on the model of the eponym function in former_1_rename.R) that defines the columns names of the new survey (you can re-use the names of the fetched data)
- [x] Create a function `prepare` in 2_prepare.R (on the model of the eponym function in former_2_prepare.R)
- [x] Create a function `convert` in 2_prepare.R (on the model of the eponym function in former_2_prepare.R), where you'll remove from the original `convert` function all lines that correspond to columns absent from the new data
- [x] For each variable that is not already cleanse in `convert`, create line(s) that cleanse them in that function. Divide the variables into batches of variables and ask me confirmation for the cleansing code after each batch. 