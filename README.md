# BA_2023_ChatGPT

In the excel file "Shiny Prompt Documentation.xlsx" you find an overview of all the prompts I used and the corresponding R file in the column "Shiny file". All the application that were created with the code ChatGPT generated have their own file and can be found in the subdirectory "Shiny Apps".

Not all Shiny Apps work. Some have errors, which were not corrected as the development process of a non-programmers should be comprehensible.

An example for a Shiny application that properly works and satisfies all the requirements specified in the thesis can be found in the file "DSS Proper Solution.R".

In the file "Prompt Analysis.R" I analyzed the prompts that are documented in the file "Shiny Prompt Documentation.xlsx". To run this R file, one must have the excel file and the R script in the same folder or adapt the source link accordingly at the top of the script.

The csv file "berlin_weekdays.csv" contains all the original dataset that was used for the case study in a reduced scope but is not of further importance.
The original source is: Gyódi, K., & Nawaro, Ł. (2021). Determinants of Airbnb prices in European cities: A spa-tial econometrics approach. Tourism Management, 86, 104319. https://doi.org/10.1016/j.tourman.2021.104319

The csv file "berlin_weekdays_subset.csv" contains the reduced dataset.
The csv file "prediction_rows.csv" contains the row that were used for the predictions in the case study.
