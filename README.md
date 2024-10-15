# ShinyExams
ShinyExams is a Shiny Web App based on R that enables you to easily analyse Exam point data and output your analysis results as a handy PDF. 

## Why ShinyExams
Honestly, I'm quite lazy when it comes to repetitive tasks. I analyze exam data every other semester and want to compare question averages to refine or evaluate questions in terms of difficulty and fairness. For this purpose, I created this point-and-click app that makes it easy to do exactly that, and get standardized reports in the process. As some people find it a hassle to do everything in point-and-click software and at the same time don't want to or can't use R, I created this app that can run easily on any computer.

## What can I do with ShinyExams
As of October 2024, only descriptive measures and three kinds of plots can be exported: 

- Means, Standard Deviations, Minimum, Maximum, N of Variables
- Boxplots, Density Plots, Histograms 
- Plots split by categorical variables, such as different minors/majors etc. are possible. 

## Planned updates

- Docker Image, so R isn't even required on the machine. 
- Full Tutorial for Shiny deployment. 
- Delete Button for the Export List.
- UI Changes: Make it prettier, make it more readable, make it FUN.
- Potential to download and upload Export list as either JSON or csv.

## Possible Updates
- Support for logistic/choice exam types: Rasch Models, marking solved questions etc.