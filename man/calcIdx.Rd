% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda_funs.r
\name{calcIdx}
\alias{calcIdx}
\title{For each level combination from two variables, calculate an index comparing grouped distribution to a total one in a dataset}
\usage{
calcIdx(grpVar, idxVar, dataset, resp.na.rm = TRUE, group.na.rm = TRUE)
}
\arguments{
\item{grpVar}{A character vector of length = 1, with a variable (column) name from \code{dataset}, values from which will be used for grouping index calculations}

\item{idxVar}{A character vector of length = 1, with a variable (column) name from \code{dataset}, for each value of which an index will be calculated per \code{grpVar}}

\item{dataset}{A tibble with tidy data (variables in columns and observations in rows). Factors are better suited for categorical variables (otherwise, may error due to missing records for some group combinations, e.g. Mixed race and Others aged 65+)}
}
\value{
A tibble with 3 columns: levels of \code{grpVar}, levels of \code{idxVar} for each, and index (idx column) for each of those level combinations
}
\description{
Implementation: tidyverse/dplyr. Used in EDA (exploratory data analysis) part of assignment.
Example. Grouping variable: s_gender. Index variable: s_age. Output: For each level in s_gender, calculate the index of each s_age level.
An index compares the grouped distribution compared to the total distribution. If for example, 50\% of all males are between 24 and 35, but only 40\% of the total population, the index would be 50/40 *100 = 125.
}
\examples{

require(haven)
require(dplyr)
survey_data <- read_sav('experRrvey/extdata/survey_data.sav')  #  not needed for the included dataset
survey_data$s_age <- factor(survey_data$s_age, levels =
                             unique(survey_data$s_age)[
                               order(as.numeric(gsub("(^[[:digit:]]{1,2}).*",
                                                "\\\\1", unique(survey_data$s_age))))])
survey_data$s_gender <- as_factor(survey_data$s_gender)
calcIdx("s_gender", "s_age", survey_data)

}
