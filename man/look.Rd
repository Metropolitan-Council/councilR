% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/look.R
\name{look}
\alias{look}
\title{Look at data.table subsets}
\usage{
look(var, n = 1, unique = TRUE)
}
\arguments{
\item{var}{A column from a data.table object}

\item{n}{numeric, How many values are used in subsetting}

\item{unique}{logical, Randomly choose from a unique set of levels for subsetting.
Otherwise, choose from all values despite potential repetition.}
}
\value{
A logical vector
}
\description{
A function to go into the i statement of a data.table that makes random subsets based on a chosen variable
}
\examples{
# load some data
library(councilR)
library(data.table)
titanic <- fread("https://raw.githubusercontent.com/Geoyi/Cleaning-Titanic-Data/master/titanic_clean.csv")

# look at a random home.dest
titanic[look(home.dest)] # re-run the this line for a different random pull

# look at a random person
titanic[look(name)]

# look at a random fare and person of that fare
titanic[look(fare)][look(embarked)]

}
