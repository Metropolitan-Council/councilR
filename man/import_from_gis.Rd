% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_from_gis.R
\name{import_from_gis}
\alias{import_from_gis}
\title{Import a dataset from ArcCatalog}
\usage{
import_from_gis(query, dbname = "GISLibrary")
}
\arguments{
\item{query}{character, string with the database connection and feature class}

\item{dbname}{character, database name. Usually either \code{"GIS"} or \code{"GISLibrary"}}
}
\value{
an \code{sf} object
}
\description{
Import a dataset from ArcCatalog
}
\examples{
\dontrun{import_from_gis("GISTransit.dbo.PublicParcelsMetroCTUs")}
}
