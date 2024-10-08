% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publications.R
\name{publications}
\alias{publications}
\alias{publications_default_columns}
\alias{publication_data}
\alias{publication_authors}
\title{HuBMAP Samples}
\usage{
publications()

publications_default_columns(as = c("tibble", "character"))

publication_data(uuid, entity_type = c("Dataset", "Sample", "Donor"))

publication_authors(uuid)
}
\arguments{
\item{as}{character(1) return format. One of \code{"tibble"} (default),
or \code{"character"}.}

\item{uuid}{character(1) corresponding to the HuBMAP Publication UUID
string. This is expected to be a 32-digit hex number.}

\item{entity_type}{character(1) selected derived entity type.
One of \code{"Sample"}, \code{"Donor"} or \code{"Dataset"} (default).}
}
\value{
\verb{*_columns()} returns a named list \code{name}
containing the column name used in the tibble returned by
\code{samples()}, \code{datasets()},  \code{donors()},
\code{collections()},  or \code{publications()}.
When \code{as = "tibble"},the return value is a tibble
with paths as elements and abbreviations as names.
}
\description{
\code{publications()} returns details about available samples,
ordered by last modified dates.
\code{publication_status == TRUE}: peer reviewed publication;
\code{publication_status == FALSE}: pre-print publication.

\verb{*_columns()} returns a tibble or named
character vector describing the content of the tibble returned
by \code{samples()}, \code{datasets()}, \code{donors()}, \code{collections()},
or \code{publications()}.

\code{publication_data()} takes a unique publication_id and
returns details about one specified publication.

\code{publication_authors()} takes a unique publication_id and
returns authors information of one specified collection as a tibble
}
\details{
Additional details are provided on the HuBMAP consortium
webpage, https://software.docs.hubmapconsortium.org/apis

Additional details are provided on the HuBMAP consortium
webpage, https://software.docs.hubmapconsortium.org/apis

Additional details are provided on the HuBMAP consortium
webpage, https://software.docs.hubmapconsortium.org/apis
}
\examples{
publications()
publications_default_columns()

uuid <- "3c7273660cdf9ab91a7901533b2cd9a5"
publication_data(uuid)

uuid <- "3c7273660cdf9ab91a7901533b2cd9a5"
publication_authors(uuid)
}
