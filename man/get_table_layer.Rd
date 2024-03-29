% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_layers.R
\name{get_table_layer}
\alias{get_table_layer}
\title{Retrieve a table from an ArcGIS REST API}
\usage{
get_table_layer(
  url,
  out_fields = "*",
  where = "1=1",
  token = "",
  head = FALSE,
  ...
)
}
\arguments{
\item{url}{A character string of the url for the layer to pull}

\item{out_fields}{A character string of the fields to pull for each layer}

\item{where}{A character string of the where condition. Default is 1=1}

\item{token}{A character string of the token (if needed)}

\item{head}{Logical or numeric. Limits the number of records returned from a
query. If TRUE, only the first 5 records will be returned. If numeric, then
the number of records specified in \code{head} will be returned}

\item{...}{Additional arguments to pass to the ArcGIS REST POST request (or
associated internal functions used to query them)}
}
\value{
A data frame of the appropriate layer
}
\description{
This function retrieves tables present in an ArcGIS REST services API and
returns them as a data frame.
}
\details{
This function retrieves tables from an ArcGIS REST API designated by the
URL. Additional querying features can be passed such as a SQL WHERE
statement (\code{where} argument) as well as any other types of queries
that the ArcGIS REST API accepts (using \code{...}).

All of the querying parameters are sent via a POST request to the URL, so
if there are issues with passing additional parameters via \code{...}
first determine how they fit into the POST request and make adjustments as
needed. This syntax can be tricky if you're not used to it.
}
