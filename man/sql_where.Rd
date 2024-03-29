% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{sql_where}
\alias{sql_where}
\title{Format a SQL where clause from arguments}
\usage{
sql_where(..., rel_op = "=")
}
\arguments{
\item{...}{Named objects to be queried by}

\item{rel_op}{Character. The relational operator in the SQL clause (i.e. "=",
"IN", "NOT IN", etc.). If a single rel_op is provide with multiple ...
parameters then it will be recycled \code{length(...)} times.}
}
\value{
A character string that can be passed to the where argument of
\code{get_spatial_layer}
}
\description{
This function will create a where statement that is compatible with
\code{\link{get_spatial_layer}}). This statement can then be passed
to the \code{where} argument in this function.
}
\examples{
\dontrun{
wbics <- sql_where(WATERBODY_WBIC = c(805400, 804600), rel_op = "IN")
lakes <- get_spatial_layer(wi_hydro_url, where = wbics)
plot_layer(lakes)
}
}
