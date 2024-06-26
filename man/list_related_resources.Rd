% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_related_resources.R
\name{list_related_resources}
\alias{list_related_resources}
\title{List the related resources of an HP}
\usage{
list_related_resources(
  db.con = NA,
  d = NA,
  field = NA,
  id = NA,
  relationshipwith = NA,
  relationshiptype = NA,
  relationshipdirect = c("from", "to"),
  disconn = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{db.con}{a `dbConnect` connection to the database.}

\item{d}{a hash() object (a Python-like dictionary).}

\item{field}{the field name that will be created in the `d` hash() object.}

\item{id}{the ID of an HP, either an UUID or an EAMENA ID. By default, NA.}

\item{relationshipwith}{the RM on which the related resources will be retrieved: "hp", "ir", "po", "cc", ...}

\item{relationshiptype}{the type of relation existing between an HP and a BC (for exmple the CIDOC-CRM 'PX_is_related_to').}

\item{disconn}{if TRUE (by defalut), will disconnect from the DB.}

\item{verbose}{if TRUE (by default), print messages.}
}
\value{
A dataframe with the IDs and UUIDs of HP and rr
}
\description{
With a given HP, find all the related resources.
}
\examples{

d <- hash::hash()
my_con <- RPostgres::dbConnect(drv = RPostgres::Postgres(),
                               user = 'xxx',
                               password = 'xxx',
                               dbname = 'eamena',
                               host = 'ec2-54-155-109-226.eu-west-1.compute.amazonaws.com',
                               port = 5432)

df <- list_related_resources(db.con = my_con,
                             d = d,
                             id = "EAMENA-0164943")
df
##            hp.id                              hp.uuid             rr.id                              rr.uuid
## 1 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000141 90400bb6-ff54-4afd-8183-65c67fa97448
## 2 EAMENA-0164943 d4feb830-10c7-4d80-a19e-e608f424be4c COMPONENT-0000143 0dab164a-6d3a-443c-954a-50d93efbff35

}
