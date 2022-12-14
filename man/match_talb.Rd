% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/areas.R
\name{match_talb}
\alias{match_talb}
\title{TA-local board matcher}
\usage{
match_talb(src_talb, out_col = "talb_code", match_col = "talb_match_name")
}
\arguments{
\item{src_talb}{Column of target TA/local boards}

\item{out_col}{Name of column to extract}

\item{match_col}{Name of column to match against ("talb_match_name" or "talb_code")}
}
\description{
Matches TA/local board details by talb_code or talb_match_name. Does a lot of cleaning and
simplification to resolve potential issues with name matches.
}
