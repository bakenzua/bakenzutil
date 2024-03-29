% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify_gas_labels.R
\name{classify_gas_labels}
\alias{classify_gas_labels}
\title{classify_gas_labels classifies raw blood gas sample type labels}
\usage{
classify_gas_labels(labels)
}
\arguments{
\item{labels}{}
}
\value{
label_classifications

exact_match_venous_strings is a vector of strings

exact_match_venous_strings is a vector of strings for use in the exact matching of sample labels
for classification of venous samples

exact_match_mixed_venous_strings is a vector of strings

exact_match_mixed_venous_strings is a vector of strings for use in the exact matching of sample labels
for classification of mixed venous samples

exact_match_pre_ecmo_strings is a vector of strings

exact_match_pre_ecmo_strings is a vector of strings for use in the exact matching of sample labels
for classification of post ecmo membrane samples

exact_match_post_ecmo_strings is a vector of strings

exact_match_post_ecmo_strings is a vector of strings for use in the exact matching of sample labels
for classification of post ecmo membrane samples

arterial_time_regexp is a regular expression

arterial_time_regexp is a regular expression for use in matching of sample labels
for classification of arterial samples. Specifically it matches the various patterns
of an arterial label suffixed with a time

venous_time_regexp is a regular expression

venous_time_regexp is a regular expression for use in matching of sample labels
for classification of venous samples. Specifically it matches the various patterns
of an venous label suffixed with a time
}
\description{
classify_gas_labels classifies vectors of raw blood gas sample type labels.
A vector of the following classifications is returned.
}
\details{
* ARETERIAL
* VENOUS
* MIXED_VENOUS
* PRE_ECMO
* POST_ECMO
* UNKNOWN
}
