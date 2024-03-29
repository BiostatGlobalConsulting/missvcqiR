% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcqi_halt_immediately.R
\name{vcqi_halt_immediately}
\alias{vcqi_halt_immediately}
\title{Exit VCQI program at the end of a run or after an error}
\usage{
vcqi_halt_immediately(VCP = "vcqi_halt_immediately", halt_message = NA)
}
\arguments{
\item{VCP}{VCQI current program name to be logged, default to be the function name}

\item{halt_message}{Error text to print when VCQI is stopped mid-run}
}
\value{
Clean exit from VCQI programs
}
\description{
Exit VCQI program at the end of a run or after an error
}
