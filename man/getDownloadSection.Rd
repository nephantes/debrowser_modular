% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadSection.R
\name{getDownloadSection}
\alias{getDownloadSection}
\title{getDownloadSection}
\usage{
getDownloadSection(flag = FALSE, choices = NULL, defaultSelected = NULL)
}
\arguments{
\item{flag, }{to show the download selection}

\item{choices, }{main vs. QC section}

\item{defaultSelected, }{default selected item}
}
\value{
the panel for download section in the menu;
}
\description{
download section button and dataset selection box in the
menu for user to download selected data.
}
\note{
\code{getDownloadSection}
}
\examples{
    x<- getDownloadSection()

}
