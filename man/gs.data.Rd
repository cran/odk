\name{gs.data}
\alias{gs.data}
\docType{data}
\title{
'Google Sheets' Data for 'odk.frame'
}
\description{
After develop a \href{https://build.opendatakit.org/}{'ODK' frame}, we can link the frame to \href{https://www.google.com/sheets/about/}{'Google Sheets'} and collect data through \href{https://www.android.com/}{'Android'}. This data uploaded to a 'Google sheets'.  The \code{gs.data} is a 'Google sheets' data collected though \code{odk.frame} file.   
Both \code{odk.frame} and \code{gs.data} are in 'xlsx' format. To develop your own \href{https://opendatakit.org/}{'ODK'} frame visit \url{https://build.opendatakit.org/}
}
\usage{data("gs.data")}

\examples{
library(odk)
data(gs.data)
data(odk.frame)

### For convert 'odk.frame' to 'SPSS' frame and input Corresponding 'Google Sheets' data

odk2spss(xlsx=odk.frame, pc.data=gs.data, out.path="spss.sps")
}
\keyword{gs.data}
