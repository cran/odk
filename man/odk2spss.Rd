\name{odk2spss}
\alias{odk2spss}

\title{
Convert 'ODK' or 'XLSForm' to 'SPSS' Data Frame
}
\description{
After develop a \href{https://build.opendatakit.org/}{'ODK' frame}, we can \href{https://www.google.com/earth/outreach/learn/odk-collect-and-google-drive-integration-to-store-and-manage-your-data/}{link the frame to 'Google Sheets'} and collect data through \href{https://www.android.com/}{'Android'}. This data uploaded to a \href{https://www.google.com/sheets/about/}{'Google sheets'}. \code{odk2spss()} function help to convert the 'ODK' frame into \href{https://www.ibm.com/analytics/us/en/technology/spss/}{'SPSS'} frame. Also able to add downloaded 'Google sheets' data by using \code{pc.data} or read data from 'Google sheets' by using 'ODK' frame 'submission_url'. 
}
\usage{
odk2spss(xlsx, pc.data = NULL, out.path = NULL)
}

\arguments{
  \item{xlsx}{
path of 'ODK' or \href{https://opendatakit.org/use/xlsform/}{XLSForm} questionniare frame in 'xlsx' format.
}
  \item{pc.data}{
path of data file in 'xlsx' format, if \code{pc.data = NULL} , then the \code{odk2spss()} function download data from 'Google Sheets', internet connection needed for this.
}
  \item{out.path}{
path of output file with file name (ex: path/output_file.sps).if \code{out.path=NULL}, then \code{odk2spss()} function will show the auto output file path.
}
}

\value{
\code{odk2spss()} create a '*.sps' file. 
Open '*.sps' file by 'SPSS' softwere and Run.
Then we will get 'SPSS' data frame with data.
}

\author{
Muntashir-Al-Arefin \email{muntashir.a2i@gmail.com}
}
\note{
\code{odk2spss()} worked nicely for taxt, single choice, multiple choice and numeric variable.  Also work on other type variables.
}


\examples{

library(odk)
data(odk.frame)
data(gs.data)
##--Convert 'odk.frame' to 'SPSS' data frame and input gs.data data set.

odk2spss(xlsx=odk.frame, pc.data=gs.data, out.path="spss.sps")

##### Internet connection needed if pc.data = NULL

}

\keyword{odk}
\keyword{XLSForm}
\keyword{SPSS}