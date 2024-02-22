\name{scrapeR_in_batches}
\alias{scrapeR_in_batches}
\title{
  Batch Web Page Content Scraper
}
\description{
  The \code{scrapeR_in_batches} function processes a dataframe in batches, scraping web content from URLs in a specified column and writing the scraped content to a column in df.
}
\usage{
scrapeR_in_batches(df, url_column, extract_contacts)
}
\arguments{
  \item{df}{
    A dataframe containing the URLs to be scraped.
  }
  \item{url_column}{
    The name of the column in \code{df} that contains the URLs.
  }
  \item{extract_contacts}{
    A function that searches scraped content for emails and phone numbers, defaults to FALSE.
  }

}
\details{
  This function divides the input dataframe into batches of a fixed size (default: 100). For each batch, it extracts the combined text content from the web pages of the URLs in the specified column. The results are appended to the df. The function also includes a throttling mechanism to pause between batch processing, reducing the load on the server being scraped.
}
\value{
  The values are returned to content column and optionally to an email and phone_number column if extract_contacts is TRUE.
}
\references{
  Refer to \href{https://www.rdocumentation.org/packages/rvest}{rvest package documentation} and \href{https://www.rdocumentation.org/packages/httr}{httr package documentation} for underlying web scraping methods.
}
\author{
  Mathieu Dubeau Ph.D
}
\note{
  Ensure that the \pkg{httr}, \pkg{rvest}, and \pkg{stringr} packages are installed and loaded. Also, handle large datasets and output files with care to avoid memory issues.
}

\seealso{
  \code{\link[httr]{GET}}, \code{\link[rvest]{read_html}}, \code{\link[rvest]{html_nodes}}, \code{\link[rvest]{html_text}}, \code{\link[utils]{write.table}}
}
\examples{

  mock_scrapeR <- function(url) {
    return(paste("Scraped content from", url))
  }

  df <- data.frame(url = c("http://site1.com", "http://site2.com"), stringsAsFactors = FALSE)

  \dontrun{
    scrapeR_in_batches(df, url_column = "url", extract_contacts = FALSE)
  }
}
\keyword{ batch processing }
\keyword{ web scraping }
\concept{ URL processing }
\concept{ data extraction }

