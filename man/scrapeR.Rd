\name{scrapeR}
\alias{scrapeR}
\title{Web Page Content Scraper}
\description{
  The \code{scrapeR} function fetches and extracts text content from the specified web page.
  It handles HTTP errors and parses HTML efficiently.
}
\usage{
scrapeR(url)
}
\arguments{
  \item{url}{A character string specifying the URL of the web page to be scraped.}
}
\details{
  The function uses \code{tryCatch} to handle potential web scraping errors. It fetches
  the webpage content, checks for HTTP errors, and then parses the HTML content to extract
  text. The text from different HTML nodes like headings and paragraphs is combined into a
  single string.
}
\value{
  A character string containing the combined text from the specified HTML nodes of the web
  page. Returns \code{NA} if an error occurs or if the page content is not accessible.
}
\references{
  Refer to the \href{https://www.rdocumentation.org/packages/rvest}{rvest package documentation}
  for underlying HTML parsing and extraction methods.
}
\author{Mathieu Dubeau, Ph.D.}
\note{
  This function requires the \pkg{httr} and \pkg{rvest} packages. Ensure that these dependencies
  are installed and loaded in your R environment.
}

\seealso{
  \code{\link[httr]{GET}}, \code{\link[rvest]{read_html}}, \code{\link[rvest]{html_nodes}},
  \code{\link[rvest]{html_text}}
}
\examples{
\donttest{
 url <- "http://www.example.com"
scraped_text <- scrapeR(url)
}
}
\keyword{web scraping}
\keyword{text extraction}
\concept{HTML parsing}
\concept{web content extraction}

