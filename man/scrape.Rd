\name{scrape}
\Rdversion{1.1}
\alias{scrape}
\title{A Tool For Scraping and Parsing HTML and XML Documents From the Web}
\description{This function assists the user with retrieving HTML and XML files, parsing their contents, and diagnosing potential errors that may have occurred along the way.}
\usage{
scrape(url=NULL,object=NULL,file=NULL,chunkSize=50,maxSleep=5,
	userAgent=unlist(options("HTTPUserAgent")),follow=FALSE,
	headers=TRUE,parse=TRUE,isXML=FALSE,verbose=FALSE)
}
\arguments{
  \item{url }{a vector of URLs, each as a character string.  Either the \code{url}, \code{object}, or the \code{file} parameter must be provided.}
  \item{object }{character; the name of an \code{R} object that contains the raw source code of an HTML or XML.  This parameter is likely useful when a previous call to \code{scrape} simply gathered document source code, followed redirects, and/or returned the headers, thus allowing the user to inspect the output first for potential problems before deciding to parse it into an \code{R}-friendly tree-like structure.  Either the \code{object}, \code{url}, or the \code{file } parameter must be provided.}
  \item{file}{a vector of paths to local files, as a character string.  Either the \code{file}, \code{url}, or the \code{object} parameter must be provided.}
\item{chunkSize }{integer; if a vector of \code{url}s is supplied whose size is greater than the value of \code{chunkSize}, the \code{url}s will be split into chunks of size \code{chunkSize}.  By splitting the \code{url}s into chunks, the number of simultaneous HTTP requests is reduced, thus placing less burden on the server.  The default value of \code{chunkSize} is \code{50}.  It is not recommended that one specifies a value of \code{chunkSize} larger than \code{100}.}
\item{maxSleep }{integer; if the vector of \code{url}s is larger than the value of \code{chunkSize}, the function will ``sleep" for \code{ceiling(runif(1,min=0,max=maxSleep))} seconds between chunks.  It is often helpful to use a sleep parameter when making repeated HTTP requests so as to not overwhelm the servers with gapless sequential requests.  The default value for this parameter is \code{5}.}
  \item{userAgent }{the User-Agent HTTP header that is supplied with any HTTP requests made by this function.  This header is used to identify your HTTP calls to the host server.  It is strongly recommended that one uses an informative User-Agent header, perhaps with a link to one's email or web address.  This information may prove helpful to system administrators when they are unsure of the legitimacy your HTTP requests, as it provides them a way of contacting you.  See the URL reference for ``User-Agent" headers below for more information.  By default, the User-Agent header is assigned the value given by \code{unlist(options("HTTPUserAgent"))}, but the user is encouraged to construct a customized version.}
  \item{follow }{logical; should these HTTP requests follow URL redirects if they are encountered?  Here, redirection will only occur with HTTP requests for which the status code is of the 3xx type (see the reference to HTTP status codes below).  This parameter is only meaningful if the \code{url} parameter is supplied.  The default value for this parameter is \code{FALSE}.}
  \item{headers }{logical; should these HTTP requests retrieve the resulting HTTP headers?  This parameter is only meaningful if the \code{url} parameter is supplied.  The default value for this parameter is \code{FALSE}.}
  \item{parse }{logical, should the \code{url} or \code{file} vectors be parsed into \code{R}-friendly tree-like structures?  See \code{\link[XML]{xmlTreeParse}} for more information about this feature and how the object is returned.  If \code{parse==TRUE}, this tree-like structure is easily navigable using the \code{XPath} language (see the corresponding url reference provided below and the help page for \code{\link[=getNodeSet]{xpathSApply}}).  The default value for this parameter is \code{TRUE}.}
  \item{isXML }{logical; do the \code{url} or \code{file} vectors point to well-formed XML files?  See \code{\link[XML]{xmlTreeParse}} for the differences between parsing XML and HTML documents.  The default value for this parameter is \code{FALSE}.}
  \item{verbose }{logical; shall the function print extra information to the console?  The default value for this parameter is \code{FALSE}.}
}
\value{
If \code{url} or \code{file} is supplied, then either the raw source code of the \code{url}s (\code{file}s) is returned as a list of (potentially long) character vectors (when \code{parse==FALSE}), or a list of \code{R}-friendly tree-like structures of the documents is returned (when \code{parse==TRUE}).  If \code{object} is supplied, then either the raw source code contained within the object is returned as a (potentially long) character string (when \code{parse==FALSE}), or an \code{R}-friendly tree-like structure of the document is returned (when \code{parse==TRUE}).  If \code{url} or \code{object} are supplied, the resulting object may have the following attributes:

\item{redirect.URL }{the destination URLs that resulted from a series of redirects, if they occurred; else \code{NA}.  This is only returned if \code{follow==TRUE}.}

\item{headers }{the HTTP headers resulting from these HTTP requests.  These are only returned if \code{headers==TRUE}.}
}
\references{
Duncan Temple Lang. (2009). XML: Tools for parsing and generating XML within R and S-Plus. \url{http://CRAN.R-project.org/package=XML}.

Duncan Temple Lang. (2009). RCurl: General network (HTTP/FTP/...) client interface for R. \url{http://CRAN.R-project.org/package=RCurl}.

Information about HTTP status codes: \url{http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html}.

Information about User-Agent headers: \url{http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.43}.
	
Information about the \code{XPath} language: \url{http://www.w3schools.com/XPath/default.asp}.
}
\author{Ryan M. Acton <racton@uci.edu> \url{http://www.ryanacton.com}}
\examples{
\dontrun{
## Example 1.  Getting all of the package names available for download
##             from CRAN (http://cran.r-project.org/web/packages/)

# First, pull in the page's source code, check for (and follow) a page redirection, 
# and retrieve the headers before deciding to parse the code.
pageSource<-scrape(url="http://cran.r-project.org/web/packages/",headers=TRUE,
	parse=FALSE)

# Second, inspect the headers to ensure a status code of 200, which means the page
# was served properly.  If okay, then parse the object into an XML tree and retrieve
# all of the package names.
if(attributes(pageSource)$headers["statusCode"]==200) {
	page<-scrape(object="pageSource")
	xpathSApply(page,"//table//td/a",xmlValue)
} else {
	cat("There was an error with the page. \n")
}
}

## Example 2. Parsing a local XML file, then pulling out information of interest

# First, locate and parse the demo recipe file supplied with this package
fileToLoad<-system.file("recipe.xml",package="scrapeR")
mmmCookies<-scrape(file=fileToLoad,isXML=TRUE)

# Next, retrieve the names of the dry ingredients that I'll need to buy
xpathSApply(mmmCookies,"//recipe/ingredient[@type='dry']/item",xmlValue)

# Next, remind myself how much flour is needed
paste(xpathSApply(mmmCookies,"//item[.='flour']/preceding-sibling::amount",
	xmlValue),xpathSApply(mmmCookies,"//item[.='flour']/
	preceding-sibling::unit",xmlValue))

# Finally, remind myself who the author of this recipe is
xpathSApply(mmmCookies,"//recipe",xmlGetAttr,"from")
}
