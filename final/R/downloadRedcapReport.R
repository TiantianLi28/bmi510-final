
#' Reads api token called redcapTokenName from the users' .Renviron file; queries redcapUrl to return the redcap Report redcapReportID; and return the contents as a tibble
#'
#' @param redcapTokenName
#' @param redcapUrl
#' @param redcapReportId
#' @return content as a tibble
#' @importFrom httr POST content
#' @examples
#' redcapTokenName = "6189879441F5C29A25245880677488BF"
#' redcapUrl = "https://redcap.emory.edu/api/"
#' redcapReportId = '46524'
#'
#' downloadRedcapReport(redcapTokenName, redcapUrl, redcapReportId)
#' @export
downloadRedcapReport = function(redcapTokenName,redcapUrl,redcapReportId){
  api_token = Sys.getenv(redcapTokenName)
  body = list("token"=token,
              content='report',
              format='csv',
              report_id=redcapReportId,
              csvDelimiter='',
              rawOrLabel='raw',
              rawOrLabelHeaders='raw',
              exportCheckboxLabel='false',
              returnFormat='csv'
  )
  response =  httr::POST(report_url, body = body)
  report_data = httr::content(response, "parsed")
  report_tibble = as_tibble(report_data)
  return(report_tibble)
}
