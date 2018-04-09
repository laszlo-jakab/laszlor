#' Set up connection to WRDS
#'
#' Follows the instructions provided by WRDS.
#'
#' @param user WRDS user name.
#' @param pass WRDS password (recommended to use hashed version).
#' @param jar.path Local path to the necessary .jar file (available on WRDS).
#'
#' @return Connection to WRDS
#'
#' @export
#'
#' @importFrom RJDBC JDBC
#' @importFrom RJDBC dbConnect

WRDSConnect <- function(user, pass, jar.path){
  drv <- JDBC("com.sas.net.sharenet.ShareNetDriver",
              jar.path,
              identifier.quote = "`")
  wrds <- dbConnect(drv,
                    "jdbc:sharenet://wrds-cloud.wharton.upenn.edu:8551/",
                    user,
                    pass)
  return(wrds)
}
