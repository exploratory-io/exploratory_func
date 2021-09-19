#' @param host
#' @param password
#' export
getAzureEndPoint <- function(host = "", securityToken = ""){
  AzureStor::storage_endpoint(endpoint = host, sas = securityToken)
}
#' @param host
#' @param sas
#' export
getAzureContainer <- function(host = "", securityToken = "", container = "") {
  endpoint <- getAzureEndPoint(host = host, securityToken = securityToken)
  AzureStor::storage_container(endpoint, container)
}
#' @param host
#' @param sas
#' export
listAzureContainers <- function(host = "", securityToken = ""){
  endpoint <- getAzureEndPoint(host = host, securityToken = securityToken)
  containers <- AzureStor::list_storage_containers(endpoint)
  df <- data.frame(matrix(unlist(containers), nrow = length(containers), byrow = TRUE))
  colnames(df) <- c("name", "endpoint", "sas", "version")
  df
}
#' @param host
#' @param sas
#' @param container
#' @param folder
#' export
listItemsInAzure <- function(host = "", securityToken = "", container = "", folder = ""){
  container <- getAzureContainer(host = host, securityToken = securityToken, container = container)
  AzureStor::list_storage_files(container, dir = folder, info = "all")
}
