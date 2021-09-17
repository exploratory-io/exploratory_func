#' @param url
#' @param sas
#' export
getAzureEndPoint <- function(url = "", sas = ""){
  AzureStor::storage_endpoint(endpoint = url, sas = sas)
}
#' @param url
#' @param sas
#' export
getAzureContainer <- function(url = "", sas = "", container = "") {
  endpoint <- getAzureEndPoint(url = url, sas = sas)
  AzureStor::storage_container(endpoint, container)
}
#' @param url
#' @param sas
#' export
listAzureContainers <- function(url = "", sas = ""){
  endpoint <- getAzureEndPoint(url = url, sas = sas)
  containers <- AzureStor::list_storage_containers(endpoint)
  df <- data.frame(matrix(unlist(containers), nrow=length(containers), byrow=TRUE))
  colnames(df) <- c("name", "endpoint", "sas", "version")
  df
}
#' @param url
#' @param sas
#' export
listItemsInAzure <- function(url = "", sas = "", container = ""){
  container <- getAzureContainer(url = url, sas = sas, container = container)
  AzureStor::list_storage_files(container)
}
