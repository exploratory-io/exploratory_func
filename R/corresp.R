
#' Extracts results from correspondence analysis result object in a dataframe column.
#' @export
tidy.mca_exploratory <- function(x, type="categories") {
  if (type == "categories") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$coord), var="category")
    res <- res %>% dplyr::select(category, `Dim 1`, `Dim 2`)
    res <- res %>% tidyr::separate(col = category, into = c("variable", "category"), sep = ":")
    res <- res %>% dplyr::mutate(variable = x$var_names_map[variable])
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
  else if (type == "variables") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$eta2), var="variable")
    res <- res %>% dplyr::select(variable, `Dim 1`, `Dim 2`)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
  else if (type == "quanti_sup") {
    res <- tibble::rownames_to_column(as.data.frame(x$quanti.sup$coord), var="variable")
    res <- res %>% dplyr::select(variable, `Dim 1`, `Dim 2`)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
  else if (type == "contrib") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$contrib), var="category")
    res <- res %>% dplyr::select(category, starts_with("Dim "))
    res <- res %>% tidyr::separate(col = category, into = c("variable", "category"), sep = ":")
    res <- res %>% dplyr::mutate(variable = x$var_names_map[variable])
    res <- res %>% tidyr::unite(Category, variable, category, sep=" - ")
    res <- res %>% tidyr::pivot_longer(cols=starts_with("Dim "), names_to="Dimension", values_to="Value")
    res <- res %>% dplyr::mutate(Dimension = stringr::str_replace(Dimension, "Dim ", "Dimension "))
    res
  }
  else if (type == "variance") {
    res <- as.data.frame(x$eig) %>% dplyr::mutate(dim=1:n())
    # Omit the tail demensions with almost 0 variances.
    res <- res %>% dplyr::filter(`percentage of variance` > 1e-15)
  }
  else if (type == "data") {
    res <- as.data.frame(x$ind$coord)
    res <- x$df %>% bind_cols(res)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
}
