#' Find association rules from itemsets.
#' It calculates support, confidence and lift values from combinations of items.
do_apriori_internal <- function(df, subject_col, key_col, minlen=1, maxlen=10,
                                min_support=0.1, max_support=1, min_confidence=0.5, lhs=NULL, rhs=NULL,
                                max_basket_items = 12) {
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("arules")
  loadNamespace("stringr")

  if(subject_col %nin% colnames(df)){
    stop(paste(subject_col, "is not in colums", sep=" "))
  }

  if(!key_col %in% colnames(df)){
    stop(paste(key_col, "is not in colums", sep=" "))
  }

  grouped_col <- grouped_by(df)

  cnames <- avoid_conflict(grouped_col, c("lhs", "rhs", "support", "confidence", "lift"))

  # This is executed by each group
  do_apriori_each <- function(df){

    # If there are too many items in a busket, combinations to search tends to explode.
    # To avoid it, when called from Exploratory Analytics View, we limit items (subject_col)
    # in baskets (key_col) only to top frequent items in each basket.
    # Default is set to 12 so that default maxlen 10 fits in it.
    if (!is.null(max_basket_items)) {
      df <- df %>% group_by(!!rlang::sym(key_col), !!rlang::sym(subject_col)) %>%
        summarize(.tmp_num_rows = n()) %>%
        top_n(max_basket_items, .tmp_num_rows) %>%
        ungroup()
    }

    mat <- sparse_cast(df, subject_col, key_col)

    # create appearance list
    if(is.null(lhs)){
      if(is.null(rhs)){
        # lhs and rhs are both NULL
        appearance <- NULL
      } else {
        # lhs is NULL and rhs is not NULL
        # find matched values by stringr::str_detect in subject_col to limit rhs
        appearance <- list(rhs = rhs, default = "lhs")
      }
    } else {
      if(is.null(rhs)){
        # rhs is NULL and lhs is not NULL
        appearance <- list(lhs = lhs, default = "rhs")
      } else {
        # rhs are both not NULL
        appearance <- list(lhs = lhs, rhs = rhs, default = "none")
      }
    }
    rules <- NULL
    # capture.output suppress summary output from arules::apriori function
    capture.output({
      rules <- arules::apriori(
        mat,
        parameter = list(
          minlen=minlen+1, # +1 is to avoid rule with nothing on the lhs. https://cran.r-project.org/web/packages/arules/arules.pdf
          maxlen=maxlen+1,
          support=min_support,
          confidence = min_confidence,
          target="rules",
          smax=max_support
        ),
        appearance = appearance)
    })
    lhs_val <- vapply(arules::LIST(rules@lhs), function(items){
      paste(items, collapse=", ")
    }, FUN.VALUE = "")
    rhs_val <- vapply(arules::LIST(rules@rhs), function(items){
      paste(items, collapse=", ")
    }, FUN.VALUE = "")

    # remove empty strings if lhs or rhs is indicated
    if(!is.null(lhs) & !is.null(rhs)){
      filtered <- lhs_val != "" & rhs_val != ""
    } else if(!is.null(lhs)){
      filtered <- lhs_val != ""
    } else if(!is.null(rhs)){
      filtered <- rhs_val != ""
    } else {
      filtered <- TRUE
    }

    quality <- rules@quality
    ret <- data.frame(
      lhs_val[filtered],
      rhs_val[filtered],
      quality$support[filtered],
      quality$confidence[filtered],
      quality$lift[filtered], stringsAsFactors = FALSE)
    colnames(ret) <- cnames
    ret
  }

  ret <- df %>%
    dplyr::do_(.dots = setNames(~do_apriori_each(.), cnames[[5]]))

  # this happens when lhs and rhs are indicated and no matching rule was found
  if(nrow(ret) == 0){
    stop("No matching rule was found.")
  }

  ret <- ret %>% dplyr::ungroup() %>% unnest_with_drop_(cnames[[5]])
  if(all(is.na(ret[[1]])) & nrow(ret)==1){
    stop("No rule was found. Smaller minimum support or minimum confidence might find rules.")
  }
  ret
}

#' Find association rules from itemsets.
#' It calculates support, confidence and lift values from combinations of items.
#' @export
do_apriori_ <- function(df, subject_col, key_col, minlen=1, maxlen=10, min_support=0.1, max_support=1, min_confidence=0.5, lhs=NULL, rhs=NULL, max_basket_items=12){
  if (min_support == "auto") { # search for min_support that returns some rules.
    ret <- NULL
    curr_min_support = 0.1
    while (curr_min_support >= 0.00001) {
      ret <- tryCatch(do_apriori_internal(df, subject_col, key_col, minlen, maxlen, curr_min_support, max_support, min_confidence, lhs, rhs,
                                          max_basket_items), error=function(e) {
        if (e$message == "No rule was found. Smaller minimum support or minimum confidence might find rules.") { #TODO: this matching is dumb.. 
          TRUE
        }
        else {
          stop(e)
        }
      })
      if (is.logical(ret) && ret == TRUE) {
        curr_min_support <- curr_min_support/10
        next
      }
      break
    }
    if (is.logical(ret) && ret == TRUE) { # after auto search for min_support, still no rule found.
      stop("There is no rules found with the criteria. You might want to set smaller minimum support or confidence values from the property to find the rules.")
    }
    ret
  }
  else {
    do_apriori_internal(df, subject_col, key_col, minlen, maxlen, min_support, max_support, min_confidence, lhs, rhs, max_basket_items)
  }
}

#' Find association rules from itemsets.
#' It calculates support, confidence and lift values from combinations of items.
#' @export
do_apriori <- function(df, subject, key, minlen=1, maxlen=10, min_support=0.1, max_support=1, min_confidence=0.5, lhs=NULL, rhs=NULL, max_basket_items=12){
  subject_col <- col_name(substitute(subject))
  key_col <- col_name(substitute(key))
  do_apriori_(df, subject_col, key_col, minlen, maxlen, min_support, max_support, min_confidence, lhs, rhs, max_basket_items)
}

# rules_metric can be "support", "confidence", or "lift".
get_arules_graph_data <- function(rules, max_rules=30, rules_metric="support") {
  rules <- rules %>% dplyr::top_n(max_rules, UQ(rlang::sym(rules_metric))) # limit within 30 rules so that they can be visualized comfortably.
  if (nrow(rules) > max_rules) { # this means there are ties. remove the rows with minimum support to fit within 30 rules.
    if (!(rules_metric == "confidence" && min(rules$confidence) == 1)) { # exception is when supports for all rules are 1.0.
      rules <- rules %>% dplyr::filter(UQ(rlang::sym(rules_metric)) != min(UQ(rlang::sym(rules_metric))))
    }
    else {
      rules <- rules %>% sample_n(max_rules) # in this case, just sample so that plotting will not take very long time.
    }
  }

  # Give names to the rules. groceries is the dataframe that is the result of the Market Basket Analysis.
  rules <- rules %>% dplyr::mutate(rule = row_number(), rule = stringr::str_c("Rule ",readr::parse_character(rule)))
  
  # Create a dataframe for the relationships from rules to right-hand side products.
  rule_rhs_edges <- rules %>%
    dplyr::select(rule, rhs) %>%
    dplyr::rename(from = rule, to = rhs)
  
  # Create a dataframe for the relationships from left-hand side products to the Rules.
  lhs_rule_edges <- rules %>%
    tidyr::separate_rows(lhs, sep = "\\s*\\,\\s*") %>%
    dplyr::select(lhs, rule) %>%
    dplyr::rename(from = lhs, to = rule)
  
  # Create a dataframe for all the relationships in the graph by binding the above 2 dataframes.
  edges <- lhs_rule_edges %>%
    dplyr::bind_rows(rule_rhs_edges)
  
  product_names <- unique(c(lhs_rule_edges$from, rule_rhs_edges$to))
  
  rule_vertices <- rules %>% dplyr::select(rule, support, confidence, lift) %>% dplyr::rename(name=rule)
  # set min of rule confidence as dummy confidence value for product vertices, so that min/max does not change
  # even after bind_rows. this helps when normalizing for color scale later.
  products_vertices <- data.frame(name=product_names, support=0, confidence=min(rule_vertices$confidence), lift=0, stringsAsFactors = FALSE)
  vertices_data <- rule_vertices %>%
    dplyr::bind_rows(products_vertices)
  
  ret <- list(edges=edges, vertices=vertices_data)
  ret <- data.frame(model=I(list(ret))) # return as data.frame. TODO: handle group_by
  class(ret$model) <- c("list", ".model", ".model.arules_graph")
  ret
}

# Code to plot the result with igraph:
#
# c_scale <- colorRamp(c("white","red"))
#
# vertices <- graph_data$vertices %>%
#   mutate(size=support/max(support)*15) %>%   # normalize support so that the largest circle size is always 15.
#          # normalize confidence to fully utilize color scale.
#   mutate(color=apply(c_scale((confidence - min(confidence))/(max(confidence)-min(confidence))), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255, alpha=0.8) ))
#
# edges <- graph_data$edges
# 
# require(igraph)
# # Set random seed for reproducibility of the chart.
# set.seed(0)
# 
# # Create a graph object
# g <- graph.data.frame(edges, directed=TRUE, vertices = vertices)
# # Do not display name of rules
# modify_label <- function(x) {if_else(str_detect(x,"^Rule "), "", x)}
# labels <- modify_label(V(g)$name)
# 
# # Plot the graph
# par(mar=c(0,0,0,0)) 
# plot(g, edge.arrow.size=0.5, vertex.label=labels, vertex.label.family="sans", vertex.label.color=rgb(0.4,0.4,0.4), vertex.label.cex=0.9, vertex.frame.color=rgb(1,0.5,0.5))
 
