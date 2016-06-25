generate_examples <- function(function_name, args){

  cat(paste(function_name, "\n\n"))

  cat("args\n")

  keys <- names(args)

  if(is.null(keys)){
    keys <- seq(length(args))
  }

  keys[keys==""] <- seq(length(keys[keys==""]))

  for(key in keys){
    arg <- args[[key]]
    cat("$")
    cat(key)
    cat("\n")
    if(is.data.frame(arg)){
      print(knitr::kable(arg, format="markdown"))
    }else {
      print(arg)
    }
    cat("\n")
  }
  cat("\n")
  cat("output")
  ret <- do.call(function_name, args)
  print(knitr::kable(ret, format="markdown"))
  ret
}

#' Check package update from cran and requiredRpackage json
check_package_versions <- function(json_path, download=NULL, overwrite=FALSE){
  loadNamespace("jsonlite")
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("stringr")
  jsontxt <- readChar(json_path, file.info(json_path)$size)
  parsed_json <- jsonlite::fromJSON(jsontxt)

  package_info <- available.packages()
  included <- rownames(package_info) %in% parsed_json$name
  included_pkg <- package_info[included,]

  #Check update
  pkg_check <- lapply(seq(nrow(included_pkg)), function(rindex){
    pkginfo <- included_pkg[rindex,]
    pkgname <- pkginfo[["Package"]]
    pkgversion <- pkginfo[["Version"]]
    current_pkgversion <- parsed_json[parsed_json$name==pkgname, "version"]
    compareVersion(pkgversion, current_pkgversion)
  })

  update <- as.data.frame(included_pkg[pkg_check == 1, c("Package", "Version")], stringsAsFactors = FALSE)

  current <- parsed_json[parsed_json$name %in% update$Package,]
  joined <- (
    dplyr::left_join(current, update, by=c("name" = "Package"))
    %>%  dplyr::mutate(file_name = paste(name,"_", Version, sep=""))
  )
  if(nrow(joined)>0){
    print("update require")
    print(joined)
  } else {
    print("every package is up to date")
  }

  # Check dependency and that version
  depends <- (
    as.data.frame(included_pkg[, c("Package", "Depends", "Imports")])
    %>%  dplyr::mutate(Dependency = paste(Depends, Imports, sep=", "))
    %>%  dplyr::mutate(Dependency = stringr::str_split(Dependency, ",[ |\n]"))
    %>%  tidyr::unnest(Dependency)
    %>%  tidyr::separate(Dependency, into=c("Dependency", "MinVer"), sep="\\(>= ")
    %>%  dplyr::mutate(MinVer = stringr::str_replace(MinVer,"\\)", ""))
    %>%  dplyr::mutate(Dependency = stringr::str_replace(Dependency," ", ""))
    %>%  dplyr::select(Package, Dependency, MinVer)
    %>%  dplyr::filter(!Dependency %in% c("NA", "R", "utils", "methods", "tools"))
  )

  # Check if upgrade is required because of dependency relations
  upgrade_require <- lapply(seq(nrow(parsed_json)), function(rindex){
    package_name <- parsed_json[rindex,"name"]
    cur_req_ver <- parsed_json[rindex,"required_version"]

    req_ver <- depends[depends[,"Dependency"]==package_name, "MinVer"]
    check_list <- lapply(req_ver$MinVer, function(ver){
      compareVersion(ver, cur_req_ver)
    })

    if(all(check_list<1)){
      character(0)
    } else {
      package_name
    }
  })

  need_upgrade <- (
    depends[depends$Dependency %in% upgrade_require, ]
    %>%  dplyr::arrange(Dependency)
  )

  if(nrow(need_upgrade)>0){
    print("dependency update require")
    print(need_upgrade)
  } else {
    print("every dependency is up to date")
  }
  if(!is.null(download)){
    loadNamespace("httr")
    for(file_name in joined$file_name){
      mac <- paste("https://cran.r-project.org/bin/macosx/mavericks/contrib/3.3/", file_name, ".tgz", sep="")
      try({dir.create(paste(download,"macosx/",sep=""))})
      mac_path <- paste(download,"macosx/",file_name,".tgz", sep="")
      print(mac)
      print(mac_path)
      httr::GET(mac, httr::write_disk(mac_path, overwrite=overwrite))
      try({dir.create(paste(download,"windows/",sep=""))})
      win <- paste("https://cran.r-project.org/bin/windows/contrib/3.3/", file_name, ".zip", sep="")
      win_path <- paste(download,"windows/",file_name,".zip", sep="")
      print(win)
      print(win_path)
      httr::GET(win, httr::write_disk(win_path, overwrite=overwrite))
    }

  }
}
