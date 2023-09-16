##------------------------------------------------------------------------------
#' @title check_tidymicrobiome_version
#' @description Check if there are packages in tidymicrobiome can be updated.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param packages core or all packages in tidymicrobiome. "core" means all the core
#' packages in tidymicrobiome and "all" means all the packages in tidymicrobiome.
#' @param from "gitlab", "github", or "tidymicrobiome.org"
#' @importFrom magrittr %>%
#' @export

check_tidymicrobiome_version <-
  function(packages = c("core", "all"),
           from = c("gitlab", "github", "shen")) {
    from <- match.arg(from)
    packages <- match.arg(packages)
    check_result <-
      c(
        "tidymicrobiome",
        "microbiomedataset",
        "microbiomeplot"
      ) %>%
      lapply(function(x) {
        y <-
          tryCatch(
            check_github(pkg = paste0("tidymicrobiome/", x)),
            error = function(e) {
              NULL
            }
          )
        if (is.null(y)) {
          y <-
            tryCatch(
              check_gitlab(pkg = paste0("tidymicrobiome/", x)),
              error = function(e) {
                NULL
              }
            )
        }

        if (is.null(y)) {
          y <-
            tryCatch(
              check_gitee(pkg = paste0("tidymicrobiome/", x)),
              error = function(e) {
                NULL
              }
            )
        }

        if (is.null(y)) {
          y <-
            tryCatch(
              check_tidymicrobiome.org(pkg = x),
              error = function(e) {
                NULL
              }
            )
        }

        if (is.null(y)) {
          y <-
            c(
              package = paste0("tidymicrobiome/", x),
              installed_version = "1.0.0",
              latest_version = "1.0.0",
              up_to_date = TRUE
            )
        }
        y$installed_version <-
          as.character(y$installed_version)
        unlist(y)
      })

    check_result <-
      do.call(rbind, check_result) %>%
      as.data.frame()

    check_result$package <-
      check_result$package %>%
      stringr::str_replace("tidymicrobiome\\/", "")

    check_result$up_to_date <-
      check_result$installed_version ==
      check_result$latest_version

    check_result$up_to_date <-
      as.logical(check_result$up_to_date)

    if (packages == "core") {
      check_result <-
        check_result %>%
        dplyr::filter(!stringr::str_detect(package, "massconverter"))
    }

    if (all(check_result$up_to_date)) {
      message("No package to update.")
    } else{
      check_result <-
        check_result %>%
        dplyr::filter(!up_to_date)
      message("Use update_tidymicrobiome() to update the following pacakges.")
      check_result
    }
  }


##------------------------------------------------------------------------------
#' @title update_tidymicrobiome
#' @description Update packages in tidymicrobiome.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param packages core or all packages in tidymicrobiome. "core" means all the core
#' packages in tidymicrobiome and "all" means all the packages in tidymicrobiome.
#' @param from github, gitlab or tidymicrobiome.org
#' @param fastgit if install packages using fastgit. see
#' https://hub.fastgit.org/
#' @importFrom remotes install_github install_gitlab install_git
#' @export
update_tidymicrobiome <-
  function(packages = c("core", "all"),
           from = c("gitlab", "github", "tidymicrobiome.org"),
           fastgit = FALSE) {
    packages <- match.arg(packages)
    from <- match.arg(from)

    check_result <-
      check_tidymicrobiome_version(packages = packages)

    if (!is.null(check_result)) {
      if (from == "github") {
        for (i in check_result$package) {
          tryCatch(
            detach(name = paste0("package:", i)),
            error = function(e) {
              message(i, ".\n")
            }
          )
          if (fastgit) {
            install_fastgit(
              pkg = paste0("tidymicrobiome/", i),
              from = from,
              upgrade = "never"
            )
          } else{
            remotes::install_github(repo = paste0("tidymicrobiome/", i),
                                    upgrade = "never")
          }

        }
      }

      if (from == "gitlab") {
        for (i in check_result$package) {
          tryCatch(
            detach(name = paste0("package:", i)),
            error = function(e) {
              message(i, ".\n")
            }
          )

          if (fastgit) {
            install_fastgit(
              pkg = paste0("tidymicrobiome/", i),
              from = from,
              upgrade = "never"
            )
          } else{
            remotes::install_gitlab(repo = paste0("tidymicrobiome/", i),
                                    upgrade = "never")
          }
        }
      }


      if (from == "tidymicrobiome.org") {
        for (i in check_result$package) {
          tryCatch(
            detach(name = paste0("package:", i)),
            error = function(e) {
              message(i, ".\n")
            }
          )
          install_tidymicrobiome(which_package = i, from = "tidymicrobiome.org")
        }
      }
    }
  }
