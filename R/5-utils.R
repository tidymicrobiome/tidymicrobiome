msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("tidymicrobiome.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark))
    crayon::white(x)
  else
    crayon::black(x)

}

#' List all packages in the tidymicrobiome
#'
#' @param include_self Include tidymicrobiome in the list?
#' @export
#' @examples
#' tidymicrobiome_packages()
tidymicrobiome_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("tidymicrobiome")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <-
    vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "tidymicrobiome")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0)
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}



#####Code are from Guangchuang Yu
check_github <- function(pkg) {
  check_github_gitlab_gitee(pkg, "github")
}

check_gitlab <- function(pkg) {
  check_github_gitlab_gitee(pkg, "gitlab")
}

check_gitee <- function(pkg) {
  check_github_gitlab_gitee(pkg, "gitee")
}

check_tidymicrobiome.org <-
  function(pkg) {
    x <-
      readLines(
        paste0(
          "https://www.tidymicrobiome.org/tidymicrobiome-packages/",
          pkg,
          "_Description.txt"
        )
      )
    installed_version <-
      tryCatch(
        utils::packageVersion(gsub(".*/", "", pkg)),
        error = function(e)
          NA
      )

    remote_version <-
      gsub("Version:\\s*", "", x[grep('Version:', x)])

    res <- list(
      package = pkg,
      installed_version = installed_version,
      latest_version = remote_version,
      up_to_date = NA
    )

    if (is.na(installed_version)) {
      message(crayon::red(paste("##", pkg, "is not installed...")))
    } else {
      if (remote_version > installed_version) {
        msg <- paste("##", pkg, "is out of date...")
        message(crayon::yellow(msg))
        res$up_to_date <- FALSE
      } else if (remote_version == installed_version) {
        message("##", pkg, " is up-to-date devel version")
        res$up_to_date <- TRUE
      }
    }
    return(res)
  }


check_github_gitlab_gitee <-
  function(pkg, repo = c("github", "gitlab", "gitee")) {
    repo <- match.arg(repo)
    installed_version <-
      tryCatch(
        utils::packageVersion(gsub(".*/", "", pkg)),
        error = function(e)
          NA
      )

    if (repo == "github") {
      url <-
        paste0("https://raw.githubusercontent.com/",
               pkg,
               "/master/DESCRIPTION")

      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )

      if (is.null(x)) {
        url <-
          paste0("https://raw.githubusercontent.com/",
                 pkg,
                 "/main/DESCRIPTION")

        x <-
          tryCatch(
            readLines(url),
            error = function(e) {
              NULL
            }
          )
      }
    }

    if (repo == "gitlab") {
      url <- paste0("https://gitlab.com/", pkg, "/raw/master/DESCRIPTION")
      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )

      if (is.null(x)) {
        url <-
          paste0("https://gitlab.com/", pkg, "/raw/main/DESCRIPTION")
      }

      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )
    }

    if (repo == "gitee") {
      url <- paste0("https://gitee.com/", pkg, "/raw/master/DESCRIPTION")
      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )

      if (is.null(x)) {
        url <-
          paste0("https://gitee.com/", pkg, "/raw/main/DESCRIPTION")
      }

      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )
    }

    if (is.null(x)) {
      stop("can't read information from ", url)
    }

    remote_version <-
      gsub("Version:\\s*", "", x[grep('Version:', x)])

    res <- list(
      package = pkg,
      installed_version = installed_version,
      latest_version = remote_version,
      up_to_date = NA
    )

    if (is.na(installed_version)) {
      message(crayon::red(paste("##", pkg, "is not installed...")))
    } else {
      if (remote_version > installed_version) {
        msg <- paste("##", pkg, "is out of date...")
        message(crayon::yellow(msg))
        res$up_to_date <- FALSE
      } else if (remote_version == installed_version) {
        message("##", pkg, " is up-to-date devel version")
        res$up_to_date <- TRUE
      }
    }
    return(res)
  }

install_fastgit <-
  function (pkg,
            from = c("gitee", "gitlab", "github"),
            ...)
  {
    from <- match.arg(from)
    if (from == "gitee") {
      if (!grepl("/", pkg)) {
        stop("Invalid package name, should in 'name/repo' format.")
      }
      remotes::install_git(paste0("https://gitee.com/", pkg),
                           ...)
    }
    else {
      if (any(grepl(":", pkg))) {
        remotes::install_git(pkg, ...)
      }
      else {
        if (any(grepl("/", pkg))) {
          tryCatch(
            remotes::install_git(paste0("https://hub.fastgit.org/",
                                        pkg)),
            error = function(e) {
              message("Install error when use GitHub mirror, roll back to official GitHub.")
              remotes::install_github(pkg)
            }
          )
        }
      }
    }
  }
