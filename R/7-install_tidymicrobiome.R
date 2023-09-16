install_tidymicrobiome <-
  function(packages = c("core", "all"),
           which_package,
           from = c("gitlab", "github", "tidymicrobiome.org"),
           method = c("auto", "internal", "libcurl",
                      "wget", "curl")) {
    # if (!require(remotes)) {
    #   install.packages("remotes")
    # }

    packages <- match.arg(packages)
    from <- match.arg(from)
    method <- match.arg(method)

    temp_path <- tempdir()
    dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
    unlink(x = file.path(temp_path, dir(temp_path)),
           recursive = TRUE,
           force = TRUE)

    if (from == "gitlab") {
      file <-
        read.table(
          "https://gitlab.com/tidymicrobiome/packages_repo/-/raw/main/packages/file.csv",
          sep = ",",
          header = TRUE
        )
    }

    if (from == "github") {
      file <-
        read.table(
          "https://raw.githubusercontent.com/tidymicrobiome/packages_repo/main/packages/file.csv",
          sep = ",",
          header = TRUE
        )
    }

    if (from == "tidymicrobiome.org") {
      utils::download.file(
        url = "https://www.tidymicrobiome.org/tidymicrobiome-packages/file.csv",
        destfile = file.path(temp_path, "file.csv"),
        method = method
      )
      file <-
        read.table(file.path(temp_path, "file.csv"),
                   sep = ",",
                   header = TRUE)
    }

    ####package list
    core_package_list <-
      c(
        "microbiomeplot",
        "microbiomedataset",
        "tidymicrobiome"
      )

    if (!missing(which_package)) {
      package_list <-
        which_package
    } else{
      if (packages == "core") {
        package_list <-
          core_package_list
      } else{
        package_list <-
          c(core_package_list,
            "massconverter",
            "massdatabase")
      }
    }

    ####download the packages
    for (x in package_list) {
      message("Download ", x, "...")
      if (from == "github") {
        url <-
          paste0(
            "https://github.com/tidymicrobiome/packages_repo/raw/main/packages/",
            file$file_name.y[file$package == x]
          )
      }

      if (from == "gitlab") {
        url <-
          paste0(
            "https://gitlab.com/tidymicrobiome/packages_repo/-/raw/main/packages/",
            file$file_name.y[file$package == x],
            "?inline=false"
          )
      }

      if (from == "tidymicrobiome.org") {
        url <-
          paste0("https://www.tidymicrobiome.org/tidymicrobiome-packages/",
                 file$file_name.y[file$package == x])
      }

      utils::download.file(
        url = url,
        destfile = file.path(temp_path, file$file_name.y[file$package == x]),
        method = method
      )
    }


    ####install package
    for (x in package_list) {
      message("Install ", x, "...")
      tryCatch(
        detach(name = paste0("package:", x)),
        error = function(e) {
          message(x, " is not loaded")
        }
      )

      if (x == "tidymicrobiome") {
        # detach("package:purrr")
        # detach("package:stringr")
        # install.packages("purrr")
        # install.packages("stringr")

        install.packages(
          file.path(temp_path, file$file_name.y[file$package == x]),
          repos = NULL,
          dependencies = TRUE
        )
      } else{
        install.packages(
          file.path(temp_path, file$file_name.y[file$package == x]),
          repos = NULL,
          dependencies = TRUE
        )

        remotes::install_deps(
          pkgdir = file.path(temp_path, file$file_name.y[file$package == x]),
          dependencies = TRUE,
          upgrade = "never"
        )
      }
      unlink(file.path(temp_path, file$file_name.y[file$package == x]))
    }

    message("All done.")
  }
