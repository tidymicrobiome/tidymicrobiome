#' @title Show the logo tidymicrobiome.
#' @description The tidymicrobiome logo, using ASCII or Unicode characters
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#' on UTF-8 platforms.
#' @return A ASCII log of tidymicrobiome
#' @export
#' @importFrom dplyr filter
#' @import microbiomedataset
#' @import microbiomeplot
#' @importFrom magrittr %>%
#' @importFrom utils packageVersion install.packages read.table
#' @examples
#' tidymicrobiome_logo()
##https://onlineasciitools.com/convert-text-to-ascii-art

tidymicrobiome_logo <-
  function(unicode = l10n_info()$`UTF-8`) {
    message(crayon::green("Thank you for using tidymicrobiome!"))
    message(crayon::green("Version", tidymicrobiome_version, "(", update_date, ')'))
    message(crayon::green("More information: www.tidymicrobiome.org"))

    logo =
      c(
        "  _   _     _       __  __ _                _     _                      ",
        " | | (_)   | |     |  \\/  (_)              | |   (_)                     ",
        " | |_ _  __| |_   _| \\  / |_  ___ _ __ ___ | |__  _  ___  _ __ ___   ___ ",
        " | __| |/ _` | | | | |\\/| | |/ __| '__/ _ \\| '_ \\| |/ _ \\| '_ ` _ \\ / _ \\",
        " | |_| | (_| | |_| | |  | | | (__| | | (_) | |_) | | (_) | | | | | |  __/",
        "  \\__|_|\\__,_|\\__, |_|  |_|_|\\___|_|  \\___/|_.__/|_|\\___/|_| |_| |_|\\___|",
        "               __/ |                                                     ",
        "              |___/                                                      "
      )

    hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
    if (unicode)
      hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]

    cols <- c(
      "red",
      "yellow",
      "green",
      "magenta",
      "cyan",
      "yellow",
      "green",
      "white",
      "magenta",
      "cyan"
    )

    col_hexa <-
      purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

    for (i in 0:9) {
      pat <- paste0("\\b", i, "\\b")
      logo <- sub(pat, col_hexa[[i + 1]], logo)
    }

    structure(crayon::blue(logo), class = "tidymicrobiome_logo")
  }

#' @export

print.tidymicrobiome_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}

tidymicrobiome_version <-
  as.character(utils::packageVersion("tidymicrobiome"))
update_date <-
  as.character(Sys.time())


# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# tidymicrobiome_logo <-
#   c(
#     "  _   _     _       __  __ _                _     _                      ",
#     " | | (_)   | |     |  \\/  (_)              | |   (_)                     ",
#     " | |_ _  __| |_   _| \\  / |_  ___ _ __ ___ | |__  _  ___  _ __ ___   ___ ",
#     " | __| |/ _` | | | | |\\/| | |/ __| '__/ _ \\| '_ \\| |/ _ \\| '_ ` _ \\ / _ \\",
#     " | |_| | (_| | |_| | |  | | | (__| | | (_) | |_) | | (_) | | | | | |  __/",
#     "  \\__|_|\\__,_|\\__, |_|  |_|_|\\___|_|  \\___/|_.__/|_|\\___/|_| |_| |_|\\___|",
#     "               __/ |                                                     ",
#     "              |___/                                                      "
#   )
# cat(tidymicrobiome_logo, sep = "\n")
