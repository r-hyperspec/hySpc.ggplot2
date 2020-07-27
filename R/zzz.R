.onAttach <- function(libname, pkgname) {
  # unlockBinding(".options", asNamespace("hySpc.ggplot2"))

  desc <- utils::packageDescription("hySpc.ggplot2")
  first_url <- sub(",.*$", "", desc$URL) # To use the first URL only

  packageStartupMessage(
    "\n\n",
    "Package ", desc$Package, " (version ", desc$Version, ")\n\n",

    # "To get started, try: \n",
    # '   vignette("', desc$Package, '", package = "', desc$Package, '")', "\n",
    # '   browseURL("', first_url, '") \n',
    # "\n",
    #
    # "If you use this package, please cite it appropriately.\n",
    # "The correct reference is given by:\n",
    # '   citation("', desc$Package, '")\n\n',

    "The project's website:\n   ", first_url, "\n\n",
    sep = ""
  )
}
