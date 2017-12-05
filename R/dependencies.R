# flattens an arbitrarily nested list and returns all of the dependency
# objects it contains
flatten_dependencies <- function(knit_meta, test) {

  all_dependencies <- list()

  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be an html dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep)) && is.list(dep)) {
      inner_dependencies <- flatten_dependencies(dep, test)
      all_dependencies <- append(all_dependencies, inner_dependencies)
    } else if (test(dep)) {
      all_dependencies[[length(all_dependencies) + 1]] <- dep
    }
  }

  all_dependencies
}

# check if the passed knit_meta has any (e.g. html/latex) dependencies
has_dependencies <- function(knit_meta, class) {

  if (inherits(knit_meta, class))
    return(TRUE)

  if (is.list(knit_meta)) {
    for (dep in knit_meta) {
      if (is.null(names(dep))) {
        if (has_dependencies(dep, class))
          return(TRUE)
      } else {
        if (inherits(dep, class))
          return(TRUE)
      }
    }
  }
  FALSE
}
