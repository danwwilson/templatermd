highlighters <- function() {
  c("default",
    "tango",
    "pygments",
    "kate",
    "monochrome",
    "espresso",
    "zenburn",
    "haddock")
}

tmpfile_pattern <- "rmarkdown-str"

# return a string as a tempfile
as_tmpfile <- function(str) {
  if (length(str) > 0) {
    str_tmpfile <- tempfile(tmpfile_pattern, fileext = ".html")
    writeLines(str, str_tmpfile, useBytes =  TRUE)
    str_tmpfile
  } else {
    NULL
  }
}

# temp files created by as_tmpfile() cannot be immediately removed because they
# are needed later by the pandoc conversion; we have to clean up the temp files
# that have the pattern specified in `tmpfile_pattern` when render() exits
clean_tmpfiles <- function() {
  unlink(list.files(
    tempdir(), sprintf("^%s[0-9a-f]+[.]html$", tmpfile_pattern), full.names = TRUE
  ))
}
