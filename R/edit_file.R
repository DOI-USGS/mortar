#' Edit a text file programmatically
#'
#' Insert or transform (`txt` argument) one or more lines in a file specified
#' either by line number or a boolean function (`match` argument). Any file that
#' can be read with the \code{readLines()} function should be compatible.
#'
#' @param file chr, path to a file
#' @param txt chr or chr fun, either a single character string to insert into
#'   the file or a function that transforms a character string. If supplying a
#'   function, the first argument of the function must accept a single string
#'   and it must return a single string. The function will be applied
#'   independently to each line in the file that matches the match argument.
#'   Supports anonymous functions. See examples.
#' @param match num vector or lgl fun, either a numeric vector indicating line
#'   indices in the file or a function that returns a logical indicating lines
#'   to change. If supplying a function, the first argument of the function must
#'   accept a single string and it must return a logical vector. The function
#'   will be applied independently to each line in the file. Supports anonymous
#'   functions. See examples.
#' @param append lgl, if TRUE the text will be added as a new line after the
#'   line(s) specified in the match argument. If FALSE, then the text will
#'   replace the lines specified in the match argument
#'
#' @return (invisibly) FALSE if no lines in the specified file match the match
#'   argument and TRUE otherwise
#' @export
#'
#' @examples
#' tmpfile <- tempfile()
#'
#' writeLines(text =
#'              c("hello I am joe",
#'                "this is multiple lines of text",
#'                "12345",
#'                "",
#'                "^ line 4 is an empty line"),
#'            con = tmpfile)
#' cat(readLines(tmpfile),sep = "\n")
#'
#' # The simplest use is to append a line in a file with new text. Note that txt
#' # must be a single character string.
#' file_edit(file = tmpfile,
#'           txt = "this line is appended after the second line",
#'           match = 2,
#'           append = TRUE)
#' cat(readLines(tmpfile),sep = "\n")
#'
#' # We can instead replace a line using append = FALSE. Here, we replace the empty
#' # line in the file. After adding the line above, the empty line now occupies
#' # index 5.
#' file_edit(file = tmpfile,
#'           txt = "this line is no longer empty",
#'           match = 5,
#'           append = FALSE)
#' cat(readLines(tmpfile),sep = "\n")
#'
#' # The txt argument can also be a function, anonymous or named. It just needs to
#' # accept a single string and return a single string. Here, we replace "is" with
#' # "was" in line 6.
#' file_edit(file = tmpfile,
#'           txt = ~ stringr::str_replace(.x, "is", "was"),
#'           match = 6,
#'           append = FALSE)
#' cat(readLines(tmpfile),sep = "\n")
#'
#' # The match argument can also be a  function. It just needs to accept a single
#' # string and return a single boolean. You can use it with the txt argument
#' # for some interesting interactions. Here, we change every line containing
#' # the word "is" to uppercase
#' file_edit(file = tmpfile,
#'           txt = toupper,
#'           match = ~ stringr::str_detect(.x, " is "),
#'           append = FALSE)
#' cat(readLines(tmpfile),sep = "\n")
#'
#' # strings containing carriage returns are preserved. Here, we use the readLines
#' # and length functions to ensure we append the string to the end of the file
#' multiline <-
#' "
#' Here is a string
#' that takes up multiple lines"
#'
#' file_edit(file = tmpfile,
#'           txt = multiline,
#'           match = length(readLines(tmpfile)),
#'           append = TRUE)
#' cat(readLines(tmpfile),sep = "\n")

file_edit <- function(file, txt, match, append = TRUE){

  if(!is.character(file) | length(file) > 1){
    cli::cli_abort("Argument {.arg file} must be a single string, not a
                   {.cls {class(file)}} of length {length(file)}.")
  }
  if(!file.exists(file)){
    cli::cli_abort("I couldn't find a file at {.file {file}}.")
  }


  # txt must be character or function
  # match must be numeric vector or function

  lines <- readLines(file)

  # either match is a numeric vector
  if(all(is.numeric(match))){
    if(any(match < 0)) {
      cli::cli_abort(c(
        "{.arg match} cannot contain negative numbers.",
        x = "Negative indices are not valid.",
        i = "If {.arg match} is numeric, it is treated as indices of line numbers."
      ))
    }

    if(!rlang::is_integerish(match)) {
      cli::cli_abort(c(
        "{.arg match} cannot contain non-integer(ish) numbers.",
        x = "Non-integer indices are not valid.",
        i = "If {.arg match} is numeric, it is treated as indices of line numbers."
      ))
    }


    matched_lines <- match
  } else {   # or its a function that returns a boolean
    match <- rlang::as_function(match)

    if(!rlang::is_function(match)){
      cli::cli_abort("Invalid argument {.arg match}. This must be a named or
                     anonymous function.")
    }

    # determine lines for which match returns TRUE
    matched_lines_list <- purrr::map(lines, match)
    if(!all(
      purrr::map_lgl(matched_lines_list, is.logical),
      sum(lengths(matched_lines_list)) == length(matched_lines_list)
    )) {
      cls_out <- as.character(unique(purrr::map(matched_lines_list, class)))
      cli::cli_abort(c(
        "{.arg match} must return a single logical value for each line.",
        i = "If {.arg match} is a function, it is used to perform a logical subset on lines based on string matches.",
        x = "The function provided output(s) in the following class{?es}: {cls_out}.",
        x = "The function provided {sum(lengths(matched_lines_list))} output{?s} for {length(lines)} line{?s}."
      ))
    }
    matched_lines <- as.logical(matched_lines_list)

    # we just need the indices of the lines
    matched_lines <- which(matched_lines)
  }

  if(length(matched_lines) == 0){
    cli::cli_warn(c(
      "!" = "No lines in {.file {file}} match the {.arg match} argument.",
      "i" = "No changes were made to {.file {file}}."
    ))
    return(invisible(FALSE))
  }

  if(all(length(txt) > 1, length(txt) != length(matched_lines))) {
    cli::cli_abort(c(
      "The length of {.arg txt} must be equal to 1 or the number of matches identified by {.arg match}.",
      "i" = "{.arg match} resulted in {length(matched_lines)} matched line{?s} and {.arg txt} has a length of {length(txt)}."
    ))
    return(invisible(FALSE))
  }

  # either txt is a string or it's a function that returns another string
  if(all(is.character(txt))){
    new_txt <- txt
  } else {
    txt <- rlang::as_function(txt)

    if(! rlang::is_function(txt)){
      cli::cli_abort("Invalid argument {.arg txt}. This must be a named or
                     anonymous function.")
    }

    # apply the transformation to the matched lines
    new_txt <- purrr::map(lines[matched_lines], txt)

    if(any(
      purrr::map_chr(new_txt, class) != "character",
      sum(lengths(new_txt)) != length(matched_lines)
    )){
      cls_out <- as.character(unique(purrr::map(new_txt, class)))

      cli::cli_abort(c(
        "{.arg txt} must return a single character string for a single input.",
        x = "If {.arg txt} is a function, it is used to transform a character to another character.",
        i = "The function provided output(s) in the following class{?es}: {cls_out}.",
        i = "The function provided {sum(lengths(new_txt))} output{?s} for {length(matched_lines)} matched line{?s}."
      ))
    }

    new_txt <- as.character(new_txt)
  }

  # replace the matched lines if append = FALSE
  if(!append){
    lines[matched_lines] <- new_txt
  }
  # otherwise, insert the new_txt values after each matched line
  else{
    lines <- R.utils::insert(x = lines,
                             ats = matched_lines + 1,
                             values = new_txt,
                             useNames = FALSE)
  }

  writeLines(lines, con = file)
  return(invisible(TRUE))
}

