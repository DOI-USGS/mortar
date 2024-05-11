#' Edit a text file programmatically
#'
#' Insert or transform one or more lines in a file specified either by line
#' number or a boolean function.
#'
#' @param file chr, path to a file
#' @param txt chr or chr fun, either a single character string to insert into
#'   the file or a function that transforms a character string. Note that the
#'   first argument of the function must accept a single string and it must
#'   return a single string. The function will be applied independently to each
#'   line in the file that matches the match argument. Supports anonymous
#'   functions.
#' @param match num vector or lgl fun, either a numeric vector indicating line
#'   indices in the file or a function that returns a logical. Note that the
#'   first argument of the function must accept a single string and it must
#'   return a single logical. The function will be applied independently to each
#'   line in the file. Supports anonymous functions.
#' @param append lgl, if TRUE then the text will be added as a new line after
#'   the line(s) specified in the match argument. If FALSE, then the text will
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
#' # string and return a single boolean. You can use it with the match argument
#' # for some interesting interactions. Here, we change every line containing
#' # the word "is" to uppercase
#' file_edit(file = tmpfile, txt = toupper, match = ~ stringr::str_detect(.x, " is "), append = FALSE)
#' cat(readLines(tmpfile),sep = "\n")
#'
#' # strings containing carriage returns are preserved. Here, we use the readLines
#' # and length functions to ensure we append the string to the end of the file
#' multiline <-
#' "
#' Here is a string
#' that takes up multiple lines"
#'
#' file_edit(file = tmpfile, txt = multiline, match = length(readLines(tmpfile)), append = TRUE)
#' cat(readLines(tmpfile),sep = "\n")

file_edit <- function(file, txt, match, append = TRUE){

  if(!is.character(file) | length(file) > 1){
    cli::cli_abort("Argument {.arg file} must be a single string, not a {.cls {class(file)}} of length {length(file)}.")
  }
  else if(!file.exists(file)){
    cli::cli_abort("I couldn't find a file at {.file {file}}.")
  }


  # txt must be character or function
  # match must be numeric vector or function

  lines <- readLines(file)

  # either match is a numeric vector
  if(all(is.numeric(match))){
    matched_lines <- match
  }
  # or its a function that returns a boolean
  else{
    match <- rlang::as_function(match)

    if(!rlang::is_function(match)){
      cli::cli_abort("Invalid argument {.arg match}. This must be a named or anonymous function.")
    }

    # determine lines for which match returns TRUE
    matched_lines <- purrr::map_lgl(lines, match)

    if(!all(is.logical(matched_lines)) | length(matched_lines) != length(lines)){
      cli::cli_abort("Invalid argument {.arg match}. This function must return a single logical value.")
    }

    # we just need the indices of the lines
    matched_lines <- which(matched_lines)
  }

  if(length(matched_lines) == 0){
    cli::cli_inform(c("i" = "No lines in {.file {file}} match the {.arg match} argument."))
    return(invisible(FALSE))
  }

  # either txt is a character string
  if(all(is.character(txt))){
    new_txt <- txt
  }
  # or it's a function that returns another character string
  else{
    txt <- rlang::as_function(txt)

    if(!rlang::is_function(txt)){
      cli::cli_abort("Invalid argument {.arg txt}. This must be a named or anonymous function.")
    }

    # apply the transformation to the matched lines
    new_txt <- purrr::map_chr(lines[matched_lines], txt)

    if(!all(is.character(new_txt)) | length(new_txt) != length(matched_lines)){
      cli::cli_abort("Invalid argument {.arg new_txt}. This function must return a single character string.")
    }
  }

  # replace the matched lines if append = FALSE
  if(!append){
    lines[matched_lines] <- new_txt
  }
  # otherwise, insert the new_txt values after each matched line
  else{
    lines <-
      R.utils::insert(x = lines,
                      ats = matched_lines + 1,
                      values = new_txt,
                      useNames = FALSE)
  }

  writeLines(lines, con = file)
  return(invisible(TRUE))
}

