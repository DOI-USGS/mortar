# Why so many cases?
# - file_edit() accepts both numeric indices and matching functions; we exercise
#   both styles.
# - We verify correct behavior for appending vs. replacing, multiple matches,
#   and functional transformations.
# - We also deliberately probe edge cases (e.g., zero/negative indices,
#   mismatched output lengths) to document and protect current behavior.
testthat::test_that("file_edit appends new text after functional match", {
  tmp <- withr::local_tempfile()

  writeLines(c("alpha","beta","gamma", "delta"), tmp)

  testthat::expect_invisible(
    file_edit(
      file = tmp,
      txt  = c("fruit", "veggies"),
      match = ~ grepl("t", .x),
      append = TRUE
    )
  )

  #beta -> fruit inserted after, delta -> veggies inserted after
  testthat::expect_equal(
    readLines(tmp),
    c("alpha", "beta", "fruit", "gamma", "delta", "veggies")
  )

  # Append a literal line after any line containing 'i'
  testthat::expect_invisible(
    file_edit(
      file = tmp,
      txt  = "grains",
      match = ~ grepl("i", .x),
      append = TRUE
    )
  )

  # fruit -> grains inserted after, veggies -> grains inserted after
  testthat::expect_equal(
    readLines(tmp),
    c("alpha", "beta", "fruit", "grains", "gamma", "delta", "veggies", "grains")
  )

  # Fail when there is a mismatch between the number of lines and matches
  testthat::expect_error(
    file_edit(
      file = tmp,
      txt  = c("sweets", "fats"),
      match = ~ grepl("mm", .x),
      append = TRUE
    ),
    "`match` resulted in 1 matched line and `txt` has a length of 2."
  )
})

testthat::test_that(
  "file_edit replaces lines with character vector of equal length", {
    tmp <- withr::local_tempfile()

    writeLines(c("x", "y", "z"), tmp)

    # Replace lines 1 and 3 with different values
    testthat::expect_invisible(
      file_edit(
        file = tmp,
        txt   = c("X","Z"),
        match = c(1,3),
        append = FALSE
      )
    )
    testthat::expect_equal(readLines(tmp), c("X","y","Z"))

    # Fail with txt/match length mismatch
    testthat::expect_error(
      file_edit(
        file = tmp,
        txt   = c("X","Z"),
        match = c(1, 2, 3),
        append = FALSE
      ),
      "`match` resulted in 3 matched lines and `txt` has a length of 2."
    )
  })

testthat::test_that(
  "file_edit with zero/negative indices documents current behaviour", {
    tmp <- withr::local_tempfile()

    writeLines(c("line1", "line2", "line3"), tmp)

    # The implementation treats numeric 'match' as literal indices. If index is
    # 0, it will insert the text before the first line.
    testthat::expect_invisible(
      file_edit(file = tmp, txt = "ADDED_AFTER_LINE0", match = 0, append = TRUE)
    )

    testthat::expect_equal(
      readLines(tmp),
      c("ADDED_AFTER_LINE0", "line1", "line2", "line3")
    )

    # Negative index values are not valid.
    testthat::expect_error(
      file_edit(file = tmp, txt = "Negative index", match = -1, append = TRUE),
      "`match` cannot contain negative numbers."
    )
  }
)

testthat::test_that(
  "file_edit errors when match function returns wrong type/length", {
    tmp <- withr::local_tempfile()

    writeLines(c("a","b","c"), tmp)

    # match function must return a single logical per line; returning character
    # should raise an error via cli_abort
    testthat::expect_error(
      file_edit(tmp, txt = "x", match = function(.) "not logical", append = TRUE),
      "The function provided output\\(s) in the following class: character"
    )

    # returning a logical vector of wrong length per line is also invalid
    testthat::expect_error(
      file_edit(
        tmp,
        txt = "x",
        match = function(.) c(TRUE, FALSE),
        append = TRUE
      ),
      "The function provided 6 outputs for 3 lines."
    )
  }
)

testthat::test_that(
  "file_edit errors when txt function returns wrong type/length", {
    tmp <- withr::local_tempfile()

    writeLines(c("one","two","three"), tmp)

    # txt function must return a single string per matched line
    testthat::expect_error(
      file_edit(
        tmp,
        txt   = function(.) c("a","b"),
        match = ~ TRUE,
        append = FALSE
      ),
      "The function provided 6 outputs for 3 matched lines."
    )

    # txt function that returns non-character should error
    testthat::expect_error(
      file_edit(
        tmp,
        txt   = function(.) 123,
        match = ~ TRUE,
        append = FALSE
      ),
      "The function provided output\\(s) in the following class: numeric."
    )
  }
)

testthat::test_that(
  "file_edit handles multiline strings and replace mode", {
    tmp <- withr::local_tempfile()

    writeLines(c("hello","world"), tmp)

    multi <- "multi\nline"
    testthat::expect_invisible(
      file_edit(tmp, txt = multi, match = 1, append = FALSE)
    )

    testthat::expect_equal(readLines(tmp), c("multi", "line", "world"))
  })
