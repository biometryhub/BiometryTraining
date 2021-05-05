expect_file_2 <- function(fn, args, pattern, dir = ".", missing = F) {
  x <- do.call(fn, args)
  if (!missing) {
    expect_true(length(list.files(path = dir, pattern = pattern)) > 0)
  }
  else {
    expect_true(length(list.files(path = dir, pattern = pattern)) == 0)
  }
}
