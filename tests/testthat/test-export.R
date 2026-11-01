test_that("Pedigree export includes family and affection codes", {
  x = nuclearPed(2)
  y = preparePedFile(x, famid = TRUE, aff = "4")

  expect_equal(y$famid, rep("1", 4))
  expect_equal(y$aff, c(1, 1, 1, 2))
  expect_equal(y[, c("id", "fid", "mid", "sex")], as.data.frame(x))
})

test_that("Generated R code retains plot settings and styling", {
  x = nuclearPed(2)
  code = generateCode(x, labels(x), styles = list(hatched = "3", fill = c(`4` = 2)),
                      cex = 1.3, width = 400, height = 400)

  expect_match(code, 'hatched = "3"', fixed = TRUE)
  expect_match(code, 'fill = c("4" = 2)', fixed = TRUE)
  expect_match(code, "cex = 1.3", fixed = TRUE)
})

test_that("Generated R code includes twins and text annotations", {
  x = nuclearPed(2)
  twins = data.frame(id1 = "3", id2 = "4", code = 1)
  code = generateCode(x, labels(x), twins = twins,
                      textAnnot = list(top = c(`3` = "note")),
                      width = 400, height = 400)

  expect_match(code, 'twins = data.frame(id1 = "3", id2 = "4", code = 1)', fixed = TRUE)
  expect_match(code, 'textAnnot = list(', fixed = TRUE)
  expect_match(code, '"3" = "note"', fixed = TRUE)
})
