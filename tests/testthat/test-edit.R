test_that("Adding parents works", {
  x = nuclearPed(1)
  y = addPar(x, "1")

  expect_equal(length(labels(y)), 5)
  expect_equal(parents(y, "3"), parents(x, "3"))
  expect_length(parents(y, "1"), 2)
})

test_that("Twin status cycles through MZ, DZ, unknown and none", {
  x = nuclearPed(2, sex = c(1, 1))
  t1 = updateTwins(x, NULL, c("3", "4"))
  t2 = updateTwins(x, t1, c("3", "4"))
  t3 = updateTwins(x, t2, c("3", "4"))

  expect_equal(c(t1$code, t2$code, t3$code), 1:3)
  expect_null(updateTwins(x, t3, c("3", "4")))
})

test_that("Sex changes respect pedigree and MZ twin constraints", {
  x = nuclearPed(2, sex = c(1, 1))
  twins = updateTwins(x, NULL, c("3", "4"))

  expect_error(changeSex(x, "3", 2, twins), "MZ twin")
  expect_error(changeSex(x, "1", 0), "without children")
  expect_equal(getSex(changeSex(x, "3", 0), "3"), 0)
})

test_that("Removing a person also removes their decorations", {
  x = nuclearPed(2)
  dat = list(ped = x, hidelabs = "3", twins = data.frame(id1 = "3", id2 = "4", code = 1),
             miscarriage = NULL, hatched = "3", carrier = "4", dashed = NULL,
             deceased = NULL, proband = "4", fill = c(`3` = 1, `4` = 2),
             textAnnot = list(top = c(`3` = "A", `4` = "B")))
  y = removeSel(dat, "3", "descendants")

  expect_setequal(labels(y$ped), c("1", "2", "4"))
  expect_length(y$hidelabs, 0)
  expect_equal(nrow(y$twins), 0)
  expect_equal(y$fill, c(`4` = 2))
  expect_equal(y$textAnnot$top, c(`4` = "B"))
})
