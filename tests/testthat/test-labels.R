test_that("Relabelling also updates styles, twins and annotations", {
  x = nuclearPed(2)
  p = list(ped = x, hidelabs = "3", miscarriage = "4",
           twins = data.frame(id1 = "3", id2 = "4", code = 1))
  s = list(fill = c(`3` = 2), hatched = "4", proband = "3")
  y = updateLabelsData(p, s, list(top = c(`3` = "note")),
                       c("Father", "Mother", "A", "B"))

  expect_equal(labels(y$ped), c("Father", "Mother", "A", "B"))
  expect_equal(unname(parents(y$ped, "A")), c("Father", "Mother"))
  expect_equal(unname(y$hidelabs), "A")
  expect_equal(unname(y$miscarriage), "B")
  expect_equal(c(y$twins$id1, y$twins$id2), c("A", "B"))
  expect_equal(names(y$fill), "A")
  expect_equal(names(y$textAnnot$top), "A")
  expect_equal(unname(y$hatched), "B")
})

test_that("Invalid labels are rejected", {
  expect_error(checkNewLabs(c("A", "A")), "Duplicated")
  expect_error(checkNewLabs(c("A", "0")), "cannot be used")
  expect_error(checkNewLabs(c("A", "")), "Empty labels")
})

test_that("Colour changes replace existing values", {
  x = c(`3` = 1, `4` = 2)
  y = modifyVec(x, c(`3` = 4, `5` = 1))

  expect_equal(y[c("3", "4", "5")], c(`3` = 4, `4` = 2, `5` = 1))
})
