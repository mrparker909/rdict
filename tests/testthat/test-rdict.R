test_that("rdict", {
  d = Dictionary()
  d = Add(d, "a", 1)
  
  expect_error({
    GetKey(d, "b") # key does not exist
  })
  expect_equal(ContainsKey(d,"a"), TRUE) # contains key "a"
  expect_equal(ContainsKey(d,"b"), FALSE)  # does not contain key "b"
  expect_equal(GetValue(d,"a"), 1)  # key "a" has value 1
  expect_error(Add(d, "a", 1)) # key "a" already exists
})
