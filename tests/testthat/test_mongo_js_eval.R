context("test mongo js query evaluation by V8")

test_that("jsToMongoJson should translate js to mongo json", {
  res <- jsToMongoJson('{a:{$gt:new ISODate("2018-05-05T20:23:24.497Z")}}')
  expect_equal("{\"a\":{\"$gt\":{\"$date\":\"2018-05-05T20:23:24.497Z\"}}}", res)
  res <- jsToMongoJson('{a:{$gt:ObjectId("5af74ddef45e47233abba0f6").getTimestamp()}}')
  expect_equal("{\"a\":{\"$gt\":{\"$date\":\"2018-05-12T20:26:06.000Z\"}}}", res)
  res <- jsToMongoJson('{a:{$gt:ObjectId("5af74ddef45e47233abba0f6").toString()}}')
  expect_equal("{\"a\":{\"$gt\":\"5af74ddef45e47233abba0f6\"}}", res)
  res <- jsToMongoJson('{a:{$gt:ObjectId("5af74ddef45e47233abba0f6").valueOf()}}')
  expect_equal("{\"a\":{\"$gt\":{\"$oid\":\"5af74ddef45e47233abba0f6\"}}}", res)
  res <- jsToMongoJson('{a:{$gt:ObjectId("5af74ddef45e47233abba0f6")}}')
  expect_equal("{\"a\":{\"$gt\":{\"$oid\":\"5af74ddef45e47233abba0f6\"}}}", res)
  expect_error(res <- jsToMongoJson('{a:{$gt:new ISODate("2018-05-05T20:23:24.497Z")}'))


  # tests migrated from the js code in Exploratory Desktop.
  jsQueryStr <- '{a:"abc", b:[/.*/i], c:{$regex:/abc/, $options: "i"}, d:{$regex:"def", $options: "i"}, e:undefined, f:MinKey, g:MaxKey, h:NumberLong("123"), i:NumberLong(123), j:ObjectId("58380c7f2c5914d9465ac54b"), k:DBRef("mydate","Date"), l:Timestamp(1234,5), m:new Date(0), n:BinData(2,"4236897868432beaf"), o:new BinData(2,"4236897868432beaf"), p:ISODate("2016-11-25T07:47:09.632Z"), q:new ISODate("2016-11-25T07:47:09.632Z"), r:NumberInt(123), s:UUID("0123456789abcdeffedcba9876543210")}'
  jsonQueryStr <- jsToMongoJson(jsQueryStr)
  expect_equal(jsonQueryStr,'{"a":"abc","b":[{"$regex":".*","$options":"i"}],"c":{"$regex":"abc","$options":"i"},"d":{"$regex":"def","$options":"i"},"e":{"$undefined":true},"f":{"$minKey":1},"g":{"$maxKey":1},"h":{"$numberLong":"123"},"i":{"$numberLong":"123"},"j":{"$oid":"58380c7f2c5914d9465ac54b"},"k":{"$ref":"mydate","$id":"Date"},"l":{"$timestamp":{"t":1234,"i":5}},"m":{"$date":"1970-01-01T00:00:00.000Z"},"n":{"$binary":"4236897868432beaf","$type":"2"},"o":{"$binary":"4236897868432beaf","$type":"2"},"p":{"$date":"2016-11-25T07:47:09.632Z"},"q":{"$date":"2016-11-25T07:47:09.632Z"},"r":{"$numberLong":"123"},"s":{"$binary":"ASNFZ4mrze/+3LqYdlQyEA==","$type":"3"}}')
  # try converting the resulting mongo json again, and it should be the same string.
  jsonQueryStr <- jsToMongoJson(jsonQueryStr);
  expect_equal(jsonQueryStr, '{"a":"abc","b":[{"$regex":".*","$options":"i"}],"c":{"$regex":"abc","$options":"i"},"d":{"$regex":"def","$options":"i"},"e":{"$undefined":true},"f":{"$minKey":1},"g":{"$maxKey":1},"h":{"$numberLong":"123"},"i":{"$numberLong":"123"},"j":{"$oid":"58380c7f2c5914d9465ac54b"},"k":{"$ref":"mydate","$id":"Date"},"l":{"$timestamp":{"t":1234,"i":5}},"m":{"$date":"1970-01-01T00:00:00.000Z"},"n":{"$binary":"4236897868432beaf","$type":"2"},"o":{"$binary":"4236897868432beaf","$type":"2"},"p":{"$date":"2016-11-25T07:47:09.632Z"},"q":{"$date":"2016-11-25T07:47:09.632Z"},"r":{"$numberLong":"123"},"s":{"$binary":"ASNFZ4mrze/+3LqYdlQyEA==","$type":"3"}}')
  # try ObjectId methods.
  jsQueryStr <- '[ObjectId("58380c7f2c5914d9465ac54b").getTimestamp(),ObjectId("58380c7f2c5914d9465ac54b").toString(),ObjectId("58380c7f2c5914d9465ac54b").valueOf(),ObjectId("58380c7f2c5914d9465ac54b").str]'
  jsonQueryStr <- jsToMongoJson(jsQueryStr)
  expect_equal(jsonQueryStr, '[{"$date":"2016-11-25T10:03:43.000Z"},"58380c7f2c5914d9465ac54b",{"$oid":"58380c7f2c5914d9465ac54b"},"58380c7f2c5914d9465ac54b"]')

  # test for ObjectId query
  jsQueryStr <- '{_id:ObjectId("5757511d66172e8b80467ebc")}'
  jsonQueryStr <- jsToMongoJson(jsQueryStr)
  expect_equal(jsonQueryStr, '{"_id":{"$oid":"5757511d66172e8b80467ebc"}}')

  # test for rergex query without option
  jsQueryStr <- '{a:/abc/}'
  jsonQueryStr <- jsToMongoJson(jsQueryStr)
  expect_equal(jsonQueryStr, '{"a":{"$regex":"abc","$options":""}}')
})
