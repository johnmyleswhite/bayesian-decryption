library('RUnit')

test.suite.cipher <- defineTestSuite("cipher",
                                     dirs = file.path("tests"),
                                     testFileRegexp = '^runit.+\\.R',
                                     testFuncRegexp = "^test.+",
                                     rngKind = "Marsaglia-Multicarry",
                                     rngNormalKind = "Kinderman-Ramage")

test.result <- runTestSuite(test.suite.cipher)

printTextProtocol(test.result)
