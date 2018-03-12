add_lldb_unittest(RustTests
  RustLexTest.cpp

  LINK_LIBS
    lldbCore
    lldbPluginExpressionParserRust
  )
