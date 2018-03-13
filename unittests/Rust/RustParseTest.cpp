//===-- RustParseTest.cpp ------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "gtest/gtest.h"
#include <cstdarg>
#include "Plugins/ExpressionParser/Rust/RustParse.h"
#include "lldb/Utility/Status.h"
#include "lldb/Utility/StreamString.h"

using namespace lldb_private;
using namespace lldb_private::rust;

static void
TestParse(const char *input, const char *expected) {
  Parser parser(nullptr, input);

  Status error;
  RustExpressionUP result = parser.Expr(error);

  if (expected == nullptr) {
    EXPECT_EQ(result.get(), nullptr) << "expected failure for " << input;
    EXPECT_NE(error.AsCString(nullptr), nullptr) << "expected error message for " << input;
  } else {
    StreamString str;
    str << result;

    EXPECT_STREQ(str.GetData(), expected) << "incorrect parse for " << input;
  }
}

TEST(RustParseTest, Literals) {
  TestParse("1", "1");
}
