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
    ASSERT_NE(result.get(), nullptr) << "unexpected parse failure for " << input;

    StreamString str;
    str << result;

    EXPECT_STREQ(str.GetData(), expected) << "incorrect parse for " << input;
  }
}

TEST(RustParseTest, Literals) {
  TestParse("1", "1");

  TestParse("[1,2,3]", "[1, 2, 3]");
  TestParse("[72*3; 8]", "[(72 * 3); 8]");

  TestParse("()", "()");
  TestParse("(1,)", "(1, )");
  TestParse("(1,2,3)", "(1, 2, 3, )");
}

TEST(RustParseTest, Simple) {
  TestParse("1 + 2", "(1 + 2)");
  TestParse("1+2*3", "(1 + (2 * 3))");
  TestParse("(1+2)*3", "((1 + 2) * 3)");
  TestParse("1++2", "(1 + + (2))");
  TestParse("1--2", "(1 - - (2))");
  TestParse("1-+-2", "(1 - + (- (2)))");
  TestParse("[1,2,3][5]", "([1, 2, 3] @ 5)");
}

// TEST(RustParseTest, Members) {
//   TestParse("something.57", "something.57");
//   TestParse("something.field", "something.field");
// }

TEST(RustParseTest, Calls) {
  // TestParse("func()", "func ()");
  // TestParse("func(1,2,'b')", "func (1, 2, 'b')");
  // TestParse("s.f(7)", "s.f (7)");
  TestParse("23.mumble(8)", "23.mumble (8)");
}
