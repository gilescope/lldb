//===-- RustLex.h -------------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_RustLex_h
#define liblldb_RustLex_h

#include <memory>

#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"

#include "llvm/ADT/StringRef.h"

namespace lldb_private {

namespace rust {

// Note that single-character tokens are represented by the character
// itself.
enum TokenKind {
  STRING = 128,
  BYTESTRING,
  CHAR,
  BYTE,
  FLOAT,
  INTEGER,
  AS,
  TRUE,
  FALSE,
  SUPER,
  SELF,
  MUT,
  CONST,
  FN,
  SIZEOF,
  DOTDOT,
  OROR,
  ANDAND,
  EQEQ,
  NOTEQ,
  LTEQ,
  GTEQ,
  LSH,
  RSH,
  PLUS_EQ,
  MINUS_EQ,
  SLASH_EQ,
  STAR_EQ,
  PERCENT_EQ,
  RSH_EQ,
  LSH_EQ,
  AND_EQ,
  OR_EQ,
  XOR_EQ,
  COLONCOLON,
  ARROW,
  IDENTIFIER,
  INVALID,
  THATSALLFOLKS
};

struct Token {
  int kind;

  uint64_t uinteger;
  std::string str;

  explicit Token(int kind_)
    : kind(kind_)
  {
  }

  Token(int kind_, uint64_t val_)
    : kind(kind_),
      uinteger(val_)
  {
  }

  Token(int kind_, std::string &&str_)
    : kind(kind_),
      uinteger(0),
      str(std::move(str_))
  {
  }
};

class Lexer {
public:

  explicit Lexer(llvm::StringRef ref)
    : m_iter(ref.begin()),
      m_end(ref.end())
  {
  }

  Token Next();

private:

  Token MaybeByteLiteral();
  Token MaybeRawString(bool is_byte = false);
  Token String(bool is_byte = false);
  Token Character(bool is_byte = false);
  Token Operator();
  Token Number();
  Token Identifier();
  bool AppendEscape(std::string *result, bool is_byte);
  bool ParseHex(uint64_t *result, int min_digits, int max_digits);
  bool ParseEscape(uint64_t *result, bool is_byte);
  bool Lookup(const ::llvm::StringRef &str, int *result);

  llvm::StringRef::iterator m_iter;
  llvm::StringRef::iterator m_end;

  size_t Remaining() const { return m_end - m_iter; }

  static llvm::StringMap<TokenKind> *Keywords();

  static llvm::StringMap<TokenKind> *m_keywords;
};

} // namespace rust

} // namespace lldb_private

#endif // liblldb_RustLex_h
