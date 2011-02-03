/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: gperf --language=ANSI-C --struct-type --slot-name=name --hash-fn-name=java_hash --lookup-fn-name=java_lookup  */
/* Computed positions: -k'1-2' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif


//#include "strmake.h"
#define START_VARIABLE	1001
#define START_WORD	2001
#define START_SHARP	3001
#define START_YACC	4001
#define IS_RESERVED_WORD(a)	((a) >= START_WORD)
#define IS_RESERVED_VARIABLE(a)	((a) >= START_VARIABLE && (a) < START_WORD)
#define IS_RESERVED_SHARP(a)	((a) >= START_SHARP && (a) < START_YACC)
#define IS_RESERVED_YACC(a)	((a) >= START_YACC)

#define ABSTRACT_	2001
#define BOOLEAN_	2002
#define BREAK_	2003
#define BYTE_	2004
#define CASE_	2005
#define CATCH_	2006
#define CHAR_	2007
#define CLASS_	2008
#define CONST_	2009
#define CONTINUE_	2010
#define DEFAULT_	2011
#define DO_	2012
#define DOUBLE_	2013
#define ELSE_	2014
#define ENUM_	2015
#define EXTENDS_	2016
#define FALSE_	2017
#define FINAL_	2018
#define FINALLY_	2019
#define FLOAT_	2020
#define FOR_	2021
#define GOTO_	2022
#define IF_	2023
#define IMPLEMENTS_	2024
#define IMPORT_	2025
#define INSTANCEOF_	2026
#define INT_	2027
#define INTERFACE_	2028
#define LONG_	2029
#define NATIVE_	2030
#define NEW_	2031
#define NULL_	2032
#define PACKAGE_	2033
#define PRIVATE_	2034
#define PROTECTED_	2035
#define PUBLIC_	2036
#define RETURN_	2037
#define SHORT_	2038
#define STATIC_ 2039
#define STRICTFP_	2040
#define SUPER_	2041
#define SWITCH_	2042
#define SYNCHRONIZED_	2043
#define THIS_	2044
#define THROW_	2045
#define THROWS_	2046
#define UNION_	2047
#define TRANSIENT_	2048
#define TRUE_	2049
#define TRY_	2050
#define VOID_	2051
#define VOLATILE_	2052
#define WHILE_	2053
#define WIDEFP_	2054
struct keyword { char *name; int token; };

#define TOTAL_KEYWORDS 54
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 12
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 82
/* maximum key range = 79, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
java_hash (register const char *str, register unsigned int len)
{
  static unsigned char asso_values[] =
    {
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 25, 15, 30,
      50, 25, 20, 35,  0, 10, 83, 83, 40, 50,
      10,  5, 10, 83,  5,  5,  0,  5, 55, 40,
      15, 35, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83, 83, 83, 83, 83,
      83, 83, 83, 83, 83, 83
    };
  return len + asso_values[(unsigned char)str[1]] + asso_values[(unsigned char)str[0]];
}

#ifdef __GNUC__
__inline
#ifdef __GNUC_STDC_INLINE__
__attribute__ ((__gnu_inline__))
#endif
#endif
struct keyword *
java_lookup (register const char *str, register unsigned int len)
{
  static struct keyword wordlist[] =
    {
      {""}, {""}, {""}, {""},
      {"this", THIS_},
      {"throw", THROW_},
      {"throws", THROWS_},
      {""},
      {"try", TRY_},
      {"true", TRUE_},
      {"short", SHORT_},
      {"static", STATIC_},
      {""},
      {"strictfp", STRICTFP_},
      {"transient", TRANSIENT_},
      {"super", SUPER_},
      {""}, {""}, {""},
      {"null", NULL_},
      {"union", UNION_},
      {"public", PUBLIC_},
      {"private", PRIVATE_},
      {"int", INT_},
      {"protected", PROTECTED_},
      {"break", BREAK_},
      {""},
      {"boolean", BOOLEAN_},
      {"for", FOR_},
      {"interface", INTERFACE_},
      {"instanceof", INSTANCEOF_},
      {""},
      {"if", IF_},
      {""},
      {"char", CHAR_},
      {"final", FINAL_},
      {"return", RETURN_},
      {"finally", FINALLY_},
      {"new", NEW_},
      {"enum", ENUM_},
      {"const", CONST_},
      {"native", NATIVE_},
      {"package", PACKAGE_},
      {"continue", CONTINUE_},
      {"goto", GOTO_},
      {"while", WHILE_},
      {""},
      {"extends", EXTENDS_},
      {"abstract", ABSTRACT_},
      {"long", LONG_},
      {"false", FALSE_},
      {"switch", SWITCH_},
      {"synchronized", SYNCHRONIZED_},
      {""},
      {"byte", BYTE_},
      {""},
      {"widefp", WIDEFP_},
      {"do", DO_},
      {""},
      {"case", CASE_},
      {"catch", CATCH_},
      {"double", DOUBLE_},
      {""}, {""},
      {"void", VOID_},
      {"float", FLOAT_},
      {"import", IMPORT_},
      {""},
      {"volatile", VOLATILE_},
      {"else", ELSE_},
      {"implements", IMPLEMENTS_},
      {""}, {""}, {""}, {""},
      {"class", CLASS_},
      {""}, {""}, {""}, {""}, {""}, {""},
      {"default", DEFAULT_}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = java_hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1))
            return &wordlist[key];
        }
    }
  return 0;
}

int
getJavaReservedWordIndex(const char *str, int len)
{
	struct keyword *keyword;

	keyword = java_lookup(str, len);
	return (keyword && IS_RESERVED_WORD(keyword->token)) ? keyword->token : 0;
}
