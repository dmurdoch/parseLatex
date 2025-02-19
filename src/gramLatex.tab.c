/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 4 "parser/gramLatex.y"

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2024  The R Core Team
 *  Copyright (C) 2010--2025  Duncan Murdoch
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#ifdef PARSELATEX_WIN32
#include <wchar.h>
#endif
#include <stdbool.h>
#include <unicode/uchar.h>
#include <unicode/utf8.h>
#include <Rinternals.h>
#include <R_ext/RS.h>           /* for R_chk_* allocation */
#include <ctype.h>
#include <R_ext/Print.h>
#include <R_ext/Rdynload.h>
#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tools", String)
#else
#define _(String) (String)
#endif

/* Some definitions that are internals in the R version */
#define R_EOF -1
typedef enum {
  PARSE_NULL,
  PARSE_OK,
  PARSE_INCOMPLETE,
  PARSE_ERROR,
  PARSE_EOF
} ParseStatus;

static int   ParseErrorLine = 0; /* Line where parse error occurred */
static int   ParseErrorCol;      /* Column of start of token where parse error occurred */

#define PARSE_ERROR_SIZE 512

static char  ParseErrorMsg[PARSE_ERROR_SIZE];

static int parseError(void);

/* end of internal replacements */

/* bison creates a non-static symbol yylloc (and other) in both gramLatex.o
   and gramRd.o, so remap */

#define yylloc yyllocL
#undef yynerrs /* from Defn.h */
#define yynerrs yynerrsL
#undef yychar /* from Defn.h */
#define yychar yycharL
#undef yylval /* from Defn.h */
#define yylval yylvalL

#define DEBUGVALS 0    /* 1 causes detailed internal state output to R console */
#define DEBUGMODE 0    /* 1 causes Bison output of parse state, to stdout or stderr */

#define YYERROR_VERBOSE 1

static void yyerror(const char *);
static int yylex(void);
static int yyparse(void);

#define yyconst const

typedef struct yyltype
{
  int first_line;
  int first_column;
  int first_byte;

  int last_line;
  int last_column;
  int last_byte;
} yyltype;

# define YYLTYPE yyltype
# define YYLLOC_DEFAULT(Current, Rhs, N)\
do                  \
  if (N)            \
  {                 \
    (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;    \
    (Current).first_column = YYRHSLOC (Rhs, 1).first_column;  \
    (Current).first_byte   = YYRHSLOC (Rhs, 1).first_byte;    \
    (Current).last_line    = YYRHSLOC (Rhs, N).last_line;     \
    (Current).last_column  = YYRHSLOC (Rhs, N).last_column;    \
    (Current).last_byte    = YYRHSLOC (Rhs, N).last_byte;     \
  }\
      else \
  {        \
    (Current).first_line   = (Current).last_line   =    \
      YYRHSLOC (Rhs, 0).last_line;                      \
    (Current).first_column = (Current).last_column =    \
      YYRHSLOC (Rhs, 0).last_column;                    \
    (Current).first_byte   = (Current).last_byte =      \
      YYRHSLOC (Rhs, 0).last_byte;                      \
  }        \
while (0)

/* Useful defines so editors don't get confused ... */

#define LBRACE  '{'
#define RBRACE  '}'

/* Functions used in the parsing process */

static void     GrowList(SEXP, SEXP);
static SEXP     AppendList(SEXP, SEXP);
static int      KeywordLookup(const char *);
static SEXP     NewList(void);
static SEXP     makeSrcref(YYLTYPE *);
static YYLTYPE  noSrcref = {0,0,0,0,0,0};
static UChar32  xxgetc(void);
static int      xxungetc(int);
static void     xxincomplete(SEXP, YYLTYPE *);
static void     xxincompleteBegin(SEXP, YYLTYPE *);

/* Internal lexer / parser state variables */


static char const yyunknown[] = "unknown macro"; /* our message, not bison's */

typedef struct ParseState ParseState;
struct ParseState {
  int  xxlineno, xxbyteno, xxcolno;
  int  xxDebugTokens;  /* non-zero causes debug output to R console */
  SEXP  Value;
  int   xxinitvalue;
  SEXP  xxInVerbEnv;      /* Are we currently in a verbatim
                             environment? If so, this is
                             the string to end it. If not,
                             this is NULL */
  SEXP  xxVerbatimList;   /* A STRSXP containing all the
                             verbatim environment names */
  SEXP  xxKwdList;        /* A STRSXP containing all the
                             verbatim and definition command names */
  SEXP  xxKwdType;        /* An INTSXP with 1=VERB, 2=DEFCMD, 3=DEFENV */
  SEXP  xxCodepoints;     /* A vector of codepoints with catcodes given */
  SEXP  xxCatcodes;       /* Corresponding catcodes */
  int   xxGetArgs;        /* Collecting args to macro */
  int   xxIgnoreKeywords; /* Ignore keywords while getting args */
  int   xxBraceDepth;     /* Brace depth important while
                             collecting args */
  int   xxBracketDepth;   /* So is bracket depth */
  int   xxMathMode;       /* In single $ mode */

  SEXP  mset; /* precious mset for protecting parser semantic values */
  int   recover;          /* Whether to attempt to recover */
  ParseState *prevState;
};

static Rboolean busy = FALSE;
static ParseState parseState;

#define PRESERVE_SV(x) R_PreserveInMSet((x), parseState.mset)
#define RELEASE_SV(x)  R_ReleaseFromMSet((x), parseState.mset)

/* Routines used to build the parse tree */

static SEXP  xxnewlist(SEXP);
static SEXP  xxlist(SEXP, SEXP);
static void  xxsavevalue(SEXP, YYLTYPE *);
static SEXP  xxtag(SEXP, int, YYLTYPE *);
static void  xxgettext(char *, size_t, SEXP);
static SEXP  xxenv(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP  xxnewdef(SEXP, SEXP, YYLTYPE *);
static SEXP  xxmath(SEXP, YYLTYPE *, Rboolean);
static SEXP  xxenterMathMode(void);
static SEXP  xxblock(SEXP, YYLTYPE *);
static SEXP  xxerrblock(SEXP);
static void  xxSetInVerbEnv(SEXP);
static SEXP  xxpushMode(int, int);
static void  xxpopMode(SEXP);
static void  xxArg(SEXP);

static SEXP  xxfakeStart(const char *, SEXP);
static SEXP  xxfakeBegin(SEXP, SEXP);
static SEXP  xxwrapError(SEXP);

static int magicComment(const uint8_t *, int);
static SEXP addString(const uint8_t *, size_t, SEXP);
static SEXP addInteger(int value, SEXP);

#define END_OF_ARGS_CHAR 0xFFFE /* not a legal character */

static int  mkMarkup(int);
static int  mkText(int);
static int  mkComment(int);
static int  mkSpecial(int, int);
static int  mkVerb(int);
static int  mkVerb2(const uint8_t *, int);
static int  mkVerbEnv(void);
static int  mkDollar(int);

static SEXP LatexTagSymbol = NULL;

#define YYSTYPE    SEXP


#line 295 "gramLatex.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    END_OF_INPUT = 258,            /* END_OF_INPUT  */
    ERROR = 259,                   /* ERROR  */
    MACRO = 260,                   /* MACRO  */
    TEXT = 261,                    /* TEXT  */
    COMMENT = 262,                 /* COMMENT  */
    BEGIN = 263,                   /* BEGIN  */
    END = 264,                     /* END  */
    VERB = 265,                    /* VERB  */
    VERB2 = 266,                   /* VERB2  */
    NEWENV = 267,                  /* NEWENV  */
    NEWCMD = 268,                  /* NEWCMD  */
    END_OF_ARGS = 269,             /* END_OF_ARGS  */
    TWO_DOLLARS = 270,             /* TWO_DOLLARS  */
    SPECIAL = 271                  /* SPECIAL  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;

int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_END_OF_INPUT = 3,               /* END_OF_INPUT  */
  YYSYMBOL_ERROR = 4,                      /* ERROR  */
  YYSYMBOL_MACRO = 5,                      /* MACRO  */
  YYSYMBOL_TEXT = 6,                       /* TEXT  */
  YYSYMBOL_COMMENT = 7,                    /* COMMENT  */
  YYSYMBOL_BEGIN = 8,                      /* BEGIN  */
  YYSYMBOL_END = 9,                        /* END  */
  YYSYMBOL_VERB = 10,                      /* VERB  */
  YYSYMBOL_VERB2 = 11,                     /* VERB2  */
  YYSYMBOL_NEWENV = 12,                    /* NEWENV  */
  YYSYMBOL_NEWCMD = 13,                    /* NEWCMD  */
  YYSYMBOL_END_OF_ARGS = 14,               /* END_OF_ARGS  */
  YYSYMBOL_TWO_DOLLARS = 15,               /* TWO_DOLLARS  */
  YYSYMBOL_SPECIAL = 16,                   /* SPECIAL  */
  YYSYMBOL_17_ = 17,                       /* '{'  */
  YYSYMBOL_18_ = 18,                       /* '}'  */
  YYSYMBOL_19_ = 19,                       /* '$'  */
  YYSYMBOL_YYACCEPT = 20,                  /* $accept  */
  YYSYMBOL_Init = 21,                      /* Init  */
  YYSYMBOL_Items = 22,                     /* Items  */
  YYSYMBOL_nonMath = 23,                   /* nonMath  */
  YYSYMBOL_Item = 24,                      /* Item  */
  YYSYMBOL_envname = 25,                   /* envname  */
  YYSYMBOL_begin = 26,                     /* begin  */
  YYSYMBOL_environment = 27,               /* environment  */
  YYSYMBOL_mathstart = 28,                 /* mathstart  */
  YYSYMBOL_math = 29,                      /* math  */
  YYSYMBOL_displaymath = 30,               /* displaymath  */
  YYSYMBOL_block = 31,                     /* block  */
  YYSYMBOL_newdefine = 32,                 /* newdefine  */
  YYSYMBOL_33_1 = 33,                      /* @1  */
  YYSYMBOL_34_2 = 34                       /* @2  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  34
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   258

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  20
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  15
/* YYNRULES -- Number of rules.  */
#define YYNRULES  46
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  70

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   271


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    19,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    17,     2,    18,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   247,   247,   249,   251,   253,   254,   255,   256,   257,
     258,   259,   265,   266,   268,   269,   270,   272,   274,   275,
     276,   277,   278,   280,   281,   282,   283,   285,   289,   292,
     295,   297,   301,   303,   305,   308,   312,   314,   317,   321,
     322,   323,   325,   328,   328,   333,   333
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "END_OF_INPUT",
  "ERROR", "MACRO", "TEXT", "COMMENT", "BEGIN", "END", "VERB", "VERB2",
  "NEWENV", "NEWCMD", "END_OF_ARGS", "TWO_DOLLARS", "SPECIAL", "'{'",
  "'}'", "'$'", "$accept", "Init", "Items", "nonMath", "Item", "envname",
  "begin", "environment", "mathstart", "math", "displaymath", "block",
  "newdefine", "@1", "@2", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-50)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-43)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      43,   -50,   -50,   -50,   -50,   -50,   -13,   -50,   -50,   -50,
     -50,   213,   -50,    60,   -50,     7,    79,   -50,    96,   -50,
     226,   -50,   -50,   -50,   -50,    14,   239,   239,   -50,   200,
     -50,   -50,   -50,   113,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,    -9,   132,   -50,   183,   -50,   -50,    -3,   149,   166,
     -50,   -50,   -50,    38,   -50,    14,    -2,    12,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,     5,    14,   -50,     8,   -50
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     4,     3,    16,    14,    15,     0,    18,    19,    45,
      43,     0,    17,     0,    32,     0,     0,     5,     0,    21,
       0,     6,     7,    20,    22,     0,     0,     0,    37,     0,
      12,    41,    40,     0,     1,    11,     2,     8,     9,    10,
      30,     0,     0,    34,     0,    23,    24,     0,     0,     0,
      38,    36,    13,    11,    39,     0,    11,     0,    35,    33,
      25,    26,    27,    46,    44,     0,     0,    29,     0,    28
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -50,   -50,   109,    19,   -11,   -49,   -50,   -50,   -50,   -14,
      -6,   -50,   -50,   -50,   -50
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,    15,    16,    29,    17,    47,    18,    19,    20,    21,
      22,    23,    24,    27,    26
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      30,   -31,    38,    60,    25,    37,    65,    34,    55,    30,
      39,    60,   -31,    61,    60,    62,   -31,    68,    52,    38,
      45,    61,    37,    67,    61,     0,    69,    39,    38,    66,
      46,    37,     0,    52,    38,    38,    39,    37,    37,    44,
       0,   -42,    39,    39,     1,     0,     2,   -42,     3,     4,
       5,     6,   -42,     7,     8,     9,    10,     0,    11,    12,
      13,    31,    14,     0,     0,     3,     4,     5,     6,     0,
       7,     8,     9,    10,     0,    11,    12,    13,    32,    14,
      35,     0,    36,     0,     3,     4,     5,     6,     0,     7,
       8,     9,    10,     0,    11,    12,    13,    40,    14,     0,
       0,     3,     4,     5,     6,    41,     7,     8,     9,    10,
       0,    11,    12,    13,    53,    14,     0,     0,     3,     4,
       5,     6,    33,     7,     8,     9,    10,    42,    11,    12,
      13,    54,    14,    56,     0,    48,    49,     3,     4,     5,
       6,    57,     7,     8,     9,    10,     0,    11,    12,    13,
      35,    14,     0,     0,     3,     4,     5,     6,     0,     7,
       8,     9,    10,    63,    11,    12,    13,    35,    14,     0,
       0,     3,     4,     5,     6,     0,     7,     8,     9,    10,
      64,    11,    12,    13,    58,    14,     0,     0,     3,     4,
       5,     6,     0,     7,     8,     9,    10,     0,     0,    12,
      13,    50,    59,     0,     0,     3,     4,     5,     6,     0,
       7,     8,     9,    10,    28,    51,    12,    13,     3,     4,
       5,     6,     0,     7,     8,     9,    10,    43,     0,    12,
      13,     3,     4,     5,     6,     0,     7,     8,     9,    10,
       0,     0,    12,    13,     3,     4,     5,     6,     0,     7,
       8,     9,    10,     0,    11,    12,    13,     0,    14
};

static const yytype_int8 yycheck[] =
{
      11,     3,    16,     6,    17,    16,    55,     0,    17,    20,
      16,     6,    14,    16,     6,    18,    18,    66,    29,    33,
       6,    16,    33,    18,    16,    -1,    18,    33,    42,    17,
      16,    42,    -1,    44,    48,    49,    42,    48,    49,    20,
      -1,     3,    48,    49,     1,    -1,     3,     9,     5,     6,
       7,     8,    14,    10,    11,    12,    13,    -1,    15,    16,
      17,     1,    19,    -1,    -1,     5,     6,     7,     8,    -1,
      10,    11,    12,    13,    -1,    15,    16,    17,    18,    19,
       1,    -1,     3,    -1,     5,     6,     7,     8,    -1,    10,
      11,    12,    13,    -1,    15,    16,    17,     1,    19,    -1,
      -1,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      -1,    15,    16,    17,     1,    19,    -1,    -1,     5,     6,
       7,     8,    13,    10,    11,    12,    13,    18,    15,    16,
      17,    18,    19,     1,    -1,    26,    27,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    -1,    15,    16,    17,
       1,    19,    -1,    -1,     5,     6,     7,     8,    -1,    10,
      11,    12,    13,    14,    15,    16,    17,     1,    19,    -1,
      -1,     5,     6,     7,     8,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,     1,    19,    -1,    -1,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,    -1,    -1,    16,
      17,     1,    19,    -1,    -1,     5,     6,     7,     8,    -1,
      10,    11,    12,    13,     1,    15,    16,    17,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,     1,    -1,    16,
      17,     5,     6,     7,     8,    -1,    10,    11,    12,    13,
      -1,    -1,    16,    17,     5,     6,     7,     8,    -1,    10,
      11,    12,    13,    -1,    15,    16,    17,    -1,    19
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,     5,     6,     7,     8,    10,    11,    12,
      13,    15,    16,    17,    19,    21,    22,    24,    26,    27,
      28,    29,    30,    31,    32,    17,    34,    33,     1,    23,
      24,     1,    18,    22,     0,     1,     3,    24,    29,    30,
       1,     9,    22,     1,    23,     6,    16,    25,    22,    22,
       1,    15,    24,     1,    18,    17,     1,     9,     1,    19,
       6,    16,    18,    14,    14,    25,    17,    18,    25,    18
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    20,    21,    21,    21,    22,    22,    22,    22,    22,
      22,    22,    23,    23,    24,    24,    24,    24,    24,    24,
      24,    24,    24,    25,    25,    25,    25,    26,    27,    27,
      27,    27,    28,    29,    29,    29,    30,    30,    30,    31,
      31,    31,    31,    33,    32,    34,    32
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     1,     1,     1,     1,     1,     2,     2,
       2,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     4,     6,     5,
       2,     3,     1,     3,     2,     3,     3,     2,     3,     3,
       2,     2,     3,     0,     4,     0,     4
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  switch (yykind)
    {
    case YYSYMBOL_MACRO: /* MACRO  */
#line 243 "parser/gramLatex.y"
            { RELEASE_SV((*yyvaluep)); }
#line 1553 "gramLatex.tab.c"
        break;

    case YYSYMBOL_TEXT: /* TEXT  */
#line 243 "parser/gramLatex.y"
            { RELEASE_SV((*yyvaluep)); }
#line 1559 "gramLatex.tab.c"
        break;

    case YYSYMBOL_COMMENT: /* COMMENT  */
#line 243 "parser/gramLatex.y"
            { RELEASE_SV((*yyvaluep)); }
#line 1565 "gramLatex.tab.c"
        break;

    case YYSYMBOL_BEGIN: /* BEGIN  */
#line 243 "parser/gramLatex.y"
            { RELEASE_SV((*yyvaluep)); }
#line 1571 "gramLatex.tab.c"
        break;

    case YYSYMBOL_END: /* END  */
#line 243 "parser/gramLatex.y"
            { RELEASE_SV((*yyvaluep)); }
#line 1577 "gramLatex.tab.c"
        break;

      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Init: Items END_OF_INPUT  */
#line 247 "parser/gramLatex.y"
                               { xxsavevalue(yyvsp[-1], &(yyloc));
                                 YYACCEPT; }
#line 1876 "gramLatex.tab.c"
    break;

  case 3: /* Init: END_OF_INPUT  */
#line 249 "parser/gramLatex.y"
                               { xxsavevalue(NULL, &(yyloc));
                                 YYACCEPT; }
#line 1883 "gramLatex.tab.c"
    break;

  case 4: /* Init: error  */
#line 251 "parser/gramLatex.y"
                               { YYABORT; }
#line 1889 "gramLatex.tab.c"
    break;

  case 5: /* Items: Item  */
#line 253 "parser/gramLatex.y"
                       { yyval = xxnewlist(yyvsp[0]); }
#line 1895 "gramLatex.tab.c"
    break;

  case 6: /* Items: math  */
#line 254 "parser/gramLatex.y"
                       { yyval = xxnewlist(yyvsp[0]); }
#line 1901 "gramLatex.tab.c"
    break;

  case 7: /* Items: displaymath  */
#line 255 "parser/gramLatex.y"
                       { yyval = xxnewlist(yyvsp[0]); }
#line 1907 "gramLatex.tab.c"
    break;

  case 8: /* Items: Items Item  */
#line 256 "parser/gramLatex.y"
                       { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
#line 1913 "gramLatex.tab.c"
    break;

  case 9: /* Items: Items math  */
#line 257 "parser/gramLatex.y"
                       { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
#line 1919 "gramLatex.tab.c"
    break;

  case 10: /* Items: Items displaymath  */
#line 258 "parser/gramLatex.y"
                       { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
#line 1925 "gramLatex.tab.c"
    break;

  case 11: /* Items: Items error  */
#line 259 "parser/gramLatex.y"
                       { yyclearin;
                         parseError();
                         GrowList(yyvsp[-1], xxwrapError(xxfakeStart(CHAR(STRING_ELT(yylval, 0)), NULL)));
                         yyval = yyvsp[-1];
                       }
#line 1935 "gramLatex.tab.c"
    break;

  case 12: /* nonMath: Item  */
#line 265 "parser/gramLatex.y"
                       { yyval = xxnewlist(yyvsp[0]); }
#line 1941 "gramLatex.tab.c"
    break;

  case 13: /* nonMath: nonMath Item  */
#line 266 "parser/gramLatex.y"
                       { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
#line 1947 "gramLatex.tab.c"
    break;

  case 14: /* Item: TEXT  */
#line 268 "parser/gramLatex.y"
                    { xxArg(NULL); yyval = xxtag(yyvsp[0], TEXT, &(yyloc)); }
#line 1953 "gramLatex.tab.c"
    break;

  case 15: /* Item: COMMENT  */
#line 269 "parser/gramLatex.y"
                    { yyval = xxtag(yyvsp[0], COMMENT, &(yyloc)); }
#line 1959 "gramLatex.tab.c"
    break;

  case 16: /* Item: MACRO  */
#line 270 "parser/gramLatex.y"
                    { xxArg(NULL);
                      yyval = xxtag(yyvsp[0], MACRO, &(yyloc)); }
#line 1966 "gramLatex.tab.c"
    break;

  case 17: /* Item: SPECIAL  */
#line 272 "parser/gramLatex.y"
                    { xxArg(yyvsp[0]);
                      yyval = xxtag(yyvsp[0], SPECIAL, &(yyloc)); }
#line 1973 "gramLatex.tab.c"
    break;

  case 18: /* Item: VERB  */
#line 274 "parser/gramLatex.y"
                    { yyval = xxtag(yyvsp[0], VERB, &(yyloc)); }
#line 1979 "gramLatex.tab.c"
    break;

  case 19: /* Item: VERB2  */
#line 275 "parser/gramLatex.y"
                    { yyval = xxtag(yyvsp[0], VERB, &(yyloc)); }
#line 1985 "gramLatex.tab.c"
    break;

  case 20: /* Item: block  */
#line 276 "parser/gramLatex.y"
                    { xxArg(NULL); yyval = yyvsp[0]; }
#line 1991 "gramLatex.tab.c"
    break;

  case 21: /* Item: environment  */
#line 277 "parser/gramLatex.y"
                    { yyval = yyvsp[0]; }
#line 1997 "gramLatex.tab.c"
    break;

  case 22: /* Item: newdefine  */
#line 278 "parser/gramLatex.y"
                    { yyval = yyvsp[0]; }
#line 2003 "gramLatex.tab.c"
    break;

  case 23: /* envname: TEXT  */
#line 280 "parser/gramLatex.y"
                 { yyval = xxnewlist(xxtag(yyvsp[0], TEXT, &(yylsp[0]))); }
#line 2009 "gramLatex.tab.c"
    break;

  case 24: /* envname: SPECIAL  */
#line 281 "parser/gramLatex.y"
                 { yyval = xxnewlist(xxtag(yyvsp[0], SPECIAL, &(yylsp[0]))); }
#line 2015 "gramLatex.tab.c"
    break;

  case 25: /* envname: envname TEXT  */
#line 282 "parser/gramLatex.y"
                         { yyval = xxlist(yyvsp[-1], xxtag(yyvsp[0], TEXT, &(yylsp[0]))); }
#line 2021 "gramLatex.tab.c"
    break;

  case 26: /* envname: envname SPECIAL  */
#line 283 "parser/gramLatex.y"
                         { yyval = xxlist(yyvsp[-1], xxtag(yyvsp[0], SPECIAL, &(yylsp[0]))); }
#line 2027 "gramLatex.tab.c"
    break;

  case 27: /* begin: BEGIN '{' envname '}'  */
#line 285 "parser/gramLatex.y"
                               { xxSetInVerbEnv(yyvsp[-1]);
                                 RELEASE_SV(yyvsp[-3]);
                                 yyval = yyvsp[-1]; }
#line 2035 "gramLatex.tab.c"
    break;

  case 28: /* environment: begin Items END '{' envname '}'  */
#line 290 "parser/gramLatex.y"
                          { yyval = xxenv(yyvsp[-5], yyvsp[-4], yyvsp[-1], &(yyloc));
                            RELEASE_SV(yyvsp[-3]); }
#line 2042 "gramLatex.tab.c"
    break;

  case 29: /* environment: begin END '{' envname '}'  */
#line 293 "parser/gramLatex.y"
                          { yyval = xxenv(yyvsp[-4], NULL, yyvsp[-1], &(yyloc));
                            RELEASE_SV(yyvsp[-3]); }
#line 2049 "gramLatex.tab.c"
    break;

  case 30: /* environment: begin error  */
#line 295 "parser/gramLatex.y"
                          { xxincompleteBegin(yyvsp[-1], &(yylsp[-1]));
                            yyval = xxwrapError(xxfakeBegin(yyvsp[-1], NULL)); }
#line 2056 "gramLatex.tab.c"
    break;

  case 31: /* environment: begin Items error  */
#line 297 "parser/gramLatex.y"
                                {
                            xxincompleteBegin(yyvsp[-2], &(yylsp[-2]));
                            yyval = xxwrapError(xxfakeBegin(yyvsp[-2], yyvsp[-1])); }
#line 2064 "gramLatex.tab.c"
    break;

  case 32: /* mathstart: '$'  */
#line 301 "parser/gramLatex.y"
                          { yyval = xxenterMathMode(); }
#line 2070 "gramLatex.tab.c"
    break;

  case 33: /* math: mathstart nonMath '$'  */
#line 303 "parser/gramLatex.y"
                                { xxpopMode(yyvsp[-2]);
                            yyval = xxmath(yyvsp[-1], &(yyloc), FALSE); }
#line 2077 "gramLatex.tab.c"
    break;

  case 34: /* math: mathstart error  */
#line 305 "parser/gramLatex.y"
                          { xxpopMode(yyvsp[-1]);
                            xxincomplete(mkString("$"), &(yylsp[-1]));
                            yyval = xxwrapError(xxfakeStart("$", NULL)); }
#line 2085 "gramLatex.tab.c"
    break;

  case 35: /* math: mathstart nonMath error  */
#line 308 "parser/gramLatex.y"
                                { xxpopMode(yyvsp[-2]);
                            xxincomplete(mkString("$"), &(yylsp[-2]));
                            yyval = xxwrapError(xxfakeStart("$", yyvsp[-1])); }
#line 2093 "gramLatex.tab.c"
    break;

  case 36: /* displaymath: TWO_DOLLARS nonMath TWO_DOLLARS  */
#line 313 "parser/gramLatex.y"
                          { yyval = xxmath(yyvsp[-1], &(yyloc), TRUE); }
#line 2099 "gramLatex.tab.c"
    break;

  case 37: /* displaymath: TWO_DOLLARS error  */
#line 315 "parser/gramLatex.y"
                          { xxincomplete(mkString("$$"), &(yylsp[-1]));
                            yyval = xxwrapError(xxfakeStart("$$", NULL)); }
#line 2106 "gramLatex.tab.c"
    break;

  case 38: /* displaymath: TWO_DOLLARS nonMath error  */
#line 318 "parser/gramLatex.y"
                          { xxincomplete(mkString("$$"), &(yylsp[-2]));
                            yyval = xxwrapError(xxfakeStart("$$", yyvsp[-1])); }
#line 2113 "gramLatex.tab.c"
    break;

  case 39: /* block: '{' Items '}'  */
#line 321 "parser/gramLatex.y"
                          { yyval = xxblock(yyvsp[-1], &(yyloc)); }
#line 2119 "gramLatex.tab.c"
    break;

  case 40: /* block: '{' '}'  */
#line 322 "parser/gramLatex.y"
                          { yyval = xxblock(NULL, &(yyloc)); }
#line 2125 "gramLatex.tab.c"
    break;

  case 41: /* block: '{' error  */
#line 323 "parser/gramLatex.y"
                          { xxincomplete(mkString("{"), &(yylsp[-1]));
                            yyval = xxwrapError(xxfakeStart("{", NULL)); }
#line 2132 "gramLatex.tab.c"
    break;

  case 42: /* block: '{' Items error  */
#line 325 "parser/gramLatex.y"
                          { xxincomplete(mkString("{"), &(yylsp[-2]));
                            yyval = xxwrapError(xxfakeStart("{", yyvsp[-1])); }
#line 2139 "gramLatex.tab.c"
    break;

  case 43: /* @1: %empty  */
#line 328 "parser/gramLatex.y"
                          { yyval = xxpushMode(2, 1); }
#line 2145 "gramLatex.tab.c"
    break;

  case 44: /* newdefine: NEWCMD @1 Items END_OF_ARGS  */
#line 330 "parser/gramLatex.y"
                          { xxpopMode(yyvsp[-2]);
                            yyval = xxnewdef(xxtag(yyvsp[-3], MACRO, &(yylsp[-3])),
                                        yyvsp[-1], &(yyloc)); }
#line 2153 "gramLatex.tab.c"
    break;

  case 45: /* @2: %empty  */
#line 333 "parser/gramLatex.y"
                          { yyval = xxpushMode(3, 1); }
#line 2159 "gramLatex.tab.c"
    break;

  case 46: /* newdefine: NEWENV @2 Items END_OF_ARGS  */
#line 335 "parser/gramLatex.y"
                          {  xxpopMode(yyvsp[-2]);
                             yyval = xxnewdef(xxtag(yyvsp[-3], MACRO, &(yylsp[-3])),
                                        yyvsp[-1], &(yyloc)); }
#line 2167 "gramLatex.tab.c"
    break;


#line 2171 "gramLatex.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 339 "parser/gramLatex.y"


static int parseError(void)
{
  if (parseState.recover)
    warning("Parse error at %d:%d: %s", ParseErrorLine,
            ParseErrorCol, ParseErrorMsg);
  else
    error("Parse error at %d:%d: %s", ParseErrorLine,
          ParseErrorCol, ParseErrorMsg);
  return ERROR;
}

static SEXP xxnewlist(SEXP item)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif
    PRESERVE_SV(ans = NewList());
    if (item) {
      GrowList(ans, item);
      RELEASE_SV(item);
    }
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxlist(SEXP list, SEXP item)
{
#if DEBUGVALS
    Rprintf("xxlist(list=%p, item=%p)", list, item);
#endif
    GrowList(list, item);
    RELEASE_SV(item);
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", list, length(list));
#endif
    return list;
}

static void xxgettext(char *buffer, size_t bufsize,
                       SEXP object) {
  if (TYPEOF(object) == LISTSXP) {
    int len, done = 0;
    object = CDR(object);
    while (!isNull(object)) {
      len = snprintf(buffer + done, bufsize - done,
                     "%s", CHAR(STRING_ELT(CAR(object), 0)));
      if (len > 0)
        done += len;
      object = CDR(object);
    }
  } else if (TYPEOF(object) == STRSXP)
    snprintf(buffer, bufsize,
             "%s", CHAR(STRING_ELT(object, 0)));
  else
    buffer[0] = '\0';
}

static SEXP xxenv(SEXP begin, SEXP body, SEXP end, YYLTYPE *lloc)
{
  SEXP ans;
  char ename1[256], ename2[256];
  xxgettext(ename1, sizeof(ename1), begin);
  xxgettext(ename2, sizeof(ename2), end);
  if (strncmp(ename1, ename2, sizeof(ename1)) != 0) {
    char buffer[PARSE_ERROR_SIZE + 2*sizeof(ename1) + 40];
    snprintf(buffer, sizeof(buffer),
             "\\begin{%s} at %d:%d ended by \\end{%s}",
             ename1, lloc->first_line, lloc->first_column,
             ename2);
    yyerror(buffer);
    parseError();
    snprintf(buffer, sizeof(buffer),
             "\\end{%s}", ename2);
    PRESERVE_SV(ans = xxfakeBegin(begin, body));
    GrowList(ans, xxtag(mkString(buffer), TEXT, lloc));
    return xxwrapError(ans);
  }

  if (strcmp("document", ename1) == 0) {
    PRESERVE_SV(yylval = mkString("\\end{document}"));
    xxungetc(R_EOF);  /* Stop reading after \end{document} */
  }
#if DEBUGVALS
  Rprintf("xxenv(begin=%p, body=%p, end=%p)", begin, body, end);
#endif
  if (body && !isNull(body)) {
    PRESERVE_SV(ans = PairToVectorList(CDR(body)));
    RELEASE_SV(body);
  } else
    PRESERVE_SV(ans = allocVector(VECSXP, 0));


  setAttrib(ans, install("envname"), mkString(ename1));
  RELEASE_SV(begin);
  if (!isNull(end))
    RELEASE_SV(end);
  setAttrib(ans, install("srcref"), makeSrcref(lloc));
  setAttrib(ans, LatexTagSymbol, mkString("ENVIRONMENT"));
  setAttrib(ans, R_ClassSymbol, mkString("LaTeX2item"));
#if DEBUGVALS
  Rprintf(" result: %p\n", ans);
#endif
  return ans;
}

static SEXP xxnewdef(SEXP cmd, SEXP items,
                     YYLTYPE *lloc)
{
  SEXP ans, temp;
  int n;

  PRESERVE_SV(temp = PairToVectorList(CDR(items)));
  RELEASE_SV(items);
  n = length(temp);
  PRESERVE_SV(ans = allocVector(VECSXP, n + 1));
  for (int i=0; i < n; i++)
    SET_VECTOR_ELT(ans, i + 1, VECTOR_ELT(temp, i));
  RELEASE_SV(temp);
  SET_VECTOR_ELT(ans, 0, cmd);
  RELEASE_SV(cmd);

  setAttrib(ans, install("srcref"), makeSrcref(lloc));
  setAttrib(ans, LatexTagSymbol, mkString("DEFINITION"));
  setAttrib(ans, R_ClassSymbol, mkString("LaTeX2item"));

  return ans;
}

static SEXP xxenterMathMode(void) {
    SEXP ans;
    PRESERVE_SV(ans = allocVector(INTSXP, 5));
    INTEGER(ans)[0] = parseState.xxGetArgs;
    INTEGER(ans)[1] = parseState.xxIgnoreKeywords;
    INTEGER(ans)[2] = parseState.xxBraceDepth;
    INTEGER(ans)[3] = parseState.xxBracketDepth;
    INTEGER(ans)[4] = parseState.xxMathMode;
    parseState.xxBraceDepth = 0;
    parseState.xxBracketDepth = 0;
    parseState.xxMathMode = 1;
    return ans;

}
static SEXP xxpushMode(int getArgs,
                       int ignoreKeywords) {
    SEXP ans;

    PRESERVE_SV(ans = allocVector(INTSXP, 5));
    INTEGER(ans)[0] = parseState.xxGetArgs;
    INTEGER(ans)[1] = parseState.xxIgnoreKeywords;
    INTEGER(ans)[2] = parseState.xxBraceDepth;
    INTEGER(ans)[3] = parseState.xxBracketDepth;
    INTEGER(ans)[4] = parseState.xxMathMode;
    parseState.xxGetArgs = getArgs;
    parseState.xxIgnoreKeywords = ignoreKeywords;
    parseState.xxBraceDepth = 0;
    parseState.xxBracketDepth = 0;
    return ans;
}

static void xxpopMode(SEXP oldmode) {
  parseState.xxGetArgs = INTEGER(oldmode)[0];
  parseState.xxIgnoreKeywords = INTEGER(oldmode)[1];
  parseState.xxBraceDepth = INTEGER(oldmode)[2];
  parseState.xxBracketDepth = INTEGER(oldmode)[3];
  parseState.xxMathMode = INTEGER(oldmode)[4];
  RELEASE_SV(oldmode);
}

static void xxArg(SEXP arg) {
  if (parseState.xxGetArgs == 0 ||
      parseState.xxBraceDepth > 0 ||
      parseState.xxBracketDepth > 0) return;
  /* arg is only non-NULL for SPECIALS */
  if (arg) {
    int catcode = INTEGER(getAttrib(arg, install("catcode")))[0];
    switch (catcode) {
    case 5:
    case 9:
    case 10:
      break;    /* ignore whitespace */
    case 4:
    case 7:
    case 8:
      parseState.xxGetArgs--;  /* &, ^ and _ can be args */
      break;
    }
  } else {
    parseState.xxGetArgs--;
  }

  if (parseState.xxGetArgs == 0) {
  /* We've just completed the final arg we were waiting for */
    xxungetc(END_OF_ARGS_CHAR);  /* push a non-character to signal the end */
  }
}

static SEXP xxmath(SEXP body, YYLTYPE *lloc, Rboolean display)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmath(body=%p, display=%d)", body, display);
#endif
    PRESERVE_SV(ans = PairToVectorList(CDR(body)));
    RELEASE_SV(body);
    setAttrib(ans, install("srcref"), makeSrcref(lloc));
    setAttrib(ans, LatexTagSymbol,
    mkString(display ? "DISPLAYMATH" : "MATH"));
    setAttrib(ans, R_ClassSymbol, mkString("LaTeX2item"));
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);
#endif
    return ans;
}

static SEXP xxblock(SEXP body, YYLTYPE *lloc)
{
  SEXP ans;
#if DEBUGVALS
  Rprintf("xxblock(body=%p)", body);
#endif
  if (!body)
    PRESERVE_SV(ans = allocVector(VECSXP, 0));
  else {
    PRESERVE_SV(ans = PairToVectorList(CDR(body)));
    RELEASE_SV(body);
  }
  setAttrib(ans, install("srcref"), makeSrcref(lloc));
  setAttrib(ans, LatexTagSymbol, mkString("BLOCK"));
  setAttrib(ans, R_ClassSymbol, mkString("LaTeX2item"));
#if DEBUGVALS
  Rprintf(" result: %p\n", ans);
#endif
  return ans;
}

static SEXP xxerrblock(SEXP body)
{
  SEXP ans;
  if (!body)
    PRESERVE_SV(ans = allocVector(VECSXP, 0));
  else {
    PRESERVE_SV(ans = PairToVectorList(CDR(body)));
    RELEASE_SV(body);
  }
  setAttrib(ans, install("srcref"), makeSrcref(&yylloc));
  setAttrib(ans, LatexTagSymbol, mkString("ERROR"));
  setAttrib(ans, R_ClassSymbol, mkString("LaTeX2item"));
  setAttrib(ans, install("errormsg"), mkString(ParseErrorMsg));

  return ans;
}

static int VerbatimLookup(const char *s)
{
  int i;
  for (i = 0; i < length(parseState.xxVerbatimList); i++) {
    if (strcmp(s, CHAR(STRING_ELT(parseState.xxVerbatimList, i))) == 0)
      return TRUE;
  }
  return FALSE;
}

static void xxSetInVerbEnv(SEXP envname)
{
  char ename[256];
  char buffer[sizeof(ename) + 10];
  xxgettext(ename, sizeof(ename), envname);

  if (VerbatimLookup(ename)) {
    snprintf(buffer, sizeof(buffer), "\\end{%s}", ename);
    PRESERVE_SV(parseState.xxInVerbEnv = ScalarString(mkChar(buffer)));
  } else parseState.xxInVerbEnv = NULL;
}

static void xxsavevalue(SEXP items, YYLTYPE *lloc)
{
    if (items) {
      PRESERVE_SV(parseState.Value = PairToVectorList(CDR(items)));
      RELEASE_SV(items);
    } else {
      PRESERVE_SV(parseState.Value = allocVector(VECSXP, 1));
      SET_VECTOR_ELT(parseState.Value, 0, ScalarString(mkChar("")));
      setAttrib(VECTOR_ELT(parseState.Value, 0), LatexTagSymbol, mkString("TEXT"));
      setAttrib(VECTOR_ELT(parseState.Value, 0), R_ClassSymbol,
        mkString("LaTeX2item"));
    }
    if (!isNull(parseState.Value)) {
      setAttrib(parseState.Value, R_ClassSymbol, mkString("LaTeX2"));
      setAttrib(parseState.Value, install("srcref"), makeSrcref(lloc));
    }
}

static SEXP xxtag(SEXP item, int type, YYLTYPE *lloc)
{
    setAttrib(item, LatexTagSymbol, mkString(yytname[YYTRANSLATE(type)]));
    setAttrib(item, install("srcref"), makeSrcref(lloc));
    setAttrib(item, R_ClassSymbol, mkString("LaTeX2item"));
    return item;
}

/*----------------------------------------------------------------------------*/


static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth  */

#define PUSHBACK_BUFSIZE 30

static UChar32 pushback[PUSHBACK_BUFSIZE];
static unsigned int npush = 0;

static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];

static UChar32 xxgetc(void)
{
  UChar32 c;
  int oldpos;

  if(npush)
    c = pushback[--npush];
  else {
    uint8_t utf8_bytes[4];  // Buffer for UTF-8 character (max 4 bytes)
    int i, byte_count = 0;
    int first_byte = (uint8_t)ptr_getc();
    if (first_byte == (uint8_t)EOF)
      c = EOF;
    else {
      int expected_length = U8_COUNT_TRAIL_BYTES(first_byte);
      utf8_bytes[byte_count++] = (uint8_t)first_byte;
      // Read remaining bytes if needed
      for (i = 0; i < expected_length; i++) {
        int next_byte = ptr_getc();
        if (next_byte == EOF) {
          // Unexpected EOF in the middle of a character
          break;
        }
        utf8_bytes[byte_count++] = (uint8_t)next_byte;
      }
      if (i < expected_length)
        c = EOF;
      else {
        int32_t offset = 0;
        U8_NEXT_OR_FFFD(utf8_bytes, offset, byte_count, c);
      }
    }
  }

  oldpos = prevpos;
  prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
  prevbytes[prevpos] = parseState.xxbyteno;
  prevlines[prevpos] = parseState.xxlineno;
  /* We only advance the column for the 1st byte in UTF-8, so handle later bytes specially */
  if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF) {
    parseState.xxcolno--;
    prevcols[prevpos] = prevcols[oldpos];
  } else
    prevcols[prevpos] = parseState.xxcolno;

  if (c == EOF) return R_EOF;

  if (c == '\n') {
    parseState.xxlineno += 1;
    parseState.xxcolno = 1;
    parseState.xxbyteno = 1;
  } else {
    parseState.xxcolno++;
    parseState.xxbyteno++;
  }

  if (c == '\t') parseState.xxcolno = ((parseState.xxcolno + 6) & ~7) + 1;

  return c;
}

static UChar32 xxungetc(int c)
{
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    parseState.xxlineno = prevlines[prevpos];
    parseState.xxbyteno = prevbytes[prevpos];
    parseState.xxcolno  = prevcols[prevpos];
    prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;

    if(npush >= PUSHBACK_BUFSIZE - 2) return R_EOF;
    pushback[npush++] = c;
    return c;
}

static SEXP makeSrcref(YYLTYPE *lloc)
{
    SEXP val;

    PROTECT(val = allocVector(INTSXP, 6));
    INTEGER(val)[0] = lloc->first_line;
    INTEGER(val)[1] = lloc->first_byte;
    INTEGER(val)[2] = lloc->last_line;
    INTEGER(val)[3] = lloc->last_byte;
    INTEGER(val)[4] = lloc->first_column;
    INTEGER(val)[5] = lloc->last_column;
    setAttrib(val, R_ClassSymbol, mkString("srcref"));
    UNPROTECT(1); /* val */
    return val;
}

static SEXP mkString2(const uint8_t *s, size_t len)
{
    SEXP t;
    cetype_t enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE((const char*)s, (int) len, enc));
    UNPROTECT(1); /* t */
    return t;
}


/* Stretchy List Structures : Lists are created and grown using a special */
/* dotted pair.  The CAR of the list points to the last cons-cell in the */
/* list and the CDR points to the first.  The list can be extracted from */
/* the pair by taking its CDR, while the CAR gives fast access to the end */
/* of the list. */


/* Create a stretchy-list dotted pair */

static SEXP NewList(void)
{
    SEXP s = CONS(R_NilValue, R_NilValue);
    SETCAR(s, s);
    return s;
}

/* Add a new element at the end of a stretchy list */

static void GrowList(SEXP l, SEXP s)
{
    SEXP tmp;
    tmp = CONS(s, R_NilValue);
    SETCDR(CAR(l), tmp);
    SETCAR(l, tmp);
}

/* Append one list after another */

static SEXP AppendList(SEXP head, SEXP tail)
{
    SETCDR(CAR(head), CDR(tail));
    SETCAR(head, CAR(tail));
    return head;
}

/* Make items that look like the start of an env */

static SEXP xxfakeStart(const char * start, SEXP items)
{
  SEXP temp;
  YYLTYPE *lloc = &noSrcref;
  PRESERVE_SV(temp = xxnewlist(xxtag(mkString(start),
                                     TEXT, lloc)));
  if (items)
    AppendList(temp, items);
  return temp;
}

/* Make items that look like the start of an env */

static SEXP xxfakeBegin(SEXP envname, SEXP items)
{
  SEXP temp;
  YYLTYPE *lloc = &noSrcref;
  PRESERVE_SV(temp = xxnewlist(xxtag(mkString("\\begin"),
                                     TEXT, lloc)));
  GrowList(temp, xxblock(envname, lloc));
  if (items)
    AppendList(temp, items);
  return temp;
}

/* Wrap a list to indicate an error message */
static SEXP xxwrapError(SEXP list)
{
  SEXP temp;
  PROTECT(temp = NewList());
  if (list) {
    if (TYPEOF(list) != LISTSXP)
      PROTECT(list = xxnewlist(list));
    else
      PROTECT(list);
    AppendList(temp, list);
    UNPROTECT(1);
    RELEASE_SV(list);
  }
  temp = xxerrblock(temp);
  UNPROTECT(1);
  return temp;
}

/*--------------------------------------------------------------------------*/

static void PutState(ParseState *state) {
    state->xxlineno = parseState.xxlineno;
    state->xxbyteno = parseState.xxbyteno;
    state->xxcolno = parseState.xxcolno;
    state->xxDebugTokens = parseState.xxDebugTokens;
    state->Value = parseState.Value;
    state->xxinitvalue = parseState.xxinitvalue;
    state->xxInVerbEnv = parseState.xxInVerbEnv;
    state->xxVerbatimList = parseState.xxVerbatimList;
    state->xxKwdList = parseState.xxKwdList;
    state->xxKwdType = parseState.xxKwdType;
    state->xxGetArgs = parseState.xxGetArgs;
    state->xxIgnoreKeywords = parseState.xxIgnoreKeywords;
    state->xxBraceDepth = parseState.xxBraceDepth;
    state->xxBracketDepth = parseState.xxBracketDepth;
    state->xxMathMode = parseState.xxMathMode;
    state->prevState = parseState.prevState;

}

static void UseState(ParseState *state) {
    parseState.xxlineno = state->xxlineno;
    parseState.xxbyteno = state->xxbyteno;
    parseState.xxcolno = state->xxcolno;
    parseState.xxDebugTokens = state->xxDebugTokens;
    parseState.Value = state->Value;
    parseState.xxinitvalue = state->xxinitvalue;
    parseState.xxInVerbEnv = state->xxInVerbEnv;
    parseState.xxVerbatimList = state->xxVerbatimList;
    parseState.xxKwdList = state->xxKwdList;
    parseState.xxKwdType = state->xxKwdType;
    parseState.prevState = state->prevState;
}

static SEXP ParseLatex(ParseStatus *status)
{
    LatexTagSymbol = install("latex_tag");

    parseState.xxInVerbEnv = NULL;
    parseState.xxGetArgs = 0;
    parseState.xxIgnoreKeywords = 0;
    parseState.xxBraceDepth = 0;
    parseState.xxBracketDepth = 0;
    parseState.xxMathMode = 0;

    parseState.xxlineno = 1;
    parseState.xxcolno = 1;
    parseState.xxbyteno = 1;

    PROTECT(parseState.mset = R_NewPreciousMSet(50));

    npush = 0;

    parseState.Value = R_NilValue;

    PRESERVE_SV(yylval = mkString(""));

    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;

#if DEBUGVALS
    Rprintf("ParseRd result: %p\n", parseState.Value);
#endif

    RELEASE_SV(parseState.Value);
    UNPROTECT(1); /* parseState.mset */

    return parseState.Value;
}

static const char * nextchar_parse;

/* need to handle incomplete last line */
static int char_getc(void)
{
    int c;

    c = *nextchar_parse++;
    if (!c) {
      c = R_EOF;
      nextchar_parse--;
    }
    return (c);
}

/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.
 *
 */


/* Special Symbols */
/* Section and R code headers */

struct {
    char *name;
    int token;
}
static keywords[] = {
    /* These sections contain Latex-like text */

    { "\\begin",  BEGIN },
    { "\\end",    END },
    { "\\verb",   VERB },
    { 0,     0        }
    /* All other markup macros are rejected. */
};

/* Record the longest # directive here */
#define DIRECTIVE_LEN 7

static int KeywordLookup(const char *s)
{
  int i;
  if (parseState.xxIgnoreKeywords)
    return MACRO;

  for (i = 0; keywords[i].name; i++) {
    if (strcmp(keywords[i].name, s) == 0)
      return keywords[i].token;
  }

  for (i = 0; i < length(parseState.xxKwdList); i++) {
    if (strcmp(CHAR(STRING_ELT(parseState.xxKwdList, i)), s) == 0)
      switch(INTEGER(parseState.xxKwdType)[i]) {
      case 1: return VERB2;
      case 2: return NEWCMD;
      case 3: return NEWENV;
      }
  }

  return MACRO;
}

static void xxincomplete(SEXP what, YYLTYPE *where)
{
  char buffer[PARSE_ERROR_SIZE + 32];
  snprintf(buffer, sizeof(buffer), "%s\n  '%s' at %d:%d is still open",
           ParseErrorMsg,
           CHAR(STRING_ELT(what, 0)),
           where->first_line, where->first_column);
  yyerror(buffer);
  parseError();
}

static void xxincompleteBegin(SEXP what, YYLTYPE *where)
{
  char start[64] = "\\begin{";
  PROTECT(what);
  xxgettext(start + strlen(start), sizeof(start) - strlen(start) - 1, what);
  start[strlen(start) + 1] = 0;
  start[strlen(start)] = '}';
  xxincomplete(mkString(start), where);
}

static void yyerror(const char *s)
{
  static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
     column are translations for users.
     The first YYENGLISH from the right column are English to be translated,
     the rest are to be copied literally.  The #if 0 block below allows xgettext
     to see these.
     */
#define YYENGLISH 3
    "$undefined",  "input",
    "LATEXMACRO",  "macro",
    "ESCAPE",  "macro",
    0,    0
    };
  static char const yyunexpected[] = "syntax error, unexpected ";
  static char const yyexpecting[] = ", expecting ";
  static char const yyshortunexpected[] = "unexpected %s";
  static char const yylongunexpected[] = "unexpected %s '%s'";
  char *expecting;

  ParseErrorLine     = yylloc.first_line;
  ParseErrorCol      = yylloc.first_column;

  if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
    int i, translated = FALSE;
    /* Edit the error message */
    expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    if (expecting) *expecting = '\0';
    for (i = 0; yytname_translations[i]; i += 2) {
      if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
        if (yychar < 256 || yychar == END_OF_INPUT)
          snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
                   _(yyshortunexpected),
                   i/2 < YYENGLISH ? _(yytname_translations[i+1])
                     : yytname_translations[i+1]);
        else
          snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
                   _(yylongunexpected),
                   i/2 < YYENGLISH ? _(yytname_translations[i+1])
                     : yytname_translations[i+1],
                       CHAR(STRING_ELT(yylval, 0)));
        translated = TRUE;
        break;
      }
    }
    if (!translated) {
      if (yychar < 256 || yychar == END_OF_INPUT)
        snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
                 _(yyshortunexpected),
                 s + sizeof yyunexpected - 1);
      else
        snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
                 _(yylongunexpected),
                 s + sizeof yyunexpected - 1, CHAR(STRING_ELT(yylval, 0)));
    }
    if (expecting) {
      translated = FALSE;
      for (i = 0; yytname_translations[i]; i += 2) {
        if (!strcmp(expecting + sizeof yyexpecting - 1, yytname_translations[i])) {
          strcat(ParseErrorMsg, _(yyexpecting));
          strcat(ParseErrorMsg, i/2 < YYENGLISH ? _(yytname_translations[i+1])
                   : yytname_translations[i+1]);
          translated = TRUE;
          break;
        }
      }
      if (!translated) {
        strcat(ParseErrorMsg, _(yyexpecting));
        strcat(ParseErrorMsg, expecting + sizeof yyexpecting - 1);
      }
    }
  } else if (!strncmp(s, yyunknown, sizeof yyunknown-1)) {
    snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
             "%s '%s'", s, CHAR(STRING_ELT(yylval, 0)));
  } else {
    snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,"%s", s);
  }
}

#define TEXT_PUSH(c) do {        \
  size_t nc = bp - stext;        \
  if (nc >= nstext - 4) {        \
      uint8_t *old = stext;      \
      nstext *= 2;               \
      stext = malloc(nstext);    \
      if(!stext) error(_("unable to allocate buffer for long string at line %d"), parseState.xxlineno);\
      memmove(stext, old, nc);   \
      if(st1) free(st1);         \
      st1 = stext;               \
      bp = stext+nc; }           \
  U8_APPEND(stext, nc, nstext, c, isError);        \
  if (isError) error(_("UTF-8 encoding error at line %d"), parseState.xxlineno); \
  bp = stext+nc;                 \
} while(0)

static void setfirstloc(void)
{
    yylloc.first_line = parseState.xxlineno;
    yylloc.first_column = parseState.xxcolno;
    yylloc.first_byte = parseState.xxbyteno;
}

static void setlastloc(void)
{
    yylloc.last_line = prevlines[prevpos];
    yylloc.last_column = prevcols[prevpos];
    yylloc.last_byte = prevbytes[prevpos];
}

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int tex_catcode(UChar32 c) {
  if (c == R_EOF) return -1;
  for (int i = 0; i < length(parseState.xxCatcodes); i++)
    if (c == INTEGER(parseState.xxCodepoints)[i])
      return INTEGER(parseState.xxCatcodes)[i];
#ifdef PARSELATEX_WIN32
  if (iswalpha(c)) return 11;
#else
  if (u_hasBinaryProperty(c, UCHAR_ALPHABETIC)) return 11;
#endif
  if (c < 32) return 15;
  return 12;
}

static int token(void)
{
    int cat;
    UChar32 c = 0;

    if (parseState.xxinitvalue) {
      yylloc.first_line = 0;
      yylloc.first_column = 0;
      yylloc.first_byte = 0;
      yylloc.last_line = 0;
      yylloc.last_column = 0;
      yylloc.last_byte = 0;
      PRESERVE_SV(yylval = mkString(""));
      c = parseState.xxinitvalue;
      parseState.xxinitvalue = 0;
      return(c);
    }

    setfirstloc();

    if (parseState.xxInVerbEnv)
      return mkVerbEnv();

    c = xxgetc();

    if (c == END_OF_ARGS_CHAR)
      return END_OF_ARGS;

    cat = tex_catcode(c);

    switch (cat) {
      case 0:  return mkMarkup(c);
      case 1:  {
        parseState.xxBraceDepth++;
        return '{';
      }
      case 2:  {
        parseState.xxBraceDepth--;
        return '}';
      }
      case 3:  return mkDollar(c);
      case 11: return mkText(c);
      case 14: return mkComment(c);

      case -1:return END_OF_INPUT;
      default: return mkSpecial(c, cat);
    }
}

#define INITBUFSIZE 128

static int mkText(int c)
{
    uint8_t st0[INITBUFSIZE];
    uint8_t *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    uint8_t *stext = st0, *bp = st0;
    UBool isError = false;

    if (parseState.xxGetArgs > 0 &&
        parseState.xxBraceDepth == 0 &&
        parseState.xxBracketDepth == 0) {
        TEXT_PUSH(c);
    } else {
      do {
        TEXT_PUSH(c);
        c = xxgetc();
      } while (tex_catcode(c) == 11);
      xxungetc(c);
    }
    PRESERVE_SV(yylval = mkString2(stext,  bp - stext));
    if(st1) free(st1);
    return TEXT;
}

static int mkComment(int c)
{
    uint8_t st0[INITBUFSIZE];
    uint8_t *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    uint8_t *stext = st0, *bp = st0;
    UBool isError = false;
    int cat;

    do {
      TEXT_PUSH(c);
      c = xxgetc();
      cat = tex_catcode(c);
    } while (cat != 5 && cat != -1);

    if (cat == -1) xxungetc(c);
    else TEXT_PUSH(c);

    int magic = magicComment(stext, bp - stext);
    if (magic == 1) {

      // want to loop until we find a type 2 magic comment.
      do {
        c = xxgetc();
        cat = tex_catcode(c);
        if (cat == 14) { // have a comment
          int start = bp - stext;
          do {
            TEXT_PUSH(c);
            c = xxgetc();
            cat = tex_catcode(c);
          } while (cat != 5 && cat != -1);
          if (cat == -1) xxungetc(c);
          else {
            TEXT_PUSH(c);
            if (magicComment(stext + start, bp - stext - start) == 2)
              break;
          }
        } else if (cat != -1)
          TEXT_PUSH(c);
      } while (cat != -1);
    }

    PRESERVE_SV(yylval = mkString2(stext,  bp - stext));

    if(st1) free(st1);
    return COMMENT;
}

static SEXP addString(const uint8_t *string,
                      size_t len,
                      SEXP strings) {
  size_t n = length(strings);
  SEXP temp;
  PRESERVE_SV(temp = allocVector(STRSXP, n + 1));
  for (int i=0; i < n; i++)
    SET_STRING_ELT(temp, i, STRING_ELT(strings, i));
  RELEASE_SV(strings);
  SET_STRING_ELT(temp, n,
                 mkCharLenCE((const char *)string, len, CE_UTF8));
  return temp;
}

static SEXP addInteger(int value,
                      SEXP values) {
  size_t n = length(values);
  SEXP temp;
  PRESERVE_SV(temp = allocVector(INTSXP, n + 1));
  for (int i=0; i < n; i++)
    INTEGER(temp)[i] = INTEGER(values)[i];
  RELEASE_SV(values);
  INTEGER(temp)[n] = value;
  return temp;
}

/* magic comment codes:
 * 0 = not magic
 * 1 = % !parser off
 * 2 = % !parser on
 * 3 = % !parser verb \macro
 * 4 = % !parser verbatim envname
 */
static int magicComment(const uint8_t *s, int len)
{
  const uint8_t *text, *name;

  for (text = s + 1; text - s < len && *text == ' '; text++) {};
  if (text - s == len) return 0;
  int n = strlen("!parser ");
  if (text - s + n >= len ||
      strncmp((char *)text, "!parser ", n) != 0) return 0;
  for (text += n; text - s < len && *text == ' '; text++) {};
  if (text - s + 2 >= len) return 0;
  if (strncmp((char *)text, "on", 2) == 0) {
    text += 2;
    for (; text - s < len && *text == ' '; text++) {};
    return (*text == '\n' || text - s >= len) ? 2 : 0;
  }
  if (text - s + 3 >= len) return 0;
  if (strncmp((char *)text, "off", 3) == 0) {
    text += 3;
    for (; text - s < len && *text == ' '; text++) {};
    return (*text == '\n' || text - s >= len) ? 1 : 0;
  }
  if (text - s + 5 >= len) return 0;
  if (strncmp((char *)text, "verb ", 5) == 0) {
    text += 5;
    for (; text - s < len && *text == ' '; text++) {};
    if (*text == '\n' || text - s >= len) return 0;
    name = text;
    for (text++; text - s < len && *text != ' ' && *text != '\n'; text++) {};
    if (text - s < len) {
      parseState.xxKwdList = addString(name, text - name, parseState.xxKwdList);
      parseState.xxKwdType = addInteger(1, parseState.xxKwdType);
      return 3;
    }
  }
  if (text - s + 7 >= len) return 0;
  if (strncmp((char *)text, "defcmd ", 7) == 0) {
    text += 7;
    for (; text - s < len && *text == ' '; text++) {};
    if (*text == '\n' || text - s >= len) return 0;
    name = text;
    for (text++; text - s < len && *text != ' ' && *text != '\n'; text++) {};
    if (text - s < len) {
      parseState.xxKwdList = addString(name, text - name, parseState.xxKwdList);
      parseState.xxKwdType = addInteger(2, parseState.xxKwdType);
      return 4;
    }
  }
  if (strncmp((char *)text, "defenv ", 7) == 0) {
    text += 7;
    for (; text - s < len && *text == ' '; text++) {};
    if (*text == '\n' || text - s >= len) return 0;
    name = text;
    for (text++; text - s < len && *text != ' ' && *text != '\n'; text++) {};
    if (text - s < len) {
      parseState.xxKwdList = addString(name, text - name, parseState.xxKwdList);
      parseState.xxKwdType = addInteger(3, parseState.xxKwdType);
      return 5;
    }
  }
  if (text - s + 9 >= len) return 0;
  if (strncmp((char *)text, "verbatim ", 9) == 0) {
    text += 9;
    for (; text - s < len && *text == ' '; text++) {};
    if (*text == '\n' || text - s >= len) return 0;
    name = text;
    for (text++; text - s < len && *text != ' ' && *text != '\n'; text++) {};
    if (text - s < len) {
      parseState.xxVerbatimList = addString(name, text - name, parseState.xxVerbatimList);
      return 6;
    }
  }
  return 0;
}

static int mkDollar(int c)
{
  int retval = c, cat;

  if (parseState.xxMathMode != 1) {
    c = xxgetc();
    cat = tex_catcode(c);

    if (cat == 3)
      retval = TWO_DOLLARS;
    else
      xxungetc(c);
  }
  return retval;
}

static int mkMarkup(int c)
{
    uint8_t st0[INITBUFSIZE];
    uint8_t *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    uint8_t *stext = st0, *bp = st0;
    UBool isError = false;
    int retval = 0, cat;

    do {
      TEXT_PUSH(c);
      c = xxgetc();
      cat = tex_catcode(c);
    } while (cat == 11);

    /* One non-alpha allowed */
    if (bp - stext == 1) {
      TEXT_PUSH(c);
      TEXT_PUSH('\0');
      retval = MACRO;
    } else {
      TEXT_PUSH('\0');
      retval = KeywordLookup((char *)stext);
      if (retval == VERB)
        retval = mkVerb(c); /* This makes the yylval */
      else if (retval == VERB2)
        retval = mkVerb2(stext, c); /* ditto */
      else if (cat != 10) /* Eat a space, but keep other terminators */
        xxungetc(c);
    }
    if (retval != VERB) {
      PRESERVE_SV(yylval = mkString((char*)stext));
    }
    if(st1) free(st1);
    return retval;
}

static int mkSpecial(int c, int cat)
{
  uint8_t st0[INITBUFSIZE];
  uint8_t *st1 = NULL;
  unsigned int nstext = INITBUFSIZE;
  uint8_t *stext = st0, *bp = st0;
  UBool isError = false;
  if (cat == 12) {
    if (c == '[')
      parseState.xxBracketDepth++;
    else if (c == ']')
      parseState.xxBracketDepth--;
  }
  TEXT_PUSH(c);
  PRESERVE_SV(yylval = mkString2(stext, bp - stext));
  setAttrib(yylval, install("catcode"), Rf_ScalarInteger(cat));
  setAttrib(yylval, R_ClassSymbol, mkString("LaTeX2item"));
  if(st1) free(st1);
  return SPECIAL;
}

static int mkVerb(int c)
{
    uint8_t st0[INITBUFSIZE];
    uint8_t *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    uint8_t *stext = st0, *bp = st0;
    UBool isError = false;
    int delim = c;

    TEXT_PUSH('\\'); TEXT_PUSH('v'); TEXT_PUSH('e'); TEXT_PUSH('r'); TEXT_PUSH('b');
    TEXT_PUSH(c);
    while (((c = xxgetc()) != delim) && c != R_EOF) TEXT_PUSH(c);
    if (c != R_EOF) TEXT_PUSH(c);

    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if(st1) free(st1);
    return VERB;
}

static int mkVerb2(const uint8_t *s, int c)
{
  uint8_t st0[INITBUFSIZE];
  uint8_t *st1 = NULL;
  unsigned int nstext = INITBUFSIZE;
  uint8_t *stext = st0, *bp = st0;
  UBool isError = false;
  const uint8_t *macro = s;
  char buffer[256];

  while (*s) TEXT_PUSH(*s++);

  /* eat whitespace */
  while (tex_catcode(c) == 10) {
    TEXT_PUSH(c);
    c = xxgetc();
  }
  if (c == '}') {
    snprintf(buffer, sizeof(buffer), "unexpected '}'\n'%s' has no argument", macro);
    yyerror(buffer);
    return parseError();
  } else if (c != '{') { /* it's a one-character argument */
    TEXT_PUSH(c);
  } else {
    int depth = 1;
    do {
      TEXT_PUSH(c);
      c = xxgetc();
      if (c == '{') depth++;
      else if (c == '}') depth--;
    } while (depth > 0 && c != R_EOF);

    if (c == R_EOF) {
      snprintf(buffer, sizeof(buffer), "unexpected END_OF_INPUT\n'%s' is still open", macro);
      yyerror(buffer);
      return parseError();
    } else
      TEXT_PUSH(c);
  }
  PRESERVE_SV(yylval = mkString2(stext, bp - stext));
  if(st1) free(st1);
  return VERB;
}

static int mkVerbEnv(void)
{
    uint8_t st0[INITBUFSIZE];
    uint8_t *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    uint8_t *stext = st0, *bp = st0;
    UBool isError = false;
    int matched = 0, i;
    int c;

    while ((c = xxgetc()) != R_EOF && CHAR(STRING_ELT(parseState.xxInVerbEnv, 0))[matched]) {
      TEXT_PUSH(c);
      if (c == CHAR(STRING_ELT(parseState.xxInVerbEnv, 0))[matched])
          matched++;
      else
          matched = 0;
    }

    if (c == R_EOF || !CHAR(STRING_ELT(parseState.xxInVerbEnv, 0))[matched] ) {
      xxungetc(c);
      for (i = matched-1; i >= 0; i--)
          xxungetc(*(--bp));
      RELEASE_SV(parseState.xxInVerbEnv);
      parseState.xxInVerbEnv = NULL;
    }

    PRESERVE_SV(yylval = mkString2(stext, bp - stext));

    if (st1) free(st1);
    return VERB;
}

static int yylex(void)
{
    int tok = token();

    if (parseState.xxDebugTokens) {
      Rprintf("%d:%d: %s", yylloc.first_line, yylloc.first_column, yytname[YYTRANSLATE(tok)]);
      if (tok > 255 && tok != END_OF_INPUT)
        Rprintf(": %s", CHAR(STRING_ELT(yylval, 0)));
      Rprintf("\n");
    }
    setlastloc();
    return tok;
}

static void PushState(void) {
    if (busy) {
      ParseState *prev = malloc(sizeof(ParseState));
      if (prev == NULL) error("unable to allocate in PushState");
      PutState(prev);
      parseState.prevState = prev;
    } else
      parseState.prevState = NULL;
    busy = TRUE;
}

static void PopState(void) {
    if (parseState.prevState) {
      ParseState *prev = parseState.prevState;
      UseState(prev);
      free(prev);
    } else
      busy = FALSE;
}

SEXP parseLatex(SEXP args)
{
  args = CDR(args);

  SEXP s = R_NilValue, text;
  ParseStatus status;

#if DEBUGMODE
  yydebug = 1;
#endif

  ParseErrorLine = 0;
  ParseErrorMsg[0] = '\0';

  PushState();

  text = CAR(args);  args = CDR(args);

  if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    error(_("invalid '%s' value"), "verbose");

  parseState.xxDebugTokens = asInteger(CAR(args)); args = CDR(args);
  parseState.xxVerbatimList = CAR(args); args = CDR(args);
  parseState.xxKwdList = CAR(args); args = CDR(args);
  parseState.xxKwdType = CAR(args); args = CDR(args);
  parseState.xxCodepoints = CAR(args); args = CDR(args);
  parseState.xxCatcodes = CAR(args); args = CDR(args);
  parseState.recover = LOGICAL(CAR(args))[0]; args = CDR(args);

  nextchar_parse = translateCharUTF8(STRING_ELT(text, 0));
  ptr_getc = char_getc;
  s = ParseLatex(&status);
  PopState();

  if (status != PARSE_OK) parseError();
  return s;
}

/* R package initialization code */

static const R_ExternalMethodDef ExtEntries[] = {
  {"C_parseLatex", (DL_FUNC) &parseLatex, 8},

  {NULL, NULL, 0}
};

void
R_init_parseLatex(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, NULL, ExtEntries);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

