/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 1



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     END_OF_INPUT = 258,
     ERROR = 259,
     MACRO = 260,
     TEXT = 261,
     COMMENT = 262,
     BEGIN = 263,
     END = 264,
     VERB = 265,
     VERB2 = 266,
     TWO_DOLLARS = 267,
     SPECIAL = 268
   };
#endif
/* Tokens.  */
#define END_OF_INPUT 258
#define ERROR 259
#define MACRO 260
#define TEXT 261
#define COMMENT 262
#define BEGIN 263
#define END 264
#define VERB 265
#define VERB2 266
#define TWO_DOLLARS 267
#define SPECIAL 268




/* Copy the first part of user declarations.  */
#line 1 "gramLatex.y"

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2024  The R Core Team
 *  Copyright (C) 2010 Duncan Murdoch
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

static int	R_ParseError = 0; /* Line where parse error occurred */
static int	R_ParseErrorCol;  /* Column of start of token where parse error occurred */
/* the next is currently unused */
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
static char	R_ParseErrorMsg[PARSE_ERROR_SIZE] = "";
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */
static char	R_ParseContext[PARSE_CONTEXT_SIZE] = "";
static int	R_ParseContextLast = 0; /* last character in context buffer */
static int	R_ParseContextLine; /* Line in input of the above */

static NORET void parseError()
{
  error("parse error at %d:%d: %s", R_ParseError, R_ParseErrorCol, R_ParseErrorMsg);
}

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

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

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
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
	if (N)								\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).first_byte   = YYRHSLOC (Rhs, 1).first_byte;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	  (Current).last_byte    = YYRHSLOC (Rhs, N).last_byte;		\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	  (Current).first_byte   = (Current).last_byte =		\
	    YYRHSLOC (Rhs, 0).last_byte;				\
	}								\
    while (0)

/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

/* Functions used in the parsing process */

static void     GrowList(SEXP, SEXP);
static int      KeywordLookup(const char *);
static SEXP     NewList(void);
static SEXP     makeSrcref(YYLTYPE *);
static UChar32  xxgetc(void);
static int      xxungetc(int);
static void     xxincomplete(SEXP, YYLTYPE *);

/* Internal lexer / parser state variables */


static char const yyunknown[] = "unknown macro"; /* our message, not bison's */

typedef struct ParseState ParseState;
struct ParseState {
  int	xxlineno, xxbyteno, xxcolno;
  int	xxDebugTokens;  /* non-zero causes debug output to R console */
SEXP	Value;
int	xxinitvalue;
SEXP	xxInVerbEnv;    /* Are we currently in a verbatim environment? If
 so, this is the string to end it. If not,
 this is NULL */
SEXP	xxVerbatimList;/* A STRSXP containing all the verbatim environment names */
SEXP	xxVerbList;    /* A STRSXP containing all the verbatim command names */
SEXP  xxCodepoints;  /* A vector of codepoints with catcodes given */
SEXP  xxCatcodes;    /* Corresponding catcodes */

SEXP mset; /* precious mset for protecting parser semantic values */
ParseState *prevState;
};

static Rboolean busy = FALSE;
static ParseState parseState;

#define PRESERVE_SV(x) R_PreserveInMSet((x), parseState.mset)
#define RELEASE_SV(x)  R_ReleaseFromMSet((x), parseState.mset)

/* Routines used to build the parse tree */

static SEXP	xxnewlist(SEXP);
static SEXP	xxlist(SEXP, SEXP);
static void	xxsavevalue(SEXP, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static void xxgettext(char *, size_t, SEXP);
static SEXP xxenv(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxmath(SEXP, YYLTYPE *, Rboolean);
static SEXP	xxblock(SEXP, YYLTYPE *);
static void	xxSetInVerbEnv(SEXP);

static int	mkMarkup(int);
static int	mkText(int);
static int 	mkComment(int);
static int  mkSpecial(int, int);
static int  mkVerb(int);
static int	mkVerb2(const uint8_t *, int);
static int  mkVerbEnv(void);
static int	mkDollar(int);

static SEXP R_LatexTagSymbol = NULL;

#define YYSTYPE		SEXP



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 1
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 340 "gramLatex.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

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
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
    YYLTYPE yyls;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  30
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   147

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  17
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  11
/* YYNRULES -- Number of rules.  */
#define YYNRULES  34
/* YYNRULES -- Number of states.  */
#define YYNSTATES  51

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   268

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    16,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    14,     2,    15,     2,     2,     2,     2,
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
       5,     6,     7,     8,     9,    10,    11,    12,    13
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     6,     8,    10,    12,    14,    16,    19,
      22,    25,    27,    30,    32,    34,    36,    38,    40,    42,
      44,    46,    48,    50,    53,    56,    57,    68,    71,    75,
      78,    82,    85,    89,    92
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      18,     0,    -1,    19,     3,    -1,     3,    -1,     1,    -1,
      21,    -1,    25,    -1,    26,    -1,    19,    21,    -1,    19,
      25,    -1,    19,    26,    -1,    21,    -1,    20,    21,    -1,
       6,    -1,     7,    -1,     5,    -1,    13,    -1,    10,    -1,
      11,    -1,    23,    -1,    27,    -1,     6,    -1,    13,    -1,
      22,     6,    -1,    22,    13,    -1,    -1,     8,    14,    22,
      15,    24,    19,     9,    14,    22,    15,    -1,     8,     1,
      -1,    16,    20,    16,    -1,    16,     1,    -1,    12,    20,
      12,    -1,    12,     1,    -1,    14,    19,    15,    -1,    14,
      15,    -1,    14,     1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   216,   216,   217,   218,   221,   222,   223,   224,   225,
     226,   228,   229,   231,   232,   233,   234,   235,   236,   237,
     238,   240,   241,   242,   243,   245,   245,   249,   251,   252,
     254,   255,   257,   258,   259
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "END_OF_INPUT", "ERROR", "MACRO", "TEXT",
  "COMMENT", "BEGIN", "END", "VERB", "VERB2", "TWO_DOLLARS", "SPECIAL",
  "'{'", "'}'", "'$'", "$accept", "Init", "Items", "nonMath", "Item",
  "envname", "environment", "@1", "math", "displaymath", "block", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   123,   125,    36
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    17,    18,    18,    18,    19,    19,    19,    19,    19,
      19,    20,    20,    21,    21,    21,    21,    21,    21,    21,
      21,    22,    22,    22,    22,    24,    23,    23,    25,    25,
      26,    26,    27,    27,    27
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     1,     1,     1,     1,     2,     2,
       2,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     0,    10,     2,     3,     2,
       3,     2,     3,     2,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     4,     3,    15,    13,    14,     0,    17,    18,     0,
      16,     0,     0,     0,     0,     5,    19,     6,     7,    20,
      27,     0,    31,     0,    11,    34,    33,     0,    29,     0,
       1,     2,     8,     9,    10,    21,    22,     0,    30,    12,
      32,    28,    23,    24,    25,     0,     0,     0,     0,     0,
      26
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    13,    14,    23,    15,    37,    16,    45,    17,    18,
      19
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -39
static const yytype_int16 yypact[] =
{
      38,   -39,   -39,   -39,   -39,   -39,    11,   -39,   -39,    64,
     -39,    16,    75,     7,    50,   -39,   -39,   -39,   -39,   -39,
     -39,     3,   -39,   133,   -39,   -39,   -39,    85,   -39,   121,
     -39,   -39,   -39,   -39,   -39,   -39,   -39,    -2,   -39,   -39,
     -39,   -39,   -39,   -39,   -39,   109,    97,    -6,     3,    27,
     -39
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -39,   -39,   -10,    24,    -9,   -38,   -39,   -39,   -12,    -8,
     -39
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      24,    27,    33,    24,    42,    32,    34,    30,    48,    35,
      49,    43,    20,    44,    39,    33,    36,    25,    32,    34,
      39,     3,     4,     5,     6,    21,     7,     8,     9,    10,
      11,    26,    12,    42,    33,    46,    29,    32,    34,     1,
      43,     2,    50,     3,     4,     5,     6,     0,     7,     8,
       9,    10,    11,    31,    12,     3,     4,     5,     6,     0,
       7,     8,     9,    10,    11,    22,    12,     0,     0,     3,
       4,     5,     6,     0,     7,     8,    28,    10,    11,     0,
       3,     4,     5,     6,     0,     7,     8,     0,    10,    11,
       3,     4,     5,     6,     0,     7,     8,     9,    10,    11,
      40,    12,     3,     4,     5,     6,    47,     7,     8,     9,
      10,    11,     0,    12,     3,     4,     5,     6,     0,     7,
       8,     9,    10,    11,     0,    12,     3,     4,     5,     6,
       0,     7,     8,     0,    10,    11,     0,    41,     3,     4,
       5,     6,     0,     7,     8,    38,    10,    11
};

static const yytype_int8 yycheck[] =
{
       9,    11,    14,    12,     6,    14,    14,     0,    14,     6,
      48,    13,     1,    15,    23,    27,    13,     1,    27,    27,
      29,     5,     6,     7,     8,    14,    10,    11,    12,    13,
      14,    15,    16,     6,    46,    45,    12,    46,    46,     1,
      13,     3,    15,     5,     6,     7,     8,    -1,    10,    11,
      12,    13,    14,     3,    16,     5,     6,     7,     8,    -1,
      10,    11,    12,    13,    14,     1,    16,    -1,    -1,     5,
       6,     7,     8,    -1,    10,    11,     1,    13,    14,    -1,
       5,     6,     7,     8,    -1,    10,    11,    -1,    13,    14,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    -1,    16,     5,     6,     7,     8,    -1,    10,
      11,    12,    13,    14,    -1,    16,     5,     6,     7,     8,
      -1,    10,    11,    -1,    13,    14,    -1,    16,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,    14
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     5,     6,     7,     8,    10,    11,    12,
      13,    14,    16,    18,    19,    21,    23,    25,    26,    27,
       1,    14,     1,    20,    21,     1,    15,    19,     1,    20,
       0,     3,    21,    25,    26,     6,    13,    22,    12,    21,
      15,    16,     6,    13,    15,    24,    19,     9,    14,    22,
      15
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
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



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
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
	    /* Fall through.  */
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

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {
      case 5: /* "MACRO" */
#line 212 "gramLatex.y"
	{ RELEASE_SV((*yyvaluep)); };
#line 1294 "gramLatex.tab.c"
	break;
      case 6: /* "TEXT" */
#line 212 "gramLatex.y"
	{ RELEASE_SV((*yyvaluep)); };
#line 1299 "gramLatex.tab.c"
	break;
      case 7: /* "COMMENT" */
#line 212 "gramLatex.y"
	{ RELEASE_SV((*yyvaluep)); };
#line 1304 "gramLatex.tab.c"
	break;
      case 8: /* "BEGIN" */
#line 212 "gramLatex.y"
	{ RELEASE_SV((*yyvaluep)); };
#line 1309 "gramLatex.tab.c"
	break;
      case 9: /* "END" */
#line 212 "gramLatex.y"
	{ RELEASE_SV((*yyvaluep)); };
#line 1314 "gramLatex.tab.c"
	break;

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;
/* Location data for the look-ahead symbol.  */
YYLTYPE yylloc;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;

  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[2];

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;
#if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 0;
#endif

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
	YYSTACK_RELOCATE (yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
  *++yylsp = yylloc;
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
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 216 "gramLatex.y"
    { xxsavevalue((yyvsp[(1) - (2)]), &(yyloc)); YYACCEPT; ;}
    break;

  case 3:
#line 217 "gramLatex.y"
    { xxsavevalue(NULL, &(yyloc)); YYACCEPT; ;}
    break;

  case 4:
#line 218 "gramLatex.y"
    { PRESERVE_SV(parseState.Value = R_NilValue);  YYABORT; ;}
    break;

  case 5:
#line 221 "gramLatex.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 6:
#line 222 "gramLatex.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 7:
#line 223 "gramLatex.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 8:
#line 224 "gramLatex.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 9:
#line 225 "gramLatex.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 10:
#line 226 "gramLatex.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 11:
#line 228 "gramLatex.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 12:
#line 229 "gramLatex.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 13:
#line 231 "gramLatex.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), TEXT, &(yyloc)); ;}
    break;

  case 14:
#line 232 "gramLatex.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), COMMENT, &(yyloc)); ;}
    break;

  case 15:
#line 233 "gramLatex.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), MACRO, &(yyloc)); ;}
    break;

  case 16:
#line 234 "gramLatex.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), SPECIAL, &(yyloc)); ;}
    break;

  case 17:
#line 235 "gramLatex.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), VERB, &(yyloc)); ;}
    break;

  case 18:
#line 236 "gramLatex.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), VERB, &(yyloc)); ;}
    break;

  case 19:
#line 237 "gramLatex.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 20:
#line 238 "gramLatex.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 21:
#line 240 "gramLatex.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 22:
#line 241 "gramLatex.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 23:
#line 242 "gramLatex.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 24:
#line 243 "gramLatex.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 25:
#line 245 "gramLatex.y"
    { xxSetInVerbEnv((yyvsp[(3) - (4)])); ;}
    break;

  case 26:
#line 246 "gramLatex.y"
    { (yyval) = xxenv((yyvsp[(3) - (10)]), (yyvsp[(6) - (10)]), (yyvsp[(9) - (10)]), &(yyloc));
                                              RELEASE_SV((yyvsp[(1) - (10)]));
                                              RELEASE_SV((yyvsp[(7) - (10)])); ;}
    break;

  case 27:
#line 249 "gramLatex.y"
    { xxincomplete((yyvsp[(1) - (2)]), &(yylsp[(1) - (2)])); ;}
    break;

  case 28:
#line 251 "gramLatex.y"
    { (yyval) = xxmath((yyvsp[(2) - (3)]), &(yyloc), FALSE); ;}
    break;

  case 29:
#line 252 "gramLatex.y"
    { xxincomplete(mkString("$"), &(yylsp[(1) - (2)])); ;}
    break;

  case 30:
#line 254 "gramLatex.y"
    { (yyval) = xxmath((yyvsp[(2) - (3)]), &(yyloc), TRUE); ;}
    break;

  case 31:
#line 255 "gramLatex.y"
    { xxincomplete(mkString("$$"), &(yylsp[(1) - (2)])); ;}
    break;

  case 32:
#line 257 "gramLatex.y"
    { (yyval) = xxblock((yyvsp[(2) - (3)]), &(yyloc)); ;}
    break;

  case 33:
#line 258 "gramLatex.y"
    { (yyval) = xxblock(NULL, &(yyloc)); ;}
    break;

  case 34:
#line 259 "gramLatex.y"
    { xxincomplete(mkString("{"), &(yylsp[(1) - (2)])); ;}
    break;


/* Line 1267 of yacc.c.  */
#line 1805 "gramLatex.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
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

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[0] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the look-ahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 261 "gramLatex.y"


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
  char ename[256];
  xxgettext(ename, sizeof(ename), begin);

#if DEBUGVALS
  Rprintf("xxenv(begin=%p, body=%p, end=%p)", begin, body, end);
#endif
  if (!isNull(body)) {
    PRESERVE_SV(ans = PairToVectorList(CDR(body)));
    RELEASE_SV(body);
  } else
    PRESERVE_SV(ans = allocVector(VECSXP, 0));

  /* FIXME:  check that begin and end match */
  setAttrib(ans, install("envname"), mkString(ename));
  RELEASE_SV(begin);
  if (!isNull(end))
    RELEASE_SV(end);
  setAttrib(ans, install("srcref"), makeSrcref(lloc));
  setAttrib(ans, R_LatexTagSymbol, mkString("ENVIRONMENT"));
  setAttrib(ans, R_ClassSymbol, mkString("LaTeX2item"));
#if DEBUGVALS
  Rprintf(" result: %p\n", ans);
#endif
  return ans;
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
    setAttrib(ans, R_LatexTagSymbol,
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
  setAttrib(ans, R_LatexTagSymbol, mkString("BLOCK"));
  setAttrib(ans, R_ClassSymbol, mkString("LaTeX2item"));
#if DEBUGVALS
  Rprintf(" result: %p\n", ans);
#endif
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
  char buffer[256];
  char ename[256];
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
	    setAttrib(VECTOR_ELT(parseState.Value, 0), R_LatexTagSymbol, mkString("TEXT"));
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
    setAttrib(item, R_LatexTagSymbol, mkString(yytname[YYTRANSLATE(type)]));
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

  if(npush) c = pushback[--npush];
  else {
    uint8_t utf8_bytes[4];  // Buffer for UTF-8 character (max 4 bytes)
    int i, byte_count = 0;
    int first_byte = (uint8_t)ptr_getc();
    if (first_byte == (uint8_t)EOF) {
      c = EOF;
    } else {
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

  R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
  R_ParseContext[R_ParseContextLast] = (char) c;

  if (c == '\n') {
    parseState.xxlineno += 1;
    parseState.xxcolno = 1;
    parseState.xxbyteno = 1;
  } else {
    parseState.xxcolno++;
    parseState.xxbyteno++;
  }

  if (c == '\t') parseState.xxcolno = ((parseState.xxcolno + 6) & ~7) + 1;

  R_ParseContextLine = parseState.xxlineno;

  return c;
}

static UChar32 xxungetc(int c)
{
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    parseState.xxlineno = prevlines[prevpos];
    parseState.xxbyteno = prevbytes[prevpos];
    parseState.xxcolno  = prevcols[prevpos];
    prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;

    R_ParseContextLine = parseState.xxlineno;

    R_ParseContext[R_ParseContextLast] = '\0';
    /* macOS requires us to keep this non-negative */
    R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE - 1)
	% PARSE_CONTEXT_SIZE;
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
    state->xxVerbList = parseState.xxVerbList;
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
    parseState.xxVerbList = state->xxVerbList;
    parseState.prevState = state->prevState;
}

static void InitSymbols(void)
{
  if (!R_LatexTagSymbol)
	  R_LatexTagSymbol = install("latex_tag");
}

static SEXP ParseLatex(ParseStatus *status)
{
    InitSymbols();

    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';

    parseState.xxInVerbEnv = NULL;

    parseState.xxlineno = 1;
    parseState.xxcolno = 1;
    parseState.xxbyteno = 1;

    PROTECT(parseState.mset = R_NewPreciousMSet(50));

    npush = 0;

    parseState.Value = R_NilValue;

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

static
SEXP R_ParseLatex(SEXP text, ParseStatus *status)
{
    nextchar_parse = translateCharUTF8(STRING_ELT(text, 0));
    ptr_getc = char_getc;
    return ParseLatex(status);
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
    { 0,	   0	      }
    /* All other markup macros are rejected. */
};

/* Record the longest # directive here */
#define DIRECTIVE_LEN 7

static int KeywordLookup(const char *s)
{
  int i;
  for (i = 0; keywords[i].name; i++) {
    if (strcmp(keywords[i].name, s) == 0)
      return keywords[i].token;
  }

  for (i = 0; i < length(parseState.xxVerbList); i++) {
    if (strcmp(CHAR(STRING_ELT(parseState.xxVerbList, i)), s) == 0)
      return VERB2;
  }

  return MACRO;
}

static void xxincomplete(SEXP what, YYLTYPE *where)
{
  char buffer[256], start[256];
  PROTECT(what);
  xxgettext(start, sizeof(start), what);
  snprintf(buffer, sizeof(buffer), "%s\n  '%s' at %d:%d is still open",
           R_ParseErrorMsg, start, where->first_line, where->first_column);
  yyerror(buffer);
  UNPROTECT(1);
  parseError();
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
    "$undefined",	"input",
    "LATEXMACRO",	"macro",
    "ESCAPE",	"macro",
    0,		0
    };
  static char const yyunexpected[] = "syntax error, unexpected ";
  static char const yyexpecting[] = ", expecting ";
  static char const yyshortunexpected[] = "unexpected %s";
  static char const yylongunexpected[] = "unexpected %s '%s'";
  char *expecting;

  R_ParseError     = yylloc.first_line;
  R_ParseErrorCol  = yylloc.first_column;

  if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
    int i, translated = FALSE;
    /* Edit the error message */
    expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    if (expecting) *expecting = '\0';
    for (i = 0; yytname_translations[i]; i += 2) {
      if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
        if (yychar < 256)
          snprintf(R_ParseErrorMsg, PARSE_ERROR_SIZE,
                   _(yyshortunexpected),
                   i/2 < YYENGLISH ? _(yytname_translations[i+1])
                     : yytname_translations[i+1]);
        else
          snprintf(R_ParseErrorMsg, PARSE_ERROR_SIZE,
                   _(yylongunexpected),
                   i/2 < YYENGLISH ? _(yytname_translations[i+1])
                     : yytname_translations[i+1],
                       CHAR(STRING_ELT(yylval, 0)));
        translated = TRUE;
        break;
      }
    }
    if (!translated) {
      if (yychar < 256)
        snprintf(R_ParseErrorMsg, PARSE_ERROR_SIZE,
                 _(yyshortunexpected),
                 s + sizeof yyunexpected - 1);
      else
        snprintf(R_ParseErrorMsg, PARSE_ERROR_SIZE,
                 _(yylongunexpected),
                 s + sizeof yyunexpected - 1, CHAR(STRING_ELT(yylval, 0)));
    }
    if (expecting) {
      translated = FALSE;
      for (i = 0; yytname_translations[i]; i += 2) {
        if (!strcmp(expecting + sizeof yyexpecting - 1, yytname_translations[i])) {
          strcat(R_ParseErrorMsg, _(yyexpecting));
          strcat(R_ParseErrorMsg, i/2 < YYENGLISH ? _(yytname_translations[i+1])
                   : yytname_translations[i+1]);
          translated = TRUE;
          break;
        }
      }
      if (!translated) {
        strcat(R_ParseErrorMsg, _(yyexpecting));
        strcat(R_ParseErrorMsg, expecting + sizeof yyexpecting - 1);
      }
    }
  } else if (!strncmp(s, yyunknown, sizeof yyunknown-1)) {
    snprintf(R_ParseErrorMsg, PARSE_ERROR_SIZE,
             "%s '%s'", s, CHAR(STRING_ELT(yylval, 0)));
  } else {
    snprintf(R_ParseErrorMsg, PARSE_ERROR_SIZE,"%s", s);
  }
}

#define TEXT_PUSH(c) do {		    \
	size_t nc = bp - stext;		    \
	if (nc >= nstext - 4) {             \
	    uint8_t *old = stext;              \
	    nstext *= 2;		    \
	    stext = malloc(nstext);	    \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), parseState.xxlineno);\
	    memmove(stext, old, nc);        \
	    if(st1) free(st1);		    \
	    st1 = stext;		    \
	    bp = stext+nc; }		    \
	U8_APPEND(stext, nc, nstext, c, isError);		    \
  bp = stext+nc; \
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
  if (u_hasBinaryProperty(c, UCHAR_ALPHABETIC)) return 11;
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

    cat = tex_catcode(c);

    switch (cat) {
    	case 0:  return mkMarkup(c);
    	case 1:  return '{';
    	case 2:  return '}';
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

    do {
    	TEXT_PUSH(c);
    	c = xxgetc();
    } while (tex_catcode(c) == 11);

    xxungetc(c);
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

    PRESERVE_SV(yylval = mkString2(stext,  bp - stext));
    if(st1) free(st1);
    return COMMENT;
}

static int mkDollar(int c)
{
    int retval = c, cat;

    c = xxgetc();
    cat = tex_catcode(c);

    if (cat == 3)
        retval = TWO_DOLLARS;
    else
        xxungetc(c);

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
    int delim = '}';

    while (*s) TEXT_PUSH(*s++);

    TEXT_PUSH(c);
    while (((c = xxgetc()) != delim) && c != R_EOF) TEXT_PUSH(c);
    if (c != R_EOF) TEXT_PUSH(c);

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
    if ( !CHAR(STRING_ELT(parseState.xxInVerbEnv, 0))[matched] ) {
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

  R_ParseError = 0;
  R_ParseErrorMsg[0] = '\0';

  PushState();

  text = CAR(args);	args = CDR(args);

  if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    error(_("invalid '%s' value"), "verbose");

  parseState.xxDebugTokens = asInteger(CAR(args)); args = CDR(args);
  parseState.xxVerbatimList = CAR(args); args = CDR(args);
  parseState.xxVerbList = CAR(args); args = CDR(args);
  parseState.xxCodepoints = CAR(args); args = CDR(args);
  parseState.xxCatcodes = CAR(args); args = CDR(args);

  s = R_ParseLatex(text, &status);

  PopState();

  if (status != PARSE_OK) parseError();
  return s;
}

static const R_ExternalMethodDef ExtEntries[] = {
  {"C_parseLatex", (DL_FUNC) &parseLatex, 6},

  {NULL, NULL, 0}
};

void
R_init_parseLatex(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, NULL, ExtEntries);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}


