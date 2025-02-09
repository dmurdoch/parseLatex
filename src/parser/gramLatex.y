// This grammar assumes bison v. 3.x or higher.

%define parse.error verbose
%{
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

static NORET void parseError(void);

static NORET void parseError(void)
{
  error("Parse error at %d:%d: %s", ParseErrorLine,
        ParseErrorCol, ParseErrorMsg);
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
static int      KeywordLookup(const char *);
static SEXP     NewList(void);
static SEXP     makeSrcref(YYLTYPE *);
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
  SEXP  xxVerbList;       /* A STRSXP containing all the
                             verbatim command names */
  SEXP  xxCodepoints;     /* A vector of codepoints with catcodes given */
  SEXP  xxCatcodes;       /* Corresponding catcodes */
  int   xxGetArgs;        /* Collecting args to macro */
  int   xxIgnoreKeywords; /* Ignore keywords while getting args */
  int   xxBraceDepth;     /* Brace depth important while
                             collecting args */
  int   xxBracketDepth;   /* So is bracket depth */

  SEXP  mset; /* precious mset for protecting parser semantic values */
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
static SEXP  xxblock(SEXP, YYLTYPE *);
static void  xxSetInVerbEnv(SEXP);
static SEXP  xxpushMode(int, int);
static void  xxpopMode(SEXP);
static void  xxArg(SEXP);

static int magicComment(const uint8_t *, int);
static SEXP addString(const uint8_t *, size_t, SEXP);

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

%}
/* token-table is needed for yytname[] to be defined in recent bison versions */
%token-table

%token    END_OF_INPUT ERROR
%token    MACRO
%token    TEXT COMMENT
%token    BEGIN END VERB VERB2 NEWENV NEWCMD END_OF_ARGS
%token    TWO_DOLLARS
%token    SPECIAL

/* Recent bison has <> to represent all of the destructors below, but we don't assume it */

/* I think we need to list everything here which occurs before the last item in a
   pattern, just in case the last item is unmatched and we need to back out.  But
   it is safe to list more, so we do. */

%destructor { RELEASE_SV($$); } MACRO TEXT COMMENT BEGIN END

%%

Init:    Items END_OF_INPUT    { xxsavevalue($1, &@$);
                                 YYACCEPT; }
  |  END_OF_INPUT              { xxsavevalue(NULL, &@$);
                                 YYACCEPT; }
  |  error                     { PRESERVE_SV(parseState.Value = R_NilValue);
                                 YYABORT; }
  ;

Items:    Item         { $$ = xxnewlist($1); }
  |  math              { $$ = xxnewlist($1); }
  |  displaymath       { $$ = xxnewlist($1); }
  |  Items Item        { $$ = xxlist($1, $2); }
  |  Items math        { $$ = xxlist($1, $2); }
  |  Items displaymath { $$ = xxlist($1, $2); }

nonMath:  Item         { $$ = xxnewlist($1); }
  |  nonMath Item      { $$ = xxlist($1, $2); }

Item:    TEXT       { xxArg(NULL); $$ = xxtag($1, TEXT, &@$); }
  | COMMENT         { $$ = xxtag($1, COMMENT, &@$); }
  | MACRO           { xxArg(NULL);
                      $$ = xxtag($1, MACRO, &@$); }
  | SPECIAL         { xxArg($1);
                      $$ = xxtag($1, SPECIAL, &@$); }
  | VERB            { $$ = xxtag($1, VERB, &@$); }
  | VERB2           { $$ = xxtag($1, VERB, &@$); }
  | block           { xxArg(NULL); $$ = $1; }
  | environment     { $$ = $1; }
  | newdefine       { $$ = $1; }

envname: TEXT    { $$ = xxnewlist(xxtag($1, TEXT, &@1)); }
  |      SPECIAL { $$ = xxnewlist(xxtag($1, SPECIAL, &@1)); }
  |      envname TEXT    { $$ = xxlist($1, xxtag($2, TEXT, &@2)); }
  |      envname SPECIAL { $$ = xxlist($1, xxtag($2, SPECIAL, &@2)); }

begin:   BEGIN '{' envname '}' { xxSetInVerbEnv($3);
                                 RELEASE_SV($1);
                                 $$ = $3; }

environment:  begin Items END '{' envname '}'
                          { $$ = xxenv($1, $2, $5, &@$);
                            RELEASE_SV($3); }
  |           begin END '{' envname '}'
                          { $$ = xxenv($1, NULL, $4, &@$);
                            RELEASE_SV($2); }
  |           begin error { xxincompleteBegin($1, &@1); }

math:   '$' nonMath '$'   { $$ = xxmath($2, &@$, FALSE); }
  |     '$' error         { xxincomplete(mkString("$"), &@1); }

displaymath:    TWO_DOLLARS nonMath TWO_DOLLARS
                          { $$ = xxmath($2, &@$, TRUE); }
  |             TWO_DOLLARS error
                          { xxincomplete(mkString("$$"), &@1); }

block:    '{'  Items  '}' { $$ = xxblock($2, &@$); }
  |  '{' '}'              { $$ = xxblock(NULL, &@$); }
  | '{' error             { xxincomplete(mkString("{"), &@1); }

newdefine :  NEWCMD       { $$ = xxpushMode(2, 1); }
               Items END_OF_ARGS
                          { xxpopMode($2);
                            $$ = xxnewdef(xxtag($1, MACRO, &@1),
                                        $3, &@$); }
  |          NEWENV       { $$ = xxpushMode(3, 1); }
               Items END_OF_ARGS
                          {  xxpopMode($2);
                             $$ = xxnewdef(xxtag($1, MACRO, &@1),
                                        $3, &@$); }

%%

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

static SEXP xxpushMode(int getArgs,
                       int ignoreKeywords) {
    SEXP ans;

    PRESERVE_SV(ans = allocVector(INTSXP, 4));
    INTEGER(ans)[0] = parseState.xxGetArgs;
    INTEGER(ans)[1] = parseState.xxIgnoreKeywords;
    INTEGER(ans)[2] = parseState.xxBraceDepth;
    INTEGER(ans)[3] = parseState.xxBracketDepth;
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
    state->xxGetArgs = parseState.xxGetArgs;
    state->xxIgnoreKeywords = parseState.xxIgnoreKeywords;
    state->xxBraceDepth = parseState.xxBraceDepth;
    state->xxBracketDepth = parseState.xxBracketDepth;
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

static SEXP ParseLatex(ParseStatus *status)
{
    LatexTagSymbol = install("latex_tag");

    parseState.xxInVerbEnv = NULL;
    parseState.xxGetArgs = 0;
    parseState.xxIgnoreKeywords = 0;
    parseState.xxBraceDepth = 0;
    parseState.xxBracketDepth = 0;

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
    { "\\newenvironment", NEWENV },
    { "\\renewenvironment", NEWENV },
    { "\\newcommand", NEWCMD },
    { "\\renewcommand", NEWCMD },
    { "\\providecommand", NEWCMD },
    { "\\def", NEWCMD },
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

  for (i = 0; i < length(parseState.xxVerbList); i++) {
    if (strcmp(CHAR(STRING_ELT(parseState.xxVerbList, i)), s) == 0)
      return VERB2;
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
      parseState.xxVerbList = addString(name, text - name, parseState.xxVerbList);
      return 3;
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
      return 4;
    }
  }
  return 0;
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
    int depth = 1;
    const uint8_t *macro = s;

    while (*s) TEXT_PUSH(*s++);

    do {
      TEXT_PUSH(c);
      c = xxgetc();
      if (c == '{') depth++;
      else if (c == '}') depth--;
    } while (depth > 0 && c != R_EOF);

    if (c == R_EOF) {
      char buffer[256];
      snprintf(buffer, sizeof(buffer), "unexpected END_OF_INPUT\n'%s' is still open", macro);
      yyerror(buffer);
      parseError();
    } else
      TEXT_PUSH(c);

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
  parseState.xxVerbList = CAR(args); args = CDR(args);
  parseState.xxCodepoints = CAR(args); args = CDR(args);
  parseState.xxCatcodes = CAR(args); args = CDR(args);

  nextchar_parse = translateCharUTF8(STRING_ELT(text, 0));
  ptr_getc = char_getc;
  s = ParseLatex(&status);

  PopState();

  if (status != PARSE_OK) parseError();
  return s;
}

/* R package initialization code */

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

