%{
   enum yytokentype {
    BEGIN_PAR = 245
    END_PAR = 246
    BEGIN_LOC = 247
    END_LOC = 248
    BEGIN_BOD = 249
    END_BOD = 250 
    DO = 251
    WHILE = 252
    LOOP_BEGIN = 253
    LOOP_END = 254
    RETURN = 256
    CONTINUE = 257
    ADD = 258,
    SUBTRACT = 259,
    MULT = 260,
    DIVI = 261,
    MOD = 262,
    ASSIGN = 263,
    BREAK = 264,
    COMMENT = 265,
    INTEGER = 266,
    FUNCTION = 267,
    IF = 268,
    ELSEIF = 269,
    ENDIF = 270,
    WRITE = 271,
    READ = 272,
    LESST = 273,
    LESSTEQ = 274,
    GREATT = 275,
    GREATTEQ = 276,
    EQUIV = 277,
    NOTEQ = 278,
    LOOP = 279,
    L_PAREN = 280,
    R_PAREN = 281,
    L_SQUARE_BRACKET = 282,
    R_SQUARE_BRACKET = 283,
    COLON = 284,
    SEMICOLON = 285,
    COMMA = 286
    NEG = 287
    TRU = 288
    FAL = 289
    NOT = 290
    AND = 291
    OR = 292
    ARRAY = 293
    THEN = 294
    };
    
    int yylval;
%}

%%
[+-]?[0-9] {yylval = atio{yytext}; return DIGIT;}
[+-]?(\.{DIGIT}+)|({DIGIT}+(\.{DIGIT}*)?([eE]?[0-9]+)?) {yyval = atio{yytext}; return DIGITS;}
[a-zA-Z0-9_]*[a-zA-Z0-9]* {return IDENTIFIER;}
"/bp" {return BEGIN_PAR;}
"/ep" {return END_PAR;}
"/bl" {return BEGIN_LOC;}
"/el" {return END_LOC;}
"/bb" {return BEGIN_BOD;}
"/eb" {return END_BOD;}
"/do" {return DO;}
"/while" {return WHILE;}
"/loop_b" {return LOOP_BEGIN;}
"/loop_e" {return LOOP_END;}
"RETURN" {return RETURN;}
"CONT" {return CONTINUE;}
"/add" {return ADD;}
"/sub" {return SUBTRACT;}
"/mult" {return MULT;}
"/div" {return DIVI;}
"/mod" {return MOD;}
"=" {return ASSIGN;}
"/end" {return BREAK;}
"#" {return COMMENT;}
"it" {return INTEGER;}
"func" {return FUNCTION;}
"/if" {return IF;}
"/elif" {return ELSEIF;}
"/fi" {return ENDIF;}
"/write" {return WRITE;}
"/read" {return READ;}
"/lt" {return LESST;}
"/lteq" {return LESSTEQ;}
"/gt" {return GREATT;}
"/gteq" {return GREATTEQ;}
"==" {return EQUIV;}
"!=" {return NOTEQ;}
"/loop" {return LOOP;}
"(" {return L_PAREN;}
")" {return R_PAREN;}
"[" {return L_SQUARE_BRACKET;}
"]" {return R_SQUARE_BRACKET;}
":" {return COLON;}
";" {return SEMICOLON;}
"," {return COMMA;}
"-" {return NEG;}
"True" {return TRU;}
"False" {return FAL;}
"!" {return NOT;}
"/and" {return AND;}
"/or" {return OR;}
"/arr" {return ARRAY;}
"/then" {return THEN;}
\n {return EOL;}
[\t] {/*ignore whitespace*/}
. {printf("Mystery character %c\n", *yytext);}

%%
