%{
int yylex();
void yyerror(const char *s);
#include <stdio.h>
%}

%union{
int integerVal;
char* stringVal;
}
%error-verbose
%start Program
%token SUB
%token ADD
%token MULT
%token DIV
%token MOD
%token EQUIV
%token NOTEQ
%token LT
%token GT
%token LTE
%token GTE
%token SEMICOLON
%token COLON
%token COMMA
%token L_PAREN
%token R_PAREN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token ASSIGN
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token BEGINLOOP
%token ENDLOOP
%token FUNCTION
%token BEGINPARAMS
%token ENDPARAMS
%token BEGINLOCALS
%token ENDLOCALS
%token BEGINBODY
%token ENDBODY
%token CONTINUE
%token READ
%token WRITE
%token AND
%token OR
%token NOT
%token TRUE
%token FALSE
%token RETURN
%token BREAK
%token <stringVal> IDENTIFIER
%token <integerVal> DIGITS


%%

Program:    { printf("Program -> epsilon\n"); }
    		| Program function { printf("Program -> Program function\n"); }
    ;

function:   FUNCTION IDENTIFIER SEMICOLON BEGINPARAMS declaration_loop ENDPARAMS BEGINLOCALS declaration_loop ENDLOCALS BEGINBODY statement_loop ENDBODY
            { printf("function -> FUNCTION IDENTIFIER SEMICOLON ");
            printf("BEGINPARAMS declaration_loop ENDPARAMS ");
            printf("BEGINLOCALS declaration_loop ENDLOCALS ");
            printf("BEGINBODY statement_loop ENDBODY\n"); }
            ;

declaration_loop: { printf("declaration_loop -> epsilon\n"); }
    			  | declaration_loop declaration SEMICOLON { printf("declaration_loop -> declaration_loop declaration SEMICOLON\n"); }
    			  ;

declaration:	 IDENTIFIER COLON INTEGER { printf("declaration -> id_loop COLON INTEGER\n"); }
				 | IDENTIFIER COLON ARRAY L_SQUARE_BRACKET DIGITS R_SQUARE_BRACKET OF INTEGER { printf("declaration -> id_loop COLON ARRAY L_SQUARE_BRACKET DIGITS %d R_SQUARE_BRACKET OF INTEGER\n", $5); }
				;

statement_loop: statement SEMICOLON { printf("statement_loop -> statement SEMICOLON\n"); }
				| statement_loop statement SEMICOLON { printf("statement_loop -> statement_loop statement SEMICOLON\n"); }
				;



statement:	  var ASSIGN expression { printf("statement -> var ASSIGN expression\n"); }
		| IF bool_expr THEN statement_loop ENDIF { printf("statement -> IF bool_expr THEN statement_loop ENDIF\n"); }
		| IF bool_expr THEN statement_loop ELSE statement_loop ENDIF { printf("statement -> IF bool_expr THEN statement_loop ELSE statement_loop ENDIF\n"); }
		| WHILE bool_expr BEGINLOOP statement_loop ENDLOOP { printf("statement -> WHILE bool_expr BEGINLOOP statement_loop ENDLOOP\n"); }
		| DO BEGINLOOP statement_loop ENDLOOP WHILE bool_expr { printf("statement -> DO BEGINLOOP statement_loop ENDLOOP WHILE bool_expr\n"); }
		| READ var_loop { printf("statement -> READ var_loop\n"); }
		| WRITE var_loop { printf("statement -> WRITE var_loop\n"); }
		| CONTINUE { printf("statement -> CONTINUE\n"); }
		| RETURN expression { printf("statement -> RETURN expression\n"); }
		;

bool_expr:	  Relation_Exps { printf("bool_expr -> Relation_Exps\n"); }
        | bool_expr OR Relation_Exps { printf("bool_expr -> bool_expr OR Relation_Exps\n"); }
        ;

Relation_Exps:	  		Relation_Exp { printf("Relation_Exps -> Relation_Exp\n"); }
        			  | Relation_Exps AND Relation_Exp { printf("Relation_Exps -> Relation_Exps AND Relation_Exp\n"); }
                      ;

Relation_Exp:	  expression comp expression { printf("Relation_Exp -> expression comp expression\n"); }
				  | NOT expression comp expression { printf("Relation_Exp -> NOT expression comp expression\n"); }
				  | TRUE { printf("Relation_Exp -> TRUE\n"); }
				  | FALSE { printf("Relation_Exp -> FALSE\n"); }
				  | L_PAREN bool_expr R_PAREN { printf("Relation_Exp -> L_PAREN bool_expr R_PAREN\n"); }
				  ;

comp:	  LT { printf("comp -> LT\n"); }
		| GT { printf("comp -> GT\n"); }
		| LTE { printf("comp -> LTE\n"); }
		| GTE { printf("comp -> GTE\n"); }
		| NOTEQ { printf("comp -> NEQ\n"); }
		| EQUIV { printf("comp -> EQ\n"); }
		;

expression: mult_expr { printf("expression -> mult_expr\n"); }
        	| expression ADD mult_expr { printf("expression -> expression ADD mult_expr\n"); }
        	| expression SUB mult_expr { printf("expression -> expression SUB mult_expr\n"); }
        ;

expression_loop:    expression { printf("expression_loop -> expression"); }
    				| expression_loop COMMA expression { printf("expression_loop -> expression_loop COMMA expression"); }
    				;

mult_expr:	  term  { printf("mult_expr -> term\n"); }
        	  | mult_expr MULT term { printf("mult_expr -> mult_expr MULT term\n"); }
			  | mult_expr DIV term { printf("mult_expr -> mult_expr DIV term\n"); }
			  | mult_expr MOD term { printf("mult_expr -> mult_expr MOD term\n"); }
        ;


term:	var { printf("term -> var\n"); }
		| SUB var { printf("term -> SUB var\n"); }
		| DIGITS { printf("term -> DIGITS %d\n", $1); }
		| SUB DIGITS { printf("term -> SUB DIGITS %d\n", $2); }
		| L_PAREN expression R_PAREN { printf("term -> L_PAREN expression R_PAREN\n"); }
		| SUB L_PAREN expression R_PAREN { printf("term -> SUB L_PAREN expression R_PAREN\n"); }
		| IDENTIFIER L_PAREN expression_loop R_PAREN { printf("term -> IDENTIFIER %s L_PAREN expression_loop R_PAREN\n", $1); }
		;

var_loop:	  var { printf("var_loop -> var\n"); }
			  | var_loop COMMA var { printf("var_loop -> var_loop COMMA var\n"); }
			  ;		
		
var:	  IDENTIFIER { printf("var -> IDENTIFIER %s\n", $1); }
		| IDENTIFIER L_SQUARE_BRACKET expression R_SQUARE_BRACKET { printf("var -> IDENTIFIER %s L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n", $1); }
		;

%%

int main(int argc, char ** argv) {

  yyparse();
}

void yyerror(const char *msg) {
    printf("Error at Line %d, Columnn %d: %s \n", row, col, msg);
}	
        
