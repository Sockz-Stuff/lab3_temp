%{
int yylex();
void yyerror(const char *s);
#include <stdio.h>
%}

%union{
int integerVal;
char* stringVal;
}

%token SUB
%token NEG
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
%token NUMBER
%token <stringVal> IDENTIFIER
%token <integerVal> DIGIT
%%



Program:    
	    | Functions 
            {printf("Program->Functions\n");}
            ;
	    
	    
Functions:  Function Functions
 	    {printf("Functions->Function Functions\n");}
	    
	    
Function:   Function IDENTIFIER SEMICOLON BEGINPARAMS Declaration SEMICOLON ENDPARAMS BEGINLOCALS Declaration SEMICOLON ENDLOCALS BEGINBODY Statement SEMICOLON ENDBODY
            {printf("Function->Function IDENTIFIER SEMICOLON BEGINPARAMS Declaration SEMICOLON ENDPARAMS BEGINLOCALS Declaration SEMICOLON ENDLOCALS BEGINBODY Statement SEMICOLON ENDBODY\n");}
            ;	    
	    
	    
Declaration:    IDENTIFIER COLON INTEGER SEMICOLON
                {printf("Declaration->IDENTIFIER COLON INTEGER SEMICOLON\n");}
                | IDENTIFIER COLON ARRAY L_SQUARE_BRACKET DIGIT R_SQUARE_BRACKET INTEGER SEMICOLON
                {printf("Declaration->IDENTIFIER COLON ARRAY L_SQUARE_BRACKET DIGIT R_SQUARE_BRACKET INTEGER SEMICOLON\n");}
                ;	    
		
		
Statement:     Var ASSIGN Expression SEMICOLON
               {printf("Statement->Var ASSIGN Expression SEMICOLON\n");}
               | IF Bool_Exp THEN Statement SEMICOLON ENDIF SEMICOLON
               {printf("Statement->IF Bool_Exp THEN Statment SEMICOLON ENDIF SEMICOLON\n");}                       
               | IF Bool_Exp THEN Statement SEMICOLON ELSE Statement SEMICOLON
               {printf("Statement->IF Bool_Exp THEN Statment SEMICOLON ELSE Statement SEMICOLON\n");}
               | Loop_Statement SEMICOLON
               {printf("Statement->Loop_Statement SEMICOLON\n");}
               | READ Var SEMICOLON
               {printf("Statement->READ Var SEMICOLON\n");}
               | WRITE Var SEMICOLON
               {printf("Statement->WRITE Var SEMICOLON\n");}
               | CONTINUE SEMICOLON
               {printf("Statement->CONTINUE SEMICOLON\n");}
               | BREAK SEMICOLON
               {printf("Statement->BREAK SEMICOLON\n");}
               | RETURN Expression SEMICOLON
               {printf("Statement->RETURN Expression SEMICOLON\n");}
               ;	
	       
	       
Expression:   Mult_Exp
		{printf("MultExp\n");}
	      |Mult_Exp ADD Expression
              {printf("Expression->Mult_Exp ADD Expression\n");}
              | Mult_Exp SUB Expression
              {printf("Expression->Mult_Exp SUB Expression\n");}
              ;
	       
	       
Loop_Statement:   DO BEGINLOOP Statement SEMICOLON ENDLOOP WHILE Bool_Exp
                  {printf("Loop_Statement->DO BEGINLOOP Statement SEMICOLON ENDLOOP WHILE Bool_Exp\n");}
                  | WHILE Bool_Exp BEGINLOOP Statement SEMICOLON ENDLOOP
                  {printf("Loop_Statement->WHILE Bool_Exp BEGINLOOP Statement SEMICOLON ENDLOOP\n");}
                  ;	       


Comp:   LT
        {printf("Comp->LT\n");}
        | LTE
        {printf("Comp->LTE\n");}
        | GT
        {printf("Comp->GT\n");}
        | GTE
        {printf("Comp->GTE\n");}
        | EQUIV
        {printf("Comp->EQUUIV\n");}
        ;
        
	
Term:   Var
        {printf("Term->Var\n");}
        | DIGIT
        {printf("Term->DIGIT\n");}
        | NEG DIGIT
        {printf("Term->NEG DIGIT\n");}
        | NEG L_PAREN Expression R_PAREN
        {printf("Term->NEG L_PAREN Expression R_PAREN\n");}
        | L_PAREN Expression R_PAREN
        {printf("Term->L_PAREN Expression R_PAREN\n");}
        | IDENTIFIER L_PAREN Expression R_PAREN
        {printf("Term->IDENTIFIER L_PAREN Expression R_PAREN\n");}
        ;
	
	
Var:    IDENTIFIER
        {printf("Var->IDENTIFIER\n");}
        | IDENTIFIER L_SQUARE_BRACKET Expression R_SQUARE_BRACKET
        {printf("Var->IDENTIFIER L_SQUARE_BRACKET Expression R_SQUARE_BRACKET\n");}
        ;
        
	
Mult_Exp:   Term
	    {printf("Term\n");}
	    |Term MULT Mult_Exp
            {printf("Mult_Exp->Term MULT Mult_Exp\n");}
            | Term DIV Mult_Exp
            {printf("Mult_Exp->Term DIV Mult_Exp\n");}
            | Term MOD Mult_Exp
            {printf("Mult_Exp->Term MOD Mult_Exp\n");}
            ;
            
              
Bool_Exp:   Relation_Exp
            {printf("Bool_Exp->Relation_Expression\n");}
            | Bool_Exp OR Relation_Exp
            {printf("Bool_Exp->Bool_Exp OR Relation_Exp\n");}
            ;
	    
            
Relation_Exp:   NOT Expression Comp Expression
                {printf("Relation_Exp->NOT Expression Comp Expression\n");}
                | Expression Comp Expression
                {printf("Relation_Exp->Expression Comp Expression\n");}
                | TRUE
                {printf("Relation_Exp->TRU\n");}
                | FALSE
                {printf("Relation_Exp->FALE\n");}
                | L_PAREN Bool_Exp R_PAREN
                {printf("Relation_Exp->L_PAREN Bool_Exp R_PAREN\n");}
                | Relation_Exp AND Relation_Exp
                {printf("Relation_Exp->Relation_Exp AND Relation_Exp\n");}
                ;

            
%%
main(int argc, char **argv)
{

  yyparse();
}
                

    
        
