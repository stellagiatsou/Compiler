%{
	
#include "tokens.h"			
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


char str_buf[256];
int error_counter = 0;
void token_print(int token_id);
int str_to_int(char *str);
void yyerror(const char* err);
int DecimalIntFunc(char *str); 
int BinaryIntFunc(char *str); 
int HexDecimalIntFunc(char *str);
double RealDecimalFunc( char *str);

%}

%option noyywrap				
%option case-insensitive	
%option yylineno			


%x STRING					

%x COMMENT


ID 				(_?[a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9])|([a-zA-Z]*)
ICONST			(0[H][1-9A-F][0-9A-F]*)|(0[B][1][0-1]*)|([1-9][0-9]*|0)
RCONST			(0[H][1-9A-F][0-9A-F]*\.[0-9A-F]+)|(0[B][1][0-1]*(\.[0-1]+))|(([1-9][0-9]*|0)\E([-]?[1-9][0-9]*|[-][0]))|(([1-9][0-9]*|0)?\.([0-9]+))
CCONST			\'[ "#$%'()*+΄_./0-9:;<=>?@A-Z[\\\]^_`a-z{|}~]\'|'\\[nftrbv]\'

%%


"PROGRAM"											{token_print(T_PROGRAM); return T_PROGRAM;}
"CONST"												{token_print(T_CONST); return T_CONST;}
"TYPE"												{token_print(T_TYPE); return T_TYPE;}
"ARRAY"												{token_print(T_ARRAY); return T_ARRAY;}
"SET"												{token_print(T_SET); return T_SET;}
"OF"												{token_print(T_OF); return T_OF;}
"RECORD"											{token_print(T_RECORD); return T_RECORD;}
"VAR"												{token_print(T_VAR); return T_VAR;}
"FORWARD"											{token_print(T_FORWARD); return T_FORWARD;}
"FUNCTION"											{token_print(T_FUNCTION); return T_FUNCTION;}
"PROCEDURE"											{token_print(T_PROCEDURE); return T_PROCEDURE;}
"INTEGER"											{token_print(T_INTEGER); return T_INTEGER;}
"REAL"												{token_print(T_REAL); return T_REAL;}
"BOOLEAN"											{token_print(T_BOOLEAN); return T_BOOLEAN;}
"CHAR"												{token_print(T_CHAR); return T_CHAR;}
"BEGIN"												{token_print(T_BEGIN); return T_BEGIN;}
"END"												{token_print(T_END); return T_END;}
"IF"												{token_print(T_IF); return T_IF;}
"THEN"												{token_print(T_THEN); return T_THEN;}
"ELSE"												{token_print(T_ELSE); return T_ELSE;}
"WHILE"												{token_print(T_WHILE); return T_WHILE;}
"DO"												{token_print(T_DO); return T_DO;}
"FOR"												{token_print(T_FOR); return T_FOR;}
"DOWNTO"											{token_print(T_DOWNTO); return T_DOWNTO;}
"TO"												{token_print(T_TO); return T_TO;}
"WITH"												{token_print(T_WITH); return T_WITH;}
"READ"												{token_print(T_READ); return T_READ;}
"WRITE"												{token_print(T_WRITE); return T_WRITE;}

"TRUE"												{token_print(T_BCONST); return T_BCONST;}
"FALSE"												{token_print(T_BCONST); return T_BCONST;}
						
	
{ID}												{token_print(T_ID); return T_ID;}

(0|[1-9][0-9]*)										{printf("LINE [%d] | Found decimal integer token '%s' {id: %d, %s}, base 10: %d\n", yylineno, yytext, T_ICONST, TOKEN_NAME[T_ICONST], DecimalIntFunc(yytext)); return T_ICONST;}
(0[B][1][0-1]*)										{printf("LINE [%d] | Found binary integer token '%s' {id: %d, %s}, base 10: %d\n", yylineno, yytext, T_ICONST, TOKEN_NAME[T_ICONST], BinaryIntFunc(yytext)); return T_ICONST;}
(0[H][1-9A-F][0-9A-F]*)								{printf("LINE [%d] | Found hex integer token '%s' {id: %d, %s}, base 10: %d\n", yylineno, yytext, T_ICONST, TOKEN_NAME[T_ICONST], HexDecimalIntFunc(yytext)); return T_ICONST;}

(([1-9][0-9]*|0)?\.([0-9]+))						{printf("Found real token '%s' {id: %d, %s} | LINE[%d] | base 10: %lf\n", yytext, T_RCONST, TOKEN_NAME[T_RCONST], yylineno, RealDecimalFunc(yytext)); return T_RCONST;}
(([1-9][0-9]*|0)\E([-]?[1-9][0-9]*|[-][0]))			{printf("Found real exp token '%s' {id: %d, %s} | LINE[%d] | base 10: %lf\n", yytext, T_RCONST, TOKEN_NAME[T_RCONST], yylineno, RealDecimalFunc(yytext)); return T_RCONST;}
(0[B][1][0-1]*(\.[0-1]+))							{printf("Found real binary token '%s' {id: %d, %s} | LINE[%d]\n", yytext, T_RCONST, TOKEN_NAME[T_RCONST], yylineno); return T_RCONST;}
(0[H][1-9A-F][0-9A-F]*\.[0-9A-F]+)					{printf("Found real hex token '%s' {id: %d, %s} | LINE[%d]\n", yytext, T_RCONST, TOKEN_NAME[T_RCONST], yylineno); return T_RCONST;}

{CCONST}											{token_print(T_CCONST); return T_CCONST;}	


\"													{ strcpy(str_buf, ""); BEGIN(STRING);}
<STRING>\"											{ BEGIN(INITIAL); printf("STRING: %s {id: %d, %s} at line %d \n", str_buf, T_SCONST, TOKEN_NAME[T_SCONST], yylineno); return T_SCONST;}	 
<STRING>(\n|\r\n)									{ yyerror("New line found without \\"); }		//BEGIN(INITIAL); yyerror("New line found without \\"); | strcpy(str_buf, ""); 
<STRING>(\\\n|\\\r\n)								{ }	
<STRING>(\\.|[^"\\])								{ 
														if(strlen(yytext) == 2){
															if(strcmp(yytext, "\\n") == 0){
																strcat(str_buf, "\n");
															} else if(strcmp(yytext, "\\f") == 0){
															strcat(str_buf, "\f");
															} else if(strcmp(yytext, "\\t") == 0){
															strcat(str_buf, "\t");
															} else if(strcmp(yytext, "\\r") == 0){
															strcat(str_buf, "\r");
															} else if(strcmp(yytext, "\\b") == 0){
															strcat(str_buf, "\b");
															} else if(strcmp(yytext, "\\v") == 0){
															strcat(str_buf, "\v");
															} else{
																strcat(str_buf, &yytext[1]);	//escape sto "H\i" -> "Hi"
															} 
														}else{
															strcat(str_buf, yytext);
														}
													}						
<STRING><<EOF>>										{ yyerror("End of string without closing it");}
<STRING>.											{ yyerror("Uknown character");}


\{													{ printf("Line:: %d | Comment found \n", yylineno); BEGIN(COMMENT); }						//sto prwto meros dilwnw char str_buf_comments[256];	strcpy(str_buf_comments, "");
<COMMENT>[^}\n]+									{ }																							//strcat(str_buf_comments, yytext);
<COMMENT>(\n|\r\n)									{ yylineno--; yyerror("Comment not finished with }"); yylineno++; BEGIN(INITIAL); }				
<COMMENT><<EOF>>									{ yyerror("End of comment without closing it");}
<COMMENT>\}											{ BEGIN(INITIAL); printf("Line:: %d | comment ended (same line)\n",yylineno);}				//%s -> str_buf_comments

">"|">="|"<"|"<="|"<>"								{token_print(T_RELOP); return T_RELOP;}
"+"|"-"												{token_print(T_ADDOP); return T_ADDOP;}
" OR "												{token_print(T_OROP); return T_OROP;}
"*"|"/"|"DIV"|"MOD"|"AND"							{token_print(T_MULDIVANDOP); return T_MULDIVANDOP;}
" NOT "												{token_print(T_NOTOP); return T_NOTOP;}
" IN "												{token_print(T_INOP); return T_INOP;}


"("													{token_print(T_LPAREN); return T_LPAREN;}
")"													{token_print(T_RPAREN); return T_RPAREN;}
";"													{token_print(T_SEMI); return T_SEMI;}
"."													{token_print(T_DOT); return T_DOT;}
","													{token_print(T_COMMA); return T_COMMA;}
"="													{token_print(T_EQU); return T_EQU;}
":"													{token_print(T_COLON); return T_COLON;}
"["													{token_print(T_LBRACK); return T_LBRACK;}
"]"													{token_print(T_RBRACK); return T_RBRACK;}
":="												{token_print(T_ASSIGN); return T_ASSIGN;}
".."												{token_print(T_DOTDOT); return T_DOTDOT;}

[ \t]												{	}
[ \n]												{	}			//νέα γραμμή για linux
\r\n												{	}			//νέα γραμμή για windows


<INITIAL><<EOF>>									{token_print(T_EOF); return T_EOF;}						
.													{yyerror("unknown character");}

%%

//MAIN
int main(int argc, char *argv[]){
	//Ανοιγεί το αρχείο
	int token;
	if(argc>1){

		yyin=fopen(argv[1], "r");
		if(yyin== NULL){
			perror("Error opening file");
			return EXIT_FAILURE;
		}
	}
	//Αναγνωρίζουμε κάθε token του αρχείου
	do{											
		token = yylex();
	}while(token!=T_EOF);

	fclose(yyin);			//Κλείσε το αρχείο
	yyterminate();			//τερμάτισε τον Λ.Α.
}

//PRINT FUNCTION
void token_print(int token_id){
	
	if(token_id == T_CCONST){
		printf("LINE [%d] | Found ASCII token/special char '%s' {id: %d, %s}\n",yylineno, yytext, token_id,TOKEN_NAME[token_id]);
	}else{
		printf("LINE [%d] | Found token '%s' {id: %d, %s}\n",yylineno, yytext, token_id,TOKEN_NAME[token_id]);
	}
}

//T_ICONST
int DecimalIntFunc(char *str){
    return atoi(str); 
} 
int BinaryIntFunc( char *str){
   int x;
   x=(int)strtol(&str[2], NULL, 2); 
   return x; 
}
int HexDecimalIntFunc(char *str){
    int x;
    x= (int)strtol(&str[2], NULL, 16);
    return x;
}

//RCONST
double RealDecimalFunc( char *str){
	return atof(str);  
}

//ERROR FUNCTION
void yyerror(const char* err){
	error_counter++;
	
	printf("ERROR(%d): %s | LINE [%d]\n", error_counter, err, yylineno);	// | TOKEN %s   , yytext
		
	if(error_counter == 5){
		printf("Maximum errors found\n");
		exit(EXIT_FAILURE);
	}

	yyless(1); 				
}