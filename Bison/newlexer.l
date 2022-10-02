%{
#define FLEX_DEBUG 1		//gia na min fainontai kai pragmata toy flex
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>  
#include "syntax.tab.h"

char str_buf[256];
int error_counter = 0;
void token_print(int token_id);
int str_to_int(char *str);
char str_to_char(char *yytext);
void yyerror(const char* err);

%}

%option noyywrap				
%option case-insensitive	
%option yylineno			



%x STRING					

%x SINGLE_LINE_COMMENT


ID 							_?[a-zA-Z_][a-zA-Z0-9]+
ICONST						(0[0H][1-9][0-9A-F]+)|(0[0B][1-9][0-9]+)|(0|[0-9]*[1-9]*)
RCONST						(((0[0B][A-Z1-9]*)|(0[0H][A-Z1-9]*)|[1-9][0-9]*|0)?(\.([0-9]*[1-9A-Z]*)|[1-9A-Z-]*))|([A-Z0-9-]*)
CCONST						\'[ "#$%'()*+΄_./0-9:;<=>?@A-Z[\\\]^_`a-z{|}~]\'|'\\[nftrbv]\'


%%



"PROGRAM"					{token_print(T_PROGRAM); return T_PROGRAM;}
"CONST"						{token_print(T_CONST); return T_CONST;}
"TYPE"						{token_print(T_TYPE); return T_TYPE;}
"ARRAY"						{token_print(T_ARRAY); return T_ARRAY;}
"SET"						{token_print(T_SET); return T_SET;}
"OF"						{token_print(T_OF); return T_OF;}
"RECORD"					{token_print(T_RECORD); return T_RECORD;}
"VAR"						{token_print(T_VAR); return T_VAR;}
"FORWARD"					{token_print(T_FORWARD); return T_FORWARD;}
"FUNCTION"					{token_print(T_FUNCTION); return T_FUNCTION;}
"PROCEDURE"					{token_print(T_PROCEDURE); return T_PROCEDURE;}
"INTEGER"					{token_print(T_INTEGER); return T_INTEGER;}
"REAL"						{token_print(T_REAL); return T_REAL;}
"BOOLEAN"					{token_print(T_BOOLEAN); return T_BOOLEAN;}
"CHAR"						{token_print(T_CHAR); return T_CHAR;}
"BEGIN"						{token_print(T_BEGIN); return T_BEGIN;}
"END"						{token_print(T_END); return T_END;}
"IF"						{token_print(T_IF); return T_IF;}
"THEN"						{token_print(T_THEN); return T_THEN;}
"ELSE"						{token_print(T_ELSE); return T_ELSE;}
"WHILE"						{token_print(T_WHILE); return T_WHILE;}
"DO"						{token_print(T_DO); return T_DO;}
"FOR"						{token_print(T_FOR); return T_FOR;}
"DOWNTO"					{token_print(T_DOWNTO); return T_DOWNTO;}
"TO"						{token_print(T_TO); return T_TO;}
"WITH"						{token_print(T_WITH); return T_WITH;}
"READ"						{token_print(T_READ); return T_READ;}
"WRITE"						{token_print(T_WRITE); return T_WRITE;}

"TRUE"						{token_print(T_BCONST); return T_BCONST;}
"FALSE"						{token_print(T_BCONST); return T_BCONST;}
						
	
{ID}						{token_print(T_ID); yylval.strval = strdup(yytext); return T_ID;}


{ICONST}					{token_print(T_ICONST); yylval.intval = str_to_int(yytext); return T_ICONST;}
{RCONST}					{token_print(T_RCONST); yylval.realval = str_to_int(yytext); return T_RCONST;}	
{CCONST}					{token_print(T_CCONST); yylval.charval = str_to_char(yytext); return T_CCONST;}		 



\"							{ strcpy(str_buf, ""); BEGIN(STRING);} 
<STRING>\"					{ BEGIN(INITIAL); printf("Found string: %s, \n", str_buf); yylval.strval = strdup(str_buf); return T_SCONST;}
<STRING>(\\\n|\\\r\n)		{ 	}
<STRING>(\\.|[^"\\])		{ 
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
<STRING>(\n|\r\n)			{ yyerror("New line found without \\. ");}
<STRING><<EOF>>				{ yyerror("End of string without closing it. ");}
<STRING>.					{ yyerror("Uknown character.");}



\{										{ printf("comment found\n"); BEGIN(SINGLE_LINE_COMMENT);}
<SINGLE_LINE_COMMENT>\}					{ BEGIN(INITIAL); printf("comment ended\n"); }
<SINGLE_LINE_COMMENT>(\\\n|\\\r\n)		{ }		//den mas endiaferei ti exei mesa sta comment	
<SINGLE_LINE_COMMENT>.					{ }		//den mas endiaferei ti exei mesa sta comment			
		


">"|">="|"<"|"<="|"<>"		{token_print(T_RELOP); return T_RELOP;}
"+"|"-"						{token_print(T_ADDOP); return T_ADDOP;}
" OR "						{token_print(T_OROP); return T_OROP;}
"*"|"/"|"DIV"|"MOD"|"AND"	{token_print(T_MULDIVANDOP); return T_MULDIVANDOP;}
" NOT "						{token_print(T_NOTOP); return T_NOTOP;}
" IN "						{token_print(T_INOP); return T_INOP;}


"("							{token_print(T_LPAREN); return T_LPAREN;}
")"							{token_print(T_RPAREN); return T_RPAREN;}
";"							{token_print(T_SEMI); return T_SEMI;}
" . "						{token_print(T_DOT); return T_DOT;}
","							{token_print(T_COMMA); return T_COMMA;}
"="							{token_print(T_EQU); return T_EQU;}
":"							{token_print(T_COLON); return T_COLON;}
"["							{token_print(T_LBRACK); return T_LBRACK;}
"]"							{token_print(T_RBRACK); return T_RBRACK;}
":="						{token_print(T_ASSIGN); return T_ASSIGN;}
".."						{token_print(T_DOTDOT); return T_DOTDOT;}


[ \t]						{	}
[ \n]						{	}			//νέα γραμμή για linux
\r\n						{	}			//νέα γραμμή για windows


<<EOF>>						{token_print(T_EOF); return T_EOF;}						
.							{yyerror("Unknown character.");}

%%

void token_print( int token_id){
	
	if( FLEX_DEBUG == 0){
        return;
    }
    
	if(token_id == T_ICONST){

		//SYNARTHSH METATROPH SE BASH 10
		printf("Found integer token %s {%d} at line %d, base 10: %d\n",yytext,token_id, yylineno, str_to_int(yytext));
	}
	else if(token_id == T_RCONST){

		printf("Found real token %s {%d} at line %d, base 10: %d\n",yytext,token_id, yylineno, str_to_int(yytext));
	}else{
	printf("Found token %s {%d} at line %d\n",yytext,token_id, yylineno);
	}
}

//yytext-> δείχνει την παρούσα αναγνωρισμένη λέξη
//yylineno-> δείχνει σε ποια γραμμή(αν το %option yylineno είναι ενεργοποιημένο)

int str_to_int(char *str) {

	if(strncmp(str, "OH", 2) == 0){

		return (int) strtoll(&str[2],NULL,16);	//Μετατροπή δεκαεξαδικού αριθμού σε δεκαδικό
	}
	else if(strncmp(str, "OB", 2) == 0){

		return(int) strtoll(&str[2],NULL, 2);	//Μετατροπή δυαδικού αριθμού σε δεκαδικό
	}
	else{
		return(int) strtoll(str, NULL, 10);
	}
}

char str_to_char(char *yytext){

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

void yyerror(const char* err){
	error_counter++;
	printf("Error %s at line %d token %s ", err, yylineno, yytext);
	
	if(error_counter == 5){
		printf("Maximum errors found\n");
		exit(EXIT_FAILURE);
	}

	yyless(1); 				//sinartisi poy diagrafei tous xaraktires poy den mporei na diavasei
}