/* tableaux.l */
%{
/*
NE PAS  ecrire
#include "iimp.tab.h"
*/
 extern int ligcour;
%}

%x COMMENT  
debcom            "/*"
endcom            "*/"
sep               [ \t]+
sign              [\+-]
lettre            [A-Za-z]
caractere         [A-Za-z0-9\-\_]  
%%
<INITIAL>{debcom}          {BEGIN COMMENT;}
<COMMENT>{endcom}          {BEGIN INITIAL;}
<COMMENT>.                 {;}
skip                       {yylval.NO=Nalloc();
                           (yylval.NO)->codop = SK;
                           tycour=creer_type(0,T_CMD);
                           (yylval.NO)->FG = NULL;
                           (yylval.NO)->FD = NULL;
                           (yylval.NO)->ETIQ = malloc(yyleng + 1);
                           strcpy((yylval.NO)->ETIQ,yytext);
                           type_copy(&((yylval.NO)->typno),creer_type(0,T_CMD));
                           return(SK);}
new{sep}array{sep}of       {return(NEWAR);}
array{sep}of               {return(T_AR);}
integer                    {type_copy(&(yylval.TYP),creer_type(0,T_INT));
                                                       return(T_INT);
                            }
if                         {return(IF);}
then                       {return(TH);}
else                       {return(EL);}
var                        {return(VAR);}
while                      {return(WH);}
do                         {return(DO);}
"+"                        {return(PL);}
"-"                        {return(MO);}
"*"                        {return(MU);}
"and"                      {return(AND);}
"or"                       {return(OR);}
"not"                      {return(NOT);}
"<"                        {return(LT);}
"="                        {return(EQ);}
<INITIAL>{lettre}{caractere}* {yylval.NO=Nalloc();
                           (yylval.NO)->codop = V; /* variable (type inconnu) */
                           (yylval.NO)->FG = NULL;
                           (yylval.NO)->FD = NULL;
                           (yylval.NO)->ETIQ = malloc(yyleng + 1);
                           strcpy((yylval.NO)->ETIQ,yytext);
                                                      return(V);}   /* variable                          */
0|[1-9][0-9]*              {yylval.NO=Nalloc(); /*entier positif; AF:cas negatif*/
                           (yylval.NO)->codop = I;  /* entier */
                           printf("analyse lex, token I,l1 \n");
                           tycour=creer_type(0,T_INT);
                           type_copy(&((yylval.NO)->typno),creer_type(0,T_INT));
                           printf("analyse lex,token I,l2 \n");
                           (yylval.NO)->FG = NULL;
                           (yylval.NO)->FD = NULL;
                           (yylval.NO)->ETIQ = malloc(yyleng + 1);
                           strcpy((yylval.NO)->ETIQ,yytext);
                           printf("analyse lex, ETIQ :%s \n",(yylval.NO)->ETIQ);
                           (yylval.NO)->codop = I;
                                                      return(I);}     /* entier */
:=                         {return(AF);}
";"                        {return(SE);}
{sep}                      {;}
\n                         {ligcour++;}
.                          {return(yytext[0]);}
%%
int yywrap()
{return(1);}

