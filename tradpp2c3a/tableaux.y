/* ppascal.y */
%{
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "arbre.h"
#include "interp.h"
#include "c3a.h"
/* ------------------VARIABLES GLOBALES -------------------------*/
  NOE syntree;          /* commande  globale                     */
  BILENVTY benvty;      /* environnement global                  */
  int ligcour=1;        /* ligne  courante                       */
  type tycour;          /* type courant                          */
  ENVTY vtycour;        /* var typee courante                    */
/* -------------------------------- -----------------------------*/
  extern int yylex();
%}
%start prog
%union{NOE NO; type TYP; BILENVTY LARGT;}
/* attributs NOE: noeud binaire (IfThEl est "binarise")                                */
/* attributs TYP: contient un type                                                     */
/* attributs LARGT: liste d'arguments types(var globales)                              */
%type <NO> prog atomic_cmd cmd expr T F typed_expr list_def
%type <TYP> type_decl
%type <LARGT> typed_arg block_decl_typed_var block_decl_non_nil_typed_var
/* Non-terminaux MP Ca cmd expr T F Et type_decl Argt L_vart L_vartnn*/
/* P:main_program; Ca:commande atomique; cmd:commande; expr:expression; T:terme; F:facteur;*/
/* Et: expr tableau; */
/* type_decl:TyPe; Argt:argument_type; */
/* L_vart: Liste_variables_typees,   L_vartnn: Liste_variables_typees non-nil */
%token <NO> I B V DEF DEP IDPROC IDFUNC MP AF AFIND AFC JZ JP SK NEWAR SE IND IF TH EL VAR WH DO PL MO MU AND OR NOT LT EQ 
%token <TYP> T_INT T_ERR T_AR T_CMD T_BOO

/* Unités lexicales<NO>: Integer Variable Main_prog                            */
/* Affectation Skip NewArrayOf                                                 */
/* Sequence Index If Then Else Var While Do                                    */
/* Plus Moins Mult And Or Not Lessthan Equal                                   */
/* Unités lexicales<TYP>:
Type_int Type_erreur Type_indefini  Type_array Type_commande                   */


%%
prog:   block_decl_typed_var list_def cmd   { benvty=$1;syntree=$3;YYACCEPT; };

expr:   expr PL T   { $$=Nalloc();
                      $$->codop=PL;
                      $$->FG=$1;
                      $$->FD=$3;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"+"); }
        | expr MO T { $$=Nalloc();
                      $$->codop=MO;
                      $$->FG=$1;
                      $$->FD=$3;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"-"); }
        | expr OR T { $$=Nalloc();
                      $$->codop=OR;
                      $$->FG=$1;
                      $$->FD=$3;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"Or"); }
        | expr LT T { $$=Nalloc();
                      $$->codop=LT;
                      $$->FG=$1;
                      $$->FD=$3;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"Lt"); }
        | expr EQ T { $$=Nalloc();
                      $$->codop=EQ;
                      $$->FG=$1;
                      $$->FD=$3;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"Eq"); }
        | T         { $$=$1; };

T:      T MU  F     { $$=Nalloc();
                      $$->codop=MU;
                      $$->FG=$1;
                      $$->FD=$3;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"*"); }
        | T AND F   { $$=Nalloc();
                      $$->codop=AND;
                      $$->FG=$1;
                      $$->FD=$3;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"And"); }
        | NOT F     { $$=Nalloc();
                      $$->codop=NOT;
                      $$->FG=$2;
                      $$->FD=NULL;
                      $$->ETIQ=malloc(2);
                      strcpy($$->ETIQ,"Not"); }
        | F         { $$=$1; };

F:      '(' expr ')'                    { $$=$2; }
        | I                             { $$=$1; } 
        | V                             { $$=$1; }
        | B                             { $$ = $1; }
        | V '(' block_expr ')'          { }
        | NEWAR type_decl '[' expr ']'  { $$=Nalloc();
                                          $$->codop=NEWAR;
                                          /* calcul_type */
                                          type_copy(&($$->typno),$2); /* DIM,TYPEF sont connus   */
                                          ($$->typno).DIM++; /* mise a jour DIM                  */
                                          $$->FG=NULL;
                                          $$->FD=$4;/* TAILLE a calculer a partir de expr */ }
        | typed_expr                    { $$=$1; };

typed_expr: V  '[' expr ']'             { $$=Nalloc();/* un seul indice                        */
                                          $$->codop=IND;
                                          $$->FG=$1;
                                          $$->FD=$3; }
            | typed_expr '[' expr ']'   { $$=Nalloc();/* plusieurs indices                     */
                                          $$->codop=IND;
                                          $$->FG=$1;
                                          $$->FD=$3; };

cmd:        cmd SE atomic_cmd   { $$=Nalloc();      /* sequence */
                                  $$->codop=SE;
                                  $$->FG=$1;
                                  $$->FD=$3;
                                  $$->ETIQ=malloc(2);
                                  strcpy($$->ETIQ,"Se"); }
            | atomic_cmd        { $$=$1; };

atomic_cmd: typed_expr AF expr                  { $$=Nalloc();
                                                  $$->codop=AF;
                                                  $$->FG=$1;
                                                  $$->FD=$3;
                                                  $$->ETIQ=malloc(2);
                                                  strcpy($$->ETIQ,"Af"); }
            | V AF expr                         { $$=Nalloc();
                                                  $$->codop=AF;
                                                  $$->FG=$1;
                                                  $$->FD=$3;
                                                  $$->ETIQ=malloc(2);
                                                  strcpy($$->ETIQ,"Af"); }
            | SK                                { $$=$1; }
            | '{' cmd '}'                       { $$=$2; }
            | IF expr TH cmd  EL atomic_cmd     { $$=Nalloc();
                                                  $$->codop=IF;
                                                  $$->FG=$2;         /* condition     */
                                                  $$->FD=Nalloc();   /* alternative   */
                                                  $$->FD->ETIQ="";   /* champ inutile */
                                                  $$->FD->FG=$4;     /* branche true  */
                                                  $$->FD->FD=$6;     /* branche false */
                                                  $$->ETIQ=malloc(2);
                                                  strcpy($$->ETIQ,"IfThEl"); }
            | WH expr DO atomic_cmd             { $$=Nalloc();
                                                  $$->codop=WH;
                                                  $$->FG=$2;         /* condition d'entree dans le while */
                                                  $$->FD=$4;         /* corps du while                   */
                                                  $$->ETIQ=malloc(2);
                                                  strcpy($$->ETIQ,"Wh"); }
            | V '(' block_expr ')'              { };

block_expr: %empty                      { }
            | block_non_nil_typed_expr  { };

block_non_nil_typed_expr:   expr                                { }
                            | expr ',' block_non_nil_typed_expr { };

/* un (ident, type) */ 
typed_arg:      V ':' type_decl         { $$ = creer_bilenvty(creer_envty($1->ETIQ, $3, 0));/* a ecrire */ };

/* un type */
type_decl:      T_BOO               { $$=$1; }
                | T_INT             { $$=$1; }
                | T_AR type_decl    { $$=$2;$$.DIM++; };

/* table des variables globales  */
block_decl_typed_var:   %empty                          { $$=bilenvty_vide(); }
                        | block_decl_non_nil_typed_var  { $$=$1; };

block_decl_non_nil_typed_var:   VAR typed_arg                                       { $$=$2; }
                                | block_decl_non_nil_typed_var ',' VAR typed_arg    { $$ = $1; $$ = concatty($1, $4); };

block_def_proc:     DEP IDPROC '(' block_expr ')'               { };
block_def_func:     DEF IDFUNC '(' block_expr ')' ':' type_decl { };

decl_def:   block_def_proc block_decl_typed_var cmd     { }
            | block_def_func block_decl_typed_var cmd   { };

list_def:   %empty              { }
            | list_def decl_def { };

%%

#include "arbre.h"
#include "lex.yy.c"


  /*  pour tester l'analyse */
//int main(int argn, char **argv)
//{yyparse();
//  ecrire_prog(benvty,syntree);
//  return(1);
//}



/*  pour tester l'interpreteur */
int main(int argn, char **argv)
{//ligcour=0;
  yyparse();
  BILQUAD c3a;
  c3a = pp2quad(syntree);
  ecrire_sep_bilquad(c3a);
  return(1);
}
/**/
int yyerror(s)
     char *s;
{
  fprintf(stderr, "%s: ligne %d\n", s, ligcour);
    return EXIT_FAILURE;
}
