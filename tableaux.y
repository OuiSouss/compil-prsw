/* ppascal.y */
%{
 #include <stdio.h>
 #include <ctype.h>
 #include <string.h>
 #include "arbre.h"
 #include "interp.h"
/* ------------------VARIABLES GLOBALES -------------------------*/
  NOE syntree;          /* commande  globale                     */
  BILENVTY benvty;      /* environnement global                  */
  int ligcour=1;        /* ligne  courante                       */
  type tycour;          /* type courant                          */
  ENVTY vtycour;        /* var typee courante                    */
/* -------------------------------- -----------------------------*/
%}
%start prog
%union{NOE NO; type TYP; BILENVTY LARGT;}
/* attributs NOE: noeud binaire (IfThEl est "binarise")                                */
/* attributs TYP: contient un type                                                     */
/* attributs LARGT: liste d'arguments types(var globales)                              */
%type <NO> prog CA C E T F ET 
%type <TYP> TP
%type <LARGT> ARGT L_VART L_VARTNN
/* Non-terminaux MP Ca C E T F Et TP Argt L_vart L_vartnn*/
/* P:main_program; Ca:commande atomique; C:commande; E:expression; T:terme; F:facteur;*/
/* Et: expr tableau; */
/* TP:TyPe; Argt:argument_type; */
/* L_vart: Liste_variables_typees,   L_vartnn: Liste_variables_typees non-nil */
%token <NO> I V Def Dep NFon NPro MP AF SK NEWAR SE IND IF TH EL VAR WH DO PL MO MU AND OR NOT LT EQ 
%token <TYP> T_INT T_ERR T_BOT T_AR T_CMD

/* Unités lexicales<NO>: Integer Variable Main_prog                            */
/* Affectation Skip NewArrayOf                                                 */
/* Sequence Index If Then Else Var While Do                                    */
/* Plus Moins Mult And Or Not Lessthan Equal                                   */
/* Unités lexicales<TYP>:
Type_int Type_erreur Type_indefini  Type_array Type_commande                   */


%%
prog :  L_VART C            {benvty=$1;
			   syntree=$2;
			   YYACCEPT;}
   ;

E : E PL T                {$$=Nalloc();
                           $$->codop=PL;
                           $$->FG=$1;
                           $$->FD=$3;
                           $$->ETIQ=malloc(2);
                           strcpy($$->ETIQ,"+");}
  | E MO T               {$$=Nalloc();
                          $$->codop=MO;
                          $$->FG=$1;
                          $$->FD=$3;
		          $$->ETIQ=malloc(2);
                          strcpy($$->ETIQ,"-");}
  | E OR T               {$$=Nalloc();
                          $$->codop=OR;
                          $$->FG=$1;
                          $$->FD=$3;
                          $$->ETIQ=malloc(2);
                          strcpy($$->ETIQ,"Or");}
  | E LT T               {$$=Nalloc();
                          $$->codop=LT;
                          $$->FG=$1;
                          $$->FD=$3;
                          $$->ETIQ=malloc(2);
                          strcpy($$->ETIQ,"Lt");}
  | E EQ T                {$$=Nalloc();
                          $$->codop=EQ;
                          $$->FG=$1;
                          $$->FD=$3;
                          $$->ETIQ=malloc(2);
                          strcpy($$->ETIQ,"Eq");}
  | T                    {$$=$1;}
  ;

T : T MU  F               {$$=Nalloc();
                           $$->codop=MU;
                           $$->FG=$1;
                           $$->FD=$3;
                           $$->ETIQ=malloc(2);
                           strcpy($$->ETIQ,"*");}

  | T AND F               {$$=Nalloc();
                           $$->codop=AND;
                           $$->FG=$1;
                           $$->FD=$3;
                           $$->ETIQ=malloc(2);
                           strcpy($$->ETIQ,"And");}
  | NOT F                  {$$=Nalloc();
                            $$->codop=NOT;
			    $$->FG=$2;
                            $$->FD=NULL;
			    $$->ETIQ=malloc(2);
                            strcpy($$->ETIQ,"Not");}
  | F                      {$$=$1;}
  ;

F : '(' E ')'                  {$$=$2;}
  | I                          {$$=$1;} 
  | V                          {$$=$1;}
  | NEWAR TP '[' E ']'         {$$=Nalloc();
                                $$->codop=NEWAR;
				/* calcul_type */
      				type_copy(&($$->typno),$2); /* DIM,TYPEF sont connus   */
				($$->typno).DIM++; /* mise a jour DIM                  */
      				$$->FG=NULL;
				$$->FD=$4;/* TAILLE a calculer a partir de E */}
  | ET                          {$$=$1;}
  ;

ET: V  '[' E ']'                {$$=Nalloc();/* un seul indice                        */
                                $$->codop=IND;
				$$->FG=$1;
				$$->FD=$3;
				}
  | ET '[' E ']'                {$$=Nalloc();/* plusieurs indices                     */
                                $$->codop=IND;
				$$->FG=$1;
				$$->FD=$3;
                                }
  ;

C : C SE CA                     {$$=Nalloc();      /* sequence */
                                 $$->codop=SE;
                                 $$->FG=$1;
                                 $$->FD=$3;
                                 $$->ETIQ=malloc(2);
                                 strcpy($$->ETIQ,"Se");
                                 }    
  | CA                          {$$=$1;}
  ;

CA : ET AF E            {$$=Nalloc();
                        $$->codop=AF;
                        $$->FG=$1;
                        $$->FD=$3;
                        $$->ETIQ=malloc(2);
                        strcpy($$->ETIQ,"Af");
			}
  | V AF E              {$$=Nalloc();
                        $$->codop=AF;
                        $$->FG=$1;
                        $$->FD=$3;
                        $$->ETIQ=malloc(2);
                        strcpy($$->ETIQ,"Af");

			}
  | SK                  {$$=$1;}      
  | '{' C '}'           {$$=$2;}     
  | IF E TH C  EL CA    {$$=Nalloc();
                        $$->codop=IF;
                        $$->FG=$2;         /* condition     */
                        $$->FD=Nalloc();   /* alternative   */
			$$->FD->ETIQ="";   /* champ inutile */
			$$->FD->FG=$4;     /* branche true  */
			$$->FD->FD=$6;     /* branche false */
                        $$->ETIQ=malloc(2);
                        strcpy($$->ETIQ,"IfThEl");
                        }
  | WH E DO CA         {$$=Nalloc();
                        $$->codop=WH;
                        $$->FG=$2;         /* condition d'entree dans le while */
                        $$->FD=$4;         /* corps du while                   */
                        $$->ETIQ=malloc(2);
                        strcpy($$->ETIQ,"Wh");
                         }
  ;


/* un (ident, type) */ 
ARGT:  V ':' TP         {$$ = creer_bilenvty(creer_envty($1->ETIQ, $3, 0));/* a ecrire */}
    ;
    
/* un type */
TP: T_INT               {$$=$1;}
  | T_AR TP             {$$=$2;$$.DIM++;}
  ;

/* table des variables globales  */
L_VART: %empty           {$$=bilenvty_vide();}
      | L_VARTNN         {$$=$1;}
      ;

L_VARTNN: VAR ARGT                {$$=$2;}
| L_VARTNN ',' VAR ARGT           {$$ = $1; $$ = concatty($1, $4);}
        ;

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
  ecrire_prog(benvty,syntree);
  init_memoire();
  printf("Les variables globales avant exec:\n");
  printf("------------------------:\n");
  ecrire_bilenvty(benvty); printf("\n");
  ecrire_memoire(5,5,20);
  semop_gp(benvty,syntree);
  printf("Les variables globales apres exec:\n");
  printf("------------------------:\n");
  ecrire_bilenvty(benvty); printf("\n");
  ecrire_memoire(5,5,20);
  return(1);
}
/**/
int yyerror(s)
     char *s;
{
  fprintf(stderr, "%s: ligne %d\n", s, ligcour);
    return EXIT_FAILURE;
}

