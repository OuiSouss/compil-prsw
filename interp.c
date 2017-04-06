#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "arbre.h"
#include "tableaux.tab.h"
#include "interp.h"

/*-------------------------------------------------------------------*/
/* ----------------------------types---------------------------------*/
/*  NOE,ENVTY,BILENVTY  : definis dans arbre.h                       */
/*-------------------------------------------------------------------*/


/* ------------------VARIABLES GLOBALES ------------------------------*/
/* le tas; (NIL=0); "vraies" adresses >=1                             */
int TAS[TAILLEMEM];
/* ADR[i]=adresse dans le tas du tab i                                */
int ADR[TAILLEADR];
/* TAL[i]=taille du tab i                                             */
int TAL[TAILLEADR];  
int ptasl=1; /* premiere place libre dans TAS[]                       */
int padrl=1; /* premiere place libre dans ADR[]                       */
/*--------------------------------------------------------------------*/
/* ----------------------traitement--memoire--------------------------*/
void init_memoire()
{int i=0;
while (i < TAILLEMEM)
  TAS[i++]=0;
i=0;
while (i < TAILLEADR)
  {ADR[i++]=0;
   TAL[i]=0;
  }
}
/* decrit la memoire: */
/* ADR[i]: adresse du tableau i dans le TAS */
/* TAL[i]: taille du tableau i; de ADR[i] a ADR[i] + TAL[i] */
/* TAS: tableau (statique) contenant tous les tableaux (dynamiques) */
void ecrire_memoire(int maxadr, int maxtal, int maxtas)
{int i;
 printf("Le tableau ADR:\n");
 printf("------------------------:\n");
 for(i=0; i < maxadr;i++)
   printf("%d:",ADR[i]);
 printf("\n");
 printf("Le tableau TAL:\n");
 printf("------------------------:\n");
 for(i=0; i < maxadr;i++)
   printf("%d:",TAL[i]);
 printf("\n");
 printf("Le tableau TAS:\n");
 printf("------------------------:\n");
 for(i=0; i < maxtas;i++)
   printf("%d:",TAS[i]);
 printf("\n");
 return;
}
/*--------------------------------------------------------------------*/	    
/*---------------semantique-------------------------------------------*/
/* N.B.allocation dynamique de tableaux; mais pas de ramasse-miettes! */

/* semantique op a grands pas des expressions                         */
/* fait agir e sur rho_gb, le  modifie, retourne val(e)               */
int semval(BILENVTY rho_gb,NOE e) 
{
    if(e != NULL)
    {
        ENVTY pos;
        int taille;
        switch(e->codop)
        {
            case (IND):
            {
                int lhs = semval(rho_gb, e->FG);
                int rhs = semval(rho_gb, e->FD);
                if (e->FG->codop == IND)
                    lhs++;
                return ADR[lhs] + rhs;
            }
            case (PL): case (MO): case (MU): case (AND): case (OR): case (LT): case  (EQ):/* op binaire     */
                return(eval(e->codop,semval(rho_gb,e->FG),semval(rho_gb,e->FD)));
            case NOT:                                            /* operation unaire      */
                return(eval(e->codop,semval(rho_gb,e->FG),0));
            case (I):                        /* numeral          */
                return (atoi(e->ETIQ));
            case (B):
                return (atoi(e->ETIQ));
            case (V):                         /* variable        */
            {
                pos=rechty(e->ETIQ,rho_gb.debut);
                return(pos->VAL);          /* rho_gb(var)     */
            }
            case (NEWAR):                     /*creation tableau */
            {
                int taspos = ptasl;
                int adrpos = padrl++;
                ADR[adrpos] = taspos;
                taille = semval(rho_gb, e->FD);
                ptasl += taille;
                TAL[adrpos] = taille;
                return adrpos;
            }
            default: return(EXIT_FAILURE);  /* codop inconnu au bataillon */
        }
    }
    else
        return(EXIT_FAILURE);
}

/* semantique op a grands pas des commandes                      */
/* fait agir c sur rho_gb, le  modifie                           */
void semop_gp(BILENVTY rho_gb, NOE c)
{
    char *lhs; int rhs;
    if(c != NULL)
    {
        switch(c->codop)
        {
            case (MP):
            {
                semop_gp(rho_gb, c->FG);
                break;
            }
            case (AF):
            {
                if (c->FG->codop==V)        /* affectation a une variable */
                {
                    lhs= c->FG->ETIQ;
                    printf("lhs vaut %s \n",lhs);
                    rhs= semval(rho_gb, c->FD);
                    printf("rhs vaut %d \n",rhs);
                    if (c->FD->codop == IND) // => operande de droite indexee
                        affectb(rho_gb, lhs, TAS[rhs]);
                    else // => operande de droite standard
                        affectb(rho_gb, lhs, rhs);
                }
                else
                {
                    assert(c->FG->codop==IND);/* affectation a un tableau */
                    int lval = semval(rho_gb, c->FG);
                    rhs = semval(rho_gb, c->FD);
                    TAS[lval] = rhs;
                }
                break;
            }
            case (SK): break;
            case (SE):
            {
                semop_gp(rho_gb, c->FG);
                semop_gp(rho_gb, c->FD);
                break;
            }
            case (IF):
            {
                if (semval(rho_gb, c->FG))
                    semop_gp(rho_gb, c->FD->FG);
                else
                    semop_gp(rho_gb, c->FD->FD);
                break;
            }
            case (WH):
            {
                while (semval(rho_gb, c->FG))
                    semop_gp(rho_gb, c->FD);
                break;
            }
            default : break;
        }
    }
}
