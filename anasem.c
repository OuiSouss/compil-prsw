/* anasem.c */
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "arbre.h"
#include "ppascal.tab.h"
/*------------------FONCTION-----------*/
void typ_error(char *mess,int ligne)
{
  fprintf(stderr, "TYPERROR: %s: ligne %d \n", mess, ligne);
  return;
}

/* renvoie le type de e : TYPEF \in T_INT, T_BOO, T_CMD, T_ERR, T_BOT          */
/* affecte ce type a la racine                                                 */
/* suppose corrects les types des fils                                         */
/* envoie message d'erreur si mal type a la racine et fils bien types          */
/* renvoie T_err si l'arbre est mal-type                                       */
type calcul_type(BILENVTY rho_gb, NOE e, int ligne)
{
  type tp;
  tp= creer_type(0,T_BOT);/* type par defaut */
  if(e != NULL)
    {
      type tfg,tfd;                /* type  du fils gauche, fils droit */
      type terr=creer_type(0,T_ERR);   /* type  erreur                 */
      /* un des fils est mal-type */
      if ((e->FG && type_eq(e->FG->typno,terr))||(e->FD && type_eq(e->FD->typno,terr)))
	{
	  type_copy(&tp,terr);/* valeur du  type                              */
	  type_copy(&(e->typno),terr); /* affecte type  erreur                */
	  return(tp);
	}
      /* e != NULL et tous les fils sont bien-types */
      switch(e->codop)
	{
	case B:
	  tp=e->typno;
	  return(tp);/* typno deja affecte     */
	case IND: /* (tab_dim k of tau) x   int -> (tab_dim k-1 of tau) */
	  {
	    type tint;    /* type  entier       */
	    tint=creer_type(0,T_INT);
	    tfg=e->FG->typno;/* type  des fils  */
	    tfd=e->FD->typno;
	    if ((tfg.DIM > 0) && type_eq(tfd,tint))
	      {
		type_copy(&tp,tfg);                 /* type  du tableau fils gauche    */
		(tp.DIM)--;              /* calcule le type */
		type_copy(&e->typno,tp); /* affecte le type */
	      }
	    else if (tfg.DIM == 0)
	      {
		type_copy(&tp,terr);     /* calcule le type */
		type_copy(&e->typno,tp); /* affecte le type */
		typ_error("index dans un tab de dim 0",ligne);   /* envoi message erreur*/
	      }
	    else
	      {
		type_copy(&tp,terr);    /* calcule le type */
		type_copy(&e->typno,tp); /* affecte le type */
		typ_error("index non entier dans un tableau",ligne);/*env message erreur*/
	      };
	    return(tp);
	  }
	case PL:case MO:case MU: /* int x  int -> int */
	  {
	    type tint;    /* type  des fils  */
	    tint=creer_type(0,T_INT);
	    tfg=e->FG->typno;
	    tfd=e->FD->typno;
	    if (type_eq(tfg,tint) && type_eq(tfd,tint))
	      {
		type_copy(&tp,tfg);        /* calcule le type */
		type_copy(&(e->typno),tfg);/* affecte le type */
	      }
	    else
	      {
		type_copy(&tp,terr);/* calcule le type                               */
		type_copy(&(e->typno),terr); /* affecte type                         */
		typ_error("op entier sur args non-entiers",ligne);/* message erreur  */
	      }
	    return(tp);
	  }
	case LT:case EQ:         /* int x int -> boo                   */
	  {
	    type tint,tboo;       /* types  entier, booleen             */
	    tint=creer_type(0,T_INT);
	    tboo=creer_type(0,T_BOO);
	    tfg=e->FG->typno;
	    tfd=e->FD->typno;
	    if (type_eq(tfg,tint) && type_eq(tfd,tint))
	      {
		type_copy(&tp,tboo);         /* valeur du  type */
		type_copy(&(e->typno),tboo); /* affecte le type */
	      }
	    else
	      {
		type_copy(&tp,terr);/* valeur du  type                               */
		type_copy(&(e->typno),terr); /* affecte type                         */
		typ_error("op de comparaison sur args non-entiers",ligne);/* message */
	      }
	    return(tp);
	  }
	case AND:case OR:        /* boo x boo -> boo                   */
	  {
	    type tboo;            /* type  booleen                      */
	    tboo=creer_type(0,T_BOO);
	    tfg=e->FG->typno;
	    tfd=e->FD->typno;
	    if (type_eq(tfg,tboo) && type_eq(tfd,tboo))
	      {
		type_copy(&tp,tboo);         /* valeur du  type */
		type_copy(&(e->typno),tboo); /* affecte le type */
	      }
	    else
	      {
		type_copy(&tp,terr);/* valeur du  type                              */
		type_copy(&(e->typno),terr); /* affecte type                        */
		typ_error("op booleen sur args non-booleens",ligne);/*message erreur*/
	      }
	    return(tp);
	  }
	case NOT:                /* boo  -> boo                        */
	  {
	    type tboo;            /* type  booleen                      */
	    tboo=creer_type(0,T_BOO);
	    tfg=e->FG->typno;
	    assert(e->FD==NULL); /* op unaire                         */
	    if (type_eq(tfg,tboo))
	      {
		type_copy(&tp,tboo);         /* valeur du  type */
		type_copy(&(e->typno),tboo); /* affecte le type */
	      }
	    else
	      {
		type_copy(&tp,terr);/* valeur du  type                               */
		type_copy(&(e->typno),terr); /* affecte type                         */
		typ_error("op booleen sur arg non-booleen",ligne);/* message erreur  */
	      }
	    return(tp);
	  }
	case I:                  /* constante T_INT        */
	  {
	    type tint;            /* type  du noeud         */
	    tint=creer_type(0,T_INT);
	    assert(type_eq(e->typno,tint));/*verif du  type */
	    type_copy(&tp,tint); /* valeur du  type         */
	    return(tp);
	  }
	case V:                  /* variable              */
	  {
	    ENVTY pos=rechty(e->ETIQ,rho_gb.debut);               /* pos var dans rho */ 
	    if (pos!=NULL)
	      {
		type_copy(&tp, pos->TYPE);        /* valeur du  type := rho(var)      */
		type_copy(&(e->typno), pos->TYPE);/* affecte le type                  */
	      }
	    else
	      {
		type_copy(&tp,terr);       /* tp := erreur                             */
		type_copy(&(e->typno),tp); /* affecte type                             */
		typ_error("variable inconnue ",ligne);/* message erreur                */
	      };
	    return(tp);                           /* renvoie le type                  */
	  }
	case NEWAR:                                             /* creation tableau */
	  {
	    type_copy(&tp,e->typno);          /* type calcule par l'analyseur synt */
	    return(tp);
	  }
	case AF:                                                     /* affectation */
	  {
	    if (type_eq(e->FG->typno,e->FD->typno)==0)/* type(lhs) <> type(rhs)    */
	      {
		type_copy(&(e->typno),terr); /* affecte type                        */
		typ_error("affectation de types incoherents ", ligne);
		return(terr);
	      }
	    else                                     /* type(lhs) == type(rhs)      */
	      type_copy(&(e->typno), creer_type(0,T_CMD));
	    return(tp); 
	  }
	case SE:
	  {
	    type tcom= creer_type(0,T_CMD);         /* type  commande               */
	    type_copy(&(e->typno),tcom);
	    type_copy(&tp,tcom);
	    return(tp);                             
	  }
	case IF:
	  {
	    type tcom= creer_type(0,T_CMD);         /* type  commande               */
	    type tboo=creer_type(0,T_BOO);          /* type  booleen                */
	    tfg=e->FG->typno;                       /* type  des  3 fils            */
	    type tthen=e->FD->FG->typno; 
	    type telse=e->FD->FD->typno;
	    if (type_eq(tfg,tboo)==0)              /* type arg0  <> booleen         */
	      {
		type_copy(&(e->typno),terr);        /* affecte type                  */
		type_copy(&tp,terr);
		typ_error("condition non booleenne dans un IFTHEL", ligne);
	      }
	    else if (!type_eq(tthen,tcom) ||!type_eq(telse,tcom))  /* arg <> tcom   */
	      {
		type_copy(&(e->typno),terr);                        /* affecte type  */
		type_copy(&tp,terr);
	      }
	    else 
	      {
		type_copy(&(e->typno),tcom);                         /* affecte type */
		type_copy(&tp,tcom);
	      }
	    return(tp);
	  }
	   
	case WH:
	  {
	    type tcom= creer_type(0,T_CMD);         /* type  commande               */
	    type tboo=creer_type(0,T_BOO);          /* type  booleen                */
	    tfg=e->FG->typno;                       /* type  des    fils            */
	    tfd=e->FD->typno;
	    if (type_eq(tfg,tboo)==0)              /* type arg0  <> booleen         */
	      {
		type_copy(&(e->typno),terr);        /* affecte type                  */
		type_copy(&tp,terr);
		typ_error("condition non booleenne dans un While", ligne);
	      }
	    else 
	      {
		type_copy(&(e->typno),tcom);                         /* affecte type */
		type_copy(&tp,tcom);
	      }
	    return(tp);
	  }
	default: return(tp);                            /* codop inconnu au bataillon */
	}/* fin switch          */
    }/* fin if (e!=NULL)         */
  else
    return(tp);                                     /* e==NULL, arbre vide, type T_BOT */
}

