#include "arbre.h"
#include "interp.h"
#include "c3a.h"
#include "tableaux.tab.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* retourne une nouvelle chaine */
char *gensym(char *prefix)
{static int counter=0;
  char *chaine;char *chcounter;
  chcounter=Idalloc();
  chaine=Idalloc();
  strcpy(chaine,prefix); 
  sprintf(chcounter,"%d",counter);   /* prefix+chaine(counter)*/
  counter=counter+1;
  strcat(chaine,chcounter);
  return(chaine);
}

/* test de gensym */
void test_constantes(int n)
{char *ident;int i=0;
  for(i=0;i<n;i++)
    {ident=gensym("CONS");
      printf("%s \n",ident);
    }
}

/* retourne un quadruplet (avec etiquette etiq) */
QUAD creer_quad(char *etiq,int op,char *arg1,char *arg2,char *res)
{QUAD qd;int lres;
  qd=(struct cellquad *)malloc(sizeof(struct cellquad));
  if (etiq !=NULL)
    {qd->ETIQ=Idalloc();
      strcpy(qd->ETIQ,etiq);}
  qd->OP=op;
  if (arg1 !=NULL)
    {qd->ARG1=Idalloc();
      strcpy(qd->ARG1,arg1);}
  if (arg2 !=NULL)
    {qd->ARG2=Idalloc();
      strcpy(qd->ARG2, arg2);}
  if (res!= NULL)
    {lres=strlen(res);
      qd->RES=(char *)malloc(lres*sizeof(char));
      strcpy(qd->RES,res);}
  return(qd);
}

/* retourne une biliste vide  */
BILQUAD bilquad_vide() 
{BILQUAD bq;
  bq.debut=NULL;bq.fin=NULL;
  return(bq);
}

/* retourne une biliste a un element  */
BILQUAD creer_bilquad(QUAD qd) 
{BILQUAD bq;
  bq.debut=qd;bq.fin=qd;
  return(bq);
}

/* fonction aux  pour la fonction rechbq */
QUAD rechq(char *chaine, QUAD qd)
{QUAD qcour;
  qcour=qd;
  if (qcour!=NULL)
    {if (strcmp(qcour->ETIQ,chaine)==0)
        {printf("trouve %s en position %p \n",chaine,qcour);
	  return qcour;}
      else
	return rechq(chaine,qcour->SUIV);
    }
  else
    return NULL;
}

/*retourne le quad etiquete par chaine, NULL s'il n'y en a pas */
QUAD rechbq(char *chaine, BILQUAD bq)
{return(rechq(chaine,bq.debut));}


BILQUAD concatq(BILQUAD bq1, BILQUAD bq2)
/* retourne la concatenation; ne detruit pas bqi; ne copie pas *bqi */
/* peut creer une boucle ! */
{BILQUAD bq;
  if (bq1.fin!= NULL)
    if (bq2.debut!=NULL)
       { bq1.fin->SUIV=bq2.debut;
        bq.debut=bq1.debut;
        bq.fin=bq2.fin;
        return(bq);}
    else
      return(bq1);  
  else
    return(bq2);
}

/* retourne bq +  skip */
BILQUAD ajouterskip(BILQUAD bq)
{QUAD nq; BILQUAD nbq;
  nq=creer_quad(gensym("ET"),SK,NULL,NULL,NULL);/* instruction skip */
  nbq=creer_bilquad(nq);
  return(concatq(bq,nbq));
}

/* retourne la "forme normale" de bq: dernier quad = skip */  
BILQUAD normal(BILQUAD bq)
{if (bq.fin== NULL)
    {return(ajouterskip(bq));}
  else
    {if (bq.fin->OP!=SK)        /* pas normal-> on normalise */
	return(ajouterskip(bq));
      else                      /* deja normal */
	return(bq);}
}

/* affiche le quadruplet v1 (pour tests) */
void ecrire_quad1(QUAD qd)
{printf("etiq:%s,op:%s,arg1:%s,arg2:%s,res:%s \n",qd->ETIQ,nomop(qd->OP),qd->ARG1,qd->ARG2,qd->RES);
}

/* affiche le quadruplet (pour generer code); puis saute a la ligne */
void ecrire_quad(QUAD qd)
{ if(strcmp(qd->ETIQ,"") == 0)       /* etiquette= mot vide */
    {printf("%-10s ","");}
  else
    {printf("%-10s:",qd->ETIQ);}
  printf("%-6s ",nomop(qd->OP));
  if (qd->ARG1!=NULL)
    {printf("%-10s",qd->ARG1);}
  else
    {printf("%-10s","");}
  if (qd->ARG2!=NULL)
    {printf("%-10s",qd->ARG2);}
  else
    {printf("%-10s","");}
  if (qd->RES!=NULL)
    {printf("%-10s\n",qd->RES);}
  else
    {printf("\n");}
  }
/* affiche le quadruplet (pour generer code) avec separateur":" ; 
puis saute a la ligne */
void ecrire_sep_quad(QUAD qd)
{ if (qd == NULL)
    return;
    if(strcmp(qd->ETIQ,"") == 0)       /* etiquette= mot vide */
    {printf("%-10s: ","");}
  else
    {printf("%-10s:",qd->ETIQ);}
  printf("%-6s:",nomop(qd->OP));
  if (qd->ARG1!=NULL)
    {printf("%-10s:",qd->ARG1);}
  else
    {printf("%-10s:","");}
  if (qd->ARG2!=NULL)
    {printf("%-10s:",qd->ARG2);}
  else
    {printf("%-10s:","");}
  if (qd->RES!=NULL)
    {printf("%-10s\n",qd->RES);}
  else
    {printf("\n");}
  }

/* ecrit le quadruplet dans un string; pas de newline             */
void secrire_quad(char *tquad, QUAD qd)
{ char mot[MAXIDENT];
  sprintf(tquad,"%-6s ",nomop(qd->OP));
  if (qd->ARG1!=NULL)
    {sprintf(mot,"%-10s",qd->ARG1);
      strcat(tquad,mot);}
  else
    {sprintf(mot,"%-10s"," ");
      tquad=strcat(tquad,mot);};
    if (qd->ARG2!=NULL)
    {sprintf(mot,"%-10s",qd->ARG2);
      strcat(tquad,mot);}
  else
    {sprintf(mot,"%-10s"," ");
      strcat(tquad,mot);};
    if (qd->RES!=NULL)
    {sprintf(mot,"%-10s",qd->RES);
      strcat(tquad,mot);}
  else
    {sprintf(mot," ");
      strcat(tquad,mot);};
    }

/* affiche la biliste de quad */
void ecrire_bilquad(BILQUAD bq)
{QUAD qcour;
  qcour=bq.debut;
  while(qcour!=NULL)
    {ecrire_quad(qcour);
      qcour=qcour->SUIV;}
}
/* affiche la biliste de quad (avec separateurs) */
void ecrire_sep_bilquad(BILQUAD bq)
{QUAD qcour;
  qcour=bq.debut;
  while(qcour!=NULL)
    {ecrire_sep_quad(qcour);
      qcour=qcour->SUIV;}
}


/**
 * Traduit une expression/commande en liste de quadruplets
 */
BILQUAD pp2quad(NOE ec)
{
	extern BILENVTY benvty;
	type t;
	BILQUAD bil_fg, bil_fd, bil_exp, bil_res;
	int newop;
	char *netiq, *netiqf, *nres;
	char* narg1 = NULL;
	char* narg2 = NULL;
	QUAD nquad;
        assert(ec != NULL);

        switch(ec->codop)
        {
            case PL: case MO: case MU:
            {
                netiq=gensym("ET");
                newop=ec->codop;

                // On traduit les arguments
                bil_fg=pp2quad(ec->FG);
                bil_fd=pp2quad(ec->FD);

                // On les simplifie si possible
                if(ec->FG->codop != V)
                {
                    narg1=Idalloc();
                    strcpy(narg1, bil_fg.fin->RES);
                } else
                {
                    narg1 = Idalloc();
                    strcpy(narg1, ec->FG->ETIQ);
                }
                if(ec->FD->codop != V)
                {
                    narg2=Idalloc();
                    strcpy(narg2, bil_fd.fin->RES);
                } else
                {
                    narg2 = Idalloc();
                    strcpy(narg2, ec->FD->ETIQ);
                }

                nres = gensym("VA");
                t.DIM = 0;
                t.TYPEF = T_INT;
                inbilenvty(&benvty, nres, t);

                nquad=creer_quad(netiq, newop, narg1, narg2, nres);
                bil_res=creer_bilquad(nquad);

                bil_fd=concatq(bil_fg, bil_fd);
                bil_res=concatq(bil_fd, bil_res);
                break;
            }
            case AND: case OR: case LT: case EQ:
            {
                netiq = gensym("ET");
                newop = ec->codop;

                bil_fg = pp2quad(ec->FG);
                bil_fd = pp2quad(ec->FD);
                if(ec->FG->codop != V)
                {
                    narg1=Idalloc();
                    strcpy(narg1, bil_fg.fin->RES);
                } else
                {
                    narg1 = Idalloc();
                    strcpy(narg1, ec->FG->ETIQ);
                }
                if(ec->FD->codop != V)
                {
                    narg2=Idalloc();
                    strcpy(narg2, bil_fd.fin->RES);
                } else
                {
                    narg2 = Idalloc();
                    strcpy(narg2, ec->FD->ETIQ);
                }

                nres = gensym("VA");
                t.DIM = 0;
                t.TYPEF = T_BOO;
                inbilenvty(&benvty, nres, t);

                nquad=creer_quad(netiq, newop, narg1, narg2, nres);
                bil_res=creer_bilquad(nquad);

                bil_fd=concatq(bil_fg, bil_fd);
                bil_res=concatq(bil_fd, bil_res);
                break;
            }
            case NOT:
            {
                netiq = gensym("ET");
                newop = NOT;
                narg1 = Idalloc();
                bil_fg=pp2quad(ec->FG);
                if(ec->FG->codop != V)
                    strcpy(narg1, bil_fg.fin->RES);
                else
                    strcpy(narg1, ec->FG->ETIQ);
                narg2 = NULL;
                nres = Idalloc();
                if (narg1[0] == '-')
                    sprintf(nres, "%s", narg1 + 1);
                else
                    sprintf(nres, "-%s", narg1);

                t.DIM = 0;
                t.TYPEF = ec->typno.TYPEF;
                nquad = creer_quad(netiq, newop, narg1, narg2, nres);
                bil_res = creer_bilquad(nquad);
                bil_res=concatq(bil_fg, bil_res);
                break;
            }
            case I:
            {
                /* les ingredients */
                netiq=gensym("ET");newop=AFC;
                narg1=Idalloc();sprintf(narg1,"%s",ec->ETIQ);
                narg2=NULL;nres=gensym("CT");
                /* on insere le nom de const dans l' environnement */
                t.DIM = 0;
                t.TYPEF = T_INT;
                inbilenvty(&benvty, nres, t);
                /* le quadruplet: ETnum, Afc, chaineconst,-, CTnum */
                nquad=creer_quad(netiq,newop,narg1,narg2,nres);
                bil_res=creer_bilquad(nquad);
                break;
            }
            case B:
            {
                netiq = gensym("ET"); newop = AFC;
                narg1 = Idalloc();
                if (strcmp(ec->ETIQ, "1") == 0)
                    sprintf(narg1, "%s", "true");
                else
                    sprintf(narg1, "%s", "false");
                narg2 = NULL; nres = gensym("CT");

                t.DIM = 0;
                t.TYPEF = T_BOO;
                inbilenvty(&benvty, nres, t);

                nquad = creer_quad(netiq, newop, narg1, narg2, nres);
                bil_res = creer_bilquad(nquad);
                break;
            }
            case IND:
            {
                break;
            }
//            case NEWAR:
//            {
//                netiq = gensym("ET"); newop = AFIND;
//                narg1 = Idalloc(); sprintf(narg1, "%s", ec->FD->ETIQ);
//                narg2 = NULL; nres = gensym("CT");
//                t.DIM = ec->typno.DIM;
//                t.TYPEF = ec->typno.TYPEF;
//                nquad = creer_quad(netiq, newop, narg1, narg2, nres);
//                bil_res = creer_bilquad(nquad);
//                break;
//            }
            case V:
            {   
                /* le quadruplet: skip, resultat dans chainevar */
                netiq=gensym("ET");newop=SK;narg1=NULL;narg2=NULL;nres=ec->ETIQ;
                nquad=creer_quad(netiq,newop,narg1,narg2,nres);
                bil_res=creer_bilquad(nquad);
                break;
            }
            case AF:
            {
                /* les ingredients */
                netiq=gensym("ET");
                newop=AF;
                /* assert(ec->FG->codop==V); */
                /* narg1= chaine en lhs */
                narg1=ec->FG->ETIQ;
                /* narg2= adresse res du code du rhs */
                bil_fd=pp2quad(ec->FD);
                narg2=Idalloc();
                strcpy(narg2,bil_fd.fin->RES);
                nres=NULL;
                /* le quadruplet: ETnum, Af, chainevar1,chaineres2, NULL */
                nquad=creer_quad(netiq,newop,narg1,narg2,nres);
                bil_res=concatq(bil_fd,creer_bilquad(nquad));
                break;
            }
            case SK:
            {
                /* les ingredients */
                netiq=gensym("ET");newop=SK;narg1=NULL;narg2=NULL;nres=NULL;
                /* le quadruplet: skip  (pas d'adresse de resultat) */
                nquad=creer_quad(netiq,newop,narg1,narg2,nres);
                bil_res=creer_bilquad(nquad);
                break;
            }
            case SE:
            {
                bil_fg = pp2quad(ec->FG);
                bil_fd = pp2quad(ec->FD);
                bil_res = concatq(bil_fg, bil_fd);
                break;
            }
            case IF:
            {
                bil_exp = pp2quad(ec->FG);    /* traduction de l'expression */
                bil_fg = pp2quad(ec->FD->FG); /* commande (cas vrai) */
                bil_fd = pp2quad(ec->FD->FD); /* commande (cas faux) */
                bil_fd = normal(bil_fd);
                /* les ingredients de bil_fg */
                netiq = gensym("ET");
                newop = JZ;
                narg1 = bil_exp.fin->RES;
                narg2 = NULL;
                nres = bil_fd.debut->ETIQ;
                /* le quadruplet bil_fg */
                nquad = creer_quad(netiq, newop, narg1, narg2, nres);
                /* nouvelle biliste */
                bil_res = concatq(bil_exp, creer_bilquad(nquad));
                bil_res=concatq(bil_res, bil_fg);
                /* les ingredients de bil_fd */
                netiq = gensym("ET");
                newop = JP;
                narg1 = NULL;
                narg2 = NULL;
                nres = bil_fd.fin->ETIQ;
                /* le quadruplet bil_fd */
                nquad = creer_quad(netiq, newop, narg1, narg2, nres);
                /* nouvelle biliste */
                bil_res = concatq(bil_res, creer_bilquad(nquad));
                bil_res = concatq(bil_res, bil_fd);
                break;
            }
            case WH:
            {
                bil_exp = pp2quad(ec->FG);    /* traduction de l'expression */
                bil_fg = pp2quad(ec->FD);     /* traduction du corps        */
                bil_fg = normal(bil_fg);
                /* les ingredients de Q1 */
                netiq = gensym("ET");
                newop = JZ;  /* etiquette de Q1            */
                narg1 = bil_exp.fin->RES;
                narg2 = NULL;
                netiqf = gensym("ET");        /* etiquette fin de traduction */
                nres = netiqf;
                /* le quadruplet Q1 */
                nquad = creer_quad(netiq, newop, narg1, narg2, nres);
                /* nouvelle biliste */
                bil_res = concatq(bil_exp, creer_bilquad(nquad));
                bil_res=concatq(bil_res, bil_fg);
                /* les ingredients de Q2 */
                newop = JP;
                /* narg1=narg2=NULL; */
                nres = bil_exp.debut->ETIQ;
                /* on substitue Q2 a la fin de bil_res */
                bil_res.fin->OP = newop;
                bil_res.fin->RES = nres;
                /* les ingredients de Q3 */
                netiq = netiqf;                   /* etiquette de Q3            */
                newop = SK;
                narg1 = NULL;
                narg2 = NULL;
                nres = NULL;
                /* le quadruplet Q3 */
                nquad = creer_quad(netiq, newop, narg1, narg2, nres);
                /* nouvelle biliste */
                bil_res = concatq(bil_res, creer_bilquad(nquad));
                break;
            }
        }
        return bil_res;
}
