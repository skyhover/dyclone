%{
#include <stdio.h>
#include <stdlib.h>
#include "dycmain.h"
#include "inputformat.lex.c"
%}


%union{
int numValue;
char nextc;
}

%start inputfile
%token <numValue> NUM PTR0 PTR1

%%

inputfile:  /* empty */
         |  inputitems
;

inputitems: inputitem
          | inputitems inputitem
;

inputitem:  NUM     { PUSHINPUT(__dyc_byteinputs, MAXBYTENUM, __dyc_bytenum, $1); }
         |  PTR0    { PUSHINPUT(__dyc_ptrinputs, MAXPTRNUM, __dyc_ptrnum, 0); }
         |  PTR1    { PUSHINPUT(__dyc_ptrinputs, MAXPTRNUM, __dyc_ptrnum, 1); }
;

%%

void yyerror(char * msg)
{
    fprintf(stderr, msg, yylval.nextc);
}

