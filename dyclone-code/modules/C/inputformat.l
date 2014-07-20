D    [0-9]

%{
#include <stdio.h>
#include <stdlib.h>
#include "inputformat.tab.h"
void yyerror(char *);
%}

%%

[+-]?{D}+  { yylval.numValue=atoi(yytext);
      return NUM;
    }
"p0" { return PTR0;}
"p1" { return PTR1;}

"(" { return '('; }
")" { return ')'; }

[ \t\n\v\r]+  ;
.   { yylval.nextc=yytext[0];
      yyerror("Unknow character: '%c'\n");
    }

%%

int yywrap(void) {
  return 1;
}

