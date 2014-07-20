/* cilly -c <thisfile>:
 * duplicate label: _L */

void test(void) 
{
    int enabled ;
_L: /* CIL Label */ 
    if ( (enabled && enabled ) || !enabled );
}

