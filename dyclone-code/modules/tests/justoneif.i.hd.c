#include "justoneif.i.hd.c.h"
extern bool __dyc_random_typdef_bool(unsigned int __dyc_exp ) 
{ bool __dyc_rsl ;

  {
  __dyc_rsl = __dyc_randompre_byte(__dyc_exp);
  return (__dyc_rsl);
}
}
extern bool __dyc_read_typdef_bool(void) 
{ bool __dyc_rsl ;

  {
  __dyc_rsl = __dyc_readpre_byte();
  return (__dyc_rsl);
}
}
extern void __dyc_print_typdef_bool(bool __dyc_thistype ) 
{ 

  {
  __dyc_printpre_byte(__dyc_thistype);
}
}
extern void *__dyc_random_ptr__fun_name_is_not_here(unsigned int __dyc_exp ) 
{ 

  {
  return (0);
}
}
extern void *__dyc_read_ptr__fun_name_is_not_here(void) 
{ 

  {
  __dyc_readpre_ptr();
  return (0);
}
}
extern void __dyc_print_ptr__fun_name_is_not_here(void const   * const  __dyc_thistype ) 
{ 

  {
  __dyc_printpre_ptr(__dyc_thistype);
}
}
extern char *__dyc_random_ptr__char(unsigned int __dyc_exp ) 
{ char *__dyc_rsl ;
  int __dyc_tmp_ptr ;

  {
  __dyc_rsl = 0;
  __dyc_tmp_ptr = __dyc_randompre_ptr(__dyc_exp);
  if (__dyc_tmp_ptr != 0) {
    {
    __dyc_exp ++;
    __dyc_rsl = (char *)calloc(1, sizeof(char ));
    *__dyc_rsl = __dyc_randompre_byte(__dyc_exp);
    __dyc_exp --;
    }
  }
  return (__dyc_rsl);
}
}
extern char *__dyc_read_ptr__char(void) 
{ char *__dyc_rsl ;
  int __dyc_tmp_ptr ;

  {
  __dyc_rsl = 0;
  __dyc_tmp_ptr = __dyc_readpre_ptr();
  if (__dyc_tmp_ptr != 0) {
    {
    __dyc_rsl = (char *)calloc(1, sizeof(char ));
    *__dyc_rsl = __dyc_readpre_byte();
    }
  }
  return (__dyc_rsl);
}
}
extern void __dyc_print_ptr__char(char const   * __restrict  __dyc_thistype ) 
{ 

  {
  __dyc_printpre_ptr(__dyc_thistype);
  if (__dyc_thistype != 0) {
    __dyc_printpre_byte(*__dyc_thistype);
  }
}
}
