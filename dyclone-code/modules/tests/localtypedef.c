/* Generated by CIL v. 1.3.6 */
/* print_CIL_Input is true */

#line 5 "localtypedef.cil.c"
struct s___0 {
   char *b ;
};
#line 9 "localtypedef.cil.c"
struct t {

};
#line 13 "localtypedef.cil.c"
struct s {
   int a ;
   char b ;
   struct s *s ;
   struct s___0 c ;
   struct t d ;
};
#line 21 "localtypedef.cil.c"
struct s___1 {
   char a ;
   int b ;
   struct s *s ;
};
#line 27 "localtypedef.cil.c"
typedef struct s___1 s;
#line 29
extern int printf(char const   * __restrict  __format  , ...) ;
#line 31
extern  __attribute__((__nothrow__, __noreturn__)) void __assert_fail(char const   *__assertion ,
                                                                      char const   *__file ,
                                                                      unsigned int __line ,
                                                                      char const   *__function ) ;
#line 36
extern int malloc() ;
#line 38 "localtypedef.cil.c"
int main(void) 
{ struct s___1 a ;
  s s___0 ;
  struct s___1 b ;
  int tmp ;
  int tmp___0 ;
  int __dyc_funcallvar_1 ;
  int __dyc_funcallvar_2 ;

  {
  {
#line 47
  a.a = (char)10;
#line 49
  b.a = (char)10;
#line 51
  s___0.b = a.b;
#line 53
  tmp___0 = __dyc_funcallvar_1;
  }
#line 55
  if ((unsigned long )tmp___0 == (unsigned long )((void *)0)) {
    {

    }
  } else {
    {
#line 60
    tmp = __dyc_funcallvar_2;

    }
  }
  goto __dyc_dummy_label;
}
}
