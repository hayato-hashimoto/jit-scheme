/* Generated from benchmark.scm by the CHICKEN compiler
   http://www.call-with-current-continuation.org
   2011-06-02 14:06
   Version 4.6.0 
   linux-unix-gnu-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2010-11-09 on archlinux (Linux)
   command line: -unsafe -fixnum-arithmetic -disable-interrupts -disable-stack-overflow-checks -inline benchmark.scm -inline-limit 1 -optimize-level 5
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[4];
static double C_possibly_force_alignment;


C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_30)
static void C_ccall f_30(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_33)
static void C_ccall f_33(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_94)
static void C_ccall f_94(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_84)
static void C_ccall f_84(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_90)
static void C_ccall f_90(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_87)
static void C_ccall f_87(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_61)
static void C_fcall f_61(C_word t0,C_word t1) C_noret;
C_noret_decl(f_65)
static void C_ccall f_65(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_35)
static void C_fcall f_35(C_word t0) C_noret;
C_noret_decl(f_41)
static void C_ccall f_41(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;

C_noret_decl(trf_61)
static void C_fcall trf_61(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_61(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_61(t0,t1);}

C_noret_decl(trf_35)
static void C_fcall trf_35(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_35(void *dummy){
C_word t0=C_pick(0);
C_adjust_stack(-1);
f_35(t0);}

C_noret_decl(tr5)
static void C_fcall tr5(C_proc5 k) C_regparm C_noret;
C_regparm static void C_fcall tr5(C_proc5 k){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
(k)(5,t0,t1,t2,t3,t4);}

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point
C_noret_decl(toplevel_trampoline)
static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall toplevel_trampoline(void *dummy){
C_toplevel(2,C_SCHEME_UNDEFINED,C_restore);}

void C_ccall C_toplevel(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);
else C_toplevel_entry(C_text("toplevel"));
C_disable_overflow_check=1;
C_resize_stack(262144);
C_check_nursery_minimum(3);
if(!C_demand(3)){
C_save(t1);
C_reclaim((void*)toplevel_trampoline,NULL);}
toplevel_initialized=1;
if(!C_demand_2(20)){
C_save(t1);
C_rereclaim2(20*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,4);
lf[2]=C_h_intern(&lf[2],25,"\003sysimplicit-exit-handler");
lf[3]=C_h_intern(&lf[3],5,"print");
C_register_lf2(lf,4,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_30,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k28 */
static void C_ccall f_30(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_30,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_33,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k31 in k28 */
static void C_ccall f_33(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[10],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_33,2,t0,t1);}
t2=C_mutate(&lf[0] /* (set! calc ...) */,(*a=C_CLOSURE_TYPE|1,a[1]=(C_word)f_35,tmp=(C_word)a,a+=2,tmp));
t3=C_mutate(&lf[1] /* (set! loop ...) */,(*a=C_CLOSURE_TYPE|1,a[1]=(C_word)f_61,tmp=(C_word)a,a+=2,tmp));
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_84,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_94,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
/* benchmark.scm:3: loop */
f_61(t5,C_fix(8));}

/* k92 in k31 in k28 */
static void C_ccall f_94(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
/* benchmark.scm:3: print */
t2=*((C_word*)lf[3]+1);
((C_proc3)(void*)(*((C_word*)t2+1)))(3,t2,((C_word*)t0)[2],t1);}

/* k82 in k31 in k28 */
static void C_ccall f_84(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_84,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_87,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_90,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
/* ##sys#implicit-exit-handler */
t4=*((C_word*)lf[2]+1);
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* k88 in k82 in k31 in k28 */
static void C_ccall f_90(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}

/* k85 in k82 in k31 in k28 */
static void C_ccall f_87(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* loop in k31 in k28 */
static void C_fcall f_61(C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_61,NULL,2,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_65,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
/* benchmark.scm:2: calc */
f_35(t3);}

/* k63 in loop in k31 in k28 */
static void C_ccall f_65(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
t2=((C_word*)t0)[3];
t3=C_eqp(t2,C_fix(0));
if(C_truep(t3)){
/* benchmark.scm:2: calc */
f_35(((C_word*)t0)[2]);}
else{
t4=C_u_fixnum_difference(((C_word*)t0)[3],C_fix(1));
/* benchmark.scm:2: loop */
f_61(((C_word*)t0)[2],t4);}}

/* calc in k31 in k28 */
static void C_fcall f_35(C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[2],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_35,NULL,1,t1);}
t2=(*a=C_CLOSURE_TYPE|1,a[1]=(C_word)f_41,tmp=(C_word)a,a+=2,tmp);
/* benchmark.scm:1: a */
t3=t2;
f_41(5,t3,t1,t2,C_fix(200000),C_fix(1));}

/* a40 in calc in k31 in k28 */
static void C_ccall f_41(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_41,5,t0,t1,t2,t3,t4);}
t5=t3;
t6=C_eqp(t5,C_fix(0));
if(C_truep(t6)){
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,t4);}
else{
t7=C_u_fixnum_plus(t3,C_fix(-1));
t8=C_u_fixnum_plus(t3,t4);
/* benchmark.scm:1: cont */
t9=t2;
((C_proc5)(void*)(*((C_word*)t9+1)))(5,t9,t1,t2,t7,t8);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[12] = {
{"toplevel:benchmark_scm",(void*)C_toplevel},
{"f_30:benchmark_scm",(void*)f_30},
{"f_33:benchmark_scm",(void*)f_33},
{"f_94:benchmark_scm",(void*)f_94},
{"f_84:benchmark_scm",(void*)f_84},
{"f_90:benchmark_scm",(void*)f_90},
{"f_87:benchmark_scm",(void*)f_87},
{"f_61:benchmark_scm",(void*)f_61},
{"f_65:benchmark_scm",(void*)f_65},
{"f_35:benchmark_scm",(void*)f_35},
{"f_41:benchmark_scm",(void*)f_41},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}
/* end of file */
