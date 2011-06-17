/* Generated from closure-benchmark.scm by the CHICKEN compiler
   http://www.call-with-current-continuation.org
   2011-06-02 14:27
   Version 4.6.0 
   linux-unix-gnu-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2010-11-09 on archlinux (Linux)
   command line: -unsafe -fixnum-arithmetic -disable-interrupts -disable-stack-overflow-checks -inline closure-benchmark.scm -inline-limit 1 -optimize-level 5
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[3];
static double C_possibly_force_alignment;


C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_45)
static void C_ccall f_45(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_48)
static void C_ccall f_48(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_160)
static void C_ccall f_160(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_128)
static void C_fcall f_128(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_153)
static void C_ccall f_153(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_149)
static void C_ccall f_149(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_132)
static void C_ccall f_132(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_56)
static void C_fcall f_56(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_93)
static void C_ccall f_93(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_62)
static void C_ccall f_62(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_166)
static void C_ccall f_166(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_172)
static void C_ccall f_172(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_169)
static void C_ccall f_169(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(trf_128)
static void C_fcall trf_128(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_128(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_128(t0,t1,t2);}

C_noret_decl(trf_56)
static void C_fcall trf_56(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_56(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_56(t0,t1,t2);}

C_noret_decl(tr6)
static void C_fcall tr6(C_proc6 k) C_regparm C_noret;
C_regparm static void C_fcall tr6(C_proc6 k){
C_word t5=C_pick(0);
C_word t4=C_pick(1);
C_word t3=C_pick(2);
C_word t2=C_pick(3);
C_word t1=C_pick(4);
C_word t0=C_pick(5);
C_adjust_stack(-6);
(k)(6,t0,t1,t2,t3,t4,t5);}

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
if(!C_demand_2(30)){
C_save(t1);
C_rereclaim2(30*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,3);
lf[0]=C_h_intern(&lf[0],25,"\003sysimplicit-exit-handler");
lf[1]=C_h_intern(&lf[1],5,"print");
lf[2]=C_h_intern(&lf[2],4,"read");
C_register_lf2(lf,3,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_45,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k43 */
static void C_ccall f_45(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_45,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_48,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k46 in k43 */
static void C_ccall f_48(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word ab[19],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_48,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_166,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=C_fix(0);
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_SCHEME_UNDEFINED;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_56,a[2]=t4,tmp=(C_word)a,a+=3,tmp));
t10=C_set_block_item(t8,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_128,a[2]=t6,a[3]=t8,tmp=(C_word)a,a+=4,tmp));
t11=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_160,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
/* closure-benchmark.scm:8: loop */
t12=((C_word*)t8)[1];
f_128(t12,t11,C_fix(8));}

/* k158 in k46 in k43 */
static void C_ccall f_160(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
/* closure-benchmark.scm:8: print */
t2=*((C_word*)lf[1]+1);
((C_proc3)(void*)(*((C_word*)t2+1)))(3,t2,((C_word*)t0)[2],t1);}

/* loop in k46 in k43 */
static void C_fcall f_128(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[12],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_128,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_132,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_149,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_153,a[2]=t4,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
/* closure-benchmark.scm:7: read */
t6=*((C_word*)lf[2]+1);
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* k151 in loop in k46 in k43 */
static void C_ccall f_153(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
/* closure-benchmark.scm:7: calc */
t2=((C_word*)((C_word*)t0)[3])[1];
f_56(t2,((C_word*)t0)[2],t1);}

/* k147 in loop in k46 in k43 */
static void C_ccall f_149(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
/* closure-benchmark.scm:7: print */
t2=*((C_word*)lf[1]+1);
((C_proc3)(void*)(*((C_word*)t2+1)))(3,t2,((C_word*)t0)[2],t1);}

/* k130 in loop in k46 in k43 */
static void C_ccall f_132(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
t2=((C_word*)t0)[4];
t3=C_eqp(t2,C_fix(0));
if(C_truep(t3)){
t4=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_fix(1));}
else{
t4=C_u_fixnum_difference(((C_word*)t0)[4],C_fix(1));
/* closure-benchmark.scm:7: loop */
t5=((C_word*)((C_word*)t0)[2])[1];
f_128(t5,((C_word*)t0)[3],t4);}}

/* calc in k46 in k43 */
static void C_fcall f_56(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[8],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_56,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_62,a[2]=t2,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_93,a[2]=t2,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
/* closure-benchmark.scm:3: a */
t5=t3;
f_62(6,t5,t1,t3,t4,C_fix(200000),C_fix(1));}

/* a92 in calc in k46 in k43 */
static void C_ccall f_93(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word *a;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_93,6,t0,t1,t2,t3,t4,t5);}
t6=C_u_fixnum_difference(((C_word*)((C_word*)t0)[3])[1],C_fix(2));
t7=C_mutate(((C_word *)((C_word*)t0)[3])+1,t6);
t8=t4;
t9=C_eqp(t8,C_fix(0));
if(C_truep(t9)){
t10=t1;
((C_proc2)(void*)(*((C_word*)t10+1)))(2,t10,C_u_fixnum_plus(((C_word*)((C_word*)t0)[3])[1],t5));}
else{
t10=C_u_fixnum_plus(t4,C_fix(-1));
t11=C_fixnum_times(((C_word*)t0)[2],t4);
t12=C_u_fixnum_plus(t11,t5);
/* closure-benchmark.scm:5: cont2 */
t13=t3;
((C_proc6)(void*)(*((C_word*)t13+1)))(6,t13,t1,t3,t2,t10,t12);}}

/* a61 in calc in k46 in k43 */
static void C_ccall f_62(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word *a;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_62,6,t0,t1,t2,t3,t4,t5);}
t6=C_u_fixnum_plus(((C_word*)((C_word*)t0)[3])[1],C_fix(3));
t7=C_mutate(((C_word *)((C_word*)t0)[3])+1,t6);
t8=t4;
t9=C_eqp(t8,C_fix(0));
if(C_truep(t9)){
t10=t1;
((C_proc2)(void*)(*((C_word*)t10+1)))(2,t10,C_u_fixnum_plus(((C_word*)((C_word*)t0)[3])[1],t5));}
else{
t10=C_u_fixnum_plus(t4,C_fix(-1));
t11=C_fixnum_times(((C_word*)t0)[2],t4);
t12=C_u_fixnum_plus(t11,t5);
/* closure-benchmark.scm:4: cont2 */
t13=t3;
((C_proc6)(void*)(*((C_word*)t13+1)))(6,t13,t1,t3,t2,t10,t12);}}

/* k164 in k46 in k43 */
static void C_ccall f_166(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_166,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_169,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_172,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
/* ##sys#implicit-exit-handler */
t4=*((C_word*)lf[0]+1);
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* k170 in k164 in k46 in k43 */
static void C_ccall f_172(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}

/* k167 in k164 in k46 in k43 */
static void C_ccall f_169(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[15] = {
{"toplevel:closure_benchmark_scm",(void*)C_toplevel},
{"f_45:closure_benchmark_scm",(void*)f_45},
{"f_48:closure_benchmark_scm",(void*)f_48},
{"f_160:closure_benchmark_scm",(void*)f_160},
{"f_128:closure_benchmark_scm",(void*)f_128},
{"f_153:closure_benchmark_scm",(void*)f_153},
{"f_149:closure_benchmark_scm",(void*)f_149},
{"f_132:closure_benchmark_scm",(void*)f_132},
{"f_56:closure_benchmark_scm",(void*)f_56},
{"f_93:closure_benchmark_scm",(void*)f_93},
{"f_62:closure_benchmark_scm",(void*)f_62},
{"f_166:closure_benchmark_scm",(void*)f_166},
{"f_172:closure_benchmark_scm",(void*)f_172},
{"f_169:closure_benchmark_scm",(void*)f_169},
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
