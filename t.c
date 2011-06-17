/* Generated from t.scm by the CHICKEN compiler
   http://www.call-with-current-continuation.org
   2011-06-15 13:29
   Version 4.6.0 
   linux-unix-gnu-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2010-11-09 on archlinux (Linux)
   command line: t.scm
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[5];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,8),40,108,111,111,112,49,49,41};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_33)
static void C_ccall f_33(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_36)
static void C_ccall f_36(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_50)
static void C_fcall f_50(C_word t0,C_word t1) C_noret;
C_noret_decl(f_74)
static void C_ccall f_74(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_60)
static void C_ccall f_60(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_63)
static void C_ccall f_63(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_39)
static void C_ccall f_39(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_42)
static void C_ccall f_42(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_48)
static void C_ccall f_48(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_45)
static void C_ccall f_45(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(trf_50)
static void C_fcall trf_50(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_50(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_50(t0,t1);}

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
C_initialize_lf(lf,5);
lf[0]=C_h_intern(&lf[0],25,"\003sysimplicit-exit-handler");
lf[1]=C_h_intern(&lf[1],7,"display");
lf[2]=C_decode_literal(C_heaptop,"\376B\000\000\003end");
lf[3]=C_decode_literal(C_heaptop,"\376B\000\000\005echo.");
lf[4]=C_h_intern(&lf[4],4,"read");
C_register_lf2(lf,5,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_33,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k31 */
static void C_ccall f_33(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_33,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_36,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k34 in k31 */
static void C_ccall f_36(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_36,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_39,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_50,a[2]=t4,a[3]=((C_word)li0),tmp=(C_word)a,a+=4,tmp));
t6=((C_word*)t4)[1];
f_50(t6,t2);}

/* loop11 in k34 in k31 */
static void C_fcall f_50(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_50,NULL,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_74,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("t.scm:2: read");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[4]+1)))(2,*((C_word*)lf[4]+1),t2);}

/* k72 in loop11 in k34 in k31 */
static void C_ccall f_74(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_74,2,t0,t1);}
t2=C_eofp(t1);
t3=C_i_not(t2);
if(C_truep(t3)){
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_60,a[2]=t3,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("t.scm:2: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[1]+1)))(3,*((C_word*)lf[1]+1),t4,lf[3]);}
else{
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_UNDEFINED);}}

/* k58 in k72 in loop11 in k34 in k31 */
static void C_ccall f_60(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_60,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_63,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("t.scm:2: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[1]+1)))(3,*((C_word*)lf[1]+1),t2,((C_word*)t0)[2]);}

/* k61 in k58 in k72 in loop11 in k34 in k31 */
static void C_ccall f_63(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("loop1114");
t2=((C_word*)((C_word*)t0)[3])[1];
f_50(t2,((C_word*)t0)[2]);}

/* k37 in k34 in k31 */
static void C_ccall f_39(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_39,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_42,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("t.scm:3: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[1]+1)))(3,*((C_word*)lf[1]+1),t2,lf[2]);}

/* k40 in k37 in k34 in k31 */
static void C_ccall f_42(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_42,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_45,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_48,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[0]))(2,*((C_word*)lf[0]+1),t3);}

/* k46 in k40 in k37 in k34 in k31 */
static void C_ccall f_48(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k43 in k40 in k37 in k34 in k31 */
static void C_ccall f_45(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[12] = {
{"toplevel:t_scm",(void*)C_toplevel},
{"f_33:t_scm",(void*)f_33},
{"f_36:t_scm",(void*)f_36},
{"f_50:t_scm",(void*)f_50},
{"f_74:t_scm",(void*)f_74},
{"f_60:t_scm",(void*)f_60},
{"f_63:t_scm",(void*)f_63},
{"f_39:t_scm",(void*)f_39},
{"f_42:t_scm",(void*)f_42},
{"f_48:t_scm",(void*)f_48},
{"f_45:t_scm",(void*)f_45},
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
