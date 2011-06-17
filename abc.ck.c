/* Generated from abc.ck.scm by the CHICKEN compiler
   http://www.call-with-current-continuation.org
   2011-06-15 13:41
   Version 4.6.0 
   linux-unix-gnu-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2010-11-09 on archlinux (Linux)
   command line: abc.ck.scm
   used units: library eval srfi_1
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_srfi_1_toplevel)
C_externimport void C_ccall C_srfi_1_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[148];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,16),40,119,114,105,116,101,45,98,121,116,101,32,98,57,57,41};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,7),40,102,108,117,115,104,41,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,12),40,97,49,50,55,50,32,105,49,49,50,41,0,0,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,12),40,97,49,50,56,53,32,105,49,49,53,41,0,0,0,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,12),40,97,49,51,48,55,32,105,49,49,56,41,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,12),40,97,49,51,50,48,32,105,49,50,49,41,0,0,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,12),40,97,49,51,51,54,32,105,49,50,52,41,0,0,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,12),40,97,49,51,53,54,32,105,49,50,57,41,0,0,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,18),40,102,95,49,50,53,50,32,120,49,48,54,32,121,49,48,55,41,0,0,0,0,0,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,22),40,114,101,108,111,99,97,116,105,111,110,45,112,114,111,99,32,112,49,48,53,41,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,12),40,97,49,52,52,54,32,105,49,52,48,41,0,0,0,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,12),40,97,49,52,53,57,32,105,49,52,51,41,0,0,0,0};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,29),40,101,110,99,32,98,105,116,115,49,51,53,32,108,101,110,49,51,54,32,118,97,108,117,101,49,51,55,41,0,0,0};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,18),40,97,49,52,56,52,32,103,49,54,48,49,54,49,49,54,50,41,0,0,0,0,0,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,18),40,97,49,52,57,48,32,103,49,53,51,49,53,52,49,53,53,41,0,0,0,0,0,0};
static C_char C_TLS li15[] C_aligned={C_lihdr(0,0,16),40,114,101,103,105,32,114,101,103,115,121,109,49,52,55,41};
static C_char C_TLS li16[] C_aligned={C_lihdr(0,0,14),40,97,49,53,49,56,32,46,32,95,49,57,53,41,0,0};
static C_char C_TLS li17[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,49,55,50,32,103,49,56,51,49,56,57,41,0,0,0,0,0,0,0};
static C_char C_TLS li18[] C_aligned={C_lihdr(0,0,19),40,109,97,99,104,105,110,101,45,99,111,100,101,32,120,49,54,54,41,0,0,0,0,0};
static C_char C_TLS li19[] C_aligned={C_lihdr(0,0,22),40,114,111,112,97,100,100,54,52,32,114,49,49,57,55,32,114,50,49,57,56,41,0,0};
static C_char C_TLS li20[] C_aligned={C_lihdr(0,0,24),40,114,111,112,115,116,111,114,101,54,52,32,114,49,50,48,51,32,114,50,50,48,52,41};
static C_char C_TLS li21[] C_aligned={C_lihdr(0,0,23),40,114,111,112,114,101,97,100,54,52,32,114,49,50,48,57,32,114,50,50,49,48,41,0};
static C_char C_TLS li22[] C_aligned={C_lihdr(0,0,24),40,114,111,112,99,111,110,115,116,54,52,32,114,49,50,49,53,32,105,49,50,49,54,41};
static C_char C_TLS li23[] C_aligned={C_lihdr(0,0,23),40,114,111,112,105,109,117,108,54,52,32,114,49,50,50,49,32,114,50,50,50,50,41,0};
static C_char C_TLS li24[] C_aligned={C_lihdr(0,0,17),40,114,111,112,99,97,108,108,54,52,32,114,49,50,50,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li25[] C_aligned={C_lihdr(0,0,19),40,114,111,112,114,101,108,99,97,108,108,51,50,32,105,50,51,50,41,0,0,0,0,0};
static C_char C_TLS li26[] C_aligned={C_lihdr(0,0,13),40,111,112,106,101,51,50,32,105,50,51,55,41,0,0,0};
static C_char C_TLS li27[] C_aligned={C_lihdr(0,0,14),40,111,112,106,109,112,51,50,32,105,50,52,50,41,0,0};
static C_char C_TLS li28[] C_aligned={C_lihdr(0,0,24),40,115,111,112,97,100,100,54,52,32,114,49,50,52,55,32,105,100,120,49,50,52,56,41};
static C_char C_TLS li29[] C_aligned={C_lihdr(0,0,25),40,115,111,112,105,109,117,108,54,52,32,114,49,50,53,54,32,105,100,120,49,50,53,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li30[] C_aligned={C_lihdr(0,0,24),40,115,111,112,109,111,118,54,52,32,114,49,50,54,53,32,105,100,120,49,50,54,54,41};
static C_char C_TLS li31[] C_aligned={C_lihdr(0,0,24),40,115,111,112,99,109,112,54,52,32,114,49,50,55,52,32,105,100,120,49,50,55,53,41};
static C_char C_TLS li32[] C_aligned={C_lihdr(0,0,25),40,115,111,112,114,101,97,100,54,52,32,114,49,50,56,51,32,105,100,120,49,50,56,52,41,0,0,0,0,0,0,0};
static C_char C_TLS li33[] C_aligned={C_lihdr(0,0,28),40,115,111,112,115,116,111,114,101,54,52,32,105,100,120,49,50,56,57,32,105,100,120,50,50,57,48,41,0,0,0,0};
static C_char C_TLS li34[] C_aligned={C_lihdr(0,0,18),40,115,111,112,99,97,108,108,54,52,32,105,100,120,50,57,53,41,0,0,0,0,0,0};
static C_char C_TLS li35[] C_aligned={C_lihdr(0,0,25),40,114,115,111,112,109,111,118,54,52,32,105,100,120,49,51,48,48,32,114,49,51,48,49,41,0,0,0,0,0,0,0};
static C_char C_TLS li36[] C_aligned={C_lihdr(0,0,14),40,114,115,112,117,115,104,32,114,49,51,48,57,41,0,0};
static C_char C_TLS li37[] C_aligned={C_lihdr(0,0,6),40,99,114,101,116,41,0,0};
static C_char C_TLS li38[] C_aligned={C_lihdr(0,0,12),40,108,101,97,118,101,32,110,51,49,53,41,0,0,0,0};
static C_char C_TLS li39[] C_aligned={C_lihdr(0,0,12),40,102,114,97,109,101,32,110,51,50,48,41,0,0,0,0};
static C_char C_TLS li40[] C_aligned={C_lihdr(0,0,11),40,108,114,101,102,32,110,51,50,53,41,0,0,0,0,0};
static C_char C_TLS li41[] C_aligned={C_lihdr(0,0,16),40,99,111,110,115,116,54,52,32,105,109,109,51,50,55,41};
static C_char C_TLS li42[] C_aligned={C_lihdr(0,0,15),40,102,95,50,49,53,53,32,105,100,120,51,51,48,41,0};
static C_char C_TLS li43[] C_aligned={C_lihdr(0,0,20),40,102,117,110,99,45,50,45,50,45,49,32,105,110,115,116,51,50,57,41,0,0,0,0};
static C_char C_TLS li44[] C_aligned={C_lihdr(0,0,15),40,102,95,50,49,54,51,32,105,100,120,51,51,51,41,0};
static C_char C_TLS li45[] C_aligned={C_lihdr(0,0,20),40,102,117,110,99,45,50,45,49,45,49,32,105,110,115,116,51,51,50,41,0,0,0,0};
static C_char C_TLS li46[] C_aligned={C_lihdr(0,0,8),40,114,101,97,100,54,52,41};
static C_char C_TLS li47[] C_aligned={C_lihdr(0,0,6),40,112,117,115,104,41,0,0};
static C_char C_TLS li48[] C_aligned={C_lihdr(0,0,12),40,97,50,50,48,50,32,109,51,53,49,41,0,0,0,0};
static C_char C_TLS li49[] C_aligned={C_lihdr(0,0,31),40,109,97,107,101,45,99,108,111,115,117,114,101,32,112,114,111,99,51,52,57,32,102,114,97,109,101,51,53,48,41,0};
static C_char C_TLS li50[] C_aligned={C_lihdr(0,0,18),40,63,112,114,111,99,101,100,117,114,101,63,32,108,51,53,56,41,0,0,0,0,0,0};
static C_char C_TLS li51[] C_aligned={C_lihdr(0,0,16),40,63,98,117,105,108,116,105,110,63,32,108,51,54,49,41};
static C_char C_TLS li52[] C_aligned={C_lihdr(0,0,15),40,63,115,121,110,116,97,120,63,32,108,51,54,52,41,0};
static C_char C_TLS li53[] C_aligned={C_lihdr(0,0,37),40,114,101,108,111,99,97,116,105,111,110,45,115,121,109,98,111,108,32,118,97,108,117,101,51,54,55,32,115,117,98,115,116,51,54,56,41,0,0,0};
static C_char C_TLS li54[] C_aligned={C_lihdr(0,0,11),40,103,51,57,53,32,98,52,48,50,41,0,0,0,0,0};
static C_char C_TLS li55[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,51,57,52,32,103,51,57,57,52,48,57,41,0,0,0,0,0,0,0};
static C_char C_TLS li56[] C_aligned={C_lihdr(0,0,11),40,108,111,111,112,32,97,51,57,49,41,0,0,0,0,0};
static C_char C_TLS li57[] C_aligned={C_lihdr(0,0,11),40,103,51,55,55,32,112,51,56,53,41,0,0,0,0,0};
static C_char C_TLS li58[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,51,55,54,32,103,51,56,49,52,49,56,41,0,0,0,0,0,0,0};
static C_char C_TLS li59[] C_aligned={C_lihdr(0,0,15),40,115,105,122,101,32,112,114,111,99,115,51,55,51,41,0};
static C_char C_TLS li60[] C_aligned={C_lihdr(0,0,8),40,102,95,50,52,53,55,41};
static C_char C_TLS li61[] C_aligned={C_lihdr(0,0,11),40,103,52,50,56,32,97,52,53,48,41,0,0,0,0,0};
static C_char C_TLS li62[] C_aligned={C_lihdr(0,0,12),40,97,50,53,56,48,32,105,52,56,51,41,0,0,0,0};
static C_char C_TLS li63[] C_aligned={C_lihdr(0,0,12),40,97,50,53,56,51,32,105,52,56,52,41,0,0,0,0};
static C_char C_TLS li64[] C_aligned={C_lihdr(0,0,12),40,97,50,56,49,48,32,105,53,55,48,41,0,0,0,0};
static C_char C_TLS li65[] C_aligned={C_lihdr(0,0,12),40,97,50,56,49,52,32,105,53,55,50,41,0,0,0,0};
static C_char C_TLS li66[] C_aligned={C_lihdr(0,0,12),40,97,50,56,51,51,32,105,53,54,50,41,0,0,0,0};
static C_char C_TLS li67[] C_aligned={C_lihdr(0,0,12),40,97,50,56,51,55,32,105,53,54,52,41,0,0,0,0};
static C_char C_TLS li68[] C_aligned={C_lihdr(0,0,12),40,97,50,56,53,54,32,105,53,53,53,41,0,0,0,0};
static C_char C_TLS li69[] C_aligned={C_lihdr(0,0,12),40,97,50,56,53,57,32,105,53,53,54,41,0,0,0,0};
static C_char C_TLS li70[] C_aligned={C_lihdr(0,0,12),40,97,50,56,56,57,32,105,53,52,55,41,0,0,0,0};
static C_char C_TLS li71[] C_aligned={C_lihdr(0,0,12),40,97,50,56,57,51,32,105,53,52,57,41,0,0,0,0};
static C_char C_TLS li72[] C_aligned={C_lihdr(0,0,12),40,97,50,57,49,54,32,105,53,52,48,41,0,0,0,0};
static C_char C_TLS li73[] C_aligned={C_lihdr(0,0,12),40,97,50,57,49,57,32,105,53,52,49,41,0,0,0,0};
static C_char C_TLS li74[] C_aligned={C_lihdr(0,0,12),40,97,50,57,50,50,32,105,53,51,56,41,0,0,0,0};
static C_char C_TLS li75[] C_aligned={C_lihdr(0,0,12),40,97,50,57,50,53,32,105,53,51,57,41,0,0,0,0};
static C_char C_TLS li76[] C_aligned={C_lihdr(0,0,11),40,103,53,56,53,32,101,53,57,50,41,0,0,0,0,0};
static C_char C_TLS li77[] C_aligned={C_lihdr(0,0,12),40,97,50,57,57,49,32,105,54,48,57,41,0,0,0,0};
static C_char C_TLS li78[] C_aligned={C_lihdr(0,0,12),40,97,50,57,57,52,32,105,54,49,48,41,0,0,0,0};
static C_char C_TLS li79[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,53,56,52,32,103,53,56,57,54,48,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li80[] C_aligned={C_lihdr(0,0,11),40,103,54,50,51,32,101,54,51,48,41,0,0,0,0,0};
static C_char C_TLS li81[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,54,50,50,32,103,54,50,55,54,51,56,41,0,0,0,0,0,0,0};
static C_char C_TLS li82[] C_aligned={C_lihdr(0,0,14),40,108,111,111,112,32,115,101,120,112,52,54,52,41,0,0};
static C_char C_TLS li83[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,52,50,55,32,103,52,51,50,52,53,52,41,0,0,0,0,0,0,0};
static C_char C_TLS li84[] C_aligned={C_lihdr(0,0,24),40,99,111,109,112,105,108,101,45,108,97,109,98,100,97,45,112,51,32,115,52,50,52,41};
static C_char C_TLS li85[] C_aligned={C_lihdr(0,0,18),40,97,52,56,57,52,32,103,54,55,57,54,56,48,54,56,49,41,0,0,0,0,0,0};
static C_char C_TLS li86[] C_aligned={C_lihdr(0,0,18),40,97,52,56,56,48,32,103,54,57,48,54,57,49,54,57,50,41,0,0,0,0,0,0};
static C_char C_TLS li87[] C_aligned={C_lihdr(0,0,7),40,97,52,56,50,54,41,0};
static C_char C_TLS li88[] C_aligned={C_lihdr(0,0,12),40,97,52,55,55,52,32,105,55,49,57,41,0,0,0,0};
static C_char C_TLS li89[] C_aligned={C_lihdr(0,0,12),40,97,52,55,55,55,32,105,55,50,48,41,0,0,0,0};
static C_char C_TLS li90[] C_aligned={C_lihdr(0,0,12),40,97,52,55,57,52,32,105,55,49,53,41,0,0,0,0};
static C_char C_TLS li91[] C_aligned={C_lihdr(0,0,12),40,97,52,55,57,55,32,105,55,49,54,41,0,0,0,0};
static C_char C_TLS li92[] C_aligned={C_lihdr(0,0,25),40,97,52,55,53,50,32,112,114,111,99,49,55,49,48,32,112,114,111,99,50,55,49,49,41,0,0,0,0,0,0,0};
static C_char C_TLS li93[] C_aligned={C_lihdr(0,0,15),40,114,101,108,45,99,97,108,108,32,105,55,50,56,41,0};
static C_char C_TLS li94[] C_aligned={C_lihdr(0,0,11),40,103,55,52,51,32,115,55,53,51,41,0,0,0,0,0};
static C_char C_TLS li95[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,55,51,55,32,103,55,52,56,55,53,57,41,0,0,0,0,0,0,0};
static C_char C_TLS li96[] C_aligned={C_lihdr(0,0,43),40,99,111,109,112,105,108,101,45,108,97,109,98,100,97,45,112,53,32,111,112,99,111,100,101,55,51,51,32,114,101,108,111,99,97,116,105,111,110,55,51,52,41,0,0,0,0,0};
static C_char C_TLS li97[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,55,56,53,32,103,55,57,54,56,48,50,41,0,0,0,0,0,0,0};
static C_char C_TLS li98[] C_aligned={C_lihdr(0,0,32),40,102,114,97,109,101,45,118,97,114,105,97,98,108,101,45,115,117,98,115,116,105,116,117,116,101,32,115,55,55,57,41};
static C_char C_TLS li99[] C_aligned={C_lihdr(0,0,11),40,103,55,55,48,32,115,56,49,48,41,0,0,0,0,0};
static C_char C_TLS li100[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,55,54,57,32,103,55,55,52,56,51,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li101[] C_aligned={C_lihdr(0,0,35),40,99,111,109,112,105,108,101,45,108,97,109,98,100,97,45,112,52,45,104,117,109,97,110,32,111,112,99,111,100,101,55,54,54,41,0,0,0,0,0};
static C_char C_TLS li102[] C_aligned={C_lihdr(0,0,7),40,97,51,56,50,55,41,0};
static C_char C_TLS li103[] C_aligned={C_lihdr(0,0,12),40,108,111,111,112,32,105,116,56,57,51,41,0,0,0,0};
static C_char C_TLS li104[] C_aligned={C_lihdr(0,0,27),40,97,51,56,51,51,32,105,110,56,56,57,32,111,117,116,56,57,48,32,112,105,100,56,57,49,41,0,0,0,0,0};
static C_char C_TLS li105[] C_aligned={C_lihdr(0,0,7),40,97,51,56,56,49,41,0};
static C_char C_TLS li106[] C_aligned={C_lihdr(0,0,12),40,108,111,111,112,32,105,116,56,56,51,41,0,0,0,0};
static C_char C_TLS li107[] C_aligned={C_lihdr(0,0,27),40,97,51,56,56,55,32,105,110,56,55,57,32,111,117,116,56,56,48,32,112,105,100,56,56,49,41,0,0,0,0,0};
static C_char C_TLS li108[] C_aligned={C_lihdr(0,0,7),40,97,51,57,50,57,41,0};
static C_char C_TLS li109[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,56,54,51,32,103,56,54,56,56,55,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li110[] C_aligned={C_lihdr(0,0,14),40,109,97,105,110,32,97,114,103,115,56,52,55,41,0,0};
static C_char C_TLS li111[] C_aligned={C_lihdr(0,0,6),40,103,57,49,52,41,0,0};
static C_char C_TLS li112[] C_aligned={C_lihdr(0,0,11),40,103,57,51,51,32,98,57,52,48,41,0,0,0,0,0};
static C_char C_TLS li113[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,57,51,50,32,103,57,51,55,57,52,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li114[] C_aligned={C_lihdr(0,0,14),40,108,111,111,112,50,32,115,101,113,57,50,57,41,0,0};
static C_char C_TLS li115[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,57,49,51,32,103,57,49,56,57,50,52,41,0,0,0,0,0,0,0};
static C_char C_TLS li116[] C_aligned={C_lihdr(0,0,11),40,108,111,111,112,32,112,57,49,48,41,0,0,0,0,0};
static C_char C_TLS li117[] C_aligned={C_lihdr(0,0,21),40,100,117,109,112,45,112,114,111,99,115,32,112,114,111,99,115,57,48,55,41,0,0,0};
static C_char C_TLS li118[] C_aligned={C_lihdr(0,0,18),40,108,111,111,112,57,56,56,32,103,57,57,57,49,48,48,53,41,0,0,0,0,0,0};
static C_char C_TLS li119[] C_aligned={C_lihdr(0,0,17),40,108,111,111,112,57,54,51,32,103,57,55,52,57,56,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li120[] C_aligned={C_lihdr(0,0,20),40,111,112,99,111,100,101,45,102,105,108,116,101,114,32,97,57,53,55,41,0,0,0,0};
static C_char C_TLS li121[] C_aligned={C_lihdr(0,0,20),40,108,111,111,112,49,48,50,54,32,103,49,48,51,49,49,48,51,55,41,0,0,0,0};
static C_char C_TLS li122[] C_aligned={C_lihdr(0,0,20),40,108,111,111,112,49,48,49,53,32,103,49,48,50,48,49,48,52,51,41,0,0,0,0};
static C_char C_TLS li123[] C_aligned={C_lihdr(0,0,20),40,111,112,99,111,100,101,45,112,114,105,110,116,32,115,49,48,49,50,41,0,0,0,0};
static C_char C_TLS li124[] C_aligned={C_lihdr(0,0,19),40,108,97,109,98,100,97,45,97,114,103,115,32,108,49,48,52,56,41,0,0,0,0,0};
static C_char C_TLS li125[] C_aligned={C_lihdr(0,0,19),40,108,97,109,98,100,97,45,98,111,100,121,32,108,49,48,53,51,41,0,0,0,0,0};
static C_char C_TLS li126[] C_aligned={C_lihdr(0,0,20),40,108,111,111,112,49,48,56,48,32,103,49,48,57,49,49,48,57,55,41,0,0,0,0};
static C_char C_TLS li127[] C_aligned={C_lihdr(0,0,20),40,108,111,111,112,49,49,48,56,32,103,49,49,49,57,49,49,50,53,41,0,0,0,0};
static C_char C_TLS li128[] C_aligned={C_lihdr(0,0,15),40,99,111,109,112,105,108,101,32,115,49,48,53,56,41,0};
static C_char C_TLS li129[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1215)
static void C_ccall f_1215(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1218)
static void C_ccall f_1218(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1221)
static void C_ccall f_1221(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1229)
static void C_ccall f_1229(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2171)
static void C_ccall f_2171(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2175)
static void C_ccall f_2175(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2179)
static void C_ccall f_2179(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2183)
static void C_ccall f_2183(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4729)
static void C_ccall f_4729(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4735)
static void C_ccall f_4735(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4732)
static void C_ccall f_4732(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4488)
static void C_ccall f_4488(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4582)
static void C_fcall f_4582(C_word t0,C_word t1) C_noret;
C_noret_decl(f_4671)
static void C_fcall f_4671(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4700)
static void C_ccall f_4700(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4684)
static void C_fcall f_4684(C_word t0,C_word t1) C_noret;
C_noret_decl(f_4601)
static void C_fcall f_4601(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4630)
static void C_ccall f_4630(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4614)
static void C_fcall f_4614(C_word t0,C_word t1) C_noret;
C_noret_decl(f_4599)
static void C_ccall f_4599(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4462)
static void C_ccall f_4462(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4436)
static void C_ccall f_4436(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4359)
static void C_ccall f_4359(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4405)
static void C_ccall f_4405(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4413)
static void C_fcall f_4413(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4381)
static void C_fcall f_4381(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4367)
static void C_ccall f_4367(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4391)
static void C_ccall f_4391(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4376)
static void C_ccall f_4376(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4423)
static void C_ccall f_4423(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4201)
static void C_ccall f_4201(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4246)
static void C_fcall f_4246(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4275)
static void C_ccall f_4275(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4259)
static void C_fcall f_4259(C_word t0,C_word t1) C_noret;
C_noret_decl(f_4296)
static void C_fcall f_4296(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4325)
static void C_ccall f_4325(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4309)
static void C_fcall f_4309(C_word t0,C_word t1) C_noret;
C_noret_decl(f_4040)
static void C_ccall f_4040(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4047)
static void C_fcall f_4047(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4066)
static void C_ccall f_4066(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4162)
static void C_fcall f_4162(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4172)
static void C_ccall f_4172(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4072)
static void C_ccall f_4072(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4088)
static void C_fcall f_4088(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4135)
static void C_fcall f_4135(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4145)
static void C_ccall f_4145(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4090)
static void C_fcall f_4090(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4097)
static void C_fcall f_4097(C_word t0,C_word t1) C_noret;
C_noret_decl(f_4110)
static void C_ccall f_4110(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4107)
static void C_ccall f_4107(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4075)
static void C_ccall f_4075(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4055)
static void C_fcall f_4055(C_word t0,C_word t1) C_noret;
C_noret_decl(f_4059)
static void C_ccall f_4059(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3775)
static void C_ccall f_3775(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3780)
static void C_ccall f_3780(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3783)
static void C_ccall f_3783(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3786)
static void C_ccall f_3786(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3795)
static void C_fcall f_3795(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3804)
static void C_fcall f_3804(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3811)
static void C_ccall f_3811(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3814)
static void C_ccall f_3814(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3965)
static void C_fcall f_3965(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3939)
static void C_ccall f_3939(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3942)
static void C_ccall f_3942(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3975)
static void C_ccall f_3975(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3817)
static void C_ccall f_3817(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3930)
static void C_ccall f_3930(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3820)
static void C_ccall f_3820(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3877)
static void C_ccall f_3877(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3888)
static void C_ccall f_3888(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_3896)
static void C_ccall f_3896(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3898)
static void C_fcall f_3898(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3911)
static void C_ccall f_3911(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3917)
static void C_ccall f_3917(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3924)
static void C_ccall f_3924(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3882)
static void C_ccall f_3882(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3823)
static void C_ccall f_3823(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3834)
static void C_ccall f_3834(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_3842)
static void C_ccall f_3842(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3844)
static void C_fcall f_3844(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3857)
static void C_ccall f_3857(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3863)
static void C_ccall f_3863(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3870)
static void C_ccall f_3870(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3828)
static void C_ccall f_3828(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3789)
static void C_ccall f_3789(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3438)
static void C_ccall f_3438(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3751)
static void C_fcall f_3751(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3761)
static void C_ccall f_3761(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3729)
static void C_ccall f_3729(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3518)
static void C_fcall f_3518(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3707)
static void C_ccall f_3707(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3679)
static void C_ccall f_3679(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3639)
static void C_ccall f_3639(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3581)
static void C_ccall f_3581(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3529)
static void C_ccall f_3529(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3444)
static void C_fcall f_3444(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3476)
static void C_fcall f_3476(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3505)
static void C_ccall f_3505(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3489)
static void C_fcall f_3489(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3320)
static void C_ccall f_3320(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_3436)
static void C_ccall f_3436(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3391)
static void C_ccall f_3391(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3399)
static void C_fcall f_3399(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3428)
static void C_ccall f_3428(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3412)
static void C_fcall f_3412(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3397)
static void C_ccall f_3397(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3326)
static void C_fcall f_3326(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3333)
static void C_fcall f_3333(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3359)
static void C_ccall f_3359(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3351)
static void C_ccall f_3351(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3282)
static void C_ccall f_3282(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3290)
static void C_ccall f_3290(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3310)
static void C_ccall f_3310(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3318)
static void C_ccall f_3318(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3314)
static void C_ccall f_3314(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3306)
static void C_ccall f_3306(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4753)
static void C_ccall f_4753(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_4798)
static void C_ccall f_4798(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4802)
static void C_ccall f_4802(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4795)
static void C_ccall f_4795(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4793)
static void C_ccall f_4793(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4761)
static void C_ccall f_4761(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4778)
static void C_ccall f_4778(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4782)
static void C_ccall f_4782(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4775)
static void C_ccall f_4775(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4773)
static void C_ccall f_4773(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4769)
static void C_ccall f_4769(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4765)
static void C_ccall f_4765(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4827)
static void C_ccall f_4827(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4871)
static void C_ccall f_4871(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4867)
static void C_ccall f_4867(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_4881)
static void C_ccall f_4881(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_4895)
static void C_ccall f_4895(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2442)
static void C_ccall f_2442(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2447)
static void C_ccall f_2447(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3248)
static void C_ccall f_3248(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2451)
static void C_ccall f_2451(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3223)
static void C_fcall f_3223(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3233)
static void C_ccall f_3233(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2489)
static void C_ccall f_2489(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2496)
static void C_ccall f_2496(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2512)
static void C_fcall f_2512(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2522)
static void C_ccall f_2522(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2552)
static void C_ccall f_2552(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2606)
static void C_ccall f_2606(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3184)
static void C_ccall f_3184(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2642)
static void C_fcall f_2642(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3170)
static void C_ccall f_3170(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2651)
static void C_fcall f_2651(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3146)
static void C_ccall f_3146(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2676)
static void C_fcall f_2676(C_word t0,C_word t1) C_noret;
C_noret_decl(f_3132)
static void C_ccall f_3132(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2717)
static void C_fcall f_2717(C_word t0,C_word t1) C_noret;
C_noret_decl(f_2768)
static void C_ccall f_2768(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2937)
static void C_ccall f_2937(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3037)
static void C_ccall f_3037(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3040)
static void C_ccall f_3040(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3088)
static void C_fcall f_3088(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3098)
static void C_ccall f_3098(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3074)
static void C_ccall f_3074(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3045)
static void C_fcall f_3045(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3049)
static void C_ccall f_3049(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3065)
static void C_ccall f_3065(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3030)
static void C_ccall f_3030(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2941)
static void C_ccall f_2941(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_3005)
static void C_fcall f_3005(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_3015)
static void C_ccall f_3015(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2974)
static void C_ccall f_2974(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2995)
static void C_ccall f_2995(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2992)
static void C_ccall f_2992(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2990)
static void C_ccall f_2990(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2945)
static void C_fcall f_2945(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2949)
static void C_ccall f_2949(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2965)
static void C_ccall f_2965(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2771)
static void C_ccall f_2771(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2926)
static void C_ccall f_2926(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2923)
static void C_ccall f_2923(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2907)
static void C_ccall f_2907(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2920)
static void C_ccall f_2920(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2917)
static void C_ccall f_2917(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2915)
static void C_ccall f_2915(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2894)
static void C_ccall f_2894(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2890)
static void C_ccall f_2890(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2888)
static void C_ccall f_2888(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2880)
static void C_ccall f_2880(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2782)
static void C_ccall f_2782(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2860)
static void C_ccall f_2860(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2857)
static void C_ccall f_2857(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2855)
static void C_ccall f_2855(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2838)
static void C_ccall f_2838(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2834)
static void C_ccall f_2834(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2832)
static void C_ccall f_2832(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2824)
static void C_ccall f_2824(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2793)
static void C_ccall f_2793(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2815)
static void C_ccall f_2815(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2811)
static void C_ccall f_2811(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2809)
static void C_ccall f_2809(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2801)
static void C_ccall f_2801(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2720)
static void C_ccall f_2720(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2723)
static void C_ccall f_2723(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2730)
static void C_ccall f_2730(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2679)
static void C_ccall f_2679(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2654)
static void C_ccall f_2654(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2600)
static void C_ccall f_2600(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2556)
static void C_ccall f_2556(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2584)
static void C_ccall f_2584(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2581)
static void C_ccall f_2581(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2579)
static void C_ccall f_2579(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2499)
static void C_ccall f_2499(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2510)
static void C_ccall f_2510(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2470)
static void C_fcall f_2470(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2483)
static void C_ccall f_2483(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2457)
static void C_ccall f_2457(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2302)
static void C_ccall f_2302(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2419)
static void C_fcall f_2419(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2429)
static void C_ccall f_2429(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2305)
static void C_fcall f_2305(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2312)
static void C_ccall f_2312(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2315)
static void C_ccall f_2315(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2407)
static void C_ccall f_2407(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2318)
static void C_ccall f_2318(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2331)
static void C_fcall f_2331(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2382)
static void C_fcall f_2382(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2392)
static void C_ccall f_2392(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2333)
static void C_fcall f_2333(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2366)
static void C_ccall f_2366(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2321)
static void C_ccall f_2321(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2288)
static void C_ccall f_2288(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_2272)
static void C_ccall f_2272(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2256)
static void C_ccall f_2256(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2240)
static void C_ccall f_2240(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2197)
static void C_ccall f_2197(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_2203)
static void C_ccall f_2203(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2207)
static void C_ccall f_2207(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2210)
static void C_ccall f_2210(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2213)
static void C_ccall f_2213(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2191)
static void C_ccall f_2191(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2185)
static void C_ccall f_2185(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2161)
static void C_ccall f_2161(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2163)
static void C_ccall f_2163(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2153)
static void C_ccall f_2153(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2155)
static void C_ccall f_2155(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2147)
static void C_ccall f_2147(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2141)
static void C_ccall f_2141(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2113)
static void C_ccall f_2113(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2139)
static void C_ccall f_2139(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2135)
static void C_ccall f_2135(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2085)
static void C_ccall f_2085(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2111)
static void C_ccall f_2111(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2107)
static void C_ccall f_2107(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2082)
static void C_ccall f_2082(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2068)
static void C_ccall f_2068(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2080)
static void C_ccall f_2080(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2076)
static void C_ccall f_2076(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2024)
static void C_ccall f_2024(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_2031)
static void C_ccall f_2031(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2066)
static void C_ccall f_2066(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2062)
static void C_ccall f_2062(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2046)
static void C_ccall f_2046(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2058)
static void C_ccall f_2058(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2054)
static void C_ccall f_2054(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2006)
static void C_ccall f_2006(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2014)
static void C_ccall f_2014(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2022)
static void C_ccall f_2022(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2018)
static void C_ccall f_2018(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1980)
static void C_ccall f_1980(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1988)
static void C_ccall f_1988(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1996)
static void C_ccall f_1996(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2004)
static void C_ccall f_2004(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_2000)
static void C_ccall f_2000(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1992)
static void C_ccall f_1992(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1962)
static void C_ccall f_1962(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1970)
static void C_ccall f_1970(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1978)
static void C_ccall f_1978(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1974)
static void C_ccall f_1974(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1918)
static void C_ccall f_1918(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1925)
static void C_ccall f_1925(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1960)
static void C_ccall f_1960(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1956)
static void C_ccall f_1956(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1940)
static void C_ccall f_1940(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1952)
static void C_ccall f_1952(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1948)
static void C_ccall f_1948(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1874)
static void C_ccall f_1874(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1881)
static void C_ccall f_1881(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1916)
static void C_ccall f_1916(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1912)
static void C_ccall f_1912(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1896)
static void C_ccall f_1896(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1908)
static void C_ccall f_1908(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1904)
static void C_ccall f_1904(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1826)
static void C_ccall f_1826(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1833)
static void C_ccall f_1833(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1872)
static void C_ccall f_1872(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1868)
static void C_ccall f_1868(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1852)
static void C_ccall f_1852(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1864)
static void C_ccall f_1864(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1860)
static void C_ccall f_1860(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1782)
static void C_ccall f_1782(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1789)
static void C_ccall f_1789(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1824)
static void C_ccall f_1824(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1820)
static void C_ccall f_1820(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1804)
static void C_ccall f_1804(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1816)
static void C_ccall f_1816(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1812)
static void C_ccall f_1812(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1768)
static void C_ccall f_1768(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1780)
static void C_ccall f_1780(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1776)
static void C_ccall f_1776(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1750)
static void C_ccall f_1750(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1766)
static void C_ccall f_1766(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1762)
static void C_ccall f_1762(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1736)
static void C_ccall f_1736(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1748)
static void C_ccall f_1748(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1744)
static void C_ccall f_1744(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1718)
static void C_ccall f_1718(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1734)
static void C_ccall f_1734(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1730)
static void C_ccall f_1730(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1684)
static void C_ccall f_1684(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1708)
static void C_ccall f_1708(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1716)
static void C_ccall f_1716(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1712)
static void C_ccall f_1712(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1704)
static void C_ccall f_1704(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1658)
static void C_ccall f_1658(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1682)
static void C_ccall f_1682(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1670)
static void C_ccall f_1670(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1678)
static void C_ccall f_1678(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1674)
static void C_ccall f_1674(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1628)
static void C_ccall f_1628(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1648)
static void C_ccall f_1648(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1656)
static void C_ccall f_1656(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1652)
static void C_ccall f_1652(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1644)
static void C_ccall f_1644(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1598)
static void C_ccall f_1598(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1618)
static void C_ccall f_1618(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1626)
static void C_ccall f_1626(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1622)
static void C_ccall f_1622(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1614)
static void C_ccall f_1614(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1568)
static void C_ccall f_1568(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1588)
static void C_ccall f_1588(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1596)
static void C_ccall f_1596(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1592)
static void C_ccall f_1592(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1584)
static void C_ccall f_1584(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1497)
static void C_ccall f_1497(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1533)
static void C_fcall f_1533(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1562)
static void C_ccall f_1562(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1546)
static void C_fcall f_1546(C_word t0,C_word t1) C_noret;
C_noret_decl(f_1531)
static void C_ccall f_1531(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1519)
static void C_ccall f_1519(C_word c,C_word t0,C_word t1,...) C_noret;
C_noret_decl(f_1473)
static void C_ccall f_1473(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1491)
static void C_ccall f_1491(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1477)
static void C_ccall f_1477(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1485)
static void C_ccall f_1485(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1383)
static void C_ccall f_1383(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_1460)
static void C_ccall f_1460(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1471)
static void C_ccall f_1471(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1447)
static void C_ccall f_1447(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1458)
static void C_ccall f_1458(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1445)
static void C_ccall f_1445(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1438)
static void C_ccall f_1438(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1434)
static void C_ccall f_1434(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1393)
static void C_ccall f_1393(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1430)
static void C_ccall f_1430(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1426)
static void C_ccall f_1426(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1406)
static void C_ccall f_1406(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1422)
static void C_ccall f_1422(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1418)
static void C_ccall f_1418(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1410)
static void C_ccall f_1410(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1250)
static void C_ccall f_1250(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1252)
static void C_ccall f_1252(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1357)
static void C_ccall f_1357(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1368)
static void C_ccall f_1368(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1375)
static void C_ccall f_1375(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1337)
static void C_ccall f_1337(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1348)
static void C_ccall f_1348(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1355)
static void C_ccall f_1355(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1321)
static void C_ccall f_1321(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1332)
static void C_ccall f_1332(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1308)
static void C_ccall f_1308(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1319)
static void C_ccall f_1319(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1286)
static void C_ccall f_1286(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1297)
static void C_ccall f_1297(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1273)
static void C_ccall f_1273(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1284)
static void C_ccall f_1284(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1241)
static void C_ccall f_1241(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1231)
static void C_ccall f_1231(C_word c,C_word t0,C_word t1,C_word t2) C_noret;

C_noret_decl(trf_4582)
static void C_fcall trf_4582(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4582(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_4582(t0,t1);}

C_noret_decl(trf_4671)
static void C_fcall trf_4671(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4671(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4671(t0,t1,t2);}

C_noret_decl(trf_4684)
static void C_fcall trf_4684(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4684(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_4684(t0,t1);}

C_noret_decl(trf_4601)
static void C_fcall trf_4601(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4601(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4601(t0,t1,t2);}

C_noret_decl(trf_4614)
static void C_fcall trf_4614(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4614(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_4614(t0,t1);}

C_noret_decl(trf_4413)
static void C_fcall trf_4413(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4413(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4413(t0,t1,t2);}

C_noret_decl(trf_4381)
static void C_fcall trf_4381(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4381(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4381(t0,t1,t2);}

C_noret_decl(trf_4246)
static void C_fcall trf_4246(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4246(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4246(t0,t1,t2);}

C_noret_decl(trf_4259)
static void C_fcall trf_4259(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4259(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_4259(t0,t1);}

C_noret_decl(trf_4296)
static void C_fcall trf_4296(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4296(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4296(t0,t1,t2);}

C_noret_decl(trf_4309)
static void C_fcall trf_4309(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4309(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_4309(t0,t1);}

C_noret_decl(trf_4047)
static void C_fcall trf_4047(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4047(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4047(t0,t1,t2);}

C_noret_decl(trf_4162)
static void C_fcall trf_4162(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4162(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4162(t0,t1,t2);}

C_noret_decl(trf_4088)
static void C_fcall trf_4088(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4088(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4088(t0,t1,t2);}

C_noret_decl(trf_4135)
static void C_fcall trf_4135(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4135(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4135(t0,t1,t2);}

C_noret_decl(trf_4090)
static void C_fcall trf_4090(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4090(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_4090(t0,t1,t2);}

C_noret_decl(trf_4097)
static void C_fcall trf_4097(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4097(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_4097(t0,t1);}

C_noret_decl(trf_4055)
static void C_fcall trf_4055(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_4055(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_4055(t0,t1);}

C_noret_decl(trf_3795)
static void C_fcall trf_3795(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3795(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_3795(t0,t1);}

C_noret_decl(trf_3804)
static void C_fcall trf_3804(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3804(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_3804(t0,t1);}

C_noret_decl(trf_3965)
static void C_fcall trf_3965(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3965(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3965(t0,t1,t2);}

C_noret_decl(trf_3898)
static void C_fcall trf_3898(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3898(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3898(t0,t1,t2);}

C_noret_decl(trf_3844)
static void C_fcall trf_3844(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3844(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3844(t0,t1,t2);}

C_noret_decl(trf_3751)
static void C_fcall trf_3751(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3751(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3751(t0,t1,t2);}

C_noret_decl(trf_3518)
static void C_fcall trf_3518(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3518(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3518(t0,t1,t2);}

C_noret_decl(trf_3444)
static void C_fcall trf_3444(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3444(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3444(t0,t1,t2);}

C_noret_decl(trf_3476)
static void C_fcall trf_3476(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3476(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3476(t0,t1,t2);}

C_noret_decl(trf_3489)
static void C_fcall trf_3489(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3489(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_3489(t0,t1);}

C_noret_decl(trf_3399)
static void C_fcall trf_3399(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3399(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3399(t0,t1,t2);}

C_noret_decl(trf_3412)
static void C_fcall trf_3412(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3412(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_3412(t0,t1);}

C_noret_decl(trf_3326)
static void C_fcall trf_3326(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3326(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3326(t0,t1,t2);}

C_noret_decl(trf_3333)
static void C_fcall trf_3333(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3333(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_3333(t0,t1);}

C_noret_decl(trf_3223)
static void C_fcall trf_3223(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3223(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3223(t0,t1,t2);}

C_noret_decl(trf_2512)
static void C_fcall trf_2512(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2512(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2512(t0,t1,t2);}

C_noret_decl(trf_2642)
static void C_fcall trf_2642(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2642(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_2642(t0,t1);}

C_noret_decl(trf_2651)
static void C_fcall trf_2651(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2651(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_2651(t0,t1);}

C_noret_decl(trf_2676)
static void C_fcall trf_2676(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2676(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_2676(t0,t1);}

C_noret_decl(trf_2717)
static void C_fcall trf_2717(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2717(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_2717(t0,t1);}

C_noret_decl(trf_3088)
static void C_fcall trf_3088(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3088(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3088(t0,t1,t2);}

C_noret_decl(trf_3045)
static void C_fcall trf_3045(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3045(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3045(t0,t1,t2);}

C_noret_decl(trf_3005)
static void C_fcall trf_3005(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_3005(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_3005(t0,t1,t2);}

C_noret_decl(trf_2945)
static void C_fcall trf_2945(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2945(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2945(t0,t1,t2);}

C_noret_decl(trf_2470)
static void C_fcall trf_2470(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2470(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2470(t0,t1,t2);}

C_noret_decl(trf_2419)
static void C_fcall trf_2419(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2419(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2419(t0,t1,t2);}

C_noret_decl(trf_2305)
static void C_fcall trf_2305(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2305(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2305(t0,t1,t2);}

C_noret_decl(trf_2331)
static void C_fcall trf_2331(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2331(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2331(t0,t1,t2);}

C_noret_decl(trf_2382)
static void C_fcall trf_2382(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2382(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2382(t0,t1,t2);}

C_noret_decl(trf_2333)
static void C_fcall trf_2333(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_2333(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_2333(t0,t1,t2);}

C_noret_decl(trf_1533)
static void C_fcall trf_1533(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1533(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_1533(t0,t1,t2);}

C_noret_decl(trf_1546)
static void C_fcall trf_1546(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1546(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_1546(t0,t1);}

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

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

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

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
if(!C_demand_2(1643)){
C_save(t1);
C_rereclaim2(1643*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,148);
lf[0]=C_h_intern(&lf[0],6,"logior");
lf[1]=C_h_intern(&lf[1],11,"bitwise-ior");
lf[2]=C_h_intern(&lf[2],6,"lognot");
lf[3]=C_h_intern(&lf[3],11,"bitwise-not");
lf[4]=C_h_intern(&lf[4],6,"logand");
lf[5]=C_h_intern(&lf[5],11,"bitwise-and");
lf[6]=C_h_intern(&lf[6],3,"ash");
lf[7]=C_h_intern(&lf[7],16,"arithmetic-shift");
lf[8]=C_h_intern(&lf[8],3,"mod");
lf[9]=C_h_intern(&lf[9],6,"modulo");
lf[10]=C_h_intern(&lf[10],10,"write-byte");
lf[11]=C_h_intern(&lf[11],7,"display");
lf[12]=C_h_intern(&lf[12],5,"flush");
lf[13]=C_h_intern(&lf[13],12,"flush-output");
lf[14]=C_h_intern(&lf[14],11,"registers32");
lf[15]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\003eax\376\003\000\000\002\376\001\000\000\003ecx\376\003\000\000\002\376\001\000\000\003edx\376\003\000\000\002\376\001\000\000\003ebx\376\003\000\000\002\376\001\000\000\003esp\376\003\000\000\002\376\001\000\000\003ebp\376\003"
"\000\000\002\376\001\000\000\003esi\376\003\000\000\002\376\001\000\000\003edi\376\377\016");
lf[16]=C_h_intern(&lf[16],11,"registers64");
lf[17]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\003rax\376\003\000\000\002\376\001\000\000\003rcx\376\003\000\000\002\376\001\000\000\003rdx\376\003\000\000\002\376\001\000\000\003rbx\376\003\000\000\002\376\001\000\000\003rsp\376\003\000\000\002\376\001\000\000\003rbp\376\003"
"\000\000\002\376\001\000\000\003rsi\376\003\000\000\002\376\001\000\000\003rdi\376\377\016");
lf[18]=C_h_intern(&lf[18],17,"registers64-extra");
lf[19]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\002r8\376\003\000\000\002\376\001\000\000\002r9\376\003\000\000\002\376\001\000\000\003r10\376\003\000\000\002\376\001\000\000\003r11\376\003\000\000\002\376\001\000\000\003r12\376\003\000\000\002\376\001\000\000\003r13\376\003\000\000"
"\002\376\001\000\000\003r14\376\003\000\000\002\376\001\000\000\003r15\376\377\016");
lf[20]=C_h_intern(&lf[20],15,"relocation-proc");
lf[21]=C_h_intern(&lf[21],17,"relocation-symbol");
lf[22]=C_h_intern(&lf[22],3,"enc");
lf[23]=C_h_intern(&lf[23],4,"expt");
lf[24]=C_h_intern(&lf[24],4,"regi");
lf[25]=C_h_intern(&lf[25],3,"eq\077");
lf[26]=C_h_intern(&lf[26],10,"list-index");
lf[27]=C_h_intern(&lf[27],12,"machine-code");
lf[28]=C_h_intern(&lf[28],13,"opcode-filter");
lf[29]=C_h_intern(&lf[29],3,"map");
lf[30]=C_h_intern(&lf[30],7,"builtin");
lf[31]=C_h_intern(&lf[31],8,"ropadd64");
lf[32]=C_h_intern(&lf[32],10,"ropstore64");
lf[33]=C_h_intern(&lf[33],9,"ropread64");
lf[34]=C_h_intern(&lf[34],10,"ropconst64");
lf[35]=C_h_intern(&lf[35],10,"\003sysappend");
lf[36]=C_h_intern(&lf[36],9,"ropimul64");
lf[37]=C_h_intern(&lf[37],9,"ropcall64");
lf[38]=C_h_intern(&lf[38],12,"roprelcall32");
lf[39]=C_h_intern(&lf[39],6,"opje32");
lf[40]=C_h_intern(&lf[40],7,"opjmp32");
lf[41]=C_h_intern(&lf[41],8,"sopadd64");
lf[42]=C_h_intern(&lf[42],9,"sopimul64");
lf[43]=C_h_intern(&lf[43],8,"sopmov64");
lf[44]=C_h_intern(&lf[44],8,"sopcmp64");
lf[45]=C_h_intern(&lf[45],9,"sopread64");
lf[46]=C_h_intern(&lf[46],10,"sopstore64");
lf[47]=C_h_intern(&lf[47],3,"rax");
lf[48]=C_h_intern(&lf[48],3,"rbx");
lf[49]=C_h_intern(&lf[49],9,"sopcall64");
lf[50]=C_h_intern(&lf[50],9,"rsopmov64");
lf[51]=C_h_intern(&lf[51],6,"rspush");
lf[52]=C_h_intern(&lf[52],4,"cret");
lf[53]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\001\000\000\000\303\376\377\016");
lf[54]=C_h_intern(&lf[54],5,"leave");
lf[55]=C_h_intern(&lf[55],5,"frame");
lf[56]=C_h_intern(&lf[56],4,"lref");
lf[57]=C_h_intern(&lf[57],7,"const64");
lf[58]=C_h_intern(&lf[58],10,"func-2-2-1");
lf[59]=C_h_intern(&lf[59],10,"func-2-1-1");
lf[60]=C_h_intern(&lf[60],5,"add64");
lf[61]=C_h_intern(&lf[61],6,"imul64");
lf[62]=C_h_intern(&lf[62],5,"cmp64");
lf[63]=C_h_intern(&lf[63],7,"store64");
lf[64]=C_h_intern(&lf[64],6,"read64");
lf[65]=C_h_intern(&lf[65],4,"push");
lf[66]=C_h_intern(&lf[66],12,"make-closure");
lf[67]=C_h_intern(&lf[67],5,"store");
lf[68]=C_decode_literal(C_heaptop,"\376U5.83075916150843e+18\000");
lf[69]=C_decode_literal(C_heaptop,"\376U1.6934819449395e+19\000");
lf[70]=C_h_intern(&lf[70],16,"call-with-memory");
lf[71]=C_h_intern(&lf[71],8,"\077number\077");
lf[72]=C_h_intern(&lf[72],7,"number\077");
lf[73]=C_h_intern(&lf[73],8,"\077symbol\077");
lf[74]=C_h_intern(&lf[74],7,"symbol\077");
lf[75]=C_h_intern(&lf[75],11,"\077procedure\077");
lf[76]=C_h_intern(&lf[76],6,"lambda");
lf[77]=C_h_intern(&lf[77],9,"\077builtin\077");
lf[78]=C_h_intern(&lf[78],8,"\077syntax\077");
lf[79]=C_h_intern(&lf[79],6,"syntax");
lf[80]=C_h_intern(&lf[80],10,"relocation");
lf[81]=C_h_intern(&lf[81],4,"size");
lf[82]=C_h_intern(&lf[82],8,"set-car!");
lf[83]=C_h_intern(&lf[83],8,"for-each");
lf[84]=C_h_intern(&lf[84],17,"compile-lambda-p5");
lf[85]=C_h_intern(&lf[85],10,"\003syssetter");
lf[86]=C_h_intern(&lf[86],5,"caddr");
lf[87]=C_h_intern(&lf[87],17,"compile-lambda-p3");
lf[88]=C_h_intern(&lf[88],14,"frame-variable");
lf[89]=C_h_intern(&lf[89],9,"arguments");
lf[90]=C_h_intern(&lf[90],7,"reverse");
lf[91]=C_h_intern(&lf[91],6,"append");
lf[92]=C_h_intern(&lf[92],6,"b-lref");
lf[93]=C_h_intern(&lf[93],4,"b-if");
lf[94]=C_h_intern(&lf[94],3,"jmp");
lf[95]=C_h_intern(&lf[95],8,"rel-call");
lf[96]=C_h_intern(&lf[96],4,"call");
lf[97]=C_h_intern(&lf[97],12,"<unexpected>");
lf[98]=C_h_intern(&lf[98],7,"compile");
lf[99]=C_h_intern(&lf[99],11,"lambda-body");
lf[100]=C_h_intern(&lf[100],11,"lambda-args");
lf[101]=C_h_intern(&lf[101],6,"b-push");
lf[102]=C_h_intern(&lf[102],6,"lstore");
lf[103]=C_h_intern(&lf[103],8,"b-lstore");
lf[104]=C_h_intern(&lf[104],12,"code-const64");
lf[105]=C_h_intern(&lf[105],14,"b-code-const64");
lf[106]=C_h_intern(&lf[106],6,"b-call");
lf[107]=C_h_intern(&lf[107],10,"b-rel-call");
lf[108]=C_h_intern(&lf[108],1,"-");
lf[109]=C_h_intern(&lf[109],2,"if");
lf[110]=C_h_intern(&lf[110],7,"b-frame");
lf[111]=C_h_intern(&lf[111],5,"cadar");
lf[112]=C_h_intern(&lf[112],23,"compile-lambda-p4-human");
lf[113]=C_h_intern(&lf[113],4,"dump");
lf[114]=C_h_intern(&lf[114],4,"main");
lf[115]=C_h_intern(&lf[115],4,"exit");
lf[116]=C_h_intern(&lf[116],7,"process");
lf[117]=C_decode_literal(C_heaptop,"\376B\000\000\030./load-binary output.bin");
lf[118]=C_h_intern(&lf[118],9,"read-line");
lf[119]=C_h_intern(&lf[119],7,"newline");
lf[120]=C_decode_literal(C_heaptop,"\376B\000\000\027ndisasm output.bin -b64");
lf[121]=C_h_intern(&lf[121],5,"print");
lf[122]=C_decode_literal(C_heaptop,"\376B\000\000\034disassembly of binary code:\012");
lf[123]=C_h_intern(&lf[123],10,"dump-procs");
lf[124]=C_h_intern(&lf[124],19,"with-output-to-file");
lf[125]=C_decode_literal(C_heaptop,"\376B\000\000\012output.bin");
lf[126]=C_decode_literal(C_heaptop,"\376B\000\000\027-----------------------");
lf[127]=C_h_intern(&lf[127],12,"opcode-print");
lf[128]=C_decode_literal(C_heaptop,"\376B\000\000\016#<lambda addr=");
lf[129]=C_decode_literal(C_heaptop,"\376B\000\000\001>");
lf[130]=C_h_intern(&lf[130],9,"base-addr");
lf[131]=C_h_intern(&lf[131],20,"call-with-input-char");
lf[132]=C_h_intern(&lf[132],21,"call-with-output-char");
lf[133]=C_h_intern(&lf[133],22,"call-with-freed-memory");
lf[134]=C_h_intern(&lf[134],4,"read");
lf[135]=C_decode_literal(C_heaptop,"\376B\000\000\006test> ");
lf[136]=C_h_intern(&lf[136],4,"iota");
lf[137]=C_decode_literal(C_heaptop,"\376B\000\000\010#<reloc>");
lf[138]=C_decode_literal(C_heaptop,"\376B\000\000\002  ");
lf[139]=C_h_intern(&lf[139],1,"+");
lf[140]=C_h_intern(&lf[140],3,"add");
lf[141]=C_h_intern(&lf[141],1,"*");
lf[142]=C_h_intern(&lf[142],4,"imul");
lf[143]=C_h_intern(&lf[143],1,"=");
lf[144]=C_h_intern(&lf[144],3,"cmp");
lf[145]=C_h_intern(&lf[145],25,"\003sysimplicit-exit-handler");
lf[146]=C_h_intern(&lf[146],7,"require");
lf[147]=C_h_intern(&lf[147],5,"posix");
C_register_lf2(lf,148,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1215,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k1213 */
static void C_ccall f_1215(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1215,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1218,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k1216 in k1213 */
static void C_ccall f_1218(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1218,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1221,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_srfi_1_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k1219 in k1216 in k1213 */
static void C_ccall f_1221(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1221,2,t0,t1);}
t2=C_mutate((C_word*)lf[0]+1 /* (set! logior ...) */,*((C_word*)lf[1]+1));
t3=C_mutate((C_word*)lf[2]+1 /* (set! lognot ...) */,*((C_word*)lf[3]+1));
t4=C_mutate((C_word*)lf[4]+1 /* (set! logand ...) */,*((C_word*)lf[5]+1));
t5=C_mutate((C_word*)lf[6]+1 /* (set! ash ...) */,*((C_word*)lf[7]+1));
t6=C_mutate((C_word*)lf[8]+1 /* (set! mod ...) */,*((C_word*)lf[9]+1));
t7=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1229,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:8: require");
((C_proc3)C_fast_retrieve_symbol_proc(lf[146]))(3,*((C_word*)lf[146]+1),t7,lf[147]);}

/* k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1229(C_word c,C_word t0,C_word t1){
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word t33;
C_word t34;
C_word t35;
C_word t36;
C_word t37;
C_word ab[96],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1229,2,t0,t1);}
t2=C_mutate((C_word*)lf[10]+1 /* (set! write-byte ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1231,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t3=C_mutate((C_word*)lf[12]+1 /* (set! flush ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1241,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[14]+1 /* (set! registers32 ...) */,lf[15]);
t5=C_mutate((C_word*)lf[16]+1 /* (set! registers64 ...) */,lf[17]);
t6=C_mutate((C_word*)lf[18]+1 /* (set! registers64-extra ...) */,lf[19]);
t7=C_mutate((C_word*)lf[20]+1 /* (set! relocation-proc ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1250,a[2]=((C_word)li9),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[22]+1 /* (set! enc ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1383,a[2]=((C_word)li12),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[24]+1 /* (set! regi ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1473,a[2]=((C_word)li15),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[27]+1 /* (set! machine-code ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1497,a[2]=((C_word)li18),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate((C_word*)lf[31]+1 /* (set! ropadd64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1568,a[2]=((C_word)li19),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[32]+1 /* (set! ropstore64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1598,a[2]=((C_word)li20),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[33]+1 /* (set! ropread64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1628,a[2]=((C_word)li21),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate((C_word*)lf[34]+1 /* (set! ropconst64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1658,a[2]=((C_word)li22),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate((C_word*)lf[36]+1 /* (set! ropimul64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1684,a[2]=((C_word)li23),tmp=(C_word)a,a+=3,tmp));
t16=C_mutate((C_word*)lf[37]+1 /* (set! ropcall64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1718,a[2]=((C_word)li24),tmp=(C_word)a,a+=3,tmp));
t17=C_mutate((C_word*)lf[38]+1 /* (set! roprelcall32 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1736,a[2]=((C_word)li25),tmp=(C_word)a,a+=3,tmp));
t18=C_mutate((C_word*)lf[39]+1 /* (set! opje32 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1750,a[2]=((C_word)li26),tmp=(C_word)a,a+=3,tmp));
t19=C_mutate((C_word*)lf[40]+1 /* (set! opjmp32 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1768,a[2]=((C_word)li27),tmp=(C_word)a,a+=3,tmp));
t20=C_mutate((C_word*)lf[41]+1 /* (set! sopadd64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1782,a[2]=((C_word)li28),tmp=(C_word)a,a+=3,tmp));
t21=C_mutate((C_word*)lf[42]+1 /* (set! sopimul64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1826,a[2]=((C_word)li29),tmp=(C_word)a,a+=3,tmp));
t22=C_mutate((C_word*)lf[43]+1 /* (set! sopmov64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1874,a[2]=((C_word)li30),tmp=(C_word)a,a+=3,tmp));
t23=C_mutate((C_word*)lf[44]+1 /* (set! sopcmp64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1918,a[2]=((C_word)li31),tmp=(C_word)a,a+=3,tmp));
t24=C_mutate((C_word*)lf[45]+1 /* (set! sopread64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1962,a[2]=((C_word)li32),tmp=(C_word)a,a+=3,tmp));
t25=C_mutate((C_word*)lf[46]+1 /* (set! sopstore64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1980,a[2]=((C_word)li33),tmp=(C_word)a,a+=3,tmp));
t26=C_mutate((C_word*)lf[49]+1 /* (set! sopcall64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2006,a[2]=((C_word)li34),tmp=(C_word)a,a+=3,tmp));
t27=C_mutate((C_word*)lf[50]+1 /* (set! rsopmov64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2024,a[2]=((C_word)li35),tmp=(C_word)a,a+=3,tmp));
t28=C_mutate((C_word*)lf[51]+1 /* (set! rspush ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2068,a[2]=((C_word)li36),tmp=(C_word)a,a+=3,tmp));
t29=C_mutate((C_word*)lf[52]+1 /* (set! cret ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2082,a[2]=((C_word)li37),tmp=(C_word)a,a+=3,tmp));
t30=C_mutate((C_word*)lf[54]+1 /* (set! leave ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2085,a[2]=((C_word)li38),tmp=(C_word)a,a+=3,tmp));
t31=C_mutate((C_word*)lf[55]+1 /* (set! frame ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2113,a[2]=((C_word)li39),tmp=(C_word)a,a+=3,tmp));
t32=C_mutate((C_word*)lf[56]+1 /* (set! lref ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2141,a[2]=((C_word)li40),tmp=(C_word)a,a+=3,tmp));
t33=C_mutate((C_word*)lf[57]+1 /* (set! const64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2147,a[2]=((C_word)li41),tmp=(C_word)a,a+=3,tmp));
t34=C_mutate((C_word*)lf[58]+1 /* (set! func-2-2-1 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2153,a[2]=((C_word)li43),tmp=(C_word)a,a+=3,tmp));
t35=C_mutate((C_word*)lf[59]+1 /* (set! func-2-1-1 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2161,a[2]=((C_word)li45),tmp=(C_word)a,a+=3,tmp));
t36=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2171,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:201: func-2-2-1");
((C_proc3)C_fast_retrieve_symbol_proc(lf[58]))(3,*((C_word*)lf[58]+1),t36,C_fast_retrieve(lf[41]));}

/* k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2171(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2171,2,t0,t1);}
t2=C_mutate((C_word*)lf[60]+1 /* (set! add64 ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2175,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:202: func-2-2-1");
((C_proc3)C_fast_retrieve_symbol_proc(lf[58]))(3,*((C_word*)lf[58]+1),t3,C_fast_retrieve(lf[42]));}

/* k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2175(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2175,2,t0,t1);}
t2=C_mutate((C_word*)lf[61]+1 /* (set! imul64 ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2179,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:203: func-2-2-1");
((C_proc3)C_fast_retrieve_symbol_proc(lf[58]))(3,*((C_word*)lf[58]+1),t3,C_fast_retrieve(lf[44]));}

/* k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2179(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2179,2,t0,t1);}
t2=C_mutate((C_word*)lf[62]+1 /* (set! cmp64 ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2183,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:204: func-2-2-1");
((C_proc3)C_fast_retrieve_symbol_proc(lf[58]))(3,*((C_word*)lf[58]+1),t3,C_fast_retrieve(lf[46]));}

/* k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2183(C_word c,C_word t0,C_word t1){
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word t33;
C_word t34;
C_word t35;
C_word t36;
C_word t37;
C_word t38;
C_word t39;
C_word t40;
C_word t41;
C_word t42;
C_word t43;
C_word t44;
C_word t45;
C_word t46;
C_word t47;
C_word t48;
C_word t49;
C_word t50;
C_word t51;
C_word t52;
C_word t53;
C_word t54;
C_word t55;
C_word t56;
C_word t57;
C_word t58;
C_word t59;
C_word t60;
C_word t61;
C_word t62;
C_word ab[144],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2183,2,t0,t1);}
t2=C_mutate((C_word*)lf[63]+1 /* (set! store64 ...) */,t1);
t3=C_mutate((C_word*)lf[64]+1 /* (set! read64 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2185,a[2]=((C_word)li46),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[65]+1 /* (set! push ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2191,a[2]=((C_word)li47),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[66]+1 /* (set! make-closure ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2197,a[2]=((C_word)li49),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[71]+1 /* (set! ?number? ...) */,*((C_word*)lf[72]+1));
t7=C_mutate((C_word*)lf[73]+1 /* (set! ?symbol? ...) */,*((C_word*)lf[74]+1));
t8=C_mutate((C_word*)lf[75]+1 /* (set! ?procedure? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2240,a[2]=((C_word)li50),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[77]+1 /* (set! ?builtin? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2256,a[2]=((C_word)li51),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[78]+1 /* (set! ?syntax? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2272,a[2]=((C_word)li52),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate((C_word*)lf[21]+1 /* (set! relocation-symbol ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2288,a[2]=((C_word)li53),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[81]+1 /* (set! size ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2302,a[2]=((C_word)li59),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[87]+1 /* (set! compile-lambda-p3 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2442,a[2]=((C_word)li84),tmp=(C_word)a,a+=3,tmp));
t14=C_a_i_cons(&a,2,C_fast_retrieve(lf[56]),C_SCHEME_END_OF_LIST);
t15=C_a_i_cons(&a,2,lf[56],t14);
t16=C_a_i_cons(&a,2,lf[30],t15);
t17=C_mutate((C_word*)lf[92]+1 /* (set! b-lref ...) */,t16);
t18=C_a_i_cons(&a,2,C_fast_retrieve(lf[65]),C_SCHEME_END_OF_LIST);
t19=C_a_i_cons(&a,2,lf[65],t18);
t20=C_a_i_cons(&a,2,lf[30],t19);
t21=C_mutate((C_word*)lf[101]+1 /* (set! b-push ...) */,t20);
t22=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4895,a[2]=((C_word)li85),tmp=(C_word)a,a+=3,tmp);
t23=C_a_i_cons(&a,2,t22,C_SCHEME_END_OF_LIST);
t24=C_a_i_cons(&a,2,lf[102],t23);
t25=C_a_i_cons(&a,2,lf[30],t24);
t26=C_mutate((C_word*)lf[103]+1 /* (set! b-lstore ...) */,t25);
t27=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4881,a[2]=((C_word)li86),tmp=(C_word)a,a+=3,tmp);
t28=C_a_i_cons(&a,2,t27,C_SCHEME_END_OF_LIST);
t29=C_a_i_cons(&a,2,lf[104],t28);
t30=C_a_i_cons(&a,2,lf[30],t29);
t31=C_mutate((C_word*)lf[105]+1 /* (set! b-code-const64 ...) */,t30);
t32=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4827,a[2]=((C_word)li87),tmp=(C_word)a,a+=3,tmp);
t33=C_a_i_cons(&a,2,t32,C_SCHEME_END_OF_LIST);
t34=C_a_i_cons(&a,2,lf[96],t33);
t35=C_a_i_cons(&a,2,lf[30],t34);
t36=C_mutate((C_word*)lf[106]+1 /* (set! b-call ...) */,t35);
t37=C_a_i_cons(&a,2,C_fast_retrieve(lf[38]),C_SCHEME_END_OF_LIST);
t38=C_a_i_cons(&a,2,lf[95],t37);
t39=C_a_i_cons(&a,2,lf[30],t38);
t40=C_mutate((C_word*)lf[107]+1 /* (set! b-rel-call ...) */,t39);
t41=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4753,a[2]=((C_word)li92),tmp=(C_word)a,a+=3,tmp);
t42=C_a_i_cons(&a,2,t41,C_SCHEME_END_OF_LIST);
t43=C_a_i_cons(&a,2,lf[109],t42);
t44=C_a_i_cons(&a,2,lf[30],t43);
t45=C_mutate((C_word*)lf[93]+1 /* (set! b-if ...) */,t44);
t46=C_a_i_cons(&a,2,C_fast_retrieve(lf[55]),C_SCHEME_END_OF_LIST);
t47=C_a_i_cons(&a,2,lf[55],t46);
t48=C_a_i_cons(&a,2,lf[30],t47);
t49=C_mutate((C_word*)lf[110]+1 /* (set! b-frame ...) */,t48);
t50=C_mutate((C_word*)lf[95]+1 /* (set! rel-call ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3282,a[2]=((C_word)li93),tmp=(C_word)a,a+=3,tmp));
t51=C_mutate((C_word*)lf[84]+1 /* (set! compile-lambda-p5 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3320,a[2]=((C_word)li96),tmp=(C_word)a,a+=3,tmp));
t52=C_mutate((C_word*)lf[112]+1 /* (set! compile-lambda-p4-human ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3438,a[2]=((C_word)li101),tmp=(C_word)a,a+=3,tmp));
t53=C_set_block_item(lf[113] /* dump */,0,C_SCHEME_FALSE);
t54=C_mutate((C_word*)lf[114]+1 /* (set! main ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3775,a[2]=((C_word)li110),tmp=(C_word)a,a+=3,tmp));
t55=C_mutate((C_word*)lf[123]+1 /* (set! dump-procs ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4040,a[2]=((C_word)li117),tmp=(C_word)a,a+=3,tmp));
t56=C_mutate((C_word*)lf[28]+1 /* (set! opcode-filter ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4201,a[2]=((C_word)li120),tmp=(C_word)a,a+=3,tmp));
t57=C_mutate((C_word*)lf[127]+1 /* (set! opcode-print ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4359,a[2]=((C_word)li123),tmp=(C_word)a,a+=3,tmp));
t58=C_mutate((C_word*)lf[100]+1 /* (set! lambda-args ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4436,a[2]=((C_word)li124),tmp=(C_word)a,a+=3,tmp));
t59=C_mutate((C_word*)lf[99]+1 /* (set! lambda-body ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4462,a[2]=((C_word)li125),tmp=(C_word)a,a+=3,tmp));
t60=C_mutate((C_word*)lf[98]+1 /* (set! compile ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4488,a[2]=((C_word)li128),tmp=(C_word)a,a+=3,tmp));
t61=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4729,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:478: main");
((C_proc3)C_fast_retrieve_symbol_proc(lf[114]))(3,*((C_word*)lf[114]+1),t61,C_SCHEME_END_OF_LIST);}

/* k4727 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4729(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4729,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4732,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4735,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[145]))(2,*((C_word*)lf[145]+1),t3);}

/* k4733 in k4727 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4735(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k4730 in k4727 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4732(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4488(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word ab[9],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4488,3,t0,t1,t2);}
t3=C_eqp(t2,lf[139]);
if(C_truep(t3)){
t4=C_a_i_cons(&a,2,C_fast_retrieve(lf[60]),C_SCHEME_END_OF_LIST);
t5=C_a_i_cons(&a,2,lf[140],t4);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_a_i_cons(&a,2,lf[30],t5));}
else{
t4=C_eqp(t2,lf[141]);
if(C_truep(t4)){
t5=C_a_i_cons(&a,2,C_fast_retrieve(lf[61]),C_SCHEME_END_OF_LIST);
t6=C_a_i_cons(&a,2,lf[142],t5);
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,C_a_i_cons(&a,2,lf[30],t6));}
else{
t5=C_eqp(t2,lf[143]);
if(C_truep(t5)){
t6=C_a_i_cons(&a,2,C_fast_retrieve(lf[62]),C_SCHEME_END_OF_LIST);
t7=C_a_i_cons(&a,2,lf[144],t6);
t8=t1;
((C_proc2)(void*)(*((C_word*)t8+1)))(2,t8,C_a_i_cons(&a,2,lf[30],t7));}
else{
t6=C_eqp(t2,lf[134]);
if(C_truep(t6)){
t7=C_a_i_cons(&a,2,C_fast_retrieve(lf[64]),C_SCHEME_END_OF_LIST);
t8=C_a_i_cons(&a,2,lf[134],t7);
t9=t1;
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,C_a_i_cons(&a,2,lf[30],t8));}
else{
t7=C_eqp(t2,lf[109]);
if(C_truep(t7)){
t8=C_a_i_cons(&a,2,lf[109],C_SCHEME_END_OF_LIST);
t9=t1;
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,C_a_i_cons(&a,2,lf[79],t8));}
else{
if(C_truep(C_i_pairp(t2))){
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4582,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t9=C_i_car(t2);
if(C_truep(C_i_pairp(t9))){
t10=C_i_caar(t2);
t11=t8;
f_4582(t11,C_eqp(t10,lf[76]));}
else{
t10=t8;
f_4582(t10,C_SCHEME_FALSE);}}
else{
t8=t1;
((C_proc2)(void*)(*((C_word*)t8+1)))(2,t8,t2);}}}}}}}

/* k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4582(C_word t0,C_word t1){
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word ab[18],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4582,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=C_i_car(((C_word*)t0)[3]);
t3=C_SCHEME_END_OF_LIST;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_FALSE;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_fast_retrieve(lf[98]);
t8=C_i_cdr(((C_word*)t0)[3]);
t9=C_i_check_list_2(t8,lf[29]);
t10=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4599,a[2]=t2,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t11=C_SCHEME_UNDEFINED;
t12=(*a=C_VECTOR_TYPE|1,a[1]=t11,tmp=(C_word)a,a+=2,tmp);
t13=C_set_block_item(t12,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4601,a[2]=t7,a[3]=t4,a[4]=t12,a[5]=t6,a[6]=((C_word)li126),tmp=(C_word)a,a+=7,tmp));
t14=((C_word*)t12)[1];
f_4601(t14,t10,t8);}
else{
t2=C_i_car(((C_word*)t0)[3]);
t3=C_eqp(t2,lf[76]);
if(C_truep(t3)){
t4=C_a_i_cons(&a,2,C_fast_retrieve(lf[60]),C_SCHEME_END_OF_LIST);
t5=C_a_i_cons(&a,2,lf[140],t4);
t6=C_a_i_cons(&a,2,lf[30],t5);
t7=C_a_i_cons(&a,2,((C_word*)t0)[3],C_SCHEME_END_OF_LIST);
t8=C_a_i_cons(&a,2,lf[130],t7);
t9=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,C_a_i_cons(&a,2,t6,t8));}
else{
t4=C_SCHEME_END_OF_LIST;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_SCHEME_FALSE;
t7=(*a=C_VECTOR_TYPE|1,a[1]=t6,tmp=(C_word)a,a+=2,tmp);
t8=C_fast_retrieve(lf[98]);
t9=((C_word*)t0)[3];
t10=C_i_check_list_2(t9,lf[29]);
t11=C_SCHEME_UNDEFINED;
t12=(*a=C_VECTOR_TYPE|1,a[1]=t11,tmp=(C_word)a,a+=2,tmp);
t13=C_set_block_item(t12,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4671,a[2]=t8,a[3]=t5,a[4]=t12,a[5]=t7,a[6]=((C_word)li127),tmp=(C_word)a,a+=7,tmp));
t14=((C_word*)t12)[1];
f_4671(t14,((C_word*)t0)[2],t9);}}}

/* loop1108 in k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4671(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4671,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4700,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=t2,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g11141122");
t5=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[3])[1]);}}

/* k4698 in loop1108 in k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4700(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4700,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4684,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[6])[1])){
t4=t3;
f_4684(t4,C_i_setslot(((C_word*)((C_word*)t0)[6])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t5=t3;
f_4684(t5,t4);}}

/* k4682 in k4698 in loop1108 in k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4684(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[6])+1,((C_word*)t0)[5]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[3])[1];
f_4671(t4,((C_word*)t0)[2],t3);}

/* loop1080 in k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4601(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4601,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4630,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=t2,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g10861094");
t5=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[3])[1]);}}

/* k4628 in loop1080 in k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4630(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4630,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4614,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[6])[1])){
t4=t3;
f_4614(t4,C_i_setslot(((C_word*)((C_word*)t0)[6])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t5=t3;
f_4614(t5,t4);}}

/* k4612 in k4628 in loop1080 in k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4614(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[6])+1,((C_word*)t0)[5]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[3])[1];
f_4601(t4,((C_word*)t0)[2],t3);}

/* k4597 in k4580 in compile in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4599(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4599,2,t0,t1);}
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[2],t1));}

/* lambda-body in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4462(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4462,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=C_eqp(lf[76],t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(C_truep(t4)?C_i_caddr(t2):t2));}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}}

/* lambda-args in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4436(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4436,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=C_eqp(lf[76],t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(C_truep(t4)?C_i_cadr(t2):C_SCHEME_END_OF_LIST));}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}}

/* opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4359(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4359,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4405,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:427: opcode-filter");
((C_proc3)C_fast_retrieve_symbol_proc(lf[28]))(3,*((C_word*)lf[28]+1),t3,t2);}

/* k4403 in opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4405(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4405,2,t0,t1);}
t2=C_i_check_list_2(t1,lf[83]);
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4413,a[2]=t4,a[3]=((C_word)li122),tmp=(C_word)a,a+=4,tmp));
t6=((C_word*)t4)[1];
f_4413(t6,((C_word*)t0)[2],t1);}

/* loop1015 in k4403 in opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4413(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word ab[14],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4413,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4423,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
t5=C_i_check_list_2(t4,lf[83]);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4376,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t7=C_SCHEME_UNDEFINED;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_set_block_item(t8,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4381,a[2]=t8,a[3]=((C_word)li121),tmp=(C_word)a,a+=4,tmp));
t10=((C_word*)t8)[1];
f_4381(t10,t6,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* loop1026 in loop1015 in k4403 in opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4381(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4381,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4391,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4367,a[2]=t4,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:427: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t5,lf[138]);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k4365 in loop1026 in loop1015 in k4403 in opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4367(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:427: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),((C_word*)t0)[3],((C_word*)t0)[2]);}

/* k4389 in loop1026 in loop1015 in k4403 in opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4391(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_4381(t3,((C_word*)t0)[2],t2);}

/* k4374 in loop1015 in k4403 in opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4376(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:427: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[119]+1)))(2,*((C_word*)lf[119]+1),((C_word*)t0)[2]);}

/* k4421 in loop1015 in k4403 in opcode-print in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4423(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_4413(t3,((C_word*)t0)[2],t2);}

/* opcode-filter in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4201(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word ab[13],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4201,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=C_eqp(lf[88],t3);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_cadr(t2));}
else{
t5=C_i_car(t2);
t6=C_eqp(lf[30],t5);
if(C_truep(t6)){
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,C_i_cadr(t2));}
else{
t7=C_i_car(t2);
t8=C_eqp(lf[80],t7);
if(C_truep(t8)){
t9=t1;
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,lf[137]);}
else{
t9=C_i_cdr(t2);
if(C_truep(C_i_pairp(t9))){
t10=C_i_cadr(t2);
t11=C_eqp(lf[88],t10);
if(C_truep(t11)){
t12=t1;
((C_proc2)(void*)(*((C_word*)t12+1)))(2,t12,C_i_caddr(t2));}
else{
t12=C_SCHEME_END_OF_LIST;
t13=(*a=C_VECTOR_TYPE|1,a[1]=t12,tmp=(C_word)a,a+=2,tmp);
t14=C_SCHEME_FALSE;
t15=(*a=C_VECTOR_TYPE|1,a[1]=t14,tmp=(C_word)a,a+=2,tmp);
t16=C_fast_retrieve(lf[28]);
t17=t2;
t18=C_i_check_list_2(t17,lf[29]);
t19=C_SCHEME_UNDEFINED;
t20=(*a=C_VECTOR_TYPE|1,a[1]=t19,tmp=(C_word)a,a+=2,tmp);
t21=C_set_block_item(t20,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4296,a[2]=t16,a[3]=t13,a[4]=t20,a[5]=t15,a[6]=((C_word)li118),tmp=(C_word)a,a+=7,tmp));
t22=((C_word*)t20)[1];
f_4296(t22,t1,t17);}}
else{
t10=C_SCHEME_END_OF_LIST;
t11=(*a=C_VECTOR_TYPE|1,a[1]=t10,tmp=(C_word)a,a+=2,tmp);
t12=C_SCHEME_FALSE;
t13=(*a=C_VECTOR_TYPE|1,a[1]=t12,tmp=(C_word)a,a+=2,tmp);
t14=C_fast_retrieve(lf[28]);
t15=t2;
t16=C_i_check_list_2(t15,lf[29]);
t17=C_SCHEME_UNDEFINED;
t18=(*a=C_VECTOR_TYPE|1,a[1]=t17,tmp=(C_word)a,a+=2,tmp);
t19=C_set_block_item(t18,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4246,a[2]=t14,a[3]=t11,a[4]=t18,a[5]=t13,a[6]=((C_word)li119),tmp=(C_word)a,a+=7,tmp));
t20=((C_word*)t18)[1];
f_4246(t20,t1,t15);}}}}}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}}

/* loop963 in opcode-filter in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4246(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4246,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4275,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=t2,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g969977");
t5=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[3])[1]);}}

/* k4273 in loop963 in opcode-filter in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4275(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4275,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4259,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[6])[1])){
t4=t3;
f_4259(t4,C_i_setslot(((C_word*)((C_word*)t0)[6])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t5=t3;
f_4259(t5,t4);}}

/* k4257 in k4273 in loop963 in opcode-filter in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4259(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[6])+1,((C_word*)t0)[5]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[3])[1];
f_4246(t4,((C_word*)t0)[2],t3);}

/* loop988 in opcode-filter in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4296(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4296,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4325,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=t2,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g9941002");
t5=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[3])[1]);}}

/* k4323 in loop988 in opcode-filter in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4325(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4325,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4309,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[6])[1])){
t4=t3;
f_4309(t4,C_i_setslot(((C_word*)((C_word*)t0)[6])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t5=t3;
f_4309(t5,t4);}}

/* k4307 in k4323 in loop988 in opcode-filter in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4309(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[6])+1,((C_word*)t0)[5]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[3])[1];
f_4296(t4,((C_word*)t0)[2],t3);}

/* dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4040(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[9],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4040,3,t0,t1,t2);}
t3=C_fix(0);
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4047,a[2]=t6,a[3]=t4,a[4]=((C_word)li116),tmp=(C_word)a,a+=5,tmp));
t8=((C_word*)t6)[1];
f_4047(t8,t1,t2);}

/* loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4047(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4047,NULL,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4055,a[2]=((C_word*)t0)[3],a[3]=((C_word)li111),tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_4066,a[2]=t3,a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=((C_word*)t0)[2],a[6]=t2,tmp=(C_word)a,a+=7,tmp);
t5=C_i_car(t2);
t6=C_i_car(t5);
t7=C_a_i_minus(&a,2,t6,((C_word*)((C_word*)t0)[3])[1]);
C_trace("abc.ck.scm:409: iota");
((C_proc3)C_fast_retrieve_symbol_proc(lf[136]))(3,*((C_word*)lf[136]+1),t4,t7);}}

/* k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4066(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[13],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4066,2,t0,t1);}
t2=C_i_check_list_2(t1,lf[83]);
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_4072,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=((C_word*)t0)[6],tmp=(C_word)a,a+=6,tmp);
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4162,a[2]=((C_word*)t0)[2],a[3]=t5,a[4]=((C_word)li115),tmp=(C_word)a,a+=5,tmp));
t7=((C_word*)t5)[1];
f_4162(t7,t3,t1);}

/* loop913 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4162(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4162,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4172,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
t5=C_a_i_list(&a,1,t4);
C_trace("g914919");
t6=((C_word*)t0)[2];
f_4055(t6,t3);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k4170 in loop913 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4172(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_4162(t3,((C_word*)t0)[2],t2);}

/* k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4072(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4072,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4075,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],tmp=(C_word)a,a+=5,tmp);
t3=C_i_car(((C_word*)t0)[5]);
t4=C_i_caddr(t3);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4088,a[2]=((C_word*)t0)[2],a[3]=t6,a[4]=((C_word)li114),tmp=(C_word)a,a+=5,tmp));
t8=((C_word*)t6)[1];
f_4088(t8,t2,t4);}

/* loop2 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4088(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4088,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4090,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word)li112),tmp=(C_word)a,a+=5,tmp);
t4=C_i_check_list_2(t2,lf[83]);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4135,a[2]=t3,a[3]=t6,a[4]=((C_word)li113),tmp=(C_word)a,a+=5,tmp));
t8=((C_word*)t6)[1];
f_4135(t8,t1,t2);}

/* loop932 in loop2 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4135(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4135,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4145,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g933938");
t5=((C_word*)t0)[2];
f_4090(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k4143 in loop932 in loop2 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4145(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_4135(t3,((C_word*)t0)[2],t2);}

/* g933 in loop2 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4090(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4090,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_4097,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=t2,tmp=(C_word)a,a+=6,tmp);
if(C_truep(C_i_pairp(t2))){
if(C_truep(t2)){
t4=C_i_car(t2);
t5=t3;
f_4097(t5,C_eqp(t4,lf[80]));}
else{
t4=t3;
f_4097(t4,C_SCHEME_FALSE);}}
else{
t4=t3;
f_4097(t4,C_SCHEME_FALSE);}}

/* k4095 in g933 in loop2 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4097(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4097,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=C_i_caddr(((C_word*)t0)[5]);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4107,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("g943944");
t4=t2;
((C_proc3)C_fast_retrieve_proc(t4))(3,t4,t3,((C_word*)((C_word*)t0)[2])[1]);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4110,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:414: write-byte");
((C_proc3)C_fast_retrieve_symbol_proc(lf[10]))(3,*((C_word*)lf[10]+1),t2,((C_word*)t0)[5]);}}

/* k4108 in k4095 in g933 in loop2 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4110(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4110,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)((C_word*)t0)[3])[1],C_fix(1));
t3=C_mutate(((C_word *)((C_word*)t0)[3])+1,t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* k4105 in k4095 in g933 in loop2 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4107(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:413: loop2");
t2=((C_word*)((C_word*)t0)[3])[1];
f_4088(t2,((C_word*)t0)[2],t1);}

/* k4073 in k4070 in k4064 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4075(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_i_cdr(((C_word*)t0)[4]);
C_trace("abc.ck.scm:415: loop");
t3=((C_word*)((C_word*)t0)[3])[1];
f_4047(t3,((C_word*)t0)[2],t2);}

/* g914 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_4055(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_4055,NULL,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4059,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:409: write-byte");
((C_proc3)C_fast_retrieve_symbol_proc(lf[10]))(3,*((C_word*)lf[10]+1),t2,C_fix(144));}

/* k4057 in g914 in loop in dump-procs in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4059(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4059,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)((C_word*)t0)[3])[1],C_fix(1));
t3=C_mutate(((C_word *)((C_word*)t0)[3])+1,t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3775(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_3775,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3780,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:387: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t3,lf[135]);}

/* k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3780(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3780,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3783,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:387: flush");
((C_proc2)C_fast_retrieve_symbol_proc(lf[12]))(2,*((C_word*)lf[12]+1),t2);}

/* k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3783(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3783,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3786,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:388: read");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[134]+1)))(2,*((C_word*)lf[134]+1),t2);}

/* k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3786(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3786,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3789,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3795,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
if(C_truep(C_i_pairp(t1))){
t4=C_i_car(t1);
t5=t3;
f_3795(t5,C_eqp(t4,lf[115]));}
else{
t4=t3;
f_3795(t4,C_SCHEME_FALSE);}}

/* k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3795(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3795,NULL,2,t0,t1);}
if(C_truep(t1)){
C_trace("abc.ck.scm:390: exit");
((C_proc2)C_fast_retrieve_symbol_proc(lf[115]))(2,*((C_word*)lf[115]+1),((C_word*)t0)[3]);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3804,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
if(C_truep(C_i_pairp(((C_word*)t0)[2]))){
t3=C_i_car(((C_word*)t0)[2]);
t4=t2;
f_3804(t4,C_eqp(t3,lf[113]));}
else{
t3=t2;
f_3804(t3,C_SCHEME_FALSE);}}}

/* k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3804(C_word t0,C_word t1){
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
C_word t13;
C_word ab[27],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3804,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=C_i_cadr(((C_word*)t0)[3]);
t3=C_mutate((C_word*)lf[113]+1 /* (set! dump ...) */,t2);
t4=((C_word*)t0)[2];
f_3789(2,t4,t3);}
else{
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3811,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=C_a_i_cons(&a,2,lf[130],C_SCHEME_END_OF_LIST);
t4=C_a_i_cons(&a,2,lf[131],t3);
t5=C_a_i_cons(&a,2,lf[132],t4);
t6=C_a_i_cons(&a,2,lf[70],t5);
t7=C_a_i_cons(&a,2,lf[133],t6);
t8=C_a_i_cons(&a,2,((C_word*)t0)[3],C_SCHEME_END_OF_LIST);
t9=C_a_i_cons(&a,2,t7,t8);
t10=C_a_i_cons(&a,2,lf[76],t9);
C_trace("abc.ck.scm:393: compile-lambda-p3");
((C_proc3)C_fast_retrieve_symbol_proc(lf[87]))(3,*((C_word*)lf[87]+1),t2,t10);}}

/* k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3811(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3811,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3814,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:394: size");
((C_proc3)C_fast_retrieve_symbol_proc(lf[81]))(3,*((C_word*)lf[81]+1),t2,t1);}

/* k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3814(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3814,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3817,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
if(C_truep(C_fast_retrieve(lf[113]))){
t3=C_i_check_list_2(((C_word*)t0)[2],lf[83]);
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3965,a[2]=t5,a[3]=((C_word)li109),tmp=(C_word)a,a+=4,tmp));
t7=((C_word*)t5)[1];
f_3965(t7,t2,((C_word*)t0)[2]);}
else{
t3=t2;
f_3817(2,t3,C_SCHEME_UNDEFINED);}}

/* loop863 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3965(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3965,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3975,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3939,a[2]=t4,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t6=C_i_car(t4);
t7=C_i_cadddr(t4);
C_trace("abc.ck.scm:395: print");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[121]+1)))(6,*((C_word*)lf[121]+1),t5,lf[128],t6,lf[129],t7);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k3937 in loop863 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3939(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3939,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3942,a[2]=((C_word*)t0)[3],tmp=(C_word)a,a+=3,tmp);
t3=C_i_cadr(((C_word*)t0)[2]);
C_trace("abc.ck.scm:395: opcode-print");
((C_proc3)C_fast_retrieve_symbol_proc(lf[127]))(3,*((C_word*)lf[127]+1),t2,t3);}

/* k3940 in k3937 in loop863 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3942(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:395: print");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[121]+1)))(3,*((C_word*)lf[121]+1),((C_word*)t0)[2],lf[126]);}

/* k3973 in loop863 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3975(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_3965(t3,((C_word*)t0)[2],t2);}

/* k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3817(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3817,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3820,a[2]=((C_word*)t0)[3],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3930,a[2]=((C_word*)t0)[2],a[3]=((C_word)li108),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:396: with-output-to-file");
((C_proc4)C_fast_retrieve_symbol_proc(lf[124]))(4,*((C_word*)lf[124]+1),t2,lf[125],t3);}

/* a3929 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3930(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3930,2,t0,t1);}
C_trace("abc.ck.scm:397: dump-procs");
((C_proc3)C_fast_retrieve_symbol_proc(lf[123]))(3,*((C_word*)lf[123]+1),t1,((C_word*)t0)[2]);}

/* k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3820(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3820,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3823,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
if(C_truep(C_fast_retrieve(lf[113]))){
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3877,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:398: print");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[121]+1)))(3,*((C_word*)lf[121]+1),t3,lf[122]);}
else{
t3=t2;
f_3823(2,t3,C_SCHEME_UNDEFINED);}}

/* k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3877(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3877,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3882,a[2]=((C_word)li105),tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3888,a[2]=((C_word)li107),tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#call-with-values");
C_call_with_values(4,0,((C_word*)t0)[2],t2,t3);}

/* a3887 in k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3888(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_3888,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3896,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:400: read-line");
((C_proc3)C_fast_retrieve_symbol_proc(lf[118]))(3,*((C_word*)lf[118]+1),t5,t2);}

/* k3894 in a3887 in k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3896(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3896,2,t0,t1);}
t2=C_SCHEME_UNDEFINED;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_set_block_item(t3,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3898,a[2]=((C_word*)t0)[3],a[3]=t3,a[4]=((C_word)li106),tmp=(C_word)a,a+=5,tmp));
t5=((C_word*)t3)[1];
f_3898(t5,((C_word*)t0)[2],t1);}

/* loop in k3894 in a3887 in k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3898(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3898,NULL,3,t0,t1,t2);}
if(C_truep(C_eofp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}
else{
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3911,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:400: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t3,t2);}}

/* k3909 in loop in k3894 in a3887 in k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3911(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3911,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3917,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:400: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[119]+1)))(2,*((C_word*)lf[119]+1),t2);}
else{
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_FALSE);}}

/* k3915 in k3909 in loop in k3894 in a3887 in k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3917(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3917,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3924,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:400: read-line");
((C_proc3)C_fast_retrieve_symbol_proc(lf[118]))(3,*((C_word*)lf[118]+1),t2,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_FALSE);}}

/* k3922 in k3915 in k3909 in loop in k3894 in a3887 in k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3924(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:400: loop");
t2=((C_word*)((C_word*)t0)[3])[1];
f_3898(t2,((C_word*)t0)[2],t1);}

/* a3881 in k3875 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3882(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3882,2,t0,t1);}
C_trace("abc.ck.scm:399: process");
((C_proc3)C_fast_retrieve_symbol_proc(lf[116]))(3,*((C_word*)lf[116]+1),t1,lf[120]);}

/* k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3823(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3823,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3828,a[2]=((C_word)li102),tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3834,a[2]=((C_word)li104),tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#call-with-values");
C_call_with_values(4,0,((C_word*)t0)[2],t2,t3);}

/* a3833 in k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3834(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_3834,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3842,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:402: read-line");
((C_proc3)C_fast_retrieve_symbol_proc(lf[118]))(3,*((C_word*)lf[118]+1),t5,t2);}

/* k3840 in a3833 in k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3842(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3842,2,t0,t1);}
t2=C_SCHEME_UNDEFINED;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_set_block_item(t3,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3844,a[2]=((C_word*)t0)[3],a[3]=t3,a[4]=((C_word)li103),tmp=(C_word)a,a+=5,tmp));
t5=((C_word*)t3)[1];
f_3844(t5,((C_word*)t0)[2],t1);}

/* loop in k3840 in a3833 in k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3844(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3844,NULL,3,t0,t1,t2);}
if(C_truep(C_eofp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}
else{
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3857,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:402: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t3,t2);}}

/* k3855 in loop in k3840 in a3833 in k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3857(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3857,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3863,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:402: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[119]+1)))(2,*((C_word*)lf[119]+1),t2);}
else{
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_FALSE);}}

/* k3861 in k3855 in loop in k3840 in a3833 in k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3863(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3863,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3870,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:402: read-line");
((C_proc3)C_fast_retrieve_symbol_proc(lf[118]))(3,*((C_word*)lf[118]+1),t2,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_FALSE);}}

/* k3868 in k3861 in k3855 in loop in k3840 in a3833 in k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3870(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:402: loop");
t2=((C_word*)((C_word*)t0)[3])[1];
f_3844(t2,((C_word*)t0)[2],t1);}

/* a3827 in k3821 in k3818 in k3815 in k3812 in k3809 in k3802 in k3793 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3828(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3828,2,t0,t1);}
C_trace("abc.ck.scm:401: process");
((C_proc3)C_fast_retrieve_symbol_proc(lf[116]))(3,*((C_word*)lf[116]+1),t1,lf[117]);}

/* k3787 in k3784 in k3781 in k3778 in main in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3789(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:403: main");
((C_proc3)C_fast_retrieve_symbol_proc(lf[114]))(3,*((C_word*)lf[114]+1),((C_word*)t0)[3],((C_word*)t0)[2]);}

/* compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3438(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word ab[31],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_3438,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_SCHEME_UNDEFINED;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_SCHEME_UNDEFINED;
t10=(*a=C_VECTOR_TYPE|1,a[1]=t9,tmp=(C_word)a,a+=2,tmp);
t11=C_set_block_item(t4,0,C_fix(-8));
t12=C_set_block_item(t6,0,C_fix(0));
t13=C_set_block_item(t8,0,C_SCHEME_END_OF_LIST);
t14=C_set_block_item(t10,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3444,a[2]=t10,a[3]=t6,a[4]=((C_word)li98),tmp=(C_word)a,a+=5,tmp));
t15=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3518,a[2]=t10,a[3]=t8,a[4]=t6,a[5]=t4,a[6]=((C_word)li99),tmp=(C_word)a,a+=7,tmp);
t16=C_i_check_list_2(t2,lf[83]);
t17=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3729,a[2]=t1,a[3]=t8,tmp=(C_word)a,a+=4,tmp);
t18=C_SCHEME_UNDEFINED;
t19=(*a=C_VECTOR_TYPE|1,a[1]=t18,tmp=(C_word)a,a+=2,tmp);
t20=C_set_block_item(t19,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3751,a[2]=t15,a[3]=t19,a[4]=((C_word)li100),tmp=(C_word)a,a+=5,tmp));
t21=((C_word*)t19)[1];
f_3751(t21,t17,t2);}

/* loop769 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3751(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3751,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3761,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g770808");
t5=((C_word*)t0)[2];
f_3518(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k3759 in loop769 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3761(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_3751(t3,((C_word*)t0)[2],t2);}

/* k3727 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3729(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3729,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fast_retrieve(lf[52]),C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[52],t2);
t4=C_a_i_cons(&a,2,lf[30],t3);
t5=C_a_i_cons(&a,2,t4,C_SCHEME_END_OF_LIST);
t6=C_a_i_cons(&a,2,t5,((C_word*)((C_word*)t0)[3])[1]);
t7=C_mutate(((C_word *)((C_word*)t0)[3])+1,t6);
t8=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t8+1)))(2,t8,((C_word*)((C_word*)t0)[3])[1]);}

/* g770 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3518(C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word t33;
C_word t34;
C_word t35;
C_word t36;
C_word t37;
C_word t38;
C_word t39;
C_word t40;
C_word t41;
C_word t42;
C_word t43;
C_word t44;
C_word t45;
C_word ab[39],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3518,NULL,3,t0,t1,t2);}
t3=C_i_car(t2);
t4=C_eqp(t3,lf[89]);
if(C_truep(t4)){
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_3529,a[2]=t1,a[3]=((C_word*)t0)[4],a[4]=t2,a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
t6=C_i_cadr(t2);
t7=C_a_i_times(&a,2,C_fix(8),t6);
C_trace("abc.ck.scm:375: +");
C_plus(5,0,t5,((C_word*)((C_word*)t0)[5])[1],t7,C_fix(8));}
else{
t5=C_i_car(t2);
t6=C_eqp(t5,lf[65]);
if(C_truep(t6)){
t7=C_a_i_plus(&a,2,((C_word*)((C_word*)t0)[5])[1],C_fix(8));
t8=C_mutate(((C_word *)((C_word*)t0)[5])+1,t7);
t9=C_a_i_minus(&a,2,((C_word*)((C_word*)t0)[4])[1],((C_word*)((C_word*)t0)[5])[1]);
t10=C_a_i_cons(&a,2,t9,C_SCHEME_END_OF_LIST);
t11=C_a_i_cons(&a,2,C_fast_retrieve(lf[103]),t10);
t12=C_a_i_cons(&a,2,t11,((C_word*)((C_word*)t0)[3])[1]);
t13=C_mutate(((C_word *)((C_word*)t0)[3])+1,t12);
t14=t1;
((C_proc2)(void*)(*((C_word*)t14+1)))(2,t14,t13);}
else{
t7=C_i_car(t2);
t8=C_eqp(t7,lf[95]);
if(C_truep(t8)){
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3581,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t10=C_a_i_minus(&a,2,((C_word*)((C_word*)t0)[5])[1],((C_word*)((C_word*)t0)[4])[1]);
t11=C_a_i_cons(&a,2,t10,C_SCHEME_END_OF_LIST);
t12=C_a_i_cons(&a,2,C_fast_retrieve(lf[110]),t11);
t13=C_i_cadr(t2);
t14=C_a_i_cons(&a,2,t13,C_SCHEME_END_OF_LIST);
t15=C_a_i_cons(&a,2,C_fast_retrieve(lf[107]),t14);
t16=C_a_i_minus(&a,2,((C_word*)((C_word*)t0)[4])[1],((C_word*)((C_word*)t0)[5])[1]);
t17=C_a_i_cons(&a,2,t16,C_SCHEME_END_OF_LIST);
t18=C_a_i_cons(&a,2,C_fast_retrieve(lf[110]),t17);
t19=C_a_i_cons(&a,2,t18,C_SCHEME_END_OF_LIST);
t20=C_a_i_cons(&a,2,t15,t19);
t21=C_a_i_cons(&a,2,t12,t20);
C_trace("abc.ck.scm:377: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[91]+1)))(4,*((C_word*)lf[91]+1),t9,t21,((C_word*)((C_word*)t0)[3])[1]);}
else{
t9=C_i_car(t2);
t10=C_eqp(t9,lf[96]);
if(C_truep(t10)){
t11=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3639,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t12=C_a_i_minus(&a,2,((C_word*)((C_word*)t0)[5])[1],((C_word*)((C_word*)t0)[4])[1]);
t13=C_a_i_cons(&a,2,t12,C_SCHEME_END_OF_LIST);
t14=C_a_i_cons(&a,2,C_fast_retrieve(lf[110]),t13);
t15=C_a_i_cons(&a,2,C_fast_retrieve(lf[106]),C_SCHEME_END_OF_LIST);
t16=C_a_i_minus(&a,2,((C_word*)((C_word*)t0)[4])[1],((C_word*)((C_word*)t0)[5])[1]);
t17=C_a_i_cons(&a,2,t16,C_SCHEME_END_OF_LIST);
t18=C_a_i_cons(&a,2,C_fast_retrieve(lf[110]),t17);
t19=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3679,a[2]=((C_word*)t0)[3],a[3]=t11,a[4]=t14,a[5]=t15,a[6]=t18,tmp=(C_word)a,a+=7,tmp);
t20=C_i_cadr(t2);
C_trace("abc.ck.scm:378: frame-variable-substitute");
t21=((C_word*)((C_word*)t0)[2])[1];
f_3444(t21,t19,t20);}
else{
t11=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3707,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:379: frame-variable-substitute");
t12=((C_word*)((C_word*)t0)[2])[1];
f_3444(t12,t11,t2);}}}}}

/* k3705 in g770 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3707(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3707,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)((C_word*)t0)[3])[1]);
t3=C_mutate(((C_word *)((C_word*)t0)[3])+1,t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* k3677 in g770 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3679(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[18],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3679,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,C_fast_retrieve(lf[105]),t2);
t4=C_a_i_cons(&a,2,t3,C_SCHEME_END_OF_LIST);
t5=C_a_i_cons(&a,2,((C_word*)t0)[6],t4);
t6=C_a_i_cons(&a,2,((C_word*)t0)[5],t5);
t7=C_a_i_cons(&a,2,((C_word*)t0)[4],t6);
C_trace("abc.ck.scm:378: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[91]+1)))(4,*((C_word*)lf[91]+1),((C_word*)t0)[3],t7,((C_word*)((C_word*)t0)[2])[1]);}

/* k3637 in g770 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3639(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[3])+1,t1);
t3=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}

/* k3579 in g770 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3581(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[3])+1,t1);
t3=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}

/* k3527 in g770 in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3529(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3529,2,t0,t1);}
t2=C_mutate(((C_word *)((C_word*)t0)[5])+1,t1);
t3=C_i_cadr(((C_word*)t0)[4]);
t4=C_a_i_times(&a,2,C_fix(8),t3);
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* frame-variable-substitute in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3444(C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word ab[13],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3444,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=C_eqp(t3,lf[88]);
if(C_truep(t4)){
t5=C_i_cadr(t2);
t6=C_a_i_times(&a,2,C_fix(8),t5);
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,C_a_i_minus(&a,2,((C_word*)((C_word*)t0)[3])[1],t6));}
else{
t5=C_SCHEME_END_OF_LIST;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_SCHEME_FALSE;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=t2;
t10=C_i_check_list_2(t9,lf[29]);
t11=C_SCHEME_UNDEFINED;
t12=(*a=C_VECTOR_TYPE|1,a[1]=t11,tmp=(C_word)a,a+=2,tmp);
t13=C_set_block_item(t12,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3476,a[2]=((C_word*)t0)[2],a[3]=t6,a[4]=t12,a[5]=t8,a[6]=((C_word)li97),tmp=(C_word)a,a+=7,tmp));
t14=((C_word*)t12)[1];
f_3476(t14,t1,t9);}}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}}

/* loop785 in frame-variable-substitute in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3476(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3476,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3505,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=t2,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g791799");
t5=((C_word*)((C_word*)t0)[2])[1];
f_3444(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[3])[1]);}}

/* k3503 in loop785 in frame-variable-substitute in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3505(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3505,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3489,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[6])[1])){
t4=t3;
f_3489(t4,C_i_setslot(((C_word*)((C_word*)t0)[6])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t5=t3;
f_3489(t5,t4);}}

/* k3487 in k3503 in loop785 in frame-variable-substitute in compile-lambda-p4-human in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3489(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[6])+1,((C_word*)t0)[5]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[3])[1];
f_3476(t4,((C_word*)t0)[2],t3);}

/* compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3320(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word ab[17],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_3320,4,t0,t1,t2,t3);}
t4=C_SCHEME_END_OF_LIST;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_SCHEME_FALSE;
t7=(*a=C_VECTOR_TYPE|1,a[1]=t6,tmp=(C_word)a,a+=2,tmp);
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3326,a[2]=t3,a[3]=((C_word)li94),tmp=(C_word)a,a+=4,tmp);
t9=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_3391,a[2]=t8,a[3]=t5,a[4]=t7,a[5]=t1,tmp=(C_word)a,a+=6,tmp);
t10=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3436,a[2]=t9,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:362: compile-lambda-p4-human");
((C_proc3)C_fast_retrieve_symbol_proc(lf[112]))(3,*((C_word*)lf[112]+1),t10,t2);}

/* k3434 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3436(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:362: reverse");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[90]+1)))(3,*((C_word*)lf[90]+1),((C_word*)t0)[2],t1);}

/* k3389 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3391(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3391,2,t0,t1);}
t2=C_i_check_list_2(t1,lf[29]);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3397,a[2]=((C_word*)t0)[5],tmp=(C_word)a,a+=3,tmp);
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3399,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t5,a[5]=((C_word*)t0)[4],a[6]=((C_word)li95),tmp=(C_word)a,a+=7,tmp));
t7=((C_word*)t5)[1];
f_3399(t7,t3,t1);}

/* loop737 in k3389 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3399(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3399,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3428,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=t2,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g743751");
t5=((C_word*)t0)[2];
f_3326(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[3])[1]);}}

/* k3426 in loop737 in k3389 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3428(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3428,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3412,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[6])[1])){
t4=t3;
f_3412(t4,C_i_setslot(((C_word*)((C_word*)t0)[6])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t5=t3;
f_3412(t5,t4);}}

/* k3410 in k3426 in loop737 in k3389 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3412(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[6])+1,((C_word*)t0)[5]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[3])[1];
f_3399(t4,((C_word*)t0)[2],t3);}

/* k3395 in k3389 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3397(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_apply(4,0,((C_word*)t0)[2],*((C_word*)lf[91]+1),t1);}

/* g743 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3326(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3326,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3333,a[2]=t2,a[3]=t1,a[4]=((C_word*)t0)[2],tmp=(C_word)a,a+=5,tmp);
t4=C_i_car(t2);
if(C_truep(C_i_pairp(t4))){
t5=C_i_caar(t2);
t6=t3;
f_3333(t6,C_eqp(t5,lf[80]));}
else{
t5=t3;
f_3333(t5,C_SCHEME_FALSE);}}

/* k3331 in g743 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3333(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3333,NULL,2,t0,t1);}
if(C_truep(t1)){
if(C_truep(((C_word*)t0)[4])){
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3359,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:360: cadar");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[111]+1)))(3,*((C_word*)lf[111]+1),t2,((C_word*)t0)[2]);}}
else{
t2=C_i_car(((C_word*)t0)[2]);
t3=C_i_caddr(t2);
t4=C_i_cdr(((C_word*)t0)[2]);
C_apply(4,0,((C_word*)t0)[3],t3,t4);}}

/* k3357 in k3331 in g743 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3359(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3359,2,t0,t1);}
t2=C_i_car(t1);
t3=C_i_caddr(t2);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3351,a[2]=t3,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:360: cadar");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[111]+1)))(3,*((C_word*)lf[111]+1),t4,((C_word*)t0)[2]);}

/* k3349 in k3357 in k3331 in g743 in compile-lambda-p5 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3351(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_i_cdr(t1);
C_apply(4,0,((C_word*)t0)[3],((C_word*)t0)[2],t2);}

/* rel-call in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3282(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_3282,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3290,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:355: frame");
((C_proc3)C_fast_retrieve_symbol_proc(lf[55]))(3,*((C_word*)lf[55]+1),t3,C_fix(-8));}

/* k3288 in rel-call in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3290(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3290,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3306,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3310,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:355: roprelcall32");
((C_proc3)C_fast_retrieve_symbol_proc(lf[38]))(3,*((C_word*)lf[38]+1),t3,((C_word*)t0)[2]);}

/* k3308 in k3288 in rel-call in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3310(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3310,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3314,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3318,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:355: leave");
((C_proc3)C_fast_retrieve_symbol_proc(lf[54]))(3,*((C_word*)lf[54]+1),t3,C_fix(8));}

/* k3316 in k3308 in k3288 in rel-call in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3318(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k3312 in k3308 in k3288 in rel-call in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3314(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k3304 in k3288 in rel-call in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3306(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3306,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(199),t1);
t3=C_a_i_cons(&a,2,C_fix(137),t2);
t4=C_a_i_cons(&a,2,C_fix(72),t3);
C_trace("##sys#append");
t5=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t5+1)))(4,t5,((C_word*)t0)[3],((C_word*)t0)[2],t4);}

/* a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4753(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[14],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_4753,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4761,a[2]=t3,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4793,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4795,a[2]=((C_word)li90),tmp=(C_word)a,a+=3,tmp);
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4798,a[2]=t2,a[3]=((C_word)li91),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:353: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t5,t6,t7);}

/* a4797 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4798(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4798,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4802,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:353: relocation-proc");
((C_proc3)C_fast_retrieve_symbol_proc(lf[20]))(3,*((C_word*)lf[20]+1),t3,*((C_word*)lf[108]+1));}

/* k4800 in a4797 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4802(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4802,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)t0)[4],C_fix(4));
C_trace("g717718");
t3=t1;
((C_proc4)C_fast_retrieve_proc(t3))(4,t3,((C_word*)t0)[3],((C_word*)t0)[2],t2);}

/* a4794 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4795(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4795,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}

/* k4791 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4793(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:353: opje32");
((C_proc3)C_fast_retrieve_symbol_proc(lf[39]))(3,*((C_word*)lf[39]+1),((C_word*)t0)[2],t1);}

/* k4759 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4761(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[17],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4761,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4765,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4769,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4773,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4775,a[2]=((C_word)li88),tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_4778,a[2]=((C_word*)t0)[2],a[3]=((C_word)li89),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:353: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t4,t5,t6);}

/* a4777 in k4759 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4778(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4778,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_4782,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:353: relocation-proc");
((C_proc3)C_fast_retrieve_symbol_proc(lf[20]))(3,*((C_word*)lf[20]+1),t3,*((C_word*)lf[108]+1));}

/* k4780 in a4777 in k4759 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4782(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4782,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)t0)[4],C_fix(4));
C_trace("g721722");
t3=t1;
((C_proc4)C_fast_retrieve_proc(t3))(4,t3,((C_word*)t0)[3],((C_word*)t0)[2],t2);}

/* a4774 in k4759 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4775(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4775,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}

/* k4771 in k4759 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4773(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:353: opjmp32");
((C_proc3)C_fast_retrieve_symbol_proc(lf[40]))(3,*((C_word*)lf[40]+1),((C_word*)t0)[2],t1);}

/* k4767 in k4759 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4769(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k4763 in k4759 in a4752 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4765(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* a4826 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4827(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4827,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4867,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_4871,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:351: ropcall64");
((C_proc3)C_fast_retrieve_symbol_proc(lf[37]))(3,*((C_word*)lf[37]+1),t3,lf[48]);}

/* k4869 in a4826 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4871(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k4865 in a4826 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4867(C_word c,C_word t0,C_word t1){
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
C_word ab[27],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_4867,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(8),t1);
t3=C_a_i_cons(&a,2,C_fix(36),t2);
t4=C_a_i_cons(&a,2,C_fix(116),t3);
t5=C_a_i_cons(&a,2,C_fix(139),t4);
t6=C_a_i_cons(&a,2,C_fix(72),t5);
t7=C_a_i_cons(&a,2,C_fix(36),t6);
t8=C_a_i_cons(&a,2,C_fix(60),t7);
t9=C_a_i_cons(&a,2,C_fix(139),t8);
t10=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t10+1)))(2,t10,C_a_i_cons(&a,2,C_fix(72),t9));}

/* a4880 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4881(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4881,3,t0,t1,t2);}
t3=C_fast_retrieve(lf[43]);
C_trace("g693694");
t4=t3;
((C_proc4)C_fast_retrieve_proc(t4))(4,t4,t1,lf[48],t2);}

/* a4894 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_4895(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_4895,3,t0,t1,t2);}
t3=C_fast_retrieve(lf[50]);
C_trace("g682683");
t4=t3;
((C_proc4)C_fast_retrieve_proc(t4))(4,t4,t1,t2,lf[47]);}

/* compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2442(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word ab[25],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2442,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_SCHEME_UNDEFINED;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_SCHEME_UNDEFINED;
t10=(*a=C_VECTOR_TYPE|1,a[1]=t9,tmp=(C_word)a,a+=2,tmp);
t11=C_SCHEME_UNDEFINED;
t12=(*a=C_VECTOR_TYPE|1,a[1]=t11,tmp=(C_word)a,a+=2,tmp);
t13=C_SCHEME_UNDEFINED;
t14=(*a=C_VECTOR_TYPE|1,a[1]=t13,tmp=(C_word)a,a+=2,tmp);
t15=C_SCHEME_UNDEFINED;
t16=(*a=C_VECTOR_TYPE|1,a[1]=t15,tmp=(C_word)a,a+=2,tmp);
t17=(*a=C_CLOSURE_TYPE|10,a[1]=(C_word)f_2447,a[2]=t1,a[3]=t2,a[4]=t16,a[5]=t14,a[6]=t12,a[7]=t10,a[8]=t8,a[9]=t6,a[10]=t4,tmp=(C_word)a,a+=11,tmp);
C_trace("abc.ck.scm:295: lambda-args");
((C_proc3)C_fast_retrieve_symbol_proc(lf[100]))(3,*((C_word*)lf[100]+1),t17,t2);}

/* k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2447(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[14],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2447,2,t0,t1);}
t2=C_mutate(((C_word *)((C_word*)t0)[10])+1,t1);
t3=(*a=C_CLOSURE_TYPE|10,a[1]=(C_word)f_2451,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[10],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],a[8]=((C_word*)t0)[7],a[9]=((C_word*)t0)[8],a[10]=((C_word*)t0)[9],tmp=(C_word)a,a+=11,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_3248,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:296: lambda-body");
((C_proc3)C_fast_retrieve_symbol_proc(lf[99]))(3,*((C_word*)lf[99]+1),t4,((C_word*)t0)[3]);}

/* k3246 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3248(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:296: compile");
((C_proc3)C_fast_retrieve_symbol_proc(lf[98]))(3,*((C_word*)lf[98]+1),((C_word*)t0)[2],t1);}

/* k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2451(C_word c,C_word t0,C_word t1){
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
C_word t13;
C_word t14;
C_word t15;
C_word ab[26],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2451,2,t0,t1);}
t2=C_mutate(((C_word *)((C_word*)t0)[10])+1,t1);
t3=C_set_block_item(((C_word*)t0)[9],0,C_fix(0));
t4=C_set_block_item(((C_word*)t0)[8],0,C_SCHEME_END_OF_LIST);
t5=C_set_block_item(((C_word*)t0)[7],0,C_SCHEME_END_OF_LIST);
t6=C_set_block_item(((C_word*)t0)[6],0,C_SCHEME_END_OF_LIST);
t7=C_mutate(((C_word *)((C_word*)t0)[5])+1,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2457,a[2]=((C_word*)t0)[9],a[3]=((C_word)li60),tmp=(C_word)a,a+=4,tmp));
t8=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2470,a[2]=((C_word*)t0)[5],a[3]=((C_word*)t0)[8],a[4]=((C_word)li61),tmp=(C_word)a,a+=5,tmp);
t9=((C_word*)((C_word*)t0)[4])[1];
t10=C_i_check_list_2(t9,lf[83]);
t11=(*a=C_CLOSURE_TYPE|9,a[1]=(C_word)f_2489,a[2]=((C_word*)t0)[10],a[3]=((C_word*)t0)[5],a[4]=((C_word*)t0)[8],a[5]=((C_word*)t0)[6],a[6]=((C_word*)t0)[2],a[7]=((C_word*)t0)[3],a[8]=((C_word*)t0)[7],a[9]=((C_word*)t0)[4],tmp=(C_word)a,a+=10,tmp);
t12=C_SCHEME_UNDEFINED;
t13=(*a=C_VECTOR_TYPE|1,a[1]=t12,tmp=(C_word)a,a+=2,tmp);
t14=C_set_block_item(t13,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3223,a[2]=t8,a[3]=t13,a[4]=((C_word)li83),tmp=(C_word)a,a+=5,tmp));
t15=((C_word*)t13)[1];
f_3223(t15,t11,t9);}

/* loop427 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3223(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3223,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3233,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g428448");
t5=((C_word*)t0)[2];
f_2470(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k3231 in loop427 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3233(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_3223(t3,((C_word*)t0)[2],t2);}

/* k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2489(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[18],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2489,2,t0,t1);}
t2=C_i_length(((C_word*)((C_word*)t0)[9])[1]);
t3=C_a_i_cons(&a,2,t2,C_SCHEME_END_OF_LIST);
t4=C_a_i_cons(&a,2,lf[89],t3);
t5=C_a_i_cons(&a,2,t4,((C_word*)((C_word*)t0)[8])[1]);
t6=C_mutate(((C_word *)((C_word*)t0)[8])+1,t5);
t7=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_2496,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[8],a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],a[8]=((C_word*)t0)[7],tmp=(C_word)a,a+=9,tmp);
C_trace("abc.ck.scm:307: make-frame-variable");
t8=((C_word*)((C_word*)t0)[3])[1];
((C_proc2)C_fast_retrieve_proc(t8))(2,t8,t7);}

/* k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2496(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[16],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2496,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_2499,a[2]=((C_word*)t0)[5],a[3]=((C_word*)t0)[6],a[4]=((C_word*)t0)[7],a[5]=((C_word*)t0)[8],tmp=(C_word)a,a+=6,tmp);
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_2512,a[2]=((C_word*)t0)[3],a[3]=t4,a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[6],a[6]=((C_word*)t0)[5],a[7]=((C_word)li82),tmp=(C_word)a,a+=8,tmp));
t6=((C_word*)t4)[1];
f_2512(t6,t2,((C_word*)((C_word*)t0)[2])[1]);}

/* loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2512(C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word ab[18],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2512,NULL,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_SCHEME_UNDEFINED;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_set_block_item(t4,0,C_SCHEME_FALSE);
t10=C_set_block_item(t6,0,C_SCHEME_FALSE);
t11=C_set_block_item(t8,0,C_SCHEME_FALSE);
t12=(*a=C_CLOSURE_TYPE|11,a[1]=(C_word)f_2522,a[2]=t4,a[3]=t6,a[4]=t8,a[5]=((C_word*)t0)[2],a[6]=((C_word*)t0)[3],a[7]=((C_word*)t0)[4],a[8]=((C_word*)t0)[5],a[9]=t1,a[10]=((C_word*)t0)[6],a[11]=t2,tmp=(C_word)a,a+=12,tmp);
C_trace("abc.ck.scm:314: ?number?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[71]))(3,*((C_word*)lf[71]+1),t12,t2);}

/* k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2522(C_word c,C_word t0,C_word t1){
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
C_word ab[18],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2522,2,t0,t1);}
if(C_truep(t1)){
t2=C_a_i_cons(&a,2,C_fast_retrieve(lf[57]),C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[57],t2);
t4=C_a_i_cons(&a,2,lf[30],t3);
t5=C_a_i_cons(&a,2,((C_word*)t0)[11],C_SCHEME_END_OF_LIST);
t6=C_a_i_cons(&a,2,t4,t5);
t7=C_a_i_cons(&a,2,t6,((C_word*)((C_word*)t0)[10])[1]);
t8=C_mutate(((C_word *)((C_word*)t0)[10])+1,t7);
t9=((C_word*)t0)[9];
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,t8);}
else{
t2=(*a=C_CLOSURE_TYPE|11,a[1]=(C_word)f_2552,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],a[8]=((C_word*)t0)[11],a[9]=((C_word*)t0)[9],a[10]=((C_word*)t0)[10],a[11]=((C_word*)t0)[8],tmp=(C_word)a,a+=12,tmp);
C_trace("abc.ck.scm:315: ?procedure?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[75]))(3,*((C_word*)lf[75]+1),t2,((C_word*)t0)[11]);}}

/* k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2552(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2552,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2556,a[2]=((C_word*)t0)[9],a[3]=((C_word*)t0)[10],a[4]=((C_word*)t0)[11],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2600,a[2]=((C_word*)t0)[11],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:316: compile-lambda-p3");
((C_proc3)C_fast_retrieve_symbol_proc(lf[87]))(3,*((C_word*)lf[87]+1),t3,((C_word*)t0)[8]);}
else{
t2=(*a=C_CLOSURE_TYPE|11,a[1]=(C_word)f_2606,a[2]=((C_word*)t0)[11],a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],a[8]=((C_word*)t0)[9],a[9]=((C_word*)t0)[10],a[10]=((C_word*)t0)[7],a[11]=((C_word*)t0)[8],tmp=(C_word)a,a+=12,tmp);
C_trace("abc.ck.scm:319: ?symbol?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[73]))(3,*((C_word*)lf[73]+1),t2,((C_word*)t0)[8]);}}

/* k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2606(C_word c,C_word t0,C_word t1){
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
C_word ab[16],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2606,2,t0,t1);}
if(C_truep(t1)){
t2=C_i_assq(((C_word*)t0)[11],((C_word*)((C_word*)t0)[10])[1]);
t3=C_i_cdr(t2);
t4=C_a_i_cons(&a,2,t3,C_SCHEME_END_OF_LIST);
t5=C_a_i_cons(&a,2,C_fast_retrieve(lf[92]),t4);
t6=C_a_i_cons(&a,2,t5,((C_word*)((C_word*)t0)[9])[1]);
t7=C_mutate(((C_word *)((C_word*)t0)[9])+1,t6);
t8=((C_word*)t0)[8];
((C_proc2)(void*)(*((C_word*)t8+1)))(2,t8,t7);}
else{
if(C_truep(C_i_pairp(((C_word*)t0)[11]))){
t2=(*a=C_CLOSURE_TYPE|11,a[1]=(C_word)f_2642,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[10],a[8]=((C_word*)t0)[7],a[9]=((C_word*)t0)[9],a[10]=((C_word*)t0)[11],a[11]=((C_word*)t0)[8],tmp=(C_word)a,a+=12,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3184,a[2]=t2,a[3]=((C_word*)t0)[11],tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(((C_word*)t0)[11]);
C_trace("abc.ck.scm:321: ?builtin?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[77]))(3,*((C_word*)lf[77]+1),t3,t4);}
else{
t2=C_a_i_cons(&a,2,((C_word*)t0)[11],C_SCHEME_END_OF_LIST);
t3=((C_word*)t0)[8];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_a_i_cons(&a,2,lf[97],t2));}}}

/* k3182 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3184(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3184,2,t0,t1);}
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[3]);
if(C_truep(C_i_nullp(t2))){
t3=C_a_i_cons(&a,2,((C_word*)t0)[3],C_SCHEME_END_OF_LIST);
t4=((C_word*)t0)[2];
f_2642(t4,C_a_i_cons(&a,2,lf[97],t3));}
else{
t3=((C_word*)t0)[2];
f_2642(t3,C_SCHEME_FALSE);}}
else{
t2=((C_word*)t0)[2];
f_2642(t2,C_SCHEME_FALSE);}}

/* k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2642(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[16],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2642,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=((C_word*)t0)[11];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,t1);}
else{
t2=(*a=C_CLOSURE_TYPE|11,a[1]=(C_word)f_2651,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],a[8]=((C_word*)t0)[8],a[9]=((C_word*)t0)[11],a[10]=((C_word*)t0)[9],a[11]=((C_word*)t0)[10],tmp=(C_word)a,a+=12,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3170,a[2]=t2,a[3]=((C_word*)t0)[10],tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(((C_word*)t0)[10]);
C_trace("abc.ck.scm:322: ?builtin?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[77]))(3,*((C_word*)lf[77]+1),t3,t4);}}

/* k3168 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3170(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(C_truep(t1)){
t2=C_i_cddr(((C_word*)t0)[3]);
t3=((C_word*)t0)[2];
f_2651(t3,C_i_nullp(t2));}
else{
t2=((C_word*)t0)[2];
f_2651(t2,C_SCHEME_FALSE);}}

/* k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2651(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[16],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2651,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2654,a[2]=((C_word*)t0)[9],a[3]=((C_word*)t0)[10],a[4]=((C_word*)t0)[11],tmp=(C_word)a,a+=5,tmp);
t3=C_i_cadr(((C_word*)t0)[11]);
C_trace("abc.ck.scm:322: loop");
t4=((C_word*)((C_word*)t0)[8])[1];
f_2512(t4,t2,t3);}
else{
t2=(*a=C_CLOSURE_TYPE|11,a[1]=(C_word)f_2676,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[8],a[8]=((C_word*)t0)[9],a[9]=((C_word*)t0)[10],a[10]=((C_word*)t0)[7],a[11]=((C_word*)t0)[11],tmp=(C_word)a,a+=12,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3146,a[2]=t2,a[3]=((C_word*)t0)[11],tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(((C_word*)t0)[11]);
C_trace("abc.ck.scm:323: ?builtin?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[77]))(3,*((C_word*)lf[77]+1),t3,t4);}}

/* k3144 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3146(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
if(C_truep(t1)){
t2=C_i_cdddr(((C_word*)t0)[3]);
if(C_truep(C_i_nullp(t2))){
t3=C_i_cadr(((C_word*)t0)[3]);
t4=((C_word*)t0)[2];
f_2676(t4,C_i_symbolp(t3));}
else{
t3=((C_word*)t0)[2];
f_2676(t3,C_SCHEME_FALSE);}}
else{
t2=((C_word*)t0)[2];
f_2676(t2,C_SCHEME_FALSE);}}

/* k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2676(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2676,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_2679,a[2]=((C_word*)t0)[8],a[3]=((C_word*)t0)[9],a[4]=((C_word*)t0)[10],a[5]=((C_word*)t0)[11],tmp=(C_word)a,a+=6,tmp);
t3=C_i_caddr(((C_word*)t0)[11]);
C_trace("abc.ck.scm:323: loop");
t4=((C_word*)((C_word*)t0)[7])[1];
f_2512(t4,t2,t3);}
else{
t2=(*a=C_CLOSURE_TYPE|10,a[1]=(C_word)f_2717,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],a[8]=((C_word*)t0)[8],a[9]=((C_word*)t0)[11],a[10]=((C_word*)t0)[9],tmp=(C_word)a,a+=11,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3132,a[2]=t2,a[3]=((C_word*)t0)[11],tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(((C_word*)t0)[11]);
C_trace("abc.ck.scm:324: ?builtin?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[77]))(3,*((C_word*)lf[77]+1),t3,t4);}}

/* k3130 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3132(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(C_truep(t1)){
t2=C_i_cdddr(((C_word*)t0)[3]);
t3=((C_word*)t0)[2];
f_2717(t3,C_i_nullp(t2));}
else{
t2=((C_word*)t0)[2];
f_2717(t2,C_SCHEME_FALSE);}}

/* k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2717(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[11],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2717,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_2720,a[2]=((C_word*)t0)[6],a[3]=((C_word*)t0)[7],a[4]=((C_word*)t0)[8],a[5]=((C_word*)t0)[9],a[6]=((C_word*)t0)[10],tmp=(C_word)a,a+=7,tmp);
t3=C_i_caddr(((C_word*)t0)[9]);
C_trace("abc.ck.scm:324: loop");
t4=((C_word*)((C_word*)t0)[7])[1];
f_2512(t4,t2,t3);}
else{
t2=(*a=C_CLOSURE_TYPE|10,a[1]=(C_word)f_2768,a[2]=((C_word*)t0)[6],a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[7],a[7]=((C_word*)t0)[9],a[8]=((C_word*)t0)[5],a[9]=((C_word*)t0)[8],a[10]=((C_word*)t0)[10],tmp=(C_word)a,a+=11,tmp);
t3=C_i_car(((C_word*)t0)[9]);
C_trace("abc.ck.scm:325: ?syntax?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[78]))(3,*((C_word*)lf[78]+1),t2,t3);}}

/* k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2768(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2768,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_2771,a[2]=((C_word*)t0)[4],a[3]=((C_word*)t0)[5],a[4]=((C_word*)t0)[6],a[5]=((C_word*)t0)[7],a[6]=((C_word*)t0)[8],a[7]=((C_word*)t0)[9],a[8]=((C_word*)t0)[10],tmp=(C_word)a,a+=9,tmp);
t3=C_i_cadr(((C_word*)t0)[7]);
C_trace("abc.ck.scm:326: loop");
t4=((C_word*)((C_word*)t0)[6])[1];
f_2512(t4,t2,t3);}
else{
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_2937,a[2]=((C_word*)t0)[9],a[3]=((C_word*)t0)[7],a[4]=((C_word*)t0)[6],a[5]=((C_word*)t0)[2],a[6]=((C_word*)t0)[10],a[7]=((C_word*)t0)[3],tmp=(C_word)a,a+=8,tmp);
t3=C_i_car(((C_word*)t0)[7]);
C_trace("abc.ck.scm:336: ?procedure?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[75]))(3,*((C_word*)lf[75]+1),t2,t3);}}

/* k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2937(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2937,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_2941,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],tmp=(C_word)a,a+=8,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3030,a[2]=((C_word*)t0)[7],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(((C_word*)t0)[3]);
C_trace("abc.ck.scm:337: compile-lambda-p3");
((C_proc3)C_fast_retrieve_symbol_proc(lf[87]))(3,*((C_word*)lf[87]+1),t3,t4);}
else{
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_3037,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
C_trace("abc.ck.scm:341: make-frame-variable");
t3=((C_word*)((C_word*)t0)[5])[1];
((C_proc2)C_fast_retrieve_proc(t3))(2,t3,t2);}}

/* k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3037(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3037,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_3040,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=t1,tmp=(C_word)a,a+=8,tmp);
t3=C_i_car(((C_word*)t0)[3]);
C_trace("abc.ck.scm:342: loop");
t4=((C_word*)((C_word*)t0)[4])[1];
f_2512(t4,t2,t3);}

/* k3038 in k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3040(C_word c,C_word t0,C_word t1){
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
C_word t13;
C_word ab[27],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3040,2,t0,t1);}
t2=C_a_i_cons(&a,2,((C_word*)t0)[7],C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[65],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[6])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[6])+1,t4);
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_3045,a[2]=((C_word*)t0)[4],a[3]=((C_word*)t0)[5],a[4]=((C_word*)t0)[6],a[5]=((C_word)li80),tmp=(C_word)a,a+=6,tmp);
t7=C_i_cdr(((C_word*)t0)[3]);
t8=C_i_check_list_2(t7,lf[83]);
t9=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3074,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[6],a[4]=((C_word*)t0)[7],tmp=(C_word)a,a+=5,tmp);
t10=C_SCHEME_UNDEFINED;
t11=(*a=C_VECTOR_TYPE|1,a[1]=t10,tmp=(C_word)a,a+=2,tmp);
t12=C_set_block_item(t11,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3088,a[2]=t6,a[3]=t11,a[4]=((C_word)li81),tmp=(C_word)a,a+=5,tmp));
t13=((C_word*)t11)[1];
f_3088(t13,t9,t7);}

/* loop622 in k3038 in k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3088(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3088,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3098,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g623628");
t5=((C_word*)t0)[2];
f_3045(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k3096 in loop622 in k3038 in k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3098(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_3088(t3,((C_word*)t0)[2],t2);}

/* k3072 in k3038 in k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3074(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3074,2,t0,t1);}
t2=C_a_i_cons(&a,2,((C_word*)t0)[4],C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[96],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[3])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* g623 in k3038 in k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3045(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3045,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3049,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:343: loop");
t4=((C_word*)((C_word*)t0)[2])[1];
f_2512(t4,t3,t2);}

/* k3047 in g623 in k3038 in k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3049(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3049,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_3065,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:343: make-frame-variable");
t3=((C_word*)((C_word*)t0)[2])[1];
((C_proc2)C_fast_retrieve_proc(t3))(2,t3,t2);}

/* k3063 in k3047 in g623 in k3038 in k3035 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3065(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_3065,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[65],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[3])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* k3028 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3030(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:337: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[91]+1)))(4,*((C_word*)lf[91]+1),((C_word*)t0)[3],t1,((C_word*)((C_word*)t0)[2])[1]);}

/* k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2941(C_word c,C_word t0,C_word t1){
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
C_word ab[18],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2941,2,t0,t1);}
t2=C_mutate(((C_word *)((C_word*)t0)[7])+1,t1);
t3=C_i_car(((C_word*)((C_word*)t0)[7])[1]);
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_2945,a[2]=((C_word*)t0)[4],a[3]=((C_word*)t0)[5],a[4]=((C_word*)t0)[6],a[5]=((C_word)li76),tmp=(C_word)a,a+=6,tmp);
t5=C_i_cdr(((C_word*)t0)[3]);
t6=C_i_check_list_2(t5,lf[83]);
t7=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2974,a[2]=t3,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[6],tmp=(C_word)a,a+=5,tmp);
t8=C_SCHEME_UNDEFINED;
t9=(*a=C_VECTOR_TYPE|1,a[1]=t8,tmp=(C_word)a,a+=2,tmp);
t10=C_set_block_item(t9,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3005,a[2]=t4,a[3]=t9,a[4]=((C_word)li79),tmp=(C_word)a,a+=5,tmp));
t11=((C_word*)t9)[1];
f_3005(t11,t7,t5);}

/* loop584 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_3005(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_3005,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_3015,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g585590");
t5=((C_word*)t0)[2];
f_2945(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k3013 in loop584 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_3015(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_3005(t3,((C_word*)t0)[2],t2);}

/* k2972 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2974(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[11],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2974,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2990,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2992,a[2]=((C_word)li77),tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2995,a[2]=((C_word*)t0)[2],a[3]=((C_word)li78),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:340: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t2,t3,t4);}

/* a2994 in k2972 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2995(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2995,3,t0,t1,t2);}
t3=C_i_car(((C_word*)t0)[2]);
C_trace("abc.ck.scm:340: -");
C_minus(5,0,t1,t3,t2,C_fix(4));}

/* a2991 in k2972 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2992(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2992,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}

/* k2988 in k2972 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2990(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2990,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[95],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[3])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* g585 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2945(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2945,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2949,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:339: loop");
t4=((C_word*)((C_word*)t0)[2])[1];
f_2512(t4,t3,t2);}

/* k2947 in g585 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2949(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2949,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2965,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:339: make-frame-variable");
t3=((C_word*)((C_word*)t0)[2])[1];
((C_proc2)C_fast_retrieve_proc(t3))(2,t3,t2);}

/* k2963 in k2947 in g585 in k2939 in k2935 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2965(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2965,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[65],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[3])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2771(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[16],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2771,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_2907,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],a[8]=((C_word*)t0)[8],tmp=(C_word)a,a+=9,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2923,a[2]=((C_word)li74),tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2926,a[2]=((C_word*)t0)[2],a[3]=((C_word)li75),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:328: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t2,t3,t4);}

/* a2925 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2926(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2926,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[2])[1]);}

/* a2922 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2923(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2923,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}

/* k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2907(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[17],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2907,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|9,a[1]=(C_word)f_2915,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],a[8]=((C_word*)t0)[8],a[9]=t1,tmp=(C_word)a,a+=10,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2917,a[2]=((C_word)li72),tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2920,a[2]=((C_word*)t0)[3],a[3]=((C_word)li73),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:329: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t2,t3,t4);}

/* a2919 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2920(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2920,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[2])[1]);}

/* a2916 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2917(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2917,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}

/* k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2915(C_word c,C_word t0,C_word t1){
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
C_word ab[30],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2915,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,((C_word*)t0)[9],t2);
t4=C_a_i_cons(&a,2,C_fast_retrieve(lf[93]),t3);
t5=C_a_i_cons(&a,2,t4,((C_word*)((C_word*)t0)[8])[1]);
t6=C_mutate(((C_word *)((C_word*)t0)[8])+1,t5);
t7=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_2880,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=((C_word*)t0)[6],a[6]=((C_word*)t0)[7],a[7]=((C_word*)t0)[8],tmp=(C_word)a,a+=8,tmp);
t8=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2888,a[2]=t7,tmp=(C_word)a,a+=3,tmp);
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2890,a[2]=((C_word*)t0)[2],a[3]=((C_word)li70),tmp=(C_word)a,a+=4,tmp);
t10=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2894,a[2]=((C_word)li71),tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:330: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t8,t9,t10);}

/* a2893 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2894(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2894,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}

/* a2889 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2890(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2890,3,t0,t1,t2);}
t3=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_END_OF_LIST);}

/* k2886 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2888(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2888,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
C_trace("abc.ck.scm:330: machine-code");
((C_proc3)C_fast_retrieve_symbol_proc(lf[27]))(3,*((C_word*)lf[27]+1),((C_word*)t0)[2],t2);}

/* k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2880(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[11],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2880,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)((C_word*)t0)[7])[1]);
t3=C_mutate(((C_word *)((C_word*)t0)[7])+1,t2);
t4=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_2782,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],tmp=(C_word)a,a+=8,tmp);
t5=C_i_caddr(((C_word*)t0)[4]);
C_trace("abc.ck.scm:331: loop");
t6=((C_word*)((C_word*)t0)[3])[1];
f_2512(t6,t4,t5);}

/* k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2782(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[25],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2782,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fast_retrieve(lf[40]),C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[94],t2);
t4=C_a_i_cons(&a,2,lf[30],t3);
t5=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_2855,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],a[8]=t4,tmp=(C_word)a,a+=9,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2857,a[2]=((C_word)li68),tmp=(C_word)a,a+=3,tmp);
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2860,a[2]=((C_word*)t0)[5],a[3]=((C_word)li69),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:332: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t5,t6,t7);}

/* a2859 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2860(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2860,3,t0,t1,t2);}
C_trace("abc.ck.scm:332: -");
C_minus(5,0,t1,((C_word*)((C_word*)t0)[2])[1],t2,C_fix(4));}

/* a2856 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2857(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2857,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}

/* k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2855(C_word c,C_word t0,C_word t1){
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
C_word ab[26],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2855,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,((C_word*)t0)[8],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[7])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[7])+1,t4);
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_2824,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=((C_word*)t0)[6],a[6]=((C_word*)t0)[7],tmp=(C_word)a,a+=7,tmp);
t7=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2832,a[2]=t6,tmp=(C_word)a,a+=3,tmp);
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2834,a[2]=((C_word*)t0)[2],a[3]=((C_word)li66),tmp=(C_word)a,a+=4,tmp);
t9=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2838,a[2]=((C_word)li67),tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:333: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t7,t8,t9);}

/* a2837 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2838(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2838,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}

/* a2833 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2834(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2834,3,t0,t1,t2);}
t3=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_END_OF_LIST);}

/* k2830 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2832(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2832,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
C_trace("abc.ck.scm:333: machine-code");
((C_proc3)C_fast_retrieve_symbol_proc(lf[27]))(3,*((C_word*)lf[27]+1),((C_word*)t0)[2],t2);}

/* k2822 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2824(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2824,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)((C_word*)t0)[6])[1]);
t3=C_mutate(((C_word *)((C_word*)t0)[6])+1,t2);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2793,a[2]=((C_word*)t0)[4],a[3]=((C_word*)t0)[5],a[4]=((C_word*)t0)[6],tmp=(C_word)a,a+=5,tmp);
t5=C_i_cadddr(((C_word*)t0)[3]);
C_trace("abc.ck.scm:334: loop");
t6=((C_word*)((C_word*)t0)[2])[1];
f_2512(t6,t4,t5);}

/* k2791 in k2822 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2793(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[14],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2793,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2801,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2809,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2811,a[2]=((C_word*)t0)[2],a[3]=((C_word)li64),tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2815,a[2]=((C_word)li65),tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:335: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t3,t4,t5);}

/* a2814 in k2791 in k2822 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2815(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2815,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}

/* a2810 in k2791 in k2822 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2811(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2811,3,t0,t1,t2);}
t3=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_END_OF_LIST);}

/* k2807 in k2791 in k2822 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2809(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2809,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
C_trace("abc.ck.scm:335: machine-code");
((C_proc3)C_fast_retrieve_symbol_proc(lf[27]))(3,*((C_word*)lf[27]+1),((C_word*)t0)[2],t2);}

/* k2799 in k2791 in k2822 in k2853 in k2780 in k2878 in k2913 in k2905 in k2769 in k2766 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2801(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2801,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)((C_word*)t0)[3])[1]);
t3=C_mutate(((C_word *)((C_word*)t0)[3])+1,t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* k2718 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2720(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2720,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_2723,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=((C_word*)t0)[6],tmp=(C_word)a,a+=6,tmp);
C_trace("abc.ck.scm:324: make-frame-variable");
t3=((C_word*)((C_word*)t0)[2])[1];
((C_proc2)C_fast_retrieve_proc(t3))(2,t3,t2);}

/* k2721 in k2718 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2723(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2723,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[65],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[5])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[5])+1,t4);
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_2730,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[5],a[4]=t1,a[5]=((C_word*)t0)[4],tmp=(C_word)a,a+=6,tmp);
t7=C_i_cadr(((C_word*)t0)[4]);
C_trace("abc.ck.scm:324: loop");
t8=((C_word*)((C_word*)t0)[2])[1];
f_2512(t8,t6,t7);}

/* k2728 in k2721 in k2718 in k2715 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2730(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2730,2,t0,t1);}
t2=C_i_car(((C_word*)t0)[5]);
t3=C_a_i_cons(&a,2,((C_word*)t0)[4],C_SCHEME_END_OF_LIST);
t4=C_a_i_cons(&a,2,t2,t3);
t5=C_a_i_cons(&a,2,t4,((C_word*)((C_word*)t0)[3])[1]);
t6=C_mutate(((C_word *)((C_word*)t0)[3])+1,t5);
t7=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,t6);}

/* k2677 in k2674 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2679(C_word c,C_word t0,C_word t1){
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
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2679,2,t0,t1);}
t2=C_i_car(((C_word*)t0)[5]);
t3=C_i_cadr(((C_word*)t0)[5]);
t4=C_i_assq(t3,((C_word*)((C_word*)t0)[4])[1]);
t5=C_i_cdr(t4);
t6=C_a_i_cons(&a,2,t5,C_SCHEME_END_OF_LIST);
t7=C_a_i_cons(&a,2,t2,t6);
t8=C_a_i_cons(&a,2,t7,((C_word*)((C_word*)t0)[3])[1]);
t9=C_mutate(((C_word *)((C_word*)t0)[3])+1,t8);
t10=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t10+1)))(2,t10,t9);}

/* k2652 in k2649 in k2640 in k2604 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2654(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2654,2,t0,t1);}
t2=C_i_car(((C_word*)t0)[4]);
t3=C_a_i_cons(&a,2,t2,C_SCHEME_END_OF_LIST);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[3])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* k2598 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2600(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:316: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[91]+1)))(4,*((C_word*)lf[91]+1),((C_word*)t0)[3],t1,((C_word*)((C_word*)t0)[2])[1]);}

/* k2554 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2556(C_word c,C_word t0,C_word t1){
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
C_word ab[21],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2556,2,t0,t1);}
t2=C_mutate(((C_word *)((C_word*)t0)[4])+1,t1);
t3=C_i_car(((C_word*)((C_word*)t0)[4])[1]);
t4=C_a_i_cons(&a,2,C_fast_retrieve(lf[57]),C_SCHEME_END_OF_LIST);
t5=C_a_i_cons(&a,2,lf[57],t4);
t6=C_a_i_cons(&a,2,lf[30],t5);
t7=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2579,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t6,tmp=(C_word)a,a+=5,tmp);
t8=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2581,a[2]=((C_word)li62),tmp=(C_word)a,a+=3,tmp);
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2584,a[2]=t3,a[3]=((C_word)li63),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:318: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t7,t8,t9);}

/* a2583 in k2554 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2584(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2584,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(((C_word*)t0)[2]));}

/* a2580 in k2554 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2581(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2581,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}

/* k2577 in k2554 in k2550 in k2520 in loop in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2579(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2579,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,((C_word*)t0)[4],t2);
t4=C_a_i_cons(&a,2,t3,((C_word*)((C_word*)t0)[3])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* k2497 in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2499(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2499,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2510,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:345: reverse");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[90]+1)))(3,*((C_word*)lf[90]+1),t2,((C_word*)((C_word*)t0)[2])[1]);}

/* k2508 in k2497 in k2494 in k2487 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2510(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2510,2,t0,t1);}
t2=C_a_i_list4(&a,4,C_fix(0),t1,C_SCHEME_END_OF_LIST,((C_word*)t0)[4]);
t3=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_a_i_cons(&a,2,t2,((C_word*)((C_word*)t0)[2])[1]));}

/* g428 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2470(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2470,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2483,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:305: make-frame-variable");
t4=((C_word*)((C_word*)t0)[2])[1];
((C_proc2)C_fast_retrieve_proc(t4))(2,t4,t3);}

/* k2481 in g428 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2483(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2483,2,t0,t1);}
t2=C_a_i_cons(&a,2,((C_word*)t0)[4],t1);
t3=C_a_i_cons(&a,2,t2,((C_word*)((C_word*)t0)[3])[1]);
t4=C_mutate(((C_word *)((C_word*)t0)[3])+1,t3);
t5=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t4);}

/* f_2457 in k2449 in k2445 in compile-lambda-p3 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2457(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[10],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2457,2,t0,t1);}
t2=C_a_i_cons(&a,2,((C_word*)((C_word*)t0)[2])[1],C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[88],t2);
t4=C_a_i_plus(&a,2,C_fix(1),((C_word*)((C_word*)t0)[2])[1]);
t5=C_mutate(((C_word *)((C_word*)t0)[2])+1,t4);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t3);}

/* size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2302(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word ab[13],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2302,3,t0,t1,t2);}
t3=C_fix(0);
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2305,a[2]=t4,a[3]=((C_word)li57),tmp=(C_word)a,a+=4,tmp);
t6=C_i_check_list_2(t2,lf[83]);
t7=C_SCHEME_UNDEFINED;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_set_block_item(t8,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2419,a[2]=t5,a[3]=t8,a[4]=((C_word)li58),tmp=(C_word)a,a+=5,tmp));
t10=((C_word*)t8)[1];
f_2419(t10,t1,t2);}

/* loop376 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2419(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2419,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2429,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g377383");
t5=((C_word*)t0)[2];
f_2305(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k2427 in loop376 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2429(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_2419(t3,((C_word*)t0)[2],t2);}

/* g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2305(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2305,NULL,3,t0,t1,t2);}
t3=*((C_word*)lf[82]+1);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2312,a[2]=t2,a[3]=t1,a[4]=((C_word*)t0)[2],tmp=(C_word)a,a+=5,tmp);
C_trace("g386387");
t5=t3;
((C_proc4)C_fast_retrieve_proc(t5))(4,t5,t4,t2,((C_word*)((C_word*)t0)[2])[1]);}

/* k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2312(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2312,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2315,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("##sys#setter");
t3=*((C_word*)lf[85]+1);
((C_proc3)(void*)(*((C_word*)t3+1)))(3,t3,t2,*((C_word*)lf[86]+1));}

/* k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2315(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2315,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2318,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2407,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t4=C_i_cadr(((C_word*)t0)[2]);
C_trace("abc.ck.scm:264: compile-lambda-p5");
((C_proc4)C_fast_retrieve_symbol_proc(lf[84]))(4,*((C_word*)lf[84]+1),t3,t4,C_SCHEME_FALSE);}

/* k2405 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2407(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("g388389");
t2=((C_word*)t0)[4];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k2316 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2318(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[11],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2318,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2321,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=C_i_caddr(((C_word*)t0)[2]);
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2331,a[2]=t5,a[3]=((C_word*)t0)[4],a[4]=((C_word)li56),tmp=(C_word)a,a+=5,tmp));
t7=((C_word*)t5)[1];
f_2331(t7,t2,t3);}

/* loop in k2316 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2331(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2331,NULL,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2333,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word)li54),tmp=(C_word)a,a+=5,tmp);
t4=C_i_check_list_2(t2,lf[83]);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2382,a[2]=t3,a[3]=t6,a[4]=((C_word)li55),tmp=(C_word)a,a+=5,tmp));
t8=((C_word*)t6)[1];
f_2382(t8,t1,t2);}

/* loop394 in loop in k2316 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2382(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2382,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2392,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g395400");
t5=((C_word*)t0)[2];
f_2333(t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k2390 in loop394 in loop in k2316 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2392(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[4],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_2382(t3,((C_word*)t0)[2],t2);}

/* g395 in loop in k2316 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_2333(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_2333,NULL,3,t0,t1,t2);}
if(C_truep(C_i_numberp(t2))){
t3=C_a_i_plus(&a,2,C_fix(1),((C_word*)((C_word*)t0)[3])[1]);
t4=C_mutate(((C_word *)((C_word*)t0)[3])+1,t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t4);}
else{
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=C_eqp(t3,lf[80]);
if(C_truep(t4)){
t5=C_i_cadr(t2);
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2366,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("g406407");
t7=t5;
((C_proc3)C_fast_retrieve_proc(t7))(3,t7,t6,((C_word*)((C_word*)t0)[3])[1]);}
else{
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t2);}}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}}}

/* k2364 in g395 in loop in k2316 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2366(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:270: loop");
t2=((C_word*)((C_word*)t0)[3])[1];
f_2331(t2,((C_word*)t0)[2],t1);}

/* k2319 in k2316 in k2313 in k2310 in g377 in size in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2321(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2321,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)((C_word*)t0)[3])[1],C_fix(3));
t3=C_mutate(((C_word *)((C_word*)t0)[3])+1,t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* relocation-symbol in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2288(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_2288,4,t0,t1,t2,t3);}
t4=C_a_i_cons(&a,2,t3,C_SCHEME_END_OF_LIST);
t5=C_a_i_cons(&a,2,t2,t4);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_a_i_cons(&a,2,lf[80],t5));}

/* ?syntax? in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2272(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2272,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_eqp(t3,lf[79]));}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}}

/* ?builtin? in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2256(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2256,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_eqp(t3,lf[30]));}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}}

/* ?procedure? in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2240(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2240,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_eqp(t3,lf[76]));}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}}

/* make-closure in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2197(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_2197,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2203,a[2]=t3,a[3]=t2,a[4]=((C_word)li48),tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:209: call-with-memory");
((C_proc3)C_fast_retrieve_symbol_proc(lf[70]))(3,*((C_word*)lf[70]+1),t1,t4);}

/* a2202 in make-closure in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2203(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2203,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_2207,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("abc.ck.scm:210: store");
((C_proc4)C_fast_retrieve_symbol_proc(lf[67]))(4,*((C_word*)lf[67]+1),t3,t2,lf[69]);}

/* k2205 in a2202 in make-closure in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2207(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2207,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2210,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],tmp=(C_word)a,a+=5,tmp);
t3=C_a_i_plus(&a,2,((C_word*)t0)[5],C_fix(1));
C_trace("abc.ck.scm:211: store");
((C_proc4)C_fast_retrieve_symbol_proc(lf[67]))(4,*((C_word*)lf[67]+1),t2,t3,((C_word*)t0)[2]);}

/* k2208 in k2205 in a2202 in make-closure in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2210(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2210,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2213,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
t3=C_a_i_plus(&a,2,((C_word*)t0)[4],C_fix(2));
C_trace("abc.ck.scm:212: store");
((C_proc4)C_fast_retrieve_symbol_proc(lf[67]))(4,*((C_word*)lf[67]+1),t2,t3,lf[68]);}

/* k2211 in k2208 in k2205 in a2202 in make-closure in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2213(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2213,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)t0)[4],C_fix(3));
t3=C_a_i_plus(&a,2,((C_word*)t0)[4],C_fix(28));
t4=C_a_i_minus(&a,2,((C_word*)t0)[3],t3);
C_trace("abc.ck.scm:213: store");
((C_proc5)C_fast_retrieve_symbol_proc(lf[67]))(5,*((C_word*)lf[67]+1),((C_word*)t0)[2],t2,t4,((C_word*)t0)[4]);}

/* push in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2191(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2191,2,t0,t1);}
t2=C_fast_retrieve(lf[51]);
C_trace("g346347");
t3=t2;
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t1,lf[47]);}

/* read64 in k2181 in k2177 in k2173 in k2169 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2185(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2185,2,t0,t1);}
t2=C_fast_retrieve(lf[33]);
C_trace("g341342");
t3=t2;
((C_proc4)C_fast_retrieve_proc(t3))(4,t3,t1,lf[47],lf[47]);}

/* func-2-1-1 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2161(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2161,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2163,a[2]=t2,a[3]=((C_word)li44),tmp=(C_word)a,a+=4,tmp));}

/* f_2163 in func-2-1-1 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2163(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2163,3,t0,t1,t2);}
C_trace("abc.ck.scm:199: inst");
t3=((C_word*)t0)[2];
((C_proc4)C_fast_retrieve_proc(t3))(4,t3,t1,lf[47],t2);}

/* func-2-2-1 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2153(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2153,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2155,a[2]=t2,a[3]=((C_word)li42),tmp=(C_word)a,a+=4,tmp));}

/* f_2155 in func-2-2-1 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2155(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2155,3,t0,t1,t2);}
C_trace("abc.ck.scm:195: inst");
t3=((C_word*)t0)[2];
((C_proc4)C_fast_retrieve_proc(t3))(4,t3,t1,lf[47],t2);}

/* const64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2147(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2147,3,t0,t1,t2);}
C_trace("abc.ck.scm:150: ropconst64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[34]))(4,*((C_word*)lf[34]+1),t1,lf[47],t2);}

/* lref in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2141(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2141,3,t0,t1,t2);}
C_trace("abc.ck.scm:147: sopmov64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[43]))(4,*((C_word*)lf[43]+1),t1,lf[47],t2);}

/* frame in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2113(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2113,3,t0,t1,t2);}
if(C_truep(C_i_nequalp(t2,C_fix(0)))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}
else{
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2135,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2139,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:143: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t4,C_fix(8),C_fix(1),t2);}}

/* k2137 in frame in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2139(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k2133 in frame in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2135(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2135,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(196),t1);
t3=C_a_i_cons(&a,2,C_fix(131),t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,C_fix(72),t3));}

/* leave in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2085(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2085,3,t0,t1,t2);}
if(C_truep(C_i_nequalp(t2,C_fix(0)))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}
else{
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2107,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2111,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:137: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t4,C_fix(8),C_fix(1),t2);}}

/* k2109 in leave in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2111(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k2105 in leave in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2107(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2107,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(196),t1);
t3=C_a_i_cons(&a,2,C_fix(131),t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,C_fix(72),t3));}

/* cret in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2082(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2082,2,t0,t1);}
t2=t1;
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[53]);}

/* rspush in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2068(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2068,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2076,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2080,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:126: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t4,t2);}

/* k2078 in rspush in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2080(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:126: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(80),t1);}

/* k2074 in rspush in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2076(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2076,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST));}

/* rsopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2024(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_2024,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2031,a[2]=t3,a[3]=t2,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:122: <");
C_lessp(5,0,t4,C_fix(-128),t2,C_fix(127));}

/* k2029 in rsopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2031(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2031,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2046,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2062,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2066,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:123: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t4,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[4];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}}

/* k2064 in k2029 in rsopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2066(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:123: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k2060 in k2029 in rsopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2062(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:123: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(68),t1);}

/* k2044 in k2029 in rsopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2046(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2046,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2054,a[2]=((C_word*)t0)[3],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2058,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:123: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t3,C_fix(8),C_fix(1),((C_word*)t0)[2]);}

/* k2056 in k2044 in k2029 in rsopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2058(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k2052 in k2044 in k2029 in rsopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2054(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2054,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(36),t1);
t3=C_a_i_cons(&a,2,((C_word*)t0)[3],t2);
t4=C_a_i_cons(&a,2,C_fix(137),t3);
t5=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_cons(&a,2,C_fix(72),t4));}

/* sopcall64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2006(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_2006,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2014,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:116: sopmov64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[43]))(4,*((C_word*)lf[43]+1),t3,lf[47],t2);}

/* k2012 in sopcall64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2014(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_2014,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2018,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2022,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:117: ropcall64");
((C_proc3)C_fast_retrieve_symbol_proc(lf[37]))(3,*((C_word*)lf[37]+1),t3,lf[47]);}

/* k2020 in k2012 in sopcall64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2022(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k2016 in k2012 in sopcall64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2018(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* sopstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1980(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1980,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1988,a[2]=t3,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:111: sopmov64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[43]))(4,*((C_word*)lf[43]+1),t4,lf[47],t2);}

/* k1986 in sopstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1988(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1988,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1992,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1996,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:112: sopmov64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[43]))(4,*((C_word*)lf[43]+1),t3,lf[48],((C_word*)t0)[2]);}

/* k1994 in k1986 in sopstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1996(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1996,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2000,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2004,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:113: ropstore64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[32]))(4,*((C_word*)lf[32]+1),t3,lf[47],lf[48]);}

/* k2002 in k1994 in k1986 in sopstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2004(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1998 in k1994 in k1986 in sopstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_2000(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k1990 in k1986 in sopstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1992(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* sopread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1962(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1962,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1970,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:107: sopmov64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[43]))(4,*((C_word*)lf[43]+1),t4,t2,t3);}

/* k1968 in sopread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1970(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1970,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1974,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1978,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:108: ropread64");
((C_proc4)C_fast_retrieve_symbol_proc(lf[33]))(4,*((C_word*)lf[33]+1),t3,((C_word*)t0)[2],((C_word*)t0)[2]);}

/* k1976 in k1968 in sopread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1978(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1972 in k1968 in sopread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1974(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* sopcmp64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1918(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1918,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1925,a[2]=t2,a[3]=t3,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:102: <");
C_lessp(5,0,t4,C_fix(-128),t3,C_fix(127));}

/* k1923 in sopcmp64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1925(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1925,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1940,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1956,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1960,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:103: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t4,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[4];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}}

/* k1958 in k1923 in sopcmp64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1960(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:103: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1954 in k1923 in sopcmp64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1956(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:103: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(68),t1);}

/* k1938 in k1923 in sopcmp64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1940(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1940,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1948,a[2]=((C_word*)t0)[3],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1952,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:103: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t3,C_fix(8),C_fix(1),((C_word*)t0)[2]);}

/* k1950 in k1938 in k1923 in sopcmp64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1952(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1946 in k1938 in k1923 in sopcmp64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1948(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1948,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(36),t1);
t3=C_a_i_cons(&a,2,((C_word*)t0)[3],t2);
t4=C_a_i_cons(&a,2,C_fix(59),t3);
t5=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_cons(&a,2,C_fix(72),t4));}

/* sopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1874(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1874,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1881,a[2]=t2,a[3]=t3,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:97: <");
C_lessp(5,0,t4,C_fix(-128),t3,C_fix(127));}

/* k1879 in sopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1881(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1881,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1896,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1912,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1916,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:98: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t4,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[4];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}}

/* k1914 in k1879 in sopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1916(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:98: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1910 in k1879 in sopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1912(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:98: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(68),t1);}

/* k1894 in k1879 in sopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1896(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1896,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1904,a[2]=((C_word*)t0)[3],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1908,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:98: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t3,C_fix(8),C_fix(1),((C_word*)t0)[2]);}

/* k1906 in k1894 in k1879 in sopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1908(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1902 in k1894 in k1879 in sopmov64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1904(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1904,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(36),t1);
t3=C_a_i_cons(&a,2,((C_word*)t0)[3],t2);
t4=C_a_i_cons(&a,2,C_fix(139),t3);
t5=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_cons(&a,2,C_fix(72),t4));}

/* sopimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1826(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1826,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1833,a[2]=t2,a[3]=t3,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:92: <");
C_lessp(5,0,t4,C_fix(-128),t3,C_fix(127));}

/* k1831 in sopimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1833(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1833,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1852,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1868,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1872,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:93: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t4,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[4];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}}

/* k1870 in k1831 in sopimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1872(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:93: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1866 in k1831 in sopimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1868(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:93: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(68),t1);}

/* k1850 in k1831 in sopimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1852(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1852,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1860,a[2]=((C_word*)t0)[3],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1864,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:93: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t3,C_fix(8),C_fix(1),((C_word*)t0)[2]);}

/* k1862 in k1850 in k1831 in sopimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1864(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1858 in k1850 in k1831 in sopimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1860(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1860,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(36),t1);
t3=C_a_i_cons(&a,2,((C_word*)t0)[3],t2);
t4=C_a_i_cons(&a,2,C_fix(175),t3);
t5=C_a_i_cons(&a,2,C_fix(15),t4);
t6=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_a_i_cons(&a,2,C_fix(72),t5));}

/* sopadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1782(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1782,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1789,a[2]=t2,a[3]=t3,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("abc.ck.scm:87: <");
C_lessp(5,0,t4,C_fix(-128),t3,C_fix(127));}

/* k1787 in sopadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1789(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1789,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1804,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1820,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1824,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:88: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t4,((C_word*)t0)[2]);}
else{
t2=((C_word*)t0)[4];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}}

/* k1822 in k1787 in sopadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1824(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:88: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1818 in k1787 in sopadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1820(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:88: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(68),t1);}

/* k1802 in k1787 in sopadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1804(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1804,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1812,a[2]=((C_word*)t0)[3],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1816,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:88: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t3,C_fix(8),C_fix(1),((C_word*)t0)[2]);}

/* k1814 in k1802 in k1787 in sopadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1816(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1810 in k1802 in k1787 in sopadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1812(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1812,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(36),t1);
t3=C_a_i_cons(&a,2,((C_word*)t0)[3],t2);
t4=C_a_i_cons(&a,2,C_fix(3),t3);
t5=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_cons(&a,2,C_fix(72),t4));}

/* opjmp32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1768(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1768,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1776,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1780,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:82: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t4,C_fix(8),C_fix(4),t2);}

/* k1778 in opjmp32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1780(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1774 in opjmp32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1776(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1776,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,C_fix(233),t1));}

/* opje32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1750(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1750,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1762,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1766,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:79: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t4,C_fix(8),C_fix(4),t2);}

/* k1764 in opje32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1766(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1760 in opje32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1762(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1762,2,t0,t1);}
t2=C_a_i_cons(&a,2,C_fix(132),t1);
t3=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_a_i_cons(&a,2,C_fix(15),t2));}

/* roprelcall32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1736(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1736,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1744,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1748,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:76: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t4,C_fix(8),C_fix(4),t2);}

/* k1746 in roprelcall32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1748(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1742 in roprelcall32 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1744(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1744,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,C_fix(232),t1));}

/* ropcall64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1718(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1718,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1730,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1734,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:72: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t4,t2);}

/* k1732 in ropcall64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1734(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:72: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(208),t1);}

/* k1728 in ropcall64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1730(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1730,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_a_i_cons(&a,2,C_fix(255),t2));}

/* ropimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1684(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[7],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1684,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1704,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1708,a[2]=t2,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:69: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t5,t3);}

/* k1706 in ropimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1708(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1708,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1712,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1716,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:69: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t3,((C_word*)t0)[2]);}

/* k1714 in k1706 in ropimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1716(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:69: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1710 in k1706 in ropimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1712(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:69: logior");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(5,*((C_word*)lf[0]+1),((C_word*)t0)[3],C_fix(192),((C_word*)t0)[2],t1);}

/* k1702 in ropimul64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1704(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1704,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,C_fix(175),t2);
t4=C_a_i_cons(&a,2,C_fix(15),t3);
t5=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_cons(&a,2,C_fix(72),t4));}

/* ropconst64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1658(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[7],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1658,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1670,a[2]=t3,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1682,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:66: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t5,t2);}

/* k1680 in ropconst64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1682(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:66: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[2],C_fix(184),t1);}

/* k1668 in ropconst64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1670(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1670,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1674,a[2]=((C_word*)t0)[3],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1678,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:66: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),t3,C_fix(8),C_fix(8),((C_word*)t0)[2]);}

/* k1676 in k1668 in ropconst64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1678(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("##sys#append");
t2=*((C_word*)lf[35]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,((C_word*)t0)[2],t1,C_SCHEME_END_OF_LIST);}

/* k1672 in k1668 in ropconst64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1674(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1674,2,t0,t1);}
t2=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
t3=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_a_i_cons(&a,2,C_fix(72),t2));}

/* ropread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1628(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[7],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1628,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1644,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1648,a[2]=t2,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:63: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t5,t3);}

/* k1646 in ropread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1648(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1648,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1652,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1656,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:63: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t3,((C_word*)t0)[2]);}

/* k1654 in k1646 in ropread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1656(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:63: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1650 in k1646 in ropread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1652(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:63: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k1642 in ropread64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1644(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1644,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,C_fix(139),t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,C_fix(72),t3));}

/* ropstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1598(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[7],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1598,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1614,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1618,a[2]=t3,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:60: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t5,t2);}

/* k1616 in ropstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1618(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1618,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1622,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1626,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:60: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t3,((C_word*)t0)[2]);}

/* k1624 in k1616 in ropstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1626(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:60: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1620 in k1616 in ropstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1622(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:60: logior");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(4,*((C_word*)lf[0]+1),((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k1612 in ropstore64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1614(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1614,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,C_fix(137),t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,C_fix(72),t3));}

/* ropadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1568(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[7],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1568,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1584,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1588,a[2]=t3,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:57: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t5,t2);}

/* k1586 in ropadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1588(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1588,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1592,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1596,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:57: regi");
((C_proc3)C_fast_retrieve_symbol_proc(lf[24]))(3,*((C_word*)lf[24]+1),t3,((C_word*)t0)[2]);}

/* k1594 in k1586 in ropadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1596(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:57: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[2],t1,C_fix(3));}

/* k1590 in k1586 in ropadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1592(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:57: logior");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(5,*((C_word*)lf[0]+1),((C_word*)t0)[3],C_fix(192),((C_word*)t0)[2],t1);}

/* k1582 in ropadd64 in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1584(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1584,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,C_fix(1),t2);
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,C_fix(72),t3));}

/* machine-code in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1497(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
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
C_word t13;
C_word t14;
C_word ab[17],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1497,3,t0,t1,t2);}
t3=C_SCHEME_END_OF_LIST;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_FALSE;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_fast_retrieve(lf[28]);
t8=t2;
t9=C_i_check_list_2(t8,lf[29]);
t10=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1531,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t11=C_SCHEME_UNDEFINED;
t12=(*a=C_VECTOR_TYPE|1,a[1]=t11,tmp=(C_word)a,a+=2,tmp);
t13=C_set_block_item(t12,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1533,a[2]=t7,a[3]=t4,a[4]=t12,a[5]=t6,a[6]=((C_word)li17),tmp=(C_word)a,a+=7,tmp));
t14=((C_word*)t12)[1];
f_1533(t14,t10,t8);}

/* loop172 in machine-code in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_1533(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1533,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1562,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=t2,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("g178186");
t5=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[3])[1]);}}

/* k1560 in loop172 in machine-code in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1562(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1562,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1546,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[6])[1])){
t4=t3;
f_1546(t4,C_i_setslot(((C_word*)((C_word*)t0)[6])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t5=t3;
f_1546(t5,t4);}}

/* k1544 in k1560 in loop172 in machine-code in k1227 in k1219 in k1216 in k1213 */
static void C_fcall f_1546(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[6])+1,((C_word*)t0)[5]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[3])[1];
f_1533(t4,((C_word*)t0)[2],t3);}

/* k1529 in machine-code in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1531(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[22],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1531,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=C_a_i_cons(&a,2,lf[27],t2);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1519,a[2]=((C_word*)t0)[3],a[3]=((C_word)li16),tmp=(C_word)a,a+=4,tmp);
t5=C_a_i_cons(&a,2,t4,C_SCHEME_END_OF_LIST);
t6=C_a_i_cons(&a,2,t3,t5);
t7=C_a_i_cons(&a,2,lf[30],t6);
t8=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t8+1)))(2,t8,C_a_i_cons(&a,2,t7,C_SCHEME_END_OF_LIST));}

/* a1518 in k1529 in machine-code in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1519(C_word c,C_word t0,C_word t1,...){
C_word tmp;
C_word t2;
C_word *a;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1519,2,t0,t1);}
t2=t1;
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}

/* regi in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1473(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[8],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1473,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1477,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1491,a[2]=t2,a[3]=((C_word)li14),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:49: list-index");
((C_proc4)C_fast_retrieve_symbol_proc(lf[26]))(4,*((C_word*)lf[26]+1),t3,t4,C_fast_retrieve(lf[14]));}

/* a1490 in regi in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1491(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1491,3,t0,t1,t2);}
t3=*((C_word*)lf[25]+1);
C_trace("g156157");
t4=t3;
((C_proc4)C_fast_retrieve_proc(t4))(4,t4,t1,((C_word*)t0)[2],t2);}

/* k1475 in regi in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1477(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1477,2,t0,t1);}
if(C_truep(t1)){
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,t1);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1485,a[2]=((C_word*)t0)[2],a[3]=((C_word)li13),tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:50: list-index");
((C_proc4)C_fast_retrieve_symbol_proc(lf[26]))(4,*((C_word*)lf[26]+1),((C_word*)t0)[3],t2,C_fast_retrieve(lf[16]));}}

/* a1484 in k1475 in regi in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1485(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1485,3,t0,t1,t2);}
t3=*((C_word*)lf[25]+1);
C_trace("g163164");
t4=t3;
((C_proc4)C_fast_retrieve_proc(t4))(4,t4,t1,((C_word*)t0)[2],t2);}

/* enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1383(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word ab[15],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_1383,5,t0,t1,t2,t3,t4);}
if(C_truep(C_i_numberp(t4))){
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1393,a[2]=t2,a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1434,a[2]=t4,a[3]=t5,tmp=(C_word)a,a+=4,tmp);
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1438,a[2]=t3,a[3]=t6,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:42: expt");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[23]+1)))(4,*((C_word*)lf[23]+1),t7,C_fix(2),t2);}
else{
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1445,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1447,a[2]=t3,a[3]=t2,a[4]=t4,a[5]=((C_word)li10),tmp=(C_word)a,a+=6,tmp);
t7=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1460,a[2]=t3,a[3]=t2,a[4]=t4,a[5]=((C_word)li11),tmp=(C_word)a,a+=6,tmp);
C_trace("abc.ck.scm:46: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t5,t6,t7);}}

/* a1459 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1460(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1460,3,t0,t1,t2);}
t3=C_i_caddr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1471,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("g144145");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1469 in a1459 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1471(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:46: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),((C_word*)t0)[4],((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* a1446 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1447(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1447,3,t0,t1,t2);}
t3=C_i_cadr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1458,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("g141142");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1456 in a1446 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1458(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:46: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),((C_word*)t0)[4],((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k1443 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1445(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1445,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_list1(&a,1,t1));}

/* k1436 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1438(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:42: expt");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[23]+1)))(4,*((C_word*)lf[23]+1),((C_word*)t0)[3],t1,((C_word*)t0)[2]);}

/* k1432 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1434(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:42: mod");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[8]+1)))(4,*((C_word*)lf[8]+1),((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k1391 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1393(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[13],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1393,2,t0,t1);}
t2=C_eqp(((C_word*)t0)[4],C_fix(0));
if(C_truep(t2)){
t3=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}
else{
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1406,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[3],tmp=(C_word)a,a+=6,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1426,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1430,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
C_trace("abc.ck.scm:45: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),t5,C_fix(-1),((C_word*)t0)[2]);}}

/* k1428 in k1391 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1430(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:45: lognot");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[2]+1)))(3,*((C_word*)lf[2]+1),((C_word*)t0)[2],t1);}

/* k1424 in k1391 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1426(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:45: logand");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[4]+1)))(4,*((C_word*)lf[4]+1),((C_word*)t0)[3],t1,((C_word*)t0)[2]);}

/* k1404 in k1391 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1406(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[17],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1406,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1410,a[2]=t1,a[3]=((C_word*)t0)[5],tmp=(C_word)a,a+=4,tmp);
t3=C_a_i_minus(&a,2,((C_word*)t0)[4],C_fix(1));
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1418,a[2]=t3,a[3]=((C_word*)t0)[3],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1422,a[2]=((C_word*)t0)[2],a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("abc.ck.scm:45: -");
C_minus(3,0,t5,((C_word*)t0)[3]);}

/* k1420 in k1404 in k1391 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1422(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:45: ash");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(4,*((C_word*)lf[6]+1),((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k1416 in k1404 in k1391 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1418(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:45: enc");
((C_proc5)C_fast_retrieve_symbol_proc(lf[22]))(5,*((C_word*)lf[22]+1),((C_word*)t0)[4],((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* k1408 in k1404 in k1391 in enc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1410(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1410,2,t0,t1);}
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[2],t1));}

/* relocation-proc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1250(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1250,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1252,a[2]=t2,a[3]=((C_word)li8),tmp=(C_word)a,a+=4,tmp));}

/* f_1252 in relocation-proc in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1252(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word ab[12],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1252,4,t0,t1,t2,t3);}
t4=C_i_numberp(t2);
t5=(C_truep(t4)?C_i_numberp(t3):C_SCHEME_FALSE);
if(C_truep(t5)){
C_trace("abc.ck.scm:35: p");
t6=((C_word*)t0)[2];
((C_proc4)C_fast_retrieve_proc(t6))(4,t6,t1,t2,t3);}
else{
if(C_truep(C_i_numberp(t2))){
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1273,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=t3,a[5]=((C_word)li2),tmp=(C_word)a,a+=6,tmp);
t7=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1286,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=t3,a[5]=((C_word)li3),tmp=(C_word)a,a+=6,tmp);
C_trace("abc.ck.scm:36: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t1,t6,t7);}
else{
if(C_truep(C_i_numberp(t3))){
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1308,a[2]=t3,a[3]=((C_word*)t0)[2],a[4]=t2,a[5]=((C_word)li4),tmp=(C_word)a,a+=6,tmp);
t7=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1321,a[2]=t3,a[3]=((C_word*)t0)[2],a[4]=t2,a[5]=((C_word)li5),tmp=(C_word)a,a+=6,tmp);
C_trace("abc.ck.scm:37: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t1,t6,t7);}
else{
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1337,a[2]=((C_word*)t0)[2],a[3]=t3,a[4]=t2,a[5]=((C_word)li6),tmp=(C_word)a,a+=6,tmp);
t7=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1357,a[2]=((C_word*)t0)[2],a[3]=t3,a[4]=t2,a[5]=((C_word)li7),tmp=(C_word)a,a+=6,tmp);
C_trace("abc.ck.scm:38: relocation-symbol");
((C_proc4)C_fast_retrieve_symbol_proc(lf[21]))(4,*((C_word*)lf[21]+1),t1,t6,t7);}}}}

/* a1356 */
static void C_ccall f_1357(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1357,3,t0,t1,t2);}
t3=C_i_caddr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1368,a[2]=t2,a[3]=t1,a[4]=((C_word*)t0)[2],a[5]=((C_word*)t0)[3],tmp=(C_word)a,a+=6,tmp);
C_trace("g130131");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1366 in a1356 */
static void C_ccall f_1368(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1368,2,t0,t1);}
t2=C_i_caddr(((C_word*)t0)[5]);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1375,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("g132133");
t4=t2;
((C_proc3)C_fast_retrieve_proc(t4))(3,t4,t3,((C_word*)t0)[2]);}

/* k1373 in k1366 in a1356 */
static void C_ccall f_1375(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:38: p");
t2=((C_word*)t0)[4];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* a1336 */
static void C_ccall f_1337(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1337,3,t0,t1,t2);}
t3=C_i_cadr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1348,a[2]=t2,a[3]=t1,a[4]=((C_word*)t0)[2],a[5]=((C_word*)t0)[3],tmp=(C_word)a,a+=6,tmp);
C_trace("g125126");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1346 in a1336 */
static void C_ccall f_1348(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1348,2,t0,t1);}
t2=C_i_cadr(((C_word*)t0)[5]);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1355,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("g127128");
t4=t2;
((C_proc3)C_fast_retrieve_proc(t4))(3,t4,t3,((C_word*)t0)[2]);}

/* k1353 in k1346 in a1336 */
static void C_ccall f_1355(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:38: p");
t2=((C_word*)t0)[4];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* a1320 */
static void C_ccall f_1321(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1321,3,t0,t1,t2);}
t3=C_i_caddr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1332,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("g122123");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1330 in a1320 */
static void C_ccall f_1332(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:37: p");
t2=((C_word*)t0)[4];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],t1,((C_word*)t0)[2]);}

/* a1307 */
static void C_ccall f_1308(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1308,3,t0,t1,t2);}
t3=C_i_cadr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1319,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("g119120");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1317 in a1307 */
static void C_ccall f_1319(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:37: p");
t2=((C_word*)t0)[4];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],t1,((C_word*)t0)[2]);}

/* a1285 */
static void C_ccall f_1286(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1286,3,t0,t1,t2);}
t3=C_i_caddr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1297,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("g116117");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1295 in a1285 */
static void C_ccall f_1297(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:36: p");
t2=((C_word*)t0)[4];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* a1272 */
static void C_ccall f_1273(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1273,3,t0,t1,t2);}
t3=C_i_cadr(((C_word*)t0)[4]);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1284,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("g113114");
t5=t3;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,t2);}

/* k1282 in a1272 */
static void C_ccall f_1284(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("abc.ck.scm:36: p");
t2=((C_word*)t0)[4];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],t1);}

/* flush in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1241(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1241,2,t0,t1);}
C_trace("abc.ck.scm:26: flush-output");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(2,*((C_word*)lf[13]+1),t1);}

/* write-byte in k1227 in k1219 in k1216 in k1213 */
static void C_ccall f_1231(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1231,3,t0,t1,t2);}
t3=C_make_character(C_unfix(t2));
C_trace("abc.ck.scm:25: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t1,t3);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[385] = {
{"toplevel:abc_ck_scm",(void*)C_toplevel},
{"f_1215:abc_ck_scm",(void*)f_1215},
{"f_1218:abc_ck_scm",(void*)f_1218},
{"f_1221:abc_ck_scm",(void*)f_1221},
{"f_1229:abc_ck_scm",(void*)f_1229},
{"f_2171:abc_ck_scm",(void*)f_2171},
{"f_2175:abc_ck_scm",(void*)f_2175},
{"f_2179:abc_ck_scm",(void*)f_2179},
{"f_2183:abc_ck_scm",(void*)f_2183},
{"f_4729:abc_ck_scm",(void*)f_4729},
{"f_4735:abc_ck_scm",(void*)f_4735},
{"f_4732:abc_ck_scm",(void*)f_4732},
{"f_4488:abc_ck_scm",(void*)f_4488},
{"f_4582:abc_ck_scm",(void*)f_4582},
{"f_4671:abc_ck_scm",(void*)f_4671},
{"f_4700:abc_ck_scm",(void*)f_4700},
{"f_4684:abc_ck_scm",(void*)f_4684},
{"f_4601:abc_ck_scm",(void*)f_4601},
{"f_4630:abc_ck_scm",(void*)f_4630},
{"f_4614:abc_ck_scm",(void*)f_4614},
{"f_4599:abc_ck_scm",(void*)f_4599},
{"f_4462:abc_ck_scm",(void*)f_4462},
{"f_4436:abc_ck_scm",(void*)f_4436},
{"f_4359:abc_ck_scm",(void*)f_4359},
{"f_4405:abc_ck_scm",(void*)f_4405},
{"f_4413:abc_ck_scm",(void*)f_4413},
{"f_4381:abc_ck_scm",(void*)f_4381},
{"f_4367:abc_ck_scm",(void*)f_4367},
{"f_4391:abc_ck_scm",(void*)f_4391},
{"f_4376:abc_ck_scm",(void*)f_4376},
{"f_4423:abc_ck_scm",(void*)f_4423},
{"f_4201:abc_ck_scm",(void*)f_4201},
{"f_4246:abc_ck_scm",(void*)f_4246},
{"f_4275:abc_ck_scm",(void*)f_4275},
{"f_4259:abc_ck_scm",(void*)f_4259},
{"f_4296:abc_ck_scm",(void*)f_4296},
{"f_4325:abc_ck_scm",(void*)f_4325},
{"f_4309:abc_ck_scm",(void*)f_4309},
{"f_4040:abc_ck_scm",(void*)f_4040},
{"f_4047:abc_ck_scm",(void*)f_4047},
{"f_4066:abc_ck_scm",(void*)f_4066},
{"f_4162:abc_ck_scm",(void*)f_4162},
{"f_4172:abc_ck_scm",(void*)f_4172},
{"f_4072:abc_ck_scm",(void*)f_4072},
{"f_4088:abc_ck_scm",(void*)f_4088},
{"f_4135:abc_ck_scm",(void*)f_4135},
{"f_4145:abc_ck_scm",(void*)f_4145},
{"f_4090:abc_ck_scm",(void*)f_4090},
{"f_4097:abc_ck_scm",(void*)f_4097},
{"f_4110:abc_ck_scm",(void*)f_4110},
{"f_4107:abc_ck_scm",(void*)f_4107},
{"f_4075:abc_ck_scm",(void*)f_4075},
{"f_4055:abc_ck_scm",(void*)f_4055},
{"f_4059:abc_ck_scm",(void*)f_4059},
{"f_3775:abc_ck_scm",(void*)f_3775},
{"f_3780:abc_ck_scm",(void*)f_3780},
{"f_3783:abc_ck_scm",(void*)f_3783},
{"f_3786:abc_ck_scm",(void*)f_3786},
{"f_3795:abc_ck_scm",(void*)f_3795},
{"f_3804:abc_ck_scm",(void*)f_3804},
{"f_3811:abc_ck_scm",(void*)f_3811},
{"f_3814:abc_ck_scm",(void*)f_3814},
{"f_3965:abc_ck_scm",(void*)f_3965},
{"f_3939:abc_ck_scm",(void*)f_3939},
{"f_3942:abc_ck_scm",(void*)f_3942},
{"f_3975:abc_ck_scm",(void*)f_3975},
{"f_3817:abc_ck_scm",(void*)f_3817},
{"f_3930:abc_ck_scm",(void*)f_3930},
{"f_3820:abc_ck_scm",(void*)f_3820},
{"f_3877:abc_ck_scm",(void*)f_3877},
{"f_3888:abc_ck_scm",(void*)f_3888},
{"f_3896:abc_ck_scm",(void*)f_3896},
{"f_3898:abc_ck_scm",(void*)f_3898},
{"f_3911:abc_ck_scm",(void*)f_3911},
{"f_3917:abc_ck_scm",(void*)f_3917},
{"f_3924:abc_ck_scm",(void*)f_3924},
{"f_3882:abc_ck_scm",(void*)f_3882},
{"f_3823:abc_ck_scm",(void*)f_3823},
{"f_3834:abc_ck_scm",(void*)f_3834},
{"f_3842:abc_ck_scm",(void*)f_3842},
{"f_3844:abc_ck_scm",(void*)f_3844},
{"f_3857:abc_ck_scm",(void*)f_3857},
{"f_3863:abc_ck_scm",(void*)f_3863},
{"f_3870:abc_ck_scm",(void*)f_3870},
{"f_3828:abc_ck_scm",(void*)f_3828},
{"f_3789:abc_ck_scm",(void*)f_3789},
{"f_3438:abc_ck_scm",(void*)f_3438},
{"f_3751:abc_ck_scm",(void*)f_3751},
{"f_3761:abc_ck_scm",(void*)f_3761},
{"f_3729:abc_ck_scm",(void*)f_3729},
{"f_3518:abc_ck_scm",(void*)f_3518},
{"f_3707:abc_ck_scm",(void*)f_3707},
{"f_3679:abc_ck_scm",(void*)f_3679},
{"f_3639:abc_ck_scm",(void*)f_3639},
{"f_3581:abc_ck_scm",(void*)f_3581},
{"f_3529:abc_ck_scm",(void*)f_3529},
{"f_3444:abc_ck_scm",(void*)f_3444},
{"f_3476:abc_ck_scm",(void*)f_3476},
{"f_3505:abc_ck_scm",(void*)f_3505},
{"f_3489:abc_ck_scm",(void*)f_3489},
{"f_3320:abc_ck_scm",(void*)f_3320},
{"f_3436:abc_ck_scm",(void*)f_3436},
{"f_3391:abc_ck_scm",(void*)f_3391},
{"f_3399:abc_ck_scm",(void*)f_3399},
{"f_3428:abc_ck_scm",(void*)f_3428},
{"f_3412:abc_ck_scm",(void*)f_3412},
{"f_3397:abc_ck_scm",(void*)f_3397},
{"f_3326:abc_ck_scm",(void*)f_3326},
{"f_3333:abc_ck_scm",(void*)f_3333},
{"f_3359:abc_ck_scm",(void*)f_3359},
{"f_3351:abc_ck_scm",(void*)f_3351},
{"f_3282:abc_ck_scm",(void*)f_3282},
{"f_3290:abc_ck_scm",(void*)f_3290},
{"f_3310:abc_ck_scm",(void*)f_3310},
{"f_3318:abc_ck_scm",(void*)f_3318},
{"f_3314:abc_ck_scm",(void*)f_3314},
{"f_3306:abc_ck_scm",(void*)f_3306},
{"f_4753:abc_ck_scm",(void*)f_4753},
{"f_4798:abc_ck_scm",(void*)f_4798},
{"f_4802:abc_ck_scm",(void*)f_4802},
{"f_4795:abc_ck_scm",(void*)f_4795},
{"f_4793:abc_ck_scm",(void*)f_4793},
{"f_4761:abc_ck_scm",(void*)f_4761},
{"f_4778:abc_ck_scm",(void*)f_4778},
{"f_4782:abc_ck_scm",(void*)f_4782},
{"f_4775:abc_ck_scm",(void*)f_4775},
{"f_4773:abc_ck_scm",(void*)f_4773},
{"f_4769:abc_ck_scm",(void*)f_4769},
{"f_4765:abc_ck_scm",(void*)f_4765},
{"f_4827:abc_ck_scm",(void*)f_4827},
{"f_4871:abc_ck_scm",(void*)f_4871},
{"f_4867:abc_ck_scm",(void*)f_4867},
{"f_4881:abc_ck_scm",(void*)f_4881},
{"f_4895:abc_ck_scm",(void*)f_4895},
{"f_2442:abc_ck_scm",(void*)f_2442},
{"f_2447:abc_ck_scm",(void*)f_2447},
{"f_3248:abc_ck_scm",(void*)f_3248},
{"f_2451:abc_ck_scm",(void*)f_2451},
{"f_3223:abc_ck_scm",(void*)f_3223},
{"f_3233:abc_ck_scm",(void*)f_3233},
{"f_2489:abc_ck_scm",(void*)f_2489},
{"f_2496:abc_ck_scm",(void*)f_2496},
{"f_2512:abc_ck_scm",(void*)f_2512},
{"f_2522:abc_ck_scm",(void*)f_2522},
{"f_2552:abc_ck_scm",(void*)f_2552},
{"f_2606:abc_ck_scm",(void*)f_2606},
{"f_3184:abc_ck_scm",(void*)f_3184},
{"f_2642:abc_ck_scm",(void*)f_2642},
{"f_3170:abc_ck_scm",(void*)f_3170},
{"f_2651:abc_ck_scm",(void*)f_2651},
{"f_3146:abc_ck_scm",(void*)f_3146},
{"f_2676:abc_ck_scm",(void*)f_2676},
{"f_3132:abc_ck_scm",(void*)f_3132},
{"f_2717:abc_ck_scm",(void*)f_2717},
{"f_2768:abc_ck_scm",(void*)f_2768},
{"f_2937:abc_ck_scm",(void*)f_2937},
{"f_3037:abc_ck_scm",(void*)f_3037},
{"f_3040:abc_ck_scm",(void*)f_3040},
{"f_3088:abc_ck_scm",(void*)f_3088},
{"f_3098:abc_ck_scm",(void*)f_3098},
{"f_3074:abc_ck_scm",(void*)f_3074},
{"f_3045:abc_ck_scm",(void*)f_3045},
{"f_3049:abc_ck_scm",(void*)f_3049},
{"f_3065:abc_ck_scm",(void*)f_3065},
{"f_3030:abc_ck_scm",(void*)f_3030},
{"f_2941:abc_ck_scm",(void*)f_2941},
{"f_3005:abc_ck_scm",(void*)f_3005},
{"f_3015:abc_ck_scm",(void*)f_3015},
{"f_2974:abc_ck_scm",(void*)f_2974},
{"f_2995:abc_ck_scm",(void*)f_2995},
{"f_2992:abc_ck_scm",(void*)f_2992},
{"f_2990:abc_ck_scm",(void*)f_2990},
{"f_2945:abc_ck_scm",(void*)f_2945},
{"f_2949:abc_ck_scm",(void*)f_2949},
{"f_2965:abc_ck_scm",(void*)f_2965},
{"f_2771:abc_ck_scm",(void*)f_2771},
{"f_2926:abc_ck_scm",(void*)f_2926},
{"f_2923:abc_ck_scm",(void*)f_2923},
{"f_2907:abc_ck_scm",(void*)f_2907},
{"f_2920:abc_ck_scm",(void*)f_2920},
{"f_2917:abc_ck_scm",(void*)f_2917},
{"f_2915:abc_ck_scm",(void*)f_2915},
{"f_2894:abc_ck_scm",(void*)f_2894},
{"f_2890:abc_ck_scm",(void*)f_2890},
{"f_2888:abc_ck_scm",(void*)f_2888},
{"f_2880:abc_ck_scm",(void*)f_2880},
{"f_2782:abc_ck_scm",(void*)f_2782},
{"f_2860:abc_ck_scm",(void*)f_2860},
{"f_2857:abc_ck_scm",(void*)f_2857},
{"f_2855:abc_ck_scm",(void*)f_2855},
{"f_2838:abc_ck_scm",(void*)f_2838},
{"f_2834:abc_ck_scm",(void*)f_2834},
{"f_2832:abc_ck_scm",(void*)f_2832},
{"f_2824:abc_ck_scm",(void*)f_2824},
{"f_2793:abc_ck_scm",(void*)f_2793},
{"f_2815:abc_ck_scm",(void*)f_2815},
{"f_2811:abc_ck_scm",(void*)f_2811},
{"f_2809:abc_ck_scm",(void*)f_2809},
{"f_2801:abc_ck_scm",(void*)f_2801},
{"f_2720:abc_ck_scm",(void*)f_2720},
{"f_2723:abc_ck_scm",(void*)f_2723},
{"f_2730:abc_ck_scm",(void*)f_2730},
{"f_2679:abc_ck_scm",(void*)f_2679},
{"f_2654:abc_ck_scm",(void*)f_2654},
{"f_2600:abc_ck_scm",(void*)f_2600},
{"f_2556:abc_ck_scm",(void*)f_2556},
{"f_2584:abc_ck_scm",(void*)f_2584},
{"f_2581:abc_ck_scm",(void*)f_2581},
{"f_2579:abc_ck_scm",(void*)f_2579},
{"f_2499:abc_ck_scm",(void*)f_2499},
{"f_2510:abc_ck_scm",(void*)f_2510},
{"f_2470:abc_ck_scm",(void*)f_2470},
{"f_2483:abc_ck_scm",(void*)f_2483},
{"f_2457:abc_ck_scm",(void*)f_2457},
{"f_2302:abc_ck_scm",(void*)f_2302},
{"f_2419:abc_ck_scm",(void*)f_2419},
{"f_2429:abc_ck_scm",(void*)f_2429},
{"f_2305:abc_ck_scm",(void*)f_2305},
{"f_2312:abc_ck_scm",(void*)f_2312},
{"f_2315:abc_ck_scm",(void*)f_2315},
{"f_2407:abc_ck_scm",(void*)f_2407},
{"f_2318:abc_ck_scm",(void*)f_2318},
{"f_2331:abc_ck_scm",(void*)f_2331},
{"f_2382:abc_ck_scm",(void*)f_2382},
{"f_2392:abc_ck_scm",(void*)f_2392},
{"f_2333:abc_ck_scm",(void*)f_2333},
{"f_2366:abc_ck_scm",(void*)f_2366},
{"f_2321:abc_ck_scm",(void*)f_2321},
{"f_2288:abc_ck_scm",(void*)f_2288},
{"f_2272:abc_ck_scm",(void*)f_2272},
{"f_2256:abc_ck_scm",(void*)f_2256},
{"f_2240:abc_ck_scm",(void*)f_2240},
{"f_2197:abc_ck_scm",(void*)f_2197},
{"f_2203:abc_ck_scm",(void*)f_2203},
{"f_2207:abc_ck_scm",(void*)f_2207},
{"f_2210:abc_ck_scm",(void*)f_2210},
{"f_2213:abc_ck_scm",(void*)f_2213},
{"f_2191:abc_ck_scm",(void*)f_2191},
{"f_2185:abc_ck_scm",(void*)f_2185},
{"f_2161:abc_ck_scm",(void*)f_2161},
{"f_2163:abc_ck_scm",(void*)f_2163},
{"f_2153:abc_ck_scm",(void*)f_2153},
{"f_2155:abc_ck_scm",(void*)f_2155},
{"f_2147:abc_ck_scm",(void*)f_2147},
{"f_2141:abc_ck_scm",(void*)f_2141},
{"f_2113:abc_ck_scm",(void*)f_2113},
{"f_2139:abc_ck_scm",(void*)f_2139},
{"f_2135:abc_ck_scm",(void*)f_2135},
{"f_2085:abc_ck_scm",(void*)f_2085},
{"f_2111:abc_ck_scm",(void*)f_2111},
{"f_2107:abc_ck_scm",(void*)f_2107},
{"f_2082:abc_ck_scm",(void*)f_2082},
{"f_2068:abc_ck_scm",(void*)f_2068},
{"f_2080:abc_ck_scm",(void*)f_2080},
{"f_2076:abc_ck_scm",(void*)f_2076},
{"f_2024:abc_ck_scm",(void*)f_2024},
{"f_2031:abc_ck_scm",(void*)f_2031},
{"f_2066:abc_ck_scm",(void*)f_2066},
{"f_2062:abc_ck_scm",(void*)f_2062},
{"f_2046:abc_ck_scm",(void*)f_2046},
{"f_2058:abc_ck_scm",(void*)f_2058},
{"f_2054:abc_ck_scm",(void*)f_2054},
{"f_2006:abc_ck_scm",(void*)f_2006},
{"f_2014:abc_ck_scm",(void*)f_2014},
{"f_2022:abc_ck_scm",(void*)f_2022},
{"f_2018:abc_ck_scm",(void*)f_2018},
{"f_1980:abc_ck_scm",(void*)f_1980},
{"f_1988:abc_ck_scm",(void*)f_1988},
{"f_1996:abc_ck_scm",(void*)f_1996},
{"f_2004:abc_ck_scm",(void*)f_2004},
{"f_2000:abc_ck_scm",(void*)f_2000},
{"f_1992:abc_ck_scm",(void*)f_1992},
{"f_1962:abc_ck_scm",(void*)f_1962},
{"f_1970:abc_ck_scm",(void*)f_1970},
{"f_1978:abc_ck_scm",(void*)f_1978},
{"f_1974:abc_ck_scm",(void*)f_1974},
{"f_1918:abc_ck_scm",(void*)f_1918},
{"f_1925:abc_ck_scm",(void*)f_1925},
{"f_1960:abc_ck_scm",(void*)f_1960},
{"f_1956:abc_ck_scm",(void*)f_1956},
{"f_1940:abc_ck_scm",(void*)f_1940},
{"f_1952:abc_ck_scm",(void*)f_1952},
{"f_1948:abc_ck_scm",(void*)f_1948},
{"f_1874:abc_ck_scm",(void*)f_1874},
{"f_1881:abc_ck_scm",(void*)f_1881},
{"f_1916:abc_ck_scm",(void*)f_1916},
{"f_1912:abc_ck_scm",(void*)f_1912},
{"f_1896:abc_ck_scm",(void*)f_1896},
{"f_1908:abc_ck_scm",(void*)f_1908},
{"f_1904:abc_ck_scm",(void*)f_1904},
{"f_1826:abc_ck_scm",(void*)f_1826},
{"f_1833:abc_ck_scm",(void*)f_1833},
{"f_1872:abc_ck_scm",(void*)f_1872},
{"f_1868:abc_ck_scm",(void*)f_1868},
{"f_1852:abc_ck_scm",(void*)f_1852},
{"f_1864:abc_ck_scm",(void*)f_1864},
{"f_1860:abc_ck_scm",(void*)f_1860},
{"f_1782:abc_ck_scm",(void*)f_1782},
{"f_1789:abc_ck_scm",(void*)f_1789},
{"f_1824:abc_ck_scm",(void*)f_1824},
{"f_1820:abc_ck_scm",(void*)f_1820},
{"f_1804:abc_ck_scm",(void*)f_1804},
{"f_1816:abc_ck_scm",(void*)f_1816},
{"f_1812:abc_ck_scm",(void*)f_1812},
{"f_1768:abc_ck_scm",(void*)f_1768},
{"f_1780:abc_ck_scm",(void*)f_1780},
{"f_1776:abc_ck_scm",(void*)f_1776},
{"f_1750:abc_ck_scm",(void*)f_1750},
{"f_1766:abc_ck_scm",(void*)f_1766},
{"f_1762:abc_ck_scm",(void*)f_1762},
{"f_1736:abc_ck_scm",(void*)f_1736},
{"f_1748:abc_ck_scm",(void*)f_1748},
{"f_1744:abc_ck_scm",(void*)f_1744},
{"f_1718:abc_ck_scm",(void*)f_1718},
{"f_1734:abc_ck_scm",(void*)f_1734},
{"f_1730:abc_ck_scm",(void*)f_1730},
{"f_1684:abc_ck_scm",(void*)f_1684},
{"f_1708:abc_ck_scm",(void*)f_1708},
{"f_1716:abc_ck_scm",(void*)f_1716},
{"f_1712:abc_ck_scm",(void*)f_1712},
{"f_1704:abc_ck_scm",(void*)f_1704},
{"f_1658:abc_ck_scm",(void*)f_1658},
{"f_1682:abc_ck_scm",(void*)f_1682},
{"f_1670:abc_ck_scm",(void*)f_1670},
{"f_1678:abc_ck_scm",(void*)f_1678},
{"f_1674:abc_ck_scm",(void*)f_1674},
{"f_1628:abc_ck_scm",(void*)f_1628},
{"f_1648:abc_ck_scm",(void*)f_1648},
{"f_1656:abc_ck_scm",(void*)f_1656},
{"f_1652:abc_ck_scm",(void*)f_1652},
{"f_1644:abc_ck_scm",(void*)f_1644},
{"f_1598:abc_ck_scm",(void*)f_1598},
{"f_1618:abc_ck_scm",(void*)f_1618},
{"f_1626:abc_ck_scm",(void*)f_1626},
{"f_1622:abc_ck_scm",(void*)f_1622},
{"f_1614:abc_ck_scm",(void*)f_1614},
{"f_1568:abc_ck_scm",(void*)f_1568},
{"f_1588:abc_ck_scm",(void*)f_1588},
{"f_1596:abc_ck_scm",(void*)f_1596},
{"f_1592:abc_ck_scm",(void*)f_1592},
{"f_1584:abc_ck_scm",(void*)f_1584},
{"f_1497:abc_ck_scm",(void*)f_1497},
{"f_1533:abc_ck_scm",(void*)f_1533},
{"f_1562:abc_ck_scm",(void*)f_1562},
{"f_1546:abc_ck_scm",(void*)f_1546},
{"f_1531:abc_ck_scm",(void*)f_1531},
{"f_1519:abc_ck_scm",(void*)f_1519},
{"f_1473:abc_ck_scm",(void*)f_1473},
{"f_1491:abc_ck_scm",(void*)f_1491},
{"f_1477:abc_ck_scm",(void*)f_1477},
{"f_1485:abc_ck_scm",(void*)f_1485},
{"f_1383:abc_ck_scm",(void*)f_1383},
{"f_1460:abc_ck_scm",(void*)f_1460},
{"f_1471:abc_ck_scm",(void*)f_1471},
{"f_1447:abc_ck_scm",(void*)f_1447},
{"f_1458:abc_ck_scm",(void*)f_1458},
{"f_1445:abc_ck_scm",(void*)f_1445},
{"f_1438:abc_ck_scm",(void*)f_1438},
{"f_1434:abc_ck_scm",(void*)f_1434},
{"f_1393:abc_ck_scm",(void*)f_1393},
{"f_1430:abc_ck_scm",(void*)f_1430},
{"f_1426:abc_ck_scm",(void*)f_1426},
{"f_1406:abc_ck_scm",(void*)f_1406},
{"f_1422:abc_ck_scm",(void*)f_1422},
{"f_1418:abc_ck_scm",(void*)f_1418},
{"f_1410:abc_ck_scm",(void*)f_1410},
{"f_1250:abc_ck_scm",(void*)f_1250},
{"f_1252:abc_ck_scm",(void*)f_1252},
{"f_1357:abc_ck_scm",(void*)f_1357},
{"f_1368:abc_ck_scm",(void*)f_1368},
{"f_1375:abc_ck_scm",(void*)f_1375},
{"f_1337:abc_ck_scm",(void*)f_1337},
{"f_1348:abc_ck_scm",(void*)f_1348},
{"f_1355:abc_ck_scm",(void*)f_1355},
{"f_1321:abc_ck_scm",(void*)f_1321},
{"f_1332:abc_ck_scm",(void*)f_1332},
{"f_1308:abc_ck_scm",(void*)f_1308},
{"f_1319:abc_ck_scm",(void*)f_1319},
{"f_1286:abc_ck_scm",(void*)f_1286},
{"f_1297:abc_ck_scm",(void*)f_1297},
{"f_1273:abc_ck_scm",(void*)f_1273},
{"f_1284:abc_ck_scm",(void*)f_1284},
{"f_1241:abc_ck_scm",(void*)f_1241},
{"f_1231:abc_ck_scm",(void*)f_1231},
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
