#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "fib.h"

void *continuationr_init(void *jumpout) {
continuation* _data = (continuation*)malloc(sizeof(continuation));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _init_continuation;
  _data->u._init._jumpout = jumpout;
  return (void *)_data;
}

void *continuationr_subr1(void *n, void *k) {
continuation* _data = (continuation*)malloc(sizeof(continuation));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_continuation;
  _data->u._subr1._n = n;
  _data->u._subr1._k = k;
  return (void *)_data;
}

void *continuationr_subr2(void *k, void *v₁) {
continuation* _data = (continuation*)malloc(sizeof(continuation));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr2_continuation;
  _data->u._subr2._k = k;
  _data->u._subr2._v₁ = v₁;
  return (void *)_data;
}

int main()
{
fibr__m__cpsr__m__n = (void *)(void *)-1;
pc = &fibr__m__cps;
mount_tram();
printf("%d\n", (int)applyr__m__kr__m__v);}

void applyr__m__k()
{
continuation* _c = (continuation*)cc;
switch (_c->tag) {
case _init_continuation: {
void *jumpout = _c->u._init._jumpout;
_trstr *trstr = (_trstr *)jumpout;
longjmp(*trstr->jmpbuf, 1);
break; }
case _subr2_continuation: {
void *k = _c->u._subr2._k;
void *v₁ = _c->u._subr2._v₁;
cc = (void *)k;
applyr__m__kr__m__v = (void *)(void *)((int)v₁ + (int)applyr__m__kr__m__v);
pc = &applyr__m__k;
break; }
case _subr1_continuation: {
void *n = _c->u._subr1._n;
void *k = _c->u._subr1._k;
cc = (void *)continuationr_subr2(k,applyr__m__kr__m__v);
fibr__m__cpsr__m__n = (void *)(void *)((int)(void *)((int)n - 1) - 1);
pc = &fibr__m__cps;
break; }
}
}

void fibr__m__cps()
{
if((fibr__m__cpsr__m__n == 0)) {
  applyr__m__kr__m__v = (void *)(void *)1;
pc = &applyr__m__k;

} else if(((void *)((int)fibr__m__cpsr__m__n - 1) == 0)) {
applyr__m__kr__m__v = (void *)(void *)1;
pc = &applyr__m__k;
}
 else {
  cc = (void *)continuationr_subr1(fibr__m__cpsr__m__n,cc);
fibr__m__cpsr__m__n = (void *)(void *)((int)fibr__m__cpsr__m__n - 1);
pc = &fibr__m__cps;

}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
cc= (void *)continuationr_init(dismount);
for(;;) {
pc();
}
}
return 0;
}
