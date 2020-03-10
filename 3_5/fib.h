void *fibr__m__cpsr__m__n, *applyr__m__kr__m__v, *cc;

void (*pc)();

struct continuation;
typedef struct continuation continuation;
struct continuation {
  enum {
    _init_continuation,
    _subr1_continuation,
    _subr2_continuation
  } tag;
  union {
    struct { void *_jumpout; } _init;
    struct { void *_n; void *_k; } _subr1;
    struct { void *_k; void *_v₁; } _subr2;
  } u;
};

void *continuationr_init(void *jumpout);
void *continuationr_subr1(void *n, void *k);
void *continuationr_subr2(void *k, void *v₁);

void fibr__m__cps();
void applyr__m__k();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

