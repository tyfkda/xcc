#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"  // for sbrk

// Memory allocator by Kernighan and Ritchie,
// The C programming Language, 2nd ed.  Section 8.7.

typedef long Align;

union header {
  struct {
    union header *ptr;
    unsigned int size;
  } s;
  Align x;
};

typedef union header Header;

static Header base = {.s={.ptr=&base, .size=0}};
static Header *freep = &base;

void
free(void *ap)
{
  Header *bp, *p;

  if (ap == 0)
    return;

  bp = (Header*)ap - 1;
  for(p = freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
    if(p >= p->s.ptr && (bp > p || bp < p->s.ptr))
      break;
  if(bp + bp->s.size == p->s.ptr){
    bp->s.size += p->s.ptr->s.size;
    bp->s.ptr = p->s.ptr->s.ptr;
  } else
    bp->s.ptr = p->s.ptr;
  if(p + p->s.size == bp){
    p->s.size += bp->s.size;
    p->s.ptr = bp->s.ptr;
  } else
    p->s.ptr = bp;
  freep = p;
}

static char work[0x1000000];
static char *_pp = work;

static Header*
morecore(size_t nu)
{
  char *p;
  Header *hp;

#if 0
  if(nu < 4096)
    nu = 4096;
  p = sbrk(nu * sizeof(Header));
  if(p == (char*)-1)
    return 0;
#else
  if(nu < 128)
    nu = 128;
  size_t size = nu * sizeof(Header);
  if (_pp + size >= work + sizeof(work)) {
    fprintf(stderr, "Out of memory: nu=%ld\n", nu);
    return 0;
  }
  size_t n = (size + 15) & -16;
  p = _pp;
  _pp += n;
#endif
  hp = (Header*)p;
  hp->s.size = nu;
  free((void*)(hp + 1));
  return freep;
}

void*
malloc(size_t nbytes)
{
  Header *p, *prevp;
  size_t nunits;

  nunits = (nbytes + sizeof(Header) - 1)/sizeof(Header) + 1;
  //if(freep == NULL){
  //  base.s.ptr = freep = &base;
  //  base.s.size = 0;
  //}
  prevp = freep;
  for(p = prevp->s.ptr; ; prevp = p, p = p->s.ptr){
    if(p->s.size >= nunits){
      if(p->s.size == nunits)
        prevp->s.ptr = p->s.ptr;
      else {
        p->s.size -= nunits;
        p += p->s.size;
        p->s.size = nunits;
      }
      freep = prevp;
      return (void*)(p + 1);
    }
    if(p == freep)
      if((p = morecore(nunits)) == 0)
        return 0;
  }
}

void*
calloc(size_t nmemb, size_t size)
{
  size_t nbytes = nmemb * size;
  void *adr = malloc(nbytes);
  if (adr != NULL)
    memset(adr, 0, nbytes);
  return adr;
}

void*
realloc(void* p, size_t size)
{
  if (size <= 0) {
    free(p);
    return NULL;
  }

  if (p == NULL)
    return malloc(size);

  void* buf = malloc(size);
  if (buf != NULL) {
    Header* h = (Header*)p - 1;
    size_t s = (h->s.size - 1) * sizeof(Header);
    memcpy(buf, p, size > s ? s : size);
    free(p);
  }
  return buf;
}
