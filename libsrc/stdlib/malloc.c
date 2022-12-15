#include "_malloc.h"
#include "stdlib.h"
#include "unistd.h"  // for sbrk

// Memory allocator by Kernighan and Ritchie,
// The C programming Language, 2nd ed.  Section 8.7.

#define PAGESIZE  (4096)

static Header base = {.s={.ptr=&base, .size=0}};
static Header *freep = &base;

void free(void *ap) {
  Header *bp, *p;

  if (ap == 0)
    return;

  bp = (Header*)ap - 1;
  for (p = freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
    if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
      break;
  if (bp + bp->s.size == p->s.ptr) {
    bp->s.size += p->s.ptr->s.size;
    bp->s.ptr = p->s.ptr->s.ptr;
  } else
    bp->s.ptr = p->s.ptr;
  if (p + p->s.size == bp) {
    p->s.size += bp->s.size;
    p->s.ptr = bp->s.ptr;
  } else
    p->s.ptr = bp;
  freep = p;
}

static Header *morecore(size_t nu) {
  size_t size;
  char *p;
  Header *hp;

  size = (nu * sizeof(Header) + (PAGESIZE - 1)) & -PAGESIZE;
  p = sbrk(size);
  if (p == (char*)-1)
    return 0;
  hp = (Header*)p;
  hp->s.size = size / sizeof(Header);
  free((void*)(hp + 1));
  return freep;
}

void *malloc(size_t nbytes) {
  Header *p, *prevp;
  size_t nunits;

  nunits = (nbytes + sizeof(Header) - 1)/sizeof(Header) + 1;
  prevp = freep;
  for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
    if (p->s.size >= nunits) {
      if (p->s.size == nunits)
        prevp->s.ptr = p->s.ptr;
      else {
        p->s.size -= nunits;
        p += p->s.size;
        p->s.size = nunits;
      }
      freep = prevp;
      return (void*)(p + 1);
    }
    if (p == freep)
      if ((p = morecore(nunits)) == 0)
        return 0;
  }
}
