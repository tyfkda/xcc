#include "stdio.h"

#include "./_file.h"

int fflush(FILE *fp) {
  if (fp != NULL)
    return (fp->flush)(fp);

  // C11 7.21.5.2p2: If stream is a null pointer, the fflush function
  // performs this flushing action on all streams for which the behavior
  // is defined above (i.e. all output streams).
  // Note: stdin is input-only and intentionally not flushed.
  int result = 0;
  // Standard output streams are statically allocated and not tracked in
  // __fileman, so flush them explicitly.
  if ((stdout->flush)(stdout) != 0)
    result = EOF;
  if ((stderr->flush)(stderr) != 0)
    result = EOF;
  _flush_opened_files();
  return result;
}
