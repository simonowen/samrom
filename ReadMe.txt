SAM Coupé ROM Source
--------------------

This archive contains source code for the SAM Coupé v3.0 ROM, based on the
original release as ROM0.TXT and ROM1.TXT.  The code has been corrected to
match the official v3.0 ROM image, and split into separate files as detailed
within the source itself.  A new samrom.asm glue module joins them in the
correct order.

Also included are many versions of the ROM binaries, released with kind
permission from the ROM author, Dr Andy Wright.

Thanks to Simon N Goodwin for supplying the files, which include two dumped
from pre-production hardware.  They only work with the early hardware as they
lack the ~55ms delay needed to wait for the ASIC to become ready.

Note: early ROMs are very buggy, and tend to go mad once BASIC starts paging.
ROM10 (version 1.0) requires a manually-entered CALL after F9 or BOOT because
the ROM fails to execute the bootstrap. The CALL address depends RAM size:

On a 256K SAM use:

   CALL 229385

Or on a 512K machine use:

   CALL 491529
