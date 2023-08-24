# test.s
# To compile it, type the following in shell
# as test.s -o test.o
# ld test2.o -o test2 -macosx_version_min 13.0 -L /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib -lSystem

.section __TEXT,__text
.globl _main
_main:
  pushq %rbp
  movq %rsp, %rbp
  movl $3, %edi        # exit(3);
  callq _exit          # calling _exit(). see man _exit