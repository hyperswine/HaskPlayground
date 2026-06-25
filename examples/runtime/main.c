/*
 * Test harness for spike_test.fpr compiled to RISC-V assembly.
 *
 * Expected results:
 *   eval_0  = calc 5        = (5+1)*2-1 = 11
 *   eval_1  = 3 + 4         = 7
 *   eval_2  = safediv 10 3  = 3
 *   eval_3  = safediv 7 0   = 0
 */

#include <stdio.h>
#include <stdint.h>

/* Generated eval functions */
extern void *__eval_0(void);
extern void *__eval_1(void);
extern void *__eval_2(void);
extern void *__eval_3(void);

/* Runtime helper */
int64_t fpr_unbox_int(void *v);

int main(void) {
    void *r;

    r = __eval_0();
    printf("eval_0  (calc 5)        = %lld  (expect 11)\n",
           (long long)fpr_unbox_int(r));

    r = __eval_1();
    printf("eval_1  (3 + 4)         = %lld  (expect 7)\n",
           (long long)fpr_unbox_int(r));

    r = __eval_2();
    printf("eval_2  (safediv 10 3)  = %lld  (expect 3)\n",
           (long long)fpr_unbox_int(r));

    r = __eval_3();
    printf("eval_3  (safediv 7 0)   = %lld  (expect 0)\n",
           (long long)fpr_unbox_int(r));

    return 0;
}
