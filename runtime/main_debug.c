#include <stdio.h>
#include <stdint.h>
extern void *__eval_0(void);
extern void *__eval_1(void);
int64_t fpr_unbox_int(void *v);
int main(void) {
    printf("calling eval_0...\n");
    void *r = __eval_0();
    printf("eval_0 = %lld\n", (long long)fpr_unbox_int(r));
    printf("calling eval_1...\n");
    r = __eval_1();
    printf("eval_1 = %lld\n", (long long)fpr_unbox_int(r));
    return 0;
}
