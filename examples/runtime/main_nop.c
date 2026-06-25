#include <stdint.h>
extern void *__eval_0(void);
int64_t fpr_unbox_int(void *v);
int main(void) {
    /* Don't use printf - just do arithmetic with the result */
    void *r = __eval_0();
    int64_t v = fpr_unbox_int(r);
    /* Return 0 on success (expected 11) */
    return (v == 11) ? 0 : 1;
}
