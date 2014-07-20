/* cilly -c --domakeCFG <thisfile>
 * duplicate labels: while_1_break and while_0_break */

void foo () {
    while (1) {
        while (1) {
        }
        goto while_1_break;
    }
while_1_break: /* CIL Label */
    ;
    goto while_0_break;
while_0_break:
    ;
}

