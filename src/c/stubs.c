#include <glpk.h>

/* Mostly stubs for reading macros. */

int glp_get_dir_max() {
    return GLP_MAX;
}

int glp_get_dir_min() {
    return GLP_MIN;
}

int glp_get_glp_up() {
    return GLP_UP;
}

int glp_get_glp_lo() {
    return GLP_LO;
}
