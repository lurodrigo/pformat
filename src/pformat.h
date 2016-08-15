
/*
    This is largely a rewriting of cpython/Objects/stringlib/unicode_format.h,
    which implements Python's str.format()
*/

/*
   A SubString consists of the characters between two string or
   unicode pointers.
*/

typedef enum {
    ANS_INIT,
    ANS_AUTO,
    ANS_MANUAL
} AutoNumberState;   /* Keep track if we're auto-numbering fields */

/* Keeps track of our auto-numbering state, and which number field we're on */
typedef struct {
    AutoNumberState an_state;
    int an_field_number;
} AutoNumber;

