typedef struct {
  int    maxvar;
  int    nterms;
  long** expvecs;
  long*  coeffs;
} DistrPoly;

// non-empty list of distributed polynomials
struct DPolyList {
  DistrPoly* head;
  int head_aux; // auxilliary information used, e.g., returning factors
  struct DPolyList* tail;
};

typedef struct DPolyList Litem;


void
wrap_print(int, int, long**, long*);

long**
wrap_new_expvecs(int, int);

void
wrap_free_expvecs(long**, int);

long*
wrap_new_coeffs(int);

void
wrap_free_coeffs(long*);

DistrPoly
wrap_gcd(int, int, long**, long*, int, int, long**, long*);

struct DPolyList*
wrap_gcd_div(int, int, long**, long*, int, int, long**, long*);

DistrPoly
wrap_reduce(int, int, long**, long*, int, int, long**, long*);

int
wrap_reduce_zero(int, int, long**, long*, int, int, long**, long*);

DistrPoly
wrap_div(int, int, long**, long*, int, int, long**, long*);

struct DPolyList*
wrap_factor(int, int, long**, long*);
