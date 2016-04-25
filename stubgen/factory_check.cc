#include <factory.h>
#include <assert.h>
#include <algorithm>

int main() {
    setCharacteristic( 0 );
    On( SW_USE_EZGCD );
    // On( SW_RATIONAL );

    // Example for conversion from distributed form to recursive form by evaluation
    CanonicalForm g =
      CanonicalForm("2",10) * power(Variable(1),1) * power(Variable(2),2) * power(Variable(3),3);
    g += CanonicalForm("3",10) * power(Variable(1),3) * power(Variable(2),1) * power(Variable(3),2);
    g += CanonicalForm("4",10) * power(Variable(1),3) * power(Variable(2),7) * power(Variable(3),2);
    cout << "define cf g:  " << g << endl;

    // convert from CanonicalForm to OCaml interface repr.
    DistrPoly dg = cfToDistr(g);
    return 0;
}
