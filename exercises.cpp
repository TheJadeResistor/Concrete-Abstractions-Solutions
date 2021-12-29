
#include <math.h>
#include <iostream>
#include <assert.h>
using namespace std;

/*
 * Exercise 1
 */
int numSixes(int n) {
  int counter=0;
  int hold=abs(n);
    for(int i=0; i<=to_string(n).length(); i++){
      if(hold%10 == 6 || hold == 6){
        counter++;
      }
        hold/=10;
    }
    return counter;
}

/*
 * Exercise 2
 */
int square(int x) {
    return x*x;
}

int cube(int x) {
    return x*x*x;
}

int raiseToSelf(int x) {
    return pow(x, x);
}

int sumFunc(int low, int high, int f(int)) {
  int count=0;
    for(int i=low; i<=high; i++){
      count+= f(i);
    }
    return count;
}


/*
 * The main function tests the exercises.  Testing succeeds if
 * no assertion errors occur.
 */
int main(int argc, char** argv) {

    // Testing Exercise 1
    assert( numSixes(0) == 0);
    assert( numSixes(606060606) == 5 );
    assert( numSixes(123457890) == 0 );
    assert( numSixes(6666) == 4 );
    assert( numSixes(-6666) == 4 );

    cout << "Exercise 1 Passed" << endl;

    // Testing Exercise 2
    assert( square(5) == 25 );
    assert( cube(5) == 125 );
    assert( raiseToSelf(5) == 3125 );

    assert( sumFunc(1, 9, square) == 285 );
    assert( sumFunc(1, 9, cube) == 2025 );
    assert( sumFunc(1, 9, raiseToSelf) == 405071317 );

    cout << "Exercise 2 Passed" << endl;

}
