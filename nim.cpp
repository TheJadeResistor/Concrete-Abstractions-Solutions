
/*
 * An implementation of the game of Nim
 */

#include <iostream>
#include <assert.h>
#include <stdlib.h>

using namespace std;

struct gameState {
    int pile1;
    int pile2;
};

enum player {human, computer};

int sizeOfPile(gameState state, int pile) {
    if ( pile == 1 )
        return state.pile1;
    else if ( pile == 2 )
        return state.pile2;
    else {
        cout << "Bad pile number: " << pile << endl;
        exit(1);
    }
}

gameState removeCoinsFromPile(gameState state, int numCoins, int pile) {
    if ( pile == 1 )
        state.pile1 -= numCoins;
    else  // pile must be 2
        state.pile2 -= numCoins;
    return state;
}

int totalSize(gameState state) {
    return sizeOfPile(state,1) + sizeOfPile(state,2);
}

void displayGameState(gameState state) {
    cout << endl << "    Pile 1: " << sizeOfPile(state,1);
    cout << endl << "    Pile 2: " << sizeOfPile(state,2) << endl;
}

/* *****************************************************************
 * Exercise 3 consists of the functions computerMove, prompt,
 * humanMove, over, announceWinner, and playWithTurns
 */

/*
 * This function takes a game state as an argument, applies the
 * computer's move to it, and returns the resulting state.  The
 * computer's strategy is the simple one that removes one coin from
 * the first non-empty pile.
 */
gameState computerMove(gameState state) {
    if(state.pile1 != 0){
        return removeCoinsFromPile(state, 1, 1);
    }
    else
      return removeCoinsFromPile(state, 1, 2);
}

/*
 * This function issues the prompt string given as its argument to
 * the terminal, reads an integer from the keyboard, and returns the
 * integer.  For this you will need to use the standard input stream
 * cin.  You do not need to worry about bad input.
 */
int prompt(string promptString) {
    int num=0;
    cout<< promptString;
    cin>> num;
    return num;
}

/*
 * This function prompts the user for a pile number and number of coins,
 * then uses them to apply the user's move to the argument game state.
 * The resulting game state is returned.  You do not need to worry about
 * bad input.
 */
gameState humanMove(gameState state) {
    // you provide
    int pile=0, coins=0;
    cout<< "Pile number?";
    cin>> pile;
    cout<< "Num coins?";
    cin>> coins;
    
    return removeCoinsFromPile(state, coins, pile);
}

/*
 * This function takes a game state as an argument and returns whether
 * the game is over (true or false).
 */
bool over(gameState state) {
    // you provide
    if(state.pile1 == 0 && state.pile2 == 0){
        return true;
    }
    else
        return false;
}

/*
 * This function is called when the game is over and the winner is to
 * be announced.  The player argument is the one who was to play next,
 * so that player is the loser.  If the human loses the output should
 * be "You lose. Better luck next time.".  Otherwise the output is
 * "You win. Congratulations."  This function is called for side effect
 * only and returns nothing.
 */
void announceWinner(player p) {
    // you provide
    if(p == human){
        cout<< "You lose. Better luck next time.";
    }
    else
        cout<< "You win. Congratulations.";
}

/*
 * This function displays the current state of the game, and announces
 * the winner if the game is over.  Otherwise it appropriately calls
 * computerMove or humanMove depending on the second argument and repeats
 * itself for the other player.  This function is called for side effect
 * only and returns nothing.
 */
void playWithTurns(gameState state, player p) {
    // you provide
    displayGameState(state);
    if(over(state)){
        announceWinner(p);
    }
    else{
        if(p == human){
            playWithTurns(humanMove(state), computer);
        }
        else{
            playWithTurns(computerMove(state), human);
        }
    }
}

/*
 * Testing Exercise 12.3, the game of Nim.  While testing, you should
 * comment out code that tests functions you have not written.
 */
int main(int argc, char** argv) {

    // *******************************************************************
    // Testing Exercise 3
    gameState state;
    state.pile1 = 5;
    state.pile2 = 8;

    // *******************************************************************
    // Testing computerMove.
    // *******************************************************************
    state = computerMove(state);
    assert( sizeOfPile(state,1) == 4 ); // computer removed one from pile 1
    assert( sizeOfPile(state,2) == 8 ); // pile 2 unchanged

    cout << "Test of computerMove succeeds" << endl;

    // *******************************************************************
    // Testing prompt.
    // *******************************************************************
    cout << endl << "*** Enter 1 for next test. ***" << endl;
    int pile = prompt("Which pile will you remove from?");
    assert( pile == 1 );

    cout << "Test of prompt succeeds" << endl;

    // *******************************************************************
    // Testing humanMove.
    // *******************************************************************
    cout << endl << "*** Remove 5 coins from pile 2 for next test. ***" << endl;
    state = humanMove(state);
    assert( sizeOfPile(state,1) == 4 ); // pile 1 unchanged
    assert( sizeOfPile(state,2) == 3 ); // pile 2 goes from 8 to 3

    cout << "Test of humanMove succeeds" << endl;

    // *******************************************************************
    // Testing over.
    // *******************************************************************
    assert( !over(state) ); // state still has coins
    state = removeCoinsFromPile(state, 4, 1);
    state = removeCoinsFromPile(state, 3, 2);
    assert( over(state) );  // state has no coins

    cout << "Test of over succeeds" << endl;

    // *******************************************************************
    // Testing announceWinner.
    // *******************************************************************
    cout << endl << "The next output should be \"You lose. Better luck next time.\"" << endl;
    announceWinner(human);
    cout << endl << "The next output should be \"You win. Congratulations.\"" << endl;
    announceWinner(computer);

    cout << endl << "Is the output above correct? (yes or no)" << endl;
    string response;
    cin >> response;
    if ( response == "no" )
        assert(false);
    else
        cout << "Test of announceWinner succeeds" << endl;

    // *******************************************************************
    // Testing playWithTurns.
    // *******************************************************************
    state.pile1 = 0;
    state.pile2 = 0;
    cout << endl << "The next output should display the state " << endl <<
            "and announce human as winner." << endl;
    playWithTurns(state, computer);

    state.pile1 = 1;
    cout << endl << "The next output should display the state, " << endl <<
            "show a computer move, and announce computer as winner." << endl;
    playWithTurns(state, computer);

    cout << endl << "Is the output above correct? (yes or no)" << endl;
    cin >> response;
    if ( response == "no" )
        assert(false);
    else
        cout << endl << "OK, now play a full game." << endl;

    state.pile1 = 5;
    state.pile2 = 8;
    playWithTurns(state, human);

}
