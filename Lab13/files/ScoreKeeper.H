/* 
 * File:   ScoreKeeper.H
 * Author: Sage Magras
 */

#ifndef SCOREKEEPER_H
#define	SCOREKEEPER_H

#include "nim.H"

class ScoreKeeperInfo {
public:
    void start();
    
private:
 // you provide private instance variables and methods
    void welcome();
    void flipCoin();
    void playRepeatedly();
    void makeRandomState();
    void playAgain();
    
    string nameP="";
    bool playP= NULL;
    int winsP, winsC;
    Player playerP, playerC;
    Game game;
    GameState initialState;
    string again="";
    Player winner;
};

#endif	/* SCOREKEEPER_H */

