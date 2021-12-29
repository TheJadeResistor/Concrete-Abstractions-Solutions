/* 
 * File:   ScoreKeeper.cpp
 * Author: Sage Magras
 * 
 */

#include "nim.H"


void ScoreKeeperInfo::start() {
    // you provide private instance variables and methods
    welcome();
    flipCoin();
    playRepeatedly();
    makeRandomState();
    playAgain();
}


// definitions of private methods go here
void ScoreKeeperInfo::welcome(){
    //welcome
    cout<< "Welcome to the Game of Nim.\n"<<endl;
    cout<<"What is your name?"<<endl;
    cin>> nameP;
    playerP = new PlayerInfo(nameP);
    playerC = new AutomatedPlayerInfo("JARVIS", new IntermediateStrategyInfo);
    winsP=0;
    winsC=0;
}

void ScoreKeeperInfo::flipCoin(){
    //flipCoin
    cout<<"I will flip a coin to start."<<endl;
    int random= rand() % 2;
    string guess= "";
    cout<<"heads or tails?"<<endl;
    cin>>guess;
    while(guess != "heads" && guess != "tails"){
        cout<<"heads or tails?"<<endl;
        cin>>guess;
        if(guess == "heads" || guess == "tails"){
            break;
        }
    }
    
    //bool playP= NULL;
    if(random== 1){
        cout<<"It is heads"<<endl;
        if(guess== "heads")
            playP= true;
        else
            playP= false;
    }
    else{
        cout<<"It is tails"<<endl;
        if(guess== "tails")
            playP= true;
        else
            playP= false;
    }
}

void ScoreKeeperInfo::playRepeatedly(){
    //playRepeatedly
    makeRandomState();
    if(playP){
        cout<<"You will go first"<<endl;
        game = new GameInfo(playerP, playerC, initialState);
    }
    else{
        cout<<"JARVIS will go first"<<endl;
        game = new GameInfo(playerC, playerP, initialState);
    }
    
    winner = game->play();
    cout <<winner->getName()<< " wins.\n";//NOTE**:my (online) compiler does not display this line for some reason, crashes
    if(winner->getName() == playerP->getName())
        winsP++;
    else
        winsC++;
    
    playAgain();
    if(again != "no"){
        while(again=="yes"){
            makeRandomState();
            if(playP){
                cout<<"JARVIS will go first"<<endl;
                game = new GameInfo(playerC, playerP, initialState);
                playP= false;
            }
            else{
                cout<<"You will go first"<<endl;
                game = new GameInfo(playerP, playerC, initialState);
                playP= true;
            }
            Player winner = game->play();
            cout <<winner->getName()<< " wins.\n";//NOTE**:my (online) compiler does not display this line for some reason
            if(winner->getName() == playerP->getName())
                winsP++;
            else
                winsC++;
            playAgain();
            if(again=="no")
                exit(0);
        }
    }
    else
        exit(0);
    
}

void ScoreKeeperInfo::playAgain(){
    cout<< "Score: \n\t"<< playerP->getName()<< ": "<<winsP<<endl;
    cout<< "\t"<< playerC->getName()<< ": "<<winsC<<endl;
    
    cout<< "Another game (yes or no)?"<<endl;
    cin>> again;
    
    while(again != "yes" && again != "no"){
        cout<<"Another game (yes or no)?"<<endl;
        cin>>again;
        if(again == "yes" || again == "no"){
            break;
        }
    }
}

void ScoreKeeperInfo::makeRandomState(){
    int pile1= Utils::generateRandomInt(6, 12);
    int pile2= Utils::generateRandomInt(6, 12);
    int pile3= Utils::generateRandomInt(6, 12);
    
    initialState = new GameStateInfo(pile1, pile2, pile3);
}
