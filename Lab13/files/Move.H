/* 
 * File:   Move.H
 * Author: tcolburn
 */

#ifndef MOVE_H
#define	MOVE_H

class MoveInfo {
public:
  MoveInfo(int coins, int pile);
  int getCoins();
  int getPile();
private:
  int coins;
  int pile;
};

#endif	/* MOVE_H */

