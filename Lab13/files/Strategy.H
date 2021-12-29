/* 
 * File:   Strategy.H
 * Author: tcolburn
 *
 */

#ifndef STRATEGY_H
#define	STRATEGY_H

#include "nim.H"

class StrategyInfo {
public:
    virtual Move getMove(GameState state);
private:
};

#endif	/* STRATEGY_H */

