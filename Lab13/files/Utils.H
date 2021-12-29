/* 
 * File:   Utils.H
 * Author: tcolburn
 *
 */

#ifndef UTILS_H
#define	UTILS_H

#include <cstdlib>
#include <string>
using namespace std;

class Utils {
public:
    static string intToString(int i);
    static int generateRandomInt(int min, int max);
};

#endif	/* UTILS_H */

