#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Hackin' some macros
#define result(opt1, opt2) \
    ((opt1 == opt2)	   \
    ? "tie"					  \
    : ((opt1 > opt2) || (opt2 == 2 && opt1 == 0)) \
     ? "victory" : "loss")

#define move_name(move) \
    ((move == 0) ? "rock" : ((move == 1) ? "paper" : "scissors"))


int
main(void)
{
    // PS. never trust this shit with your random
    // numbers on security systems. Ever.
    srand(time(NULL));

    // We do some fine arithmetic to check for our winner.
    // 0: rock, 1: paper, 2: scissors.
    unsigned int option;
    printf("rock (0), paper (1), scissors (2)? > ");
    scanf("%u", &option);
    // Correct input in case user messes up
    option = (option > 2) ? 2 : option;
    unsigned int com = rand() % 3;
    printf("You chose %s\n"
	   "Com chose %s\n"
	   "%s\n",
	   move_name(option),
	   move_name(com),
	   result(option, com));
    return 0;
}
