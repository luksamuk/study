#include <iostream>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <sstream>

using namespace std;

int main(int argc, char** argv)
{
	unsigned numTests = 2000;
	if(argc > 1) {
		stringstream ss(argv[1]);
		ss >> numTests;
	}
	
	std::cout << "topology: 2 4 1" << endl;
	for(int i = numTests; i >= 0; i--) {
		int n1 = (int)(2.0 * rand() / double(RAND_MAX));
		int n2 = (int)(2.0 * rand() / double(RAND_MAX));
		int t = n1 ^ n2; // Should be 0 or 1
		std::cout << "in:  " << n1 << ".0 " << n2 << ".0"
				  << std::endl;
		std::cout << "out: " << t << ".0" << std::endl;
	}
}
