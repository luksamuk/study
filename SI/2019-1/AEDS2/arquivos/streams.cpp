#include <cstdio>
#include <fstream>
#include <string>

int
main(void)
{
    std::fstream fs;

    // As output
    fs.open("file.txt", std::fstream::out);
    if(fs.is_open()) {
        fs << "Hello, world!" << std::endl;
    fs.close();
    }

    // As input
    fs.open("file.txt", std::fstream::in);
    if(fs.is_open()) {
        std::string s;
        std::getline(fs, s);
        fs.close();
        printf("First phrase: %s\n", s.c_str());
    }


    /* Using pure C */

    // As output
    FILE* fp;
    fp = fopen("file.txt", "a");
    if(fp) {
        fprintf(fp, "Hello again!\n");
        fclose(fp);
    }

    fp = fopen("file.txt", "r");
    if(fp) {
        char buffer[80];
        fgets(buffer, 80, fp);
        fgets(buffer, 80, fp);
        printf("Second phrase: %s\n", buffer);
    }
    
    return 0;
}
