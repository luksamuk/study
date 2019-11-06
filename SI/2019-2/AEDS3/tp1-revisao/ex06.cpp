#include <cstdio>
#include <fstream>

int
main(int argc, char **argv)
{
    // O nome do arquivo sera recebido
    // via argumentos no console
    if(argc < 2) return 1;

    const char *filename = argv[1];

    std::ifstream file;
    file.open(filename);

    if(!file.is_open())
        return 1; // Erro ao abrir o arquivo

    long char_count = 0;
    while(file.good()) {
        char c = file.get();
        if(c != EOF) char_count++;
    }

    file.close();

    printf("%ld\n", char_count);
    return 0;
}
