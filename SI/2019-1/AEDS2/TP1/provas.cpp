#include <cstdio>
#include <map>

float count_answers(char* gabarito, char* answ)
{
    //printf("Gab: %s\nAns: %s\n\n", gabarito, answ);
    float count = 0.0f;
    for(int i = 0; i < 10; i++)
        if(gabarito[i] == answ[i])
            count += 1.0f;
    return count;
}

int main(void)
{
    char gabarito[11];
    scanf("%s\n", gabarito);

    std::map<float, int> nota_freq;

    int   n_alunos = 0;
    float approv = 0.0f;

    while(true) {
        int num_aluno;
        char respostas[11];
        scanf("%d", &num_aluno);
        if(num_aluno == 9999) break;
        scanf("%s", respostas);
        float right_answers = count_answers(gabarito, respostas);
        printf("%d %0.1f\n", num_aluno, right_answers);

        // Contagem de notas repetidas
        nota_freq[right_answers]++;
        // Contagem de n�mero de aprovados
        n_alunos++;
        if(right_answers >= 6.0f) approv++;
    }


    if(n_alunos != 0) {
        approv /= n_alunos;
        printf("%0.1f%%\n", approv * 100.0f);

        std::map<float, int>::iterator iter = nota_freq.begin();
        float maior_nota = iter->first;
        int maior_freq = iter->second;
        for(; iter != nota_freq.end(); iter++) {
            if(iter->second > maior_freq) {
                maior_nota = iter->first;
                maior_freq = iter->second;
            }
        }
        printf("%0.1f\n", maior_nota);
    }

    return 0;
}
