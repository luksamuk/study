#include <cstdio>
#include <cmath>

#define prompt(x, y)				\
    printf(x);					\
    scanf("%f", &y);

int
main()
{
    printf("um\n\tdois\n\t\ttres\n");
    printf("%f\n", 5.0f / 2.0f);

    double raio;
    printf("Digite o raio da esfera:\a ");
    scanf("%lf", &raio);

    printf("A area eh %lf\n", M_PI * raio * raio);

    // Incremento
    int x = 5, y = 5;
    printf("%d %d\n", (x++) + 4, (++y) + 4);

    // Incremento/decremento tem precedência sobre adição
    printf("%d\n", x+++y);

    float nota, media = 0.0;
    prompt("Digite a primeira nota: ", nota);
    media += nota;
    
    prompt("Digite a segunda nota: ", nota);
    media += nota;

    prompt("Digite a terceira nota: ", nota);
    media += nota;

    media /= 3.0;


    printf("A media final eh %f\n", media);
    
    media = 0.0;


    
    for(int i = 0; i < 3; i++) {
	printf("Digite a %s nota: ",
	       [&i]() {
		   switch(i) {
		   case 0: return "primeira";
		   case 1: return "segunda";
		   }
		   return "terceira";
	       }());
	scanf("%f", &nota);
	media += nota;
    }
    media /= 3.0f;


    
    printf("A media final eh %f\n", media);
    
    return 0;
}
