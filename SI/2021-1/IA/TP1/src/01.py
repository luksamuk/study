from typing import Final

# O pão dormido tem 60% de desconto, ou seja,
# tem 40% do valor do pão fresco
BREAD_COST: Final = 4.60
OLD_BREAD_COST: Final = BREAD_COST * 0.4

def print_aligned(prompt, number):
    print('{}{:6.2f}'.format(prompt, number))

# Input do usuário
while True:
    try:
        old_breads = int(input("Insira a quantidade de paes dormidos: "))
        if old_breads < 0:
            print("Quantidade deve ser maior ou igual a zero.")
        else:
            break
    except ValueError:
        print("Quantidade invalida.")

# Exibição das informações
print_aligned('Preco do pao:         ', BREAD_COST)
print_aligned('Preco do pao dormido: ', OLD_BREAD_COST)
print_aligned('Total da compra:      ', old_breads * OLD_BREAD_COST)

