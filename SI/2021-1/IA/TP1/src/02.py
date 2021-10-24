from random import randint
from typing import Final

def gen_random_number():
    return randint(-1, 36)

# Constantes indicando cores das casas (-1 == 00).
GREEN_NUMBERS: Final = [-1, 0]
RED_NUMBERS: Final = [
    1,   3,  5,  7,  9, 12, 14, 16, 18,
    19, 21, 23, 25, 27, 30, 32, 34, 36
]

def get_number_color(number):
    if number in GREEN_NUMBERS:
        return "Verde"
    elif number in RED_NUMBERS:
        return "Vermelho"
    return "Preto"

def get_roulette_range(number):
    if number >= 1 and number <= 18:
        return "1 a 18"
    return "19 a 36"

def format_roulette_number(number):
    return "00" if number == -1 else "{}".format(number)

def get_payment_categories(number):
    categories = []
    # Número único
    categories.append(format_roulette_number(number))
    if number > 0:
        # Vermelho ou preto
        categories.append(get_number_color(number))
        # Ímpar ou par
        categories.append("par" if number % 2 == 0 else "ímpar")
        # Intervalo da roleta
        categories.append(get_roulette_range(number))
    return categories


generated_number = gen_random_number()
print("O resultado da rodada é {}"
      .format(format_roulette_number(generated_number)))

for category in get_payment_categories(generated_number):
    print("Pagar {}".format(category))

