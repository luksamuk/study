# Exercício 04
def is_bissexto(year):
    if year % 400 == 0:
        return True
    elif year % 100 == 0:
        return False
    elif year % 4 == 0:
        return True
    return False

def num_days(month, year):
    if month == 2:
        return 29 if is_bissexto(year) else 28
    elif month in [1, 3, 5, 7, 8, 10, 12]:
        return 31
    return 30

# Deverá imprimir [31, 28, 29, 29]
test_args = [(1, 2021), (2, 2021), (2, 2020), (2, 1996)]
print(list(map(lambda p: num_days(p[0], p[1]), test_args)))
