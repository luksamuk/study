def is_bissexto(year):
    if year % 400 == 0:
        return True
    elif year % 100 == 0:
        return False
    elif year % 4 == 0:
        return True
    return False

# Resultado deve ser [True, False, True, False]
years = [2016, 2018, 2020, 2021]
print(list(map(is_bissexto, years)))
