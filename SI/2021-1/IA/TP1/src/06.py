# Exercício 04
def is_bissexto(year):
    if year % 400 == 0:
        return True
    elif year % 100 == 0:
        return False
    elif year % 4 == 0:
        return True
    return False

# Exercício 05
def num_days(month, year):
    if month == 2:
        return 29 if is_bissexto(year) else 28
    elif month in [1, 3, 5, 7, 8, 10, 12]:
        return 31
    return 30

def is_magic_date(day, month, year):
    year_two_digits = year - (int(year / 100) * 100)
    return (day * month) == year_two_digits

# Encontra todas as datas mágicas do Século XX (1901-2000).
for year in range(1901, 2001):
    for month in range(1, 13):
        for day in range(1, num_days(month, year)+1):
            if is_magic_date(day, month, year):
                print("{:02d}/{:02d}/{:04d}"
                      .format(day, month, year))

