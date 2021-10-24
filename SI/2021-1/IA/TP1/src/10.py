def string_distance(s, t):
    if len(s) == 0:
        return len(t)
    elif len(t) == 0:
        return len(s)
    cost = 0
    if s[-1] != t[-1]:
        cost = 1
    d1 = string_distance(s[:-1], t) + 1
    d2 = string_distance(s, t[:-1]) + 1
    d3 = string_distance(s[:-1], t[:-1]) + cost
    return min(d1, d2, d3)

str1 = input("Insira um texto:    ").lower()
str2 = input("Insira outro texto: ").lower()
print("Distância de edição: {}".format(string_distance(str1, str2)))

