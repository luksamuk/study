# Construção do dictionary
def build_dict():
    points = {}
    for letter in ['a', 'e', 'i', 'l', 'n', 'o', 'r', 's', 't', 'u']:
        points[letter] = 1
    for letter in ['d', 'g']:
        points[letter] = 2
    for letter in ['b', 'c', 'm', 'p']:
        points[letter] = 3
    for letter in ['f', 'h', 'v', 'w', 'y']:
        points[letter] = 4
    points['k'] = 5
    for letter in ['j', 'x']:
        points[letter] = 8
    for letter in ['q', 'z']:
        points[letter] = 10
    return points

points_dict = build_dict()
points = 0
word = input('Insira uma palavra: ').lower()

for letter in word:
    try:
        points += points_dict[letter]
    except KeyError:
        pass

print("Pontuação: {} pontos".format(points))

