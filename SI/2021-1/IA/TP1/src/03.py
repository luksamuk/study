from random import randint

num_updates = 0
maximum = randint(1, 100)
print("{}".format(maximum))

for i in range(99):
    new_num = randint(1, 100)
    if new_num > maximum:
        maximum = new_num
        num_updates += 1
        print("{} (atualizado)".format(new_num))
    else:
        print("{}".format(new_num))

print("O valor máximo encontrado foi {}".format(maximum))
print("O número máximo de vezes que o maior valor foi "
      "atualizado foi {} vezes."
      .format(num_updates))

