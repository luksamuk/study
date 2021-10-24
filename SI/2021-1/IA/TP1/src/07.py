def sieve_of_erathostenes(limit):
    is_prime = [True for i in range(limit + 1)]
    is_prime[0] = False
    is_prime[1] = False
    for p in range(2, limit):
        if is_prime[p]:
            for multiple in range(p ** 2, limit + 1, p):
                is_prime[multiple] = False
    # Geração da lista
    primes = []
    for i in range(limit + 1):
        if is_prime[i]:
            primes.append(i)
    return primes

while True:
    try:
        limit = int(input("Insira um limite: "))
        if limit < 2:
            print("Insira um número maior ou igual a 2.")
            continue
        break
    except ValueError:
        print("Valor inválido.")

print(sieve_of_erathostenes(limit))

