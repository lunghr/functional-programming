# 9 problem
for a in range (333):
    for b in range (1000-a):
        c = 1000 - a - b
        if a**2 + b**2 == c**2 and a < b < c:
            print(a * b * c)
            break

# 21 problem
def find_divisors_sum (n):
    return sum(i for i in range(1, n) if n % i == 0)

summ = 0               
for i in range (10000):
    amicable = find_divisors_sum(i)
    if i != amicable and find_divisors_sum(amicable) == i:
        summ += i

