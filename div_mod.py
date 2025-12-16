def div_mod(a : int, b: int) -> tuple[int, int]:
    div = 0
    sign = int(a / abs(a))
    while (a >= b or a < 0):
        a -= b * sign
        div += sign
    return (div, a)

print(div_mod(-5, 100))