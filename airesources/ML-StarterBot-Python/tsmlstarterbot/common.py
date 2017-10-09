import math

PLANET_MAX_NUM = 28
PER_PLANET_FEATURES = 12


def max_number_of_rounds(width, height):
    return 100 + int(math.sqrt(width * height))


def distance2(x1, y1, x2, y2):
    return (x1-x2) ** 2 + (y1-y2) ** 2


def distance(x1, y1, x2, y2):
    return math.sqrt(distance2(x1, y1, x2, y2))
