def parse_point(line):
    return tuple([int(c.split('=')[1]) for c in line.split(',')])


def unique_pairs(moons):
    pairs = []
    for i, _ in enumerate(moons):
        for j in range(i + 1, len(moons)):
            pairs.append((i, j))
    return pairs


def update_axis(a, b):
    if a < b:
        return 1, -1
    elif a > b:
        return -1, 1
    else:
        return 0, 0


def apply_gravity(moons, vels):
    for i1, i2 in unique_pairs(vels):
        p1, p2 = moons[i1], moons[i2]
        v1, v2 = vels[i1], vels[i2]
        new_v1, new_v2 = [0] * 3, [0] * 3
        for axis in range(3):
            update1, update2 = update_axis(p1[axis], p2[axis])
            new_v1[axis], new_v2[axis] = v1[axis] + update1, v2[axis] + update2
        vels[i1] = tuple(new_v1)
        vels[i2] = tuple(new_v2)


def step(moons, vels):
    while True:
        apply_gravity(moons, vels)
        for i, v in enumerate(vels):
            moons[i] = tuple([moons[i][axis] + v[axis] for axis in range(3)])
        yield tuple(moons), tuple(vels)


def total_energy(moons, vels):
    potential_energy = [sum((abs(x), abs(y), abs(z))) for x, y, z in moons]
    kinetic = [sum((abs(x), abs(y), abs(z))) for x, y, z in vels]
    total = sum([a * b for a, b in zip(potential_energy, kinetic)])
    return total


def part1(moons, vels, amount):
    for r, _ in zip(step(moons, [(0, 0, 0)] * len(moons)), range(amount)):
        moons, vels = r[0], r[1]
    return total_energy(moons, vels)


def part2(moons):
    repeat = [None] * 3
    seen = [set()] * 3
    step_iter = step(moons, [(0, 0, 0)] * len(moons))
    step_count = 0
    while any([x is None for x in repeat]):
        moons_step, vels_step = next(step_iter)
        for axis in range(3):
            m_axis = [p[axis] for p in moons_step]
            v_axis = [v[axis] for v in vels_step]
            key = tuple(m_axis + v_axis)
            if key in seen[axis]:
                if repeat[axis] is None:
                    repeat[axis] = step_count
            else:
                seen[axis].add(key)
        step_count += 1
    return repeat


def gcd(a, b):
    while b > 0:
        a, b = b, a % b
    return a


def lcm(a, b):
    return a * b / gcd(a, b)


with open('./resources/day12/input') as f:
    coords = []
    while line := f.readline().lstrip('<').rstrip('\n>'):
        coords.append(line)

moons = [parse_point(line) for line in coords]
print(part1(moons, [(0, 0, 0)] * len(moons), 1000))  # part 1

moons = [parse_point(line) for line in coords]  # cries in mutation
result = part2(moons)
print(lcm(lcm(result[0], result[1]), result[2]))  # part 2
