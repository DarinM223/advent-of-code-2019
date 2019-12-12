def count_nums(chunk: str, target: int) -> int:
    return len([n for n in chunk if int(n) == target])


def part1(width: int, height: int) -> int:
    chunk_size = width * height
    min_chunk = None
    min_zeroes = 10000
    with open('./resources/day8/input') as f:
        while chunk := f.read(chunk_size).strip():
            if (num_zeroes := count_nums(chunk, 0)) < min_zeroes:
                min_zeroes, min_chunk = num_zeroes, chunk
        return count_nums(min_chunk, 1) * count_nums(min_chunk, 2)


def update_layers(chunk1: str, chunk2: str) -> str:
    result = []
    for c1, c2 in zip(chunk1, chunk2):
        if c1 == '2':
            result.append(c2)
        else:
            result.append(c1)
    return ''.join(result)


def part2(width: int, height: int) -> None:
    chunk_size = width * height
    with open('./resources/day8/input') as f:
        layer = None
        while chunk := f.read(chunk_size).strip():
            if layer is None:
                layer = chunk
            elif len(chunk) == chunk_size:
                layer = update_layers(layer, chunk)

        for i, ch in enumerate(layer):
            if i % width == 0:
                print()
            if ch == '2':
                print(' ', end='')
            elif ch == '1':
                print('#', end='')
            else:
                print('.', end='')


part1(25, 6)
part2(25, 6)
