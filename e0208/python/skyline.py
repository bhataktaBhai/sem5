class BST:
    def __init__(self, xs: list[int]):
        xs = sorted(xs)
        height = (len(xs) - 1).bit_length()
