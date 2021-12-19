from __future__ import annotations

# from typing import Iterator, NamedTuple, Optional


class Node:
    def __init__(self, left=None, right=None, parent=None, value=None):
        self.parent = parent
        self.left = left
        self.right = right
        self.value = value

    def to_list(self):
        l = []
        for node in (self.left, self.right):
            l.append(node.value if node.is_leaf() else node.to_list())
        return l

    def is_leaf(self):
        return self.value is not None

    def is_pair(self):
        return (self.left.value is not None) and (self.right.value is not None)

    def leaves(self):
        for node in (self.left, self.right):
            if node.is_leaf():
                yield node
            else:
                yield from node.leaves()

    def set(self, **kwargs):
        for arg, value in kwargs.items():
            self.__setattr__(arg, value)

    @property
    def level(self):
        if self.parent is None:
            return 0
        return 1 + self.parent.level

    def leftest_leaf(self):
        if self.is_leaf():
            return self
        return self.left.leftest_leaf()

    def rightest_leaf(self):
        if self.is_leaf():
            return self
        return self.right.rightest_leaf()

    def get_closest_right(self):
        if self.parent is None:
            return None
        if self.parent.right is not self:
            return self.parent.right.leftest_leaf()
        return self.parent.get_closest_right()

    def get_closest_left(self):
        if self.parent is None:
            return None
        if self.parent.left is not self:
            return self.parent.left.rightest_leaf()
        return self.parent.get_closest_left()

    def magnitude(self):
        if self.is_leaf():
            return self.value
        return 3 * self.left.magnitude() + 2 * self.right.magnitude()


class SFN:
    def __init__(self, num):
        self.root = self.treeify(num)
        self._reduce()

    @classmethod
    def treeify(cls, num, parent=None):
        if isinstance(num, int):
            return Node(value=num, parent=parent)
        node = Node(parent=parent)
        node.left = cls.treeify(num[0], node)
        node.right = cls.treeify(num[1], node)
        return node

    def to_list(self):
        return self.root.to_list()

    def __repr__(self):
        return f"SFN({self.to_list()})"

    def __eq__(self, other):
        return self.to_list() == other.to_list()

    def __add__(self, other):
        return SFN([self.root.to_list(), other.root.to_list()])

    # def _reduce(self):
    #     old_list = []
    #     while old_list != (new_list := self.to_list()):
    #         old_list = new_list
    #         for leaf in self.root.leaves():
    #             if leaf.level == 5:
    #                 self.explode(leaf.parent)
    #                 break
    #             if leaf.value >= 10:
    #                 self.split(leaf)
    #                 break
    #         print(new_list)

    def _reduce(self):
        old_list = []
        while old_list != (new_list := self.to_list()):
            old_list = new_list
            for leaf in self.root.leaves():
                if leaf.level == 5:
                    self.explode(leaf.parent)
                    break
            else:
                for leaf in self.root.leaves():
                    if leaf.value >= 10:
                        self.split(leaf)
                        break

    def explode(self, pair):
        assert pair.is_pair()
        if (neighbor := pair.get_closest_left()) is not None:
            neighbor.value += pair.left.value
        if (neighbor := pair.get_closest_right()) is not None:
            neighbor.value += pair.right.value
        pair.set(left=None, right=None, value=0)

    def split(self, node):
        assert node.is_leaf()
        half = node.value // 2
        node.left = Node(parent=node, value=half)
        node.right = Node(parent=node, value=half + (node.value % 2))
        node.value = None

    def __iter__(self):
        return (l.value for l in self.root.leaves())

    def magnitude(self):
        return self.root.magnitude()


def sum_sfn(inp):
    sum_ = SFN(inp[0])
    for num in inp[1:]:
        sum_ = sum_ + SFN(num)
    return sum_


def read_puzzle_input(filename: str) -> list[list[int]]:
    with open(filename) as stream:
        return [eval(line) for line in stream.read().strip().split("\n")]


if __name__ == "__main__":
    inp = read_puzzle_input("input.txt")
    print(sum_sfn(inp).magnitude())
    print(max((SFN(a) + SFN(b)).magnitude() for a in inp for b in inp))
