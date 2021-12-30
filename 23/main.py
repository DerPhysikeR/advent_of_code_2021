from __future__ import annotations
from typing import Iterator, NamedTuple
from textwrap import dedent
from queue import PriorityQueue
from time import sleep


OUTPUT = dedent(
    """\
    #############
    #...........#
    ###A#B#C#D###
      #A#B#C#D#
      #########
    """
)


class Move(NamedTuple):
    start: int
    end: int


class State:
    MAP_STR = "#############\n#{}#\n###{}#{}#{}#{}###\n  #{}#{}#{}#{}#\n  #########\n"
    NEIGHBORS = {
        0: (1,),
        1: (0, 2),
        2: (1, 3, 11),
        3: (2, 4),
        4: (3, 5, 12),
        5: (4, 6),
        6: (5, 7, 13),
        7: (6, 8),
        8: (7, 9, 14),
        9: (8, 10),
        10: (9,),
        11: (2, 15),
        12: (4, 16),
        13: (6, 17),
        14: (8, 18),
        15: (11,),
        16: (12,),
        17: (13,),
        18: (14,),
    }
    COST = {"A": 1, "B": 10, "C": 100, "D": 1000}

    def __init__(self, state: str, last_moved: int = -1):
        self.state = state
        self.last_moved = last_moved

    def letters(self) -> Iterator[tuple[int, str]]:
        for i, letter in enumerate(self.state):
            if letter in self.COST:
                yield i, letter

    def is_free(self, idx):
        return self.state[idx] == "."

    def moves(self) -> Iterator[tuple[int, int]]:
        for idx, _ in self.letters():
            for neighbor in self.NEIGHBORS[idx]:
                if self.is_free(neighbor):
                    yield idx, neighbor

    def in_hallway(self, idx: int) -> bool:
        return 0 <= idx <= 10

    def free_way(self, start: int, end: int) -> bool:
        if not self.in_hallway(start):
            raise ValueError("Index {start} not in hallway")
        if not self.is_free(end):
            return False
        spot_before_room = {11: 2, 12: 4, 13: 6, 14: 8}[end]
        mi = min(spot_before_room, start)
        ma = max(spot_before_room, start)
        for i in range(mi, ma + 1):
            if i == start:
                continue
            if self[i] != ".":
                return False
        else:
            return True

    def can_move_in_room(self, idx: int):
        amphipod = self[idx]
        if amphipod == "A":
            return self.free_way(idx, 11)
        if amphipod == "B":
            return self.free_way(idx, 12)
        if amphipod == "C":
            return self.free_way(idx, 13)
        if amphipod == "D":
            return self.free_way(idx, 14)
        return False

    def complex_next(self) -> Iterator[tuple[int, State]]:
        for start, end in self.moves():
            cost, state = self.move(start, end)
            amphipod = self[start]

            if self.in_hallway(start) and start != self.last_moved:
                if not self.can_move_in_room(start):
                    continue

            # don't stop directly outside room
            if self[2] != "." and start != 2:
                continue
            if self[4] != "." and start != 4:
                continue
            if self[6] != "." and start != 6:
                continue
            if self[8] != "." and start != 8:
                continue

            # only move in correct room
            if start == 2 and end == 11:
                if (amphipod != "A") or (self[15] not in ".A"):
                    continue
            if start == 4 and end == 12:
                if (amphipod != "B") or (self[16] not in ".B"):
                    continue
            if start == 6 and end == 13:
                if (amphipod != "C") or (self[17] not in ".C"):
                    continue
            if start == 8 and end == 14:
                if (amphipod != "D") or (self[18] not in ".D"):
                    continue

            # don't move out of correct room
            if start == 11 and end == 2:
                if (amphipod == "A") and (self[15] in ".A"):
                    continue
            if start == 12 and end == 4:
                if (amphipod == "B") and (self[16] in ".B"):
                    continue
            if start == 13 and end == 6:
                if (amphipod == "C") and (self[17] in ".C"):
                    continue
            if start == 14 and end == 8:
                if (amphipod == "D") and (self[18] in ".D"):
                    continue

            yield cost, state

    def move(self, start, end) -> tuple[int, State]:
        l: list[str] = list(self.state)
        l[start], l[end] = l[end], l[start]
        return self.COST[self[start]], State("".join(l), end)

    def __getitem__(self, idx):
        return self.state[idx]

    def __repr__(self):
        return f"State({self.state})"

    def __str__(self) -> str:
        return self.MAP_STR.format(self.state[:11], *list(self.state[11:]))

    def __hash__(self) -> int:
        return hash((self.state, self.last_moved))

    def __lt__(self, other) -> bool:
        return hash(self) < hash(other)

    def __eq__(self, other) -> bool:
        return self.state == other.state

    @classmethod
    def from_map(cls, map: str):
        return cls(map.replace("\n", "").replace(" ", "").replace("#", ""))


def find_path(start: State, end: State):
    pq: PriorityQueue[tuple[int, State]] = PriorityQueue()
    explored: set[State] = set([start])
    pq.put((0, start))
    while not pq.empty():
        cost, state = pq.get()
        print(state)
        # sleep(0.5)
        if state == end:
            return cost
        for c, s in state.complex_next():
            if s not in explored:
                explored.add(s)
                pq.put((cost + c, s))


def sort_amphipods(map: str):
    start = State.from_map(map)
    end = State.from_map(OUTPUT)
    return find_path(start, end)


if __name__ == "__main__":
    INPUT = dedent(
        """\
        #############
        #...........#
        ###C#A#B#D###
          #D#C#A#B#
          #########
        """
    )
    print(sort_amphipods(INPUT))
