from __future__ import annotations
from typing import Iterator
from itertools import product, zip_longest
from queue import PriorityQueue
from textwrap import dedent


OUTPUT = dedent(
    """\
    #############
    #...........#
    ###A#B#C#D###
      #A#B#C#D#
      #########
    """
)

ADVANCED_OUTPUT = dedent(
    """\
    #############
    #...........#
    ###A#B#C#D###
      #A#B#C#D#
      #A#B#C#D#
      #A#B#C#D#
      #########
    """
)


class State:
    COST = {"A": 1, "B": 10, "C": 100, "D": 1000}
    VALID_HALLWAY_IDXS = [0, 1, 3, 5, 7, 9, 10]
    ROOMS_TOP_IDX = {"A": 11, "B": 12, "C": 13, "D": 14}
    ENTRANCES = {k: v for k, v in zip("ABCD", range(2, 8 + 1, 2))}
    AH = {
        0: 3,
        1: 2,
        2: 1,
        3: 2,
        4: 3,
        5: 4,
        6: 5,
        7: 6,
        8: 7,
        9: 8,
        10: 9,
        11: 0,
        12: 4,
        13: 6,
        14: 8,
        15: 0,
        16: 5,
        17: 7,
        18: 9,
        19: 0,
        20: 6,
        21: 8,
        22: 10,
        23: 0,
        24: 7,
        25: 9,
        26: 11,
    }
    BH = {
        0: 5,
        1: 4,
        2: 3,
        3: 2,
        4: 1,
        5: 2,
        6: 3,
        7: 4,
        8: 5,
        9: 6,
        10: 7,
        11: 4,
        12: 0,
        13: 4,
        14: 6,
        15: 5,
        16: 0,
        17: 5,
        18: 7,
        19: 6,
        20: 0,
        21: 6,
        22: 8,
        23: 7,
        24: 0,
        25: 7,
        26: 9,
    }
    CH = {
        0: 7,
        1: 6,
        2: 5,
        3: 4,
        4: 3,
        5: 2,
        6: 1,
        7: 2,
        8: 3,
        9: 4,
        10: 5,
        11: 6,
        12: 4,
        13: 0,
        14: 4,
        15: 7,
        16: 5,
        17: 0,
        18: 5,
        19: 8,
        20: 6,
        21: 0,
        22: 6,
        23: 9,
        24: 7,
        25: 0,
        26: 7,
    }
    DH = {
        0: 9,
        1: 8,
        2: 7,
        3: 6,
        4: 5,
        5: 4,
        6: 3,
        7: 2,
        8: 1,
        9: 2,
        10: 3,
        11: 8,
        12: 6,
        13: 4,
        14: 0,
        15: 9,
        16: 7,
        17: 5,
        18: 0,
        19: 10,
        20: 8,
        21: 6,
        22: 0,
        23: 11,
        24: 9,
        25: 7,
        26: 0,
    }
    HEURISTIC = {"A": AH, "B": BH, "C": CH, "D": DH}

    def __init__(self, state: str):
        self.state = state

    def next(self) -> Iterator[tuple[int, State]]:
        iterables = ((self.pop, self.push), self.VALID_HALLWAY_IDXS, self.ROOMS_TOP_IDX)
        for p, hallway_idx, room_idx in product(*iterables):
            try:
                next_state = p(room_idx, hallway_idx)
            except:
                continue
            yield next_state

    def distance(self, from_idx: int, to_idx: int) -> int:
        hallway_idx = min(from_idx, to_idx)
        room_idx = max(from_idx, to_idx)
        assert hallway_idx <= 10
        assert room_idx >= 11
        room_entry_idx = (((room_idx - 11) % 4) + 1) * 2
        depth: int = ((room_idx - 11) // 4) + 1
        return abs(hallway_idx - room_entry_idx) + depth

    def _validate_column_and_hallway_idx(self, column: str, idx: int):
        if not idx in self.VALID_HALLWAY_IDXS:
            raise ValueError(f"Index {idx} is not a valid position in the hallway")
        col_idx = self.ENTRANCES[column]
        for i in range(min(col_idx, idx) + 1, max(col_idx, idx)):
            if self[i] != ".":
                raise ValueError(f"Way is blocked by '{self[i]}' in position {i}")

    def _move(self, from_idx: int, to_idx) -> tuple[int, State]:
        if self[to_idx] != ".":
            raise ValueError(f"Position {to_idx} is already occupied")
        amphipod = self[from_idx]
        cost = self.distance(from_idx, to_idx) * self.COST[amphipod]
        new_state = list(self.state)
        new_state[from_idx], new_state[to_idx] = new_state[to_idx], new_state[from_idx]
        return cost, State("".join(new_state))

    def pop(self, room_idx: str, hallway_idx: int) -> tuple[int, State]:
        self._validate_column_and_hallway_idx(room_idx, hallway_idx)
        for i in range(self.ROOMS_TOP_IDX[room_idx], len(self.state), 4):
            if self[i] != ".":
                return self._move(i, hallway_idx)
        else:
            raise ValueError(f"Room '{room_idx}' is empty")

    def push(self, room_idx: str, hallway_idx: int) -> tuple[int, State]:
        self._validate_column_and_hallway_idx(room_idx, hallway_idx)
        room_top_idx = self.ROOMS_TOP_IDX[room_idx]
        if self[room_top_idx] != ".":
            raise ValueError(f"Room '{room_idx}' is already full")
        if room_idx != self[hallway_idx]:
            raise ValueError(f"Can't push 'self[hallway_idx]' into room '{room_idx}'")
        if not set(self[room_top_idx::4]).issubset(set(f".{room_idx}")):
            raise ValueError(f"Can't push 'self[hallway_idx]' into room '{room_idx}'")
        previous_idx = room_top_idx
        for i in range(room_top_idx, len(self.state), 4):
            if self[i] != ".":
                break
            previous_idx = i
        return self._move(hallway_idx, previous_idx)

    def letters(self) -> Iterator[tuple[int, str]]:
        for i, letter in enumerate(self.state):
            if letter in self.COST:
                yield i, letter

    def heuristic(self) -> int:
        h = 0
        for i, letter in self.letters():
            h += self.HEURISTIC[letter][i] * self.COST[letter]
        return h

    def __getitem__(self, idx):
        return self.state[idx]

    def __repr__(self):
        return f"{self.__class__.__name__}({self.state})"

    def __str__(self) -> str:
        rows = [
            "#############",
            f"#{self.state[:11]}#",
            "###{}#{}#{}#{}###".format(*self.state[11:15]),
        ]
        for items in grouper(self.state[15:], 4):
            rows.append("  #{}#{}#{}#{}#".format(*items))
        rows.append("  #########\n")
        return "\n".join(rows)

    def __hash__(self) -> int:
        return hash(self.state)

    def __lt__(self, other) -> bool:
        return hash(self) < hash(other)

    def __eq__(self, other) -> bool:
        return self.state == other.state

    @classmethod
    def from_map(cls, map: str):
        return cls(map.replace("\n", "").replace(" ", "").replace("#", ""))


def grouper(iterable, n):
    args = [iter(iterable)] * n
    return zip_longest(*args)


def find_path(start: State, end: State):
    explored: set[State] = set()
    pq: PriorityQueue[tuple[int, State]] = PriorityQueue()
    pq.put((0, start))
    previous_state: dict[State, State] = {}
    cost_to_reach: dict[State, int] = {start: 0}
    while not pq.empty():
        priority, state = pq.get()
        if state == end:
            print_moves(end, previous_state, cost_to_reach)
            return cost_to_reach[state]
        if state in explored:
            continue
        explored.add(state)
        for c, s in state.next():
            cost_c = cost_to_reach[state] + c
            priority = cost_c + s.heuristic()
            if s not in cost_to_reach:
                cost_to_reach[s] = cost_c
                pq.put((priority, s))
                previous_state[s] = state
            elif cost_c < cost_to_reach[s]:
                cost_to_reach[s] = cost_c
                pq.put((priority, s))
                previous_state[s] = state


def print_moves(
    end: State, previous: dict[State, State], cost_to_reach: dict[State, int]
):
    moves = [end]
    s = end
    while True:
        try:
            moves.append(s := previous[s])
        except KeyError:
            break
    for s in reversed(moves):
        print(cost_to_reach[s])
        print(s)


def sort_amphipods(start: State, end: State):
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
    print(sort_amphipods(State.from_map(INPUT), State.from_map(OUTPUT)))

    ADVANCED_INPUT = dedent(
        """\
        #############
        #...........#
        ###C#A#B#D###
          #D#C#B#A#
          #D#B#A#C#
          #D#C#A#B#
          #########
        """
    )
    print(
        sort_amphipods(
            State.from_map(ADVANCED_INPUT),
            State.from_map(ADVANCED_OUTPUT),
        )
    )
