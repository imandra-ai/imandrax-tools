const tests = {
  "test_1": {
    input: { c: { tag: "Labeled", payload: [0, 0.] } },
    expected: 3
  },
  "test_2": {
    input: { c: { tag: "Single", payload: 0 } },
    expected: -1
  },
  "test_3": {
    input: { c: { tag: "Single", payload: 1 } },
    expected: 1
  },
  "test_4": {
    input: { c: { tag: "Pair", payload: [0, 0.] } },
    expected: -2
  },
  "test_5": {
    input: { c: { tag: "Pair", payload: [0, -1.] } },
    expected: 2
  },
  "test_6": {
    input: { c: { tag: "Empty", payload: null } },
    expected: 0
  }
};