const tests = {
  "test_1": {
    input: { opt: { tag: "Some", payload: 0 } },
    expected: 0
  },
  "test_2": {
    input: { opt: { tag: "Some", payload: 1 } },
    expected: 1
  },
  "test_3": {
    input: { opt: null },
    expected: 0
  }
};