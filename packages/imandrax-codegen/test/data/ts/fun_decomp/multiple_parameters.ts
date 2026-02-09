const tests = {
  "test_1": {
    input: { b: 1, c: 2, a: 0 },
    expected: 0
  },
  "test_2": {
    input: { a: 0, c: 1, b: 1 },
    expected: 0
  },
  "test_3": {
    input: { b: 0, a: 0, c: 0 },
    expected: 0
  },
  "test_4": {
    input: { b: 0, a: 1, c: 1 },
    expected: 0
  },
  "test_5": {
    input: { a: 0, c: -1, b: -1 },
    expected: 0
  },
  "test_6": {
    input: { a: 1, c: -1, b: 0 },
    expected: 0
  }
};