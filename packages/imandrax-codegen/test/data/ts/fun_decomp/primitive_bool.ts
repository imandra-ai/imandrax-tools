const tests = {
  "test_1": {
    input: { a: false, b: false },
    expected: 0
  },
  "test_2": {
    input: { a: false, b: true },
    expected: 2
  },
  "test_3": {
    input: { a: true, b: false },
    expected: 2
  },
  "test_4": {
    input: { a: true, b: true },
    expected: 1
  }
};