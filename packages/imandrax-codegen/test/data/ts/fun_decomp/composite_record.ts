const tests = {
  "test_1": {
    input: { p: { x: 0, y: 1 } },
    expected: "positive"
  },
  "test_2": {
    input: { p: { x: -38, y: 38 } },
    expected: "origin"
  },
  "test_3": {
    input: { p: { x: 0, y: -1 } },
    expected: "negative"
  }
};