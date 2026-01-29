const tests = {
  "test_1": {
    input: { u: { id: 0, active: { tag: "Inactive", payload: null } } },
    expected: -1
  },
  "test_2": {
    input: { u: { id: 0, active: { tag: "Active", payload: null } } },
    expected: 0
  },
  "test_3": {
    input: { u: { id: 1, active: { tag: "Active", payload: null } } },
    expected: 1
  }
};