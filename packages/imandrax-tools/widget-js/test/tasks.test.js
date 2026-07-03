import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { describe, expect, it } from "vitest";

import { drawTasks } from "../src/task/view";

// Fixtures are the exact `TasksWidget` input (the `task_entries` traitlet list
// the Python side feeds the frontend), generated from real API output by
// scripts/gen_widget_input_fixtures.
function loadFixture(name) {
  // vitest runs with the package dir as cwd.
  const path = resolve(
    process.cwd(),
    `test/fixtures/inputs/${name}.widget_input.json`,
  );
  return JSON.parse(readFileSync(path, "utf8"));
}

const admitRec = loadFixture("tasks.admit_rec.iml"); // 1 task, 2 artifacts
const longProof = loadFixture("tasks.long_proof.iml"); // 7 tasks

// The task-level <summary> (kind/id/count), not an artifact's.
function taskSummary(task) {
  return task.querySelector(":scope > .imdx-task-summary");
}

describe("task", () => {
  const render = (data) => {
    const el = document.createElement("div");
    drawTasks(el, data);
    return el;
  };

  it("renders a collapsible section per task", () => {
    expect(render(longProof).querySelectorAll(".imdx-task-task").length).toBe(7);
  });

  it("shows each task's kind and id", () => {
    const summary = taskSummary(render(admitRec).querySelector(".imdx-task-task"));
    expect(summary.querySelector(".imdx-task-kind").textContent).toBe(
      "TASK_CHECK_PO",
    );
    expect(summary.querySelector(".imdx-task-id").textContent).toBe(
      admitRec[0].id,
    );
  });

  it("summarizes the artifact count", () => {
    const summary = taskSummary(render(admitRec).querySelector(".imdx-task-task"));
    expect(summary.querySelector(".imdx-task-meta").textContent).toBe(
      "2 artifacts",
    );
  });

  it("renders each artifact as a labeled <pre> carrying its text verbatim", () => {
    const el = render(admitRec);
    const arts = el.querySelectorAll(".imdx-task-art");
    expect(arts.length).toBe(2);
    // artifact kinds and text come straight from the fixture.
    const kinds = [...arts].map(
      (a) => a.querySelector(".imdx-task-art-kind").textContent,
    );
    expect(kinds).toEqual(admitRec[0].artifacts.map((a) => a.kind));
    expect(el.querySelector(".imdx-task-pre").textContent).toBe(
      admitRec[0].artifacts[0].text,
    );
  });

  it("tolerates an empty task list", () => {
    const el = render([]);
    expect(el.querySelectorAll(".imdx-task-task").length).toBe(0);
    expect(el.querySelector(".imdx-task-placeholder").textContent).toBe(
      "No tasks.",
    );
  });
});
