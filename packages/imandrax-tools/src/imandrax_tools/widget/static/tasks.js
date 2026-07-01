// anywidget view for ImandraX task artifacts.
//
// The Python side syncs a `tasks_data` trait shaped as:
//   [{ id: str, kind: str, artifacts: [{ kind: str, text: str }, ...] }, ...]
//
// All artifact text is inserted via `textContent`, so it is escaped by the
// DOM automatically -- no `<int>`/`<real>` fragments leaking into markup.

function makeArtifact({ kind, text }) {
  const details = document.createElement("details");
  details.className = "imx-artifact";
  details.open = true;

  const summary = document.createElement("summary");
  summary.className = "imx-artifact-summary";

  const label = document.createElement("span");
  label.className = "imx-artifact-kind";
  label.textContent = kind;
  summary.appendChild(label);

  const size = document.createElement("span");
  size.className = "imx-artifact-size";
  size.textContent = `${text.length.toLocaleString()} chars`;
  summary.appendChild(size);

  const copy = document.createElement("button");
  copy.className = "imx-copy";
  copy.type = "button";
  copy.textContent = "copy";
  copy.addEventListener("click", (e) => {
    e.preventDefault();
    e.stopPropagation();
    navigator.clipboard?.writeText(text).then(() => {
      copy.textContent = "copied";
      setTimeout(() => (copy.textContent = "copy"), 1200);
    });
  });
  summary.appendChild(copy);

  details.appendChild(summary);

  // Scroll container keeps large dumps from stretching the page.
  const scroll = document.createElement("div");
  scroll.className = "imx-scroll";
  const pre = document.createElement("pre");
  pre.className = "imx-pre";
  pre.textContent = text; // escaped by the DOM
  scroll.appendChild(pre);
  details.appendChild(scroll);

  return details;
}

function makeTask(task) {
  const details = document.createElement("details");
  details.className = "imx-task";
  details.open = true;

  const summary = document.createElement("summary");
  summary.className = "imx-task-summary";

  const kind = document.createElement("span");
  kind.className = "imx-task-kind";
  kind.textContent = task.kind;
  summary.appendChild(kind);

  if (task.id) {
    const id = document.createElement("span");
    id.className = "imx-task-id";
    id.textContent = task.id;
    summary.appendChild(id);
  }

  const count = document.createElement("span");
  count.className = "imx-task-count";
  const n = task.artifacts.length;
  count.textContent = `${n} artifact${n === 1 ? "" : "s"}`;
  summary.appendChild(count);

  details.appendChild(summary);

  const body = document.createElement("div");
  body.className = "imx-task-body";
  for (const art of task.artifacts) {
    body.appendChild(makeArtifact(art));
  }
  details.appendChild(body);

  return details;
}

function render({ model, el }) {
  const root = document.createElement("div");
  root.className = "imx-tasks";
  el.appendChild(root);

  const draw = () => {
    root.replaceChildren();
    const tasks = model.get("tasks_data") || [];
    if (tasks.length === 0) {
      const empty = document.createElement("div");
      empty.className = "imx-empty";
      empty.textContent = "No tasks.";
      root.appendChild(empty);
      return;
    }
    for (const task of tasks) {
      root.appendChild(makeTask(task));
    }
  };

  draw();
  model.on("change:tasks_data", draw);
}

export default { render };
