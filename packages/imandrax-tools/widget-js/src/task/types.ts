// Shapes of the task JSON the widget is handed. The Python side (see
// `imandrax_tools.widget`) fetches, decodes, and pretty-prints each task's
// artifacts into these; we only render.

// An artifact, rendered as pre-formatted text.
export interface Artifact {
  kind: string;
  text: string;
}

// One task: its kind, optional id, and its pretty-printed artifacts.
export interface TaskData {
  id: string;
  kind: string;
  artifacts: Artifact[];
}
