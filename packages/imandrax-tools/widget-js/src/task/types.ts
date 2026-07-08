// Shapes of the task JSON the widget is handed. The Python side (see
// `imandrax_tools.widget`) fetches, decodes, and pretty-prints each task's
// artifacts into these; we only render.
//
// The contract (`TaskEntry` + `ArtifactEntry`) is code-generated from the Python
// pydantic models into ../generated/node.ts (shared by all widgets).

import type { ArtifactEntry, TaskEntry } from '../generated/types';

export type { ArtifactEntry, TaskEntry };

// An artifact, rendered as pre-formatted text.
export type Artifact = ArtifactEntry;
// One task: its kind, id, and its pretty-printed artifacts.
export type TaskData = TaskEntry;
