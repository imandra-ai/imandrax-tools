"""Decomposition tree schema as per the classic IDF schema"""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, ConfigDict

_SCHEMA = """
{
    "$schema": "https://json-schema.org/draft/2020-12/schema",
    "type": "object",
    "properties": {
        "metadata": {
            "type": "object",
            "properties": {
                "internals": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {
                                "type": "string"
                            },
                            "assignableFields": {
                                "type": "array",
                                "items": {
                                    "type": "string"
                                }
                            },
                            "internalFields": {
                                "type": "array",
                                "items": {
                                    "type": "string"
                                }
                            }
                        },
                        "required": [
                            "name",
                            "assignableFields",
                            "internalFields"
                        ],
                        "additionalProperties": true
                    }
                },
                "actions": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {
                                "type": "string"
                            },
                            "fields": {
                                "type": "array",
                                "items": {
                                    "type": "string"
                                }
                            }
                        },
                        "required": [
                            "name",
                            "fields"
                        ],
                        "additionalProperties": true
                    }
                },
                "messages": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "additionalProperties": true
                    }
                }
            },
            "required": [
                "internals",
                "actions",
                "messages"
            ],
            "additionalProperties": false
        },
        "scenarios": {
            "type": "object",
            "additionalProperties": {
                "type": "object",
                "properties": {
                    "id": {
                        "type": "string"
                    },
                    "name": {
                        "type": "string"
                    },
                    "events": {
                        "type": "array",
                        "items": {
                            "type": "string"
                        }
                    },
                    "description": {
                        "type": [
                            "string",
                            "null"
                        ]
                    }
                },
                "required": [
                    "id",
                    "name",
                    "events",
                    "description"
                ],
                "additionalProperties": false
            }
        },
        "region_tree": {
            "type": "object",
            "properties": {
                "id": {
                    "type": "integer"
                },
                "index": {
                    "type": "integer"
                },
                "initial_state": {
                    "type": "object",
                    "properties": {
                        "incoming_action": {},
                        "incoming_msg": {},
                        "outgoing_msgs": {
                            "type": "array"
                        },
                        "event_counter": {
                            "type": "string"
                        },
                        "event_info": {
                            "type": "array",
                            "items": {}
                        }
                    },
                    "required": [
                        "incoming_action",
                        "incoming_msg",
                        "outgoing_msgs",
                        "event_counter",
                        "event_info"
                    ],
                    "additionalProperties": true
                },
                "invariant": {
                    "type": "object",
                    "additionalProperties": true
                },
                "invariant_internals_pp": {
                    "type": "object",
                    "additionalProperties": true
                },
                "children": {
                    "type": "array",
                    "items": {
                        "$ref": "#/$defs/regionNode"
                    }
                }
            },
            "required": [
                "id",
                "index",
                "initial_state",
                "invariant",
                "invariant_internals_pp",
                "children"
            ],
            "additionalProperties": false
        },
        "constraints_index_map": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "event_index": {
                        "type": "object",
                        "properties": {
                            "action": {
                                "type": "string"
                            },
                            "index": {
                                "type": "integer"
                            }
                        },
                        "required": [
                            "action",
                            "index"
                        ],
                        "additionalProperties": false
                    },
                    "constraints": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "additionalProperties": true
                        }
                    },
                    "constraints_pp": {
                        "type": "array",
                        "items": {
                            "type": "string"
                        }
                    }
                },
                "required": [
                    "event_index",
                    "constraints",
                    "constraints_pp"
                ],
                "additionalProperties": true
            }
        },
        "message_validations_as_constraints": {
            "type": "array",
            "items": {}
        },
        "auxiliary_funcs": {
            "type": "array",
            "items": {}
        }
    },
    "required": [
        "metadata",
        "scenarios",
        "region_tree"
    ],
    "additionalProperties": false,
    "$defs": {
        "regionNode": {
            "type": "object",
            "properties": {
                "id": {
                    "type": "integer"
                },
                "index": {
                    "type": "integer"
                },
                "scenario_ids": {
                    "type": "array",
                    "items": {
                        "type": "string"
                    }
                },
                "samples": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "event": {
                                "type": "object",
                                "additionalProperties": true
                            },
                            "state_after": {
                                "type": "object",
                                "properties": {
                                    "incoming_action": {},
                                    "incoming_msg": {},
                                    "event_counter": {
                                        "type": "string"
                                    },
                                    "event_info": {
                                        "type": "array",
                                        "items": {}
                                    },
                                    "required": [
                                        "incoming_action",
                                        "incoming_msg",
                                        "event_counter",
                                        "event_info"
                                    ],
                                    "additionalProperties": true
                                }
                            }
                        },
                        "required": [
                            "event",
                            "state_after"
                        ]
                    }
                },
                "constraints": {
                    "type": "array",
                    "items": {}
                },
                "constraints_pp": {
                    "type": "array",
                    "items": {
                        "type": "string"
                    }
                },
                "invariant": {
                    "type": "object",
                    "additionalProperties": true
                },
                "invariant_internals_pp": {
                    "type": "object",
                    "properties": {
                        "state": {
                            "type": "object",
                            "additionalProperties": true
                        }
                    },
                    "additionalProperties": true
                },
                "invariant_outgoing_messages_pp": {
                    "type": "array",
                    "items": {
                        "type": "string"
                    }
                },
                "field_expander_values_pp": {
                    "type": "object",
                    "additionalProperties": true
                },
                "expanded_region_ids": {
                    "type": "array",
                    "items": {}
                },
                "testgen": {
                    "type": "object",
                    "additionalProperties": true
                },
                "event_label": {
                    "type": "string"
                },
                "children": {
                    "type": "array",
                    "items": {
                        "$ref": "#/$defs/regionNode"
                    }
                }
            },
            "required": [
                "id",
                "index",
                "scenario_ids",
                "samples",
                "constraints",
                "constraints_pp",
                "invariant",
                "invariant_internals_pp",
                "invariant_outgoing_messages_pp",
                "field_expander_values_pp",
                "expanded_region_ids",
                "testgen",
                "children",
                "event_label"
            ],
            "additionalProperties": false
        }
    }
}
"""


# Pydantic models for _SCHEMA
# ===========================
#
# Objects whose JSON Schema declares `additionalProperties: true` use
# `extra="allow"`; those with `additionalProperties: false` use `extra="forbid"`.


# metadata
# --------


class Internal(BaseModel):
    model_config = ConfigDict(extra='allow')

    name: str
    assignableFields: list[str]
    internalFields: list[str]


class Action(BaseModel):
    model_config = ConfigDict(extra='allow')

    name: str
    fields: list[str]


class Metadata(BaseModel):
    model_config = ConfigDict(extra='forbid')

    internals: list[Internal]
    actions: list[Action]
    messages: list[dict[str, Any]]


# scenarios
# ---------


class Scenario(BaseModel):
    model_config = ConfigDict(extra='forbid')

    id: str
    name: str
    events: list[str]
    description: str | None


# region_tree
# -----------


class InitialState(BaseModel):
    model_config = ConfigDict(extra='allow')

    incoming_action: Any
    incoming_msg: Any
    outgoing_msgs: list[Any]
    event_counter: str
    event_info: list[Any]


class SampleState(BaseModel):
    model_config = ConfigDict(extra='allow')

    incoming_action: Any = None
    incoming_msg: Any = None
    event_counter: str
    event_info: list[Any]


class Sample(BaseModel):
    model_config = ConfigDict(extra='allow')

    event: dict[str, Any]
    state_after: SampleState


class InvariantInternalsPp(BaseModel):
    model_config = ConfigDict(extra='allow')

    state: dict[str, Any] | None = None


class RegionNode(BaseModel):
    model_config = ConfigDict(extra='forbid')

    id: int
    index: int
    scenario_ids: list[str]
    samples: list[Sample]
    constraints: list[Any]
    constraints_pp: list[str]
    invariant: dict[str, Any]
    invariant_internals_pp: InvariantInternalsPp
    invariant_outgoing_messages_pp: list[str]
    field_expander_values_pp: dict[str, Any]
    expanded_region_ids: list[Any]
    testgen: dict[str, Any]
    event_label: str
    children: list[RegionNode]


class RegionTree(BaseModel):
    model_config = ConfigDict(extra='forbid')

    id: int
    index: int
    initial_state: InitialState
    invariant: dict[str, Any]
    invariant_internals_pp: dict[str, Any]
    children: list[RegionNode]


# constraints_index_map
# ---------------------


class EventIndex(BaseModel):
    model_config = ConfigDict(extra='forbid')

    action: str
    index: int


class ConstraintsIndexEntry(BaseModel):
    model_config = ConfigDict(extra='allow')

    event_index: EventIndex
    constraints: list[dict[str, Any]]
    constraints_pp: list[str]


# top level
# ---------


class DecompositionTree(BaseModel):
    model_config = ConfigDict(extra='forbid')

    metadata: Metadata
    scenarios: dict[str, Scenario]
    region_tree: RegionTree
    constraints_index_map: list[ConstraintsIndexEntry] | None = None
    message_validations_as_constraints: list[Any] | None = None
    auxiliary_funcs: list[Any] | None = None
