"""
decomp requests related.

Pipeline:
1. IML code -> decomp attribute capture
2. decomp attribute capture -> decomp top application capture
3. decomp top application capture -> decomp request
"""

from typing import Any, Required, TypedDict, cast

from tree_sitter import Node, Range, Tree

from iml_query.queries import DECOMP_QUERY_SRC, DecompCapture
from iml_query.tree_sitter_utils import (
    delete_nodes,
    insert_lines,
    mk_query,
    run_query,
    unwrap_bytes,
)

from .utils import find_func_definition


class DecompParsingError(Exception):
    """Exception raised when parsing decomp fails."""

    pass


class DecompTopLabels(TypedDict, total=False):
    assuming: str | None
    basis: list[str] | None
    rule_specs: list[str] | None
    prune: bool | None
    ctx_simp: bool | None
    lift_bool: str | None


class DecompReqArgs(TypedDict, total=False):
    name: Required[str]
    assuming: str | None
    basis: list[str] | None
    rule_specs: list[str] | None
    prune: bool | None
    ctx_simp: bool | None
    lift_bool: str | None


def top_application_to_decomp(node: Node) -> DecompTopLabels:
    """Extract Decomp request from a `Decompose.top` application node."""
    assert node.type == 'application_expression'

    extract_top_arg_query = mk_query(r"""
    (application_expression
        (value_path
            (value_name) @top
            (#eq? @top "top")
        )
        (labeled_argument
            (label_name) @label
        ) @arg
        (unit)
    )
    """)

    matches = run_query(query=extract_top_arg_query, node=node)

    res: dict[str, Any] = {}

    # Process each labeled argument based on its label
    for _, capture in matches:
        label_name_b = capture['label'][0].text
        assert label_name_b, 'Never: no label'
        label_name = label_name_b.decode('utf-8')
        arg_node = capture['arg'][0]

        match label_name:
            case 'assuming':
                # Parse assuming: ~assuming:[%id simple_branch]
                assuming_query = mk_query(r"""
                (extension
                    "[%"
                    (attribute_id) @attr_id
                    (attribute_payload) @payload
                    (#eq? @attr_id "id")
                )
                """)
                assuming_matches = run_query(
                    query=assuming_query, node=arg_node
                )
                if assuming_matches:
                    payload_text = assuming_matches[0][1]['payload'][0].text
                    assert payload_text, 'Never: no assuming payload'
                    res['assuming'] = payload_text.decode('utf-8')

            case 'basis' | 'rule_specs':
                # Query each extension separately to get all identifiers
                extension_query = mk_query(r"""
                (extension
                    "[%"
                    (attribute_id)
                    (attribute_payload
                        (expression_item
                            (value_path
                                (value_name) @id
                            )
                        )
                    )
                )
                """)
                extension_matches = run_query(
                    query=extension_query, node=arg_node
                )
                if extension_matches:
                    ids: list[str] = []
                    for match in extension_matches:
                        id_node = match[1]['id'][0]
                        id_text = id_node.text
                        assert id_text, f'Never: no {label_name} id text'
                        ids.append(id_text.decode('utf-8'))
                    res[label_name] = ids

            case 'prune' | 'ctx_simp':
                # Parse boolean: ~prune:true
                bool_query = mk_query(r"""
                (boolean) @bool_val
                """)
                bool_matches = run_query(query=bool_query, node=arg_node)
                if bool_matches:
                    bool_text = bool_matches[0][1]['bool_val'][0].text
                    assert bool_text, f'Never: no {label_name} boolean text'
                    res[label_name] = bool_text.decode('utf-8') == 'true'

            case 'lift_bool':
                # Parse constructor: ~lift_bool:Default
                constructor_query = mk_query(r"""
                (constructor_path
                    (constructor_name) @constructor
                )
                """)
                constructor_matches = run_query(
                    query=constructor_query, node=arg_node
                )
                if constructor_matches:
                    constructor_text = constructor_matches[0][1]['constructor'][
                        0
                    ].text
                    assert constructor_text, (
                        'Never: no lift_bool constructor text'
                    )
                    lift_bool_value = constructor_text.decode('utf-8')
                    lift_bool_enum = [
                        'Default',
                        'Nested_equalities',
                        'Equalities',
                        'All',
                    ]
                    if lift_bool_value not in lift_bool_enum:
                        raise DecompParsingError(
                            f'Invalid lift_bool value: {lift_bool_value}',
                            f'should be one of {lift_bool_enum}',
                        )
                    res['lift_bool'] = lift_bool_value
            case _:
                assert 'False', 'Never'

    return DecompTopLabels(**res)


def decomp_req_to_top_appl_text(req: DecompTopLabels) -> str:
    """Convert a decomp request to a top application source string."""

    def mk_id(identifier_name: str) -> str:
        return f'[%id {identifier_name}]'

    labels: list[str] = []

    # Handle assuming
    assuming = req.get('assuming')
    if assuming is not None:
        labels.append(f'~assuming:{mk_id(assuming)}')

    # Handle basis
    basis = req.get('basis')
    if basis is not None and len(basis) > 0:
        s = '~basis:'
        items_str = ' ; '.join(map(mk_id, basis))
        s += f'[{items_str}]'
        labels.append(s)

    # Handle rule_specs
    rule_specs = req.get('rule_specs')
    if rule_specs is not None and len(rule_specs) > 0:
        s = '~rule_specs:'
        items_str = ' ; '.join(map(mk_id, rule_specs))
        s += f'[{items_str}]'
        labels.append(s)

    # Handle prune
    prune = req.get('prune')
    if prune is not None and prune:
        labels.append(f'~prune:{"true" if prune else "false"}')

    # Handle ctx_simp
    ctx_simp = req.get('ctx_simp')
    if ctx_simp is not None:
        labels.append(f'~ctx_simp:{"true" if ctx_simp else "false"}')

    # Handle lift_bool
    lift_bool = req.get('lift_bool')
    if lift_bool is not None:
        s = '~lift_bool:'
        s += f'{lift_bool} ()'
        labels.append(s)

    return f'top {" ".join(labels) + " "}()'


def decomp_attribute_payload_to_decomp_req_labels(
    node: Node,
) -> DecompTopLabels:
    """Parse the decomp payload (`Decomp.top` function application) to label dict."""
    assert node.type == 'attribute_payload'

    expect_appl = node.children[0].children[0]
    if expect_appl.type != 'application_expression':
        raise NotImplementedError('Composition operators are not supported yet')

    return top_application_to_decomp(expect_appl)


def decomp_capture_to_req(
    capture: DecompCapture,
) -> tuple[DecompReqArgs, Range]:
    req: dict[str, Any] = {}
    req['name'] = unwrap_bytes(capture.decomposed_func_name.text).decode('utf8')
    req_labels = decomp_attribute_payload_to_decomp_req_labels(
        capture.decomp_payload
    )
    req |= req_labels
    return (cast(DecompReqArgs, req), capture.decomp_attr.range)


def _remove_decomp_reqs(
    iml: str,
    tree: Tree,
    captures: list[DecompCapture],
) -> tuple[str, Tree]:
    """Remove decomp requests from IML code."""
    decomp_attr_nodes = [capture.decomp_attr for capture in captures]
    new_iml, new_tree = delete_nodes(iml, tree, nodes=decomp_attr_nodes)
    return new_iml, new_tree


def extract_decomp_reqs(
    iml: str, tree: Tree
) -> tuple[str, Tree, list[DecompReqArgs], list[Range]]:
    root = tree.root_node
    matches = run_query(
        mk_query(DECOMP_QUERY_SRC),
        node=root,
    )

    decomp_captures = [
        DecompCapture.from_ts_capture(capture) for _, capture in matches
    ]

    req_and_range = [
        decomp_capture_to_req(capture) for capture in decomp_captures
    ]
    if not req_and_range:
        return iml, tree, [], []
    else:
        reqs, ranges = zip(*req_and_range)
    new_iml, new_tree = _remove_decomp_reqs(iml, tree, decomp_captures)
    return new_iml, new_tree, list(reqs), list(ranges)


def insert_decomp_req(
    iml: str,
    tree: Tree,
    req: DecompReqArgs,
) -> tuple[str, Tree]:
    func_def_node = find_func_definition(tree, req['name'])
    if func_def_node is None:
        raise ValueError(f'Function {req["name"]} not found in syntax tree')

    func_def_end_row = func_def_node.end_point[0]

    top_appl_text = decomp_req_to_top_appl_text(req)
    to_insert = f'[@@decomp {top_appl_text}]'

    new_iml, new_tree = insert_lines(
        iml,
        tree,
        lines=[to_insert],
        insert_after=func_def_end_row,
    )
    return new_iml, new_tree
