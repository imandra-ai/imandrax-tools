from imandrax_api_models import Location
from tree_sitter import Node


def loc_within_range(loc: Location, node: Node) -> bool:
    """Check if a 1-based eval-error Location falls inside a tree-sitter node's range."""
    if loc.start is None or loc.stop is None:
        return False
    # eval-error locs are 1-based; tree-sitter points are 0-based.
    start = (loc.start.line - 1, loc.start.col - 1)
    stop = (loc.stop.line - 1, loc.stop.col - 1)
    node_start = node.range.start_point
    node_end = node.range.end_point
    return (node_start <= start <= node_end) and (node_start <= stop <= node_end)
