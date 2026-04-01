# Pushout Visualization - Design Specification

A visualization of the pushout construction in category theory, using force-directed layout to display two views of the pushout in set theory with animated transitions between them.

## View 1: Four Quadrant View

Shows 4 sets arranged in quadrants of a rectangular screen:
- **A** (top-left): The source set
- **B1** (top-right): Target of function f: A → B1
- **B2** (bottom-left): Target of function g: A → B2
- **P** (bottom-right): The pushout of B1 ← A → B2

### Elements and Controls

- Elements are randomly generated in A, B1, and B2.
- Sliders control the number of elements in each set (range 0-10).
- Changing a slider updates elements live and generates new random functions A→B1 and A→B2.

### Function Editing

- Users can manually edit the functions by dragging and dropping arrowheads.
- While dragging, valid target nodes are highlighted to guide the user.
- Changes via drag-and-drop trigger immediate recalculation.

### Pushout Display

The pushout P is displayed as groups of "blobs" representing equivalence classes. Each equivalence class contains circles corresponding to the elements of B1 and B2 that are identified in the pushout. Circles are colored according to their source set (B1 or B2).

## View 2: Zoomed Pushout View

A zoomed-in view of the pushout P that fills the entire visual area (rather than just one quadrant).

Shows the same equivalence class blobs as View 1, but with the mapping arrows from A→B1 and A→B2 displayed within the blobs (with redundant injections from B1/B2 to P collapsed).

For example, if View 1 has arrows a1→b1→(b1 in a blob) and a1→b2→(b2 in the same blob), then View 2 shows a1→b1 and a1→b2 within the blob containing both b1 and b2.

## View Transitions

Two toggle buttons switch between the views. Clicking triggers an animated force simulation transition that moves elements and arrows slowly, allowing users to track how positions change during the transition.
