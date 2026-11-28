module GraphViz exposing
    ( Graph, Node, Edge, Subgraph, Attribute
    , str
    , graphviz
    )

{-| Declarative Graphviz rendering using viz-js (trimmed copy of the module
in `bool-functions`). Always laid out with the `dot` engine.

The `graphviz-graph` custom element is defined in index.html. It renders
without a shadow DOM, so page CSS can style the SVG via `class` attributes.

@docs Graph, Node, Edge, Subgraph, Attribute
@docs str
@docs graphviz

-}

import Html exposing (Html, node)
import Html.Attributes exposing (attribute, class)
import Json.Encode as E


type alias Attribute =
    ( String, E.Value )


type alias Node =
    { name : String
    , attributes : List Attribute
    }


{-| An edge from `tail` to `head`.
-}
type alias Edge =
    { tail : String
    , head : String
    , attributes : List Attribute
    }


{-| A subgraph; name it `cluster_…` to get a labelled box around its nodes.
-}
type alias Subgraph =
    { name : String
    , graphAttributes : List Attribute
    , nodes : List Node
    }


type alias Graph =
    { graphAttributes : List Attribute
    , nodeAttributes : List Attribute
    , edgeAttributes : List Attribute
    , nodes : List Node
    , edges : List Edge
    , subgraphs : List Subgraph
    }


str : String -> E.Value
str =
    E.string


encodeAttributes : List Attribute -> E.Value
encodeAttributes =
    E.object


encodeNode : Node -> E.Value
encodeNode n =
    E.object [ ( "name", E.string n.name ), ( "attributes", encodeAttributes n.attributes ) ]


encodeEdge : Edge -> E.Value
encodeEdge e =
    E.object
        [ ( "tail", E.string e.tail )
        , ( "head", E.string e.head )
        , ( "attributes", encodeAttributes e.attributes )
        ]


encodeSubgraph : Subgraph -> E.Value
encodeSubgraph s =
    E.object
        [ ( "name", E.string s.name )
        , ( "graphAttributes", encodeAttributes s.graphAttributes )
        , ( "nodes", E.list encodeNode s.nodes )
        ]


encodeGraph : Graph -> E.Value
encodeGraph g =
    E.object
        [ ( "graphAttributes", encodeAttributes g.graphAttributes )
        , ( "nodeAttributes", encodeAttributes g.nodeAttributes )
        , ( "edgeAttributes", encodeAttributes g.edgeAttributes )
        , ( "nodes", E.list encodeNode g.nodes )
        , ( "edges", E.list encodeEdge g.edges )
        , ( "subgraphs", E.list encodeSubgraph g.subgraphs )
        ]


{-| Render a graph. The extra class goes on the custom element, for styling.
-}
graphviz : String -> Graph -> Html msg
graphviz cls graph =
    node "graphviz-graph"
        [ attribute "graph" (E.encode 0 (encodeGraph graph))
        , attribute "engine" "dot"
        , class cls
        ]
        []
