module GraphViz exposing
    ( Graph, Node, Edge, Attribute, AttributeValue, Engine(..)
    , emptyGraph, simpleEdge, str, num
    , graphviz
    , simpleNode
    )

{-| Module for declarative Graphviz rendering using viz-js.

@docs Graph, Node, Edge, Attribute, AttributeValue, Engine
@docs emptyGraph, simpleEdge, str, num
@docs graphviz

-}

import Html exposing (Html, node)
import Html.Attributes exposing (attribute, style)
import Json.Encode as E



-- DATA STRUCTURES


{-| A value that can be used as an attribute value in Graphviz.
Supports strings, numbers, and booleans.
-}
type AttributeValue
    = StringValue String
    | NumberValue Float
    | BoolValue Bool


{-| Graphviz layout engine.
-}
type Engine
    = Dot
    | Circo


{-| A key-value pair representing a Graphviz attribute.
-}
type alias Attribute =
    ( String, AttributeValue )


{-| A node in the graph.
-}
type alias Node =
    { name : String
    , attributes : List Attribute
    }


{-| An edge connecting two nodes.
The edge goes from `tail` to `head`.
-}
type alias Edge =
    { tail : String
    , head : String
    , attributes : List Attribute
    }


{-| A complete Graphviz graph.
-}
type alias Graph =
    { name : Maybe String
    , directed : Bool
    , graphAttributes : List Attribute
    , nodeAttributes : List Attribute
    , edgeAttributes : List Attribute
    , nodes : List Node
    , edges : List Edge
    }



-- JSON ENCODING


encodeAttributeValue : AttributeValue -> E.Value
encodeAttributeValue value =
    case value of
        StringValue s ->
            E.string s

        NumberValue n ->
            E.float n

        BoolValue b ->
            E.bool b


encodeAttributes : List Attribute -> E.Value
encodeAttributes attrs =
    E.object (List.map (\( k, v ) -> ( k, encodeAttributeValue v )) attrs)


encodeNode : Node -> E.Value
encodeNode n =
    E.object
        (( "name", E.string n.name )
            :: (if List.isEmpty n.attributes then
                    []

                else
                    [ ( "attributes", encodeAttributes n.attributes ) ]
               )
        )


encodeEdge : Edge -> E.Value
encodeEdge e =
    E.object
        ([ ( "tail", E.string e.tail )
         , ( "head", E.string e.head )
         ]
            ++ (if List.isEmpty e.attributes then
                    []

                else
                    [ ( "attributes", encodeAttributes e.attributes ) ]
               )
        )


encodeGraph : Graph -> E.Value
encodeGraph g =
    E.object
        (List.filterMap identity
            [ Maybe.map (\n -> ( "name", E.string n )) g.name
            , if g.directed then
                Nothing

              else
                Just ( "directed", E.bool False )
            , nonEmptyList "graphAttributes" encodeAttributes g.graphAttributes
            , nonEmptyList "nodeAttributes" encodeAttributes g.nodeAttributes
            , nonEmptyList "edgeAttributes" encodeAttributes g.edgeAttributes
            , nonEmptyListOf "nodes" encodeNode g.nodes
            , nonEmptyListOf "edges" encodeEdge g.edges
            ]
        )


engineToString : Engine -> String
engineToString engine =
    case engine of
        Dot ->
            "dot"

        Circo ->
            "circo"


nonEmptyList : String -> (List a -> E.Value) -> List a -> Maybe ( String, E.Value )
nonEmptyList key encoder list =
    if List.isEmpty list then
        Nothing

    else
        Just ( key, encoder list )


nonEmptyListOf : String -> (a -> E.Value) -> List a -> Maybe ( String, E.Value )
nonEmptyListOf key encoder list =
    if List.isEmpty list then
        Nothing

    else
        Just ( key, E.list encoder list )



-- HELPER FUNCTIONS FOR BUILDING GRAPHS


{-| Create an empty graph with default settings (directed).
-}
emptyGraph : Graph
emptyGraph =
    { name = Nothing
    , directed = True
    , graphAttributes = []
    , nodeAttributes = []
    , edgeAttributes = []
    , nodes = []
    , edges = []
    }


{-| Create a simple edge with no attributes.
-}
simpleEdge : String -> String -> Edge
simpleEdge tail head =
    { tail = tail, head = head, attributes = [] }


{-| String attribute helper.
-}
str : String -> AttributeValue
str =
    StringValue


{-| Number attribute helper.
-}
num : Float -> AttributeValue
num =
    NumberValue


simpleNode : String -> Node
simpleNode name =
    { name = name, attributes = [] }



-- VIEW


{-| Custom HTML element for rendering Graphviz graphs.

This element is defined in index.html using viz-js.
It takes a JSON-encoded graph object as the `graph` attribute
and an `engine` attribute for the layout engine.

-}
graphviz : Engine -> Graph -> Html msg
graphviz engine graph =
    node "graphviz-graph"
        [ attribute "graph" (E.encode 0 (encodeGraph graph))
        , attribute "engine" (engineToString engine)
        , style "display" "block"
        ]
        []
