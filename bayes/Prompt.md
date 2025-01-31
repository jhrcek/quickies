
I would like to create an interactive elm application for exploring Bayes theorem and related concepts.

As a first step, I'd like you to implement the following sketch (see the attached image) of the UI using only base elm libraries (like elm/svg).

There should be a square representing probability space of a generic experiment.
There should be three sliders (it should be possible to drag and drop them along the sides of the square) in the form of triangles, that should control respectively:
P(A) - the slider on the bottom
P(B|A) - the slider on the left
P(B|not A) - the slider on the right.

Represent the probabilities in the model as 3 Floats.

On the right side of the svg image, show probabilities that I mentioned above and also probabilities derived from these (like P(not A), P(A|B), etc)

Output a single elm file and name it Main.elm

Please follow these rules when writing elm code:
1. Prefer using qualified names, except for type names. Here's an example of import style I like:
```elm
import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Json
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
```
2. Refrain from adding non-informative comments like this:

```elm
-- MODEL
...
--- VIEW
...
```
These are unfortunately quite popular in elm code on the Internet.
Only add comments if they are needed to explain particularly hairy piece of code or programmer's intention that would be
 otherwise hard to infer just from reading the code.
