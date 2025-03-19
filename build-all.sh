#!/bin/bash
set -euo -pipefail

ROOT_DIR=$(pwd)
BUILD_DIR="$ROOT_DIR/build"
mkdir -p "$BUILD_DIR"

# Define projects with descriptions
declare -A projects
projects=(
  ["bayes"]="Manipulable to explore Bayes' theorem"
  ["bezout"]="Euclid's Algoritm and Bezout's identity visualization"
  ["binomial"]="Binomial distribution calculator"
  ["congruence-equations"]="Linear congruence equation solver"
  ["conics"]="Interactive conic sections visualizer"
  ["covariance"]="Covariance intuition builder"
  ["elm-cube"]="3D cube visualization"
  ["hexagon"]="Pascal's triangle in hexagonal grid"
  ["IEEE-754"]="IEEE-754 floating point visualizer"
  ["sampling"]="Random sampling toy"
  ["sampling-weighted"]="Weighted random sampling toy"
)

# Initialize root index.html with header
cat > "$BUILD_DIR/index.html" << EOF
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Elm LLM Quickies</title>
    <style>
        body {
            font-family: sans-serif;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
        }
        h1 {
            color: #1293D8;
        }
        ul {
            list-style-type: none;
            padding: 0;
        }
        li {
            margin: 10px 0;
            padding: 10px;
            background-color: #f5f5f5;
            border-radius: 5px;
        }
        a {
            color: #1293D8;
            text-decoration: none;
            font-weight: bold;
        }
        a:hover {
            text-decoration: underline;
        }
    </style>
</head>
<body>
    <h1>Elm LLM Quickies</h1>
    <p>Bunch of experimental / toy projects created with heavy LLM assistance</p>
    <ul>
EOF

# Process each project in alphabetical order
for project_name in $(echo "${!projects[@]}" | tr ' ' '\n' | sort); do
    description="${projects[$project_name]}"

    if [ ! -d "$project_name" ]; then
        echo "Error: Directory $project_name not found"
        exit 1
    fi

    if [ ! -f "$project_name/elm.json" ]; then
        echo "Error: $project_name is not an Elm project (no elm.json)"
        exit 1
    fi

    project_build_dir="$BUILD_DIR/$project_name"
    mkdir -p "$project_build_dir"

    pushd "$ROOT_DIR/$project_name" || exit
    echo "Building $project_name: $description"
    elm make src/Main.elm --output="$project_build_dir/index.html" --optimize

    # Add link with description to main index.html
    cat >> "$BUILD_DIR/index.html" << EOF_LINK
    <li>
        <a href="$project_name/index.html">$project_name</a>
        <div class="description">$description</div>
    </li>
EOF_LINK

    popd || exit
done

# Close root index.html
cat >> "$BUILD_DIR/index.html" << EOF
    </ul>
</body>
</html>
EOF

echo "Build completed. The deployable structure is in the 'build' directory."
