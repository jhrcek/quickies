#!/bin/bash
set -euo pipefail

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
  ["eulers-totient"]="Toy game for practicing steps to calculate Euler's totient function"
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
        .description {
            margin-top: 5px;
            color: #555;
            font-style: italic;
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

    # Compile and optimize elm code to js
    js_file="$project_build_dir/elm.js"
    min_js_file="$project_build_dir/elm.min.js"

    elm make src/Main.elm --optimize --output="$js_file"

    # Minify the JS file
    echo "Minifying JavaScript for $project_name"
    uglifyjs "$js_file" --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output "$min_js_file"
    rm "$js_file"

    # Create simple index.html that imports the minified js file
    cat > "$project_build_dir/index.html" << EOF_HTML
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>$project_name</title>
    <meta name="description" content="$description">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>body{padding:0;margin:0;}</style>
</head>
<body>
    <div id="elm"></div>
    <script src="elm.min.js"></script>
    <script>
        var app = Elm.Main.init({
            node: document.getElementById('elm')
        });
    </script>
</body>
</html>
EOF_HTML

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
