#!/bin/bash

# Define paths
ROOT_DIR=$(pwd)
BUILD_DIR="$ROOT_DIR/build"

# Create build directory
mkdir -p "$BUILD_DIR"

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

# Find all directories containing elm.json and sort them alphabetically
find . -type f -name "elm.json" -not -path "*/build/*" | sort | while read -r elm_json_path; do
    # Get the directory name
    dir_name=$(dirname "$elm_json_path" | sed 's/^\.\///')

    # Get project name from directory
    project_name=$(basename "$dir_name")

    # Create directory in build
    project_build_dir="$BUILD_DIR/$project_name"
    mkdir -p "$project_build_dir"

    # Change to the project directory
    cd "$ROOT_DIR/$dir_name"

    echo "Building $project_name..."

    # Compile Elm code and let it generate the HTML file
    elm make src/Main.elm --output="$project_build_dir/index.html" --optimize

    # Add link to main index.html
    echo "    <li><a href=\"$project_name/index.html\">$project_name</a></li>" >> "$BUILD_DIR/index.html"

    # Return to root directory
    cd "$ROOT_DIR"
done

# Close root index.html
cat >> "$BUILD_DIR/index.html" << EOF
    </ul>
</body>
</html>
EOF

echo "Build completed. The deployable structure is in the 'build' directory."
