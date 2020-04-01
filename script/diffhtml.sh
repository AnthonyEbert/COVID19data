

hash=$(git rev-parse --short HEAD)
git diff --color-words="[^[:space:],]+" HEAD^ data-raw/* | ~/tools/ansi2html.sh > docs/diffs/${hash}.html
