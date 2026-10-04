#!/usr/bin/env bash
# Renderiza el libro en una carpeta temporal y reemplaza docs/ (GitHub Pages) por el resultado.
# Uso:  ./publicar.sh
set -euo pipefail
export LC_ALL=C.UTF-8 LANG=C.UTF-8   # tildes correctas al generar archivos
cd "$(dirname "$0")"
rm -rf _docs_nuevo
quarto render --to html --output-dir _docs_nuevo
touch _docs_nuevo/.nojekyll          # evita que Jekyll ignore carpetas con guion bajo
rm -rf docs
mv _docs_nuevo docs
echo "docs/ actualizado."
