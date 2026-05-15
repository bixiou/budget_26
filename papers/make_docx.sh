#!/usr/bin/env bash
# Génère papers/budget.docx à partir de budget.tex
# Usage: bash make_docx.sh  (exécuter depuis le dossier papers/)
set -euo pipefail
cd "$(dirname "$0")"

PNG_DIR="build/figures_png"
TMPFILE="build/budget_docx_tmp.tex"
mkdir -p "$PNG_DIR"

# ── 1. Convertir chaque figure PDF en PNG (150 dpi) ───────────────────
echo "=== Conversion des figures en PNG ==="
while IFS= read -r fname; do
    base="${fname%.pdf}"
    src="../figures/${base}.pdf"
    dst="${PNG_DIR}/${base}"
    if [[ -f "$src" ]]; then
        pdftoppm -png -r 150 "$src" "$dst"
        echo "  ✓ ${base}"
    else
        echo "  ✗ introuvable : $src"
    fi
done < <(grep -o '\.\./figures/[^}]*\.pdf' budget.tex | sed 's|\.\./figures/||' | sort -u)

# ── 2. Corriger les chemins des figures dans un .tex temporaire ────────
echo "=== Correction des chemins ==="
sed 's|\.\./figures/\([^}]*\)\.pdf|build/figures_png/\1-1.png|g' budget.tex > "$TMPFILE"

# ── 3. Pandoc LaTeX → DOCX ────────────────────────────────────────────
echo "=== Génération du DOCX ==="
pandoc "$TMPFILE" \
    --from=latex \
    --to=docx \
    --output=budget_full.docx \
    --bibliography=budget.bib \
    --filter pandoc-citeproc \
    --resource-path=. \
    2>&1 | grep -v '^$' || true

echo ""
echo "=== Terminé → papers/budget_full.docx ==="
