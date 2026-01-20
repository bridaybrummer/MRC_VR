#!/bin/bash
# =============================================================================
# MRC VR Website Build Script
# =============================================================================
# Usage:
#   ./build.sh          - Full build (wrangling + render)
#   ./build.sh render   - Render only (skip wrangling)
#   ./build.sh wrangle  - Wrangling only (skip render)
#   ./build.sh preview  - Render and preview
# =============================================================================

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Functions
print_step() {
    echo -e "${GREEN}==>${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}Warning:${NC} $1"
}

print_error() {
    echo -e "${RED}Error:${NC} $1"
}

# Check if R is installed
check_r() {
    if ! command -v Rscript &> /dev/null; then
        print_error "R is not installed or not in PATH"
        exit 1
    fi
}

# Check if Quarto is installed
check_quarto() {
    if ! command -v quarto &> /dev/null; then
        print_error "Quarto is not installed or not in PATH"
        exit 1
    fi
}

# Run wrangling scripts
run_wrangling() {
    print_step "Running data wrangling scripts..."
    
    if [ -f "ME_wrangling.r" ]; then
        print_step "  Running ME_wrangling.r..."
        Rscript ME_wrangling.r || print_warning "ME_wrangling.r had issues (continuing)"
    fi
    
    if [ -f "plotting_DHA_offices.r" ]; then
        print_step "  Running plotting_DHA_offices.r..."
        Rscript plotting_DHA_offices.r || print_warning "plotting_DHA_offices.r had issues (continuing)"
    fi
    
    print_step "Wrangling complete!"
}

# Render the website
render_site() {
    print_step "Rendering Quarto website..."
    quarto render
    print_step "Website rendered to docs/"
}

# Preview the website
preview_site() {
    print_step "Starting preview server..."
    quarto preview
}

# Main
main() {
    check_r
    check_quarto
    
    case "${1:-full}" in
        "full")
            run_wrangling
            render_site
            ;;
        "render")
            render_site
            ;;
        "wrangle")
            run_wrangling
            ;;
        "preview")
            preview_site
            ;;
        *)
            echo "Usage: ./build.sh [full|render|wrangle|preview]"
            exit 1
            ;;
    esac
    
    echo ""
    print_step "Done!"
}

main "$@"
